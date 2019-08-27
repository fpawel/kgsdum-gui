unit modbus;

interface

uses comport;

type
    TBytes = TArray<byte>;

function GetResponse(addr, cmd: byte; data: TBytes; w: TComportWorker;
  FParse: TParseResponse): TBytes;
function CRC16(bs: TBytes; index_from, index_to: integer): word;
function Read3(addr: byte; AVar: word; w: TComportWorker): double;

implementation

uses hardware_errors, sysutils, stringutils, bcd;

type
    TModbusRequest = record
        addr: byte;
        cmd: byte;
        data: TArray<byte>;
        function Bytes: TBytes;
        procedure CheckResponse(response: TBytes);
    end;

function Read3(addr: byte; AVar: word; w: TComportWorker): double;
var
    _result: double;
begin
    GetResponse(addr, 3, [AVar shr 8, AVar, 0, 2], w,
        procedure(response: TBytes)
        var
            lenMustBe: integer;
        begin
            lenMustBe := 2 * 2 + 5;
            if length(response) <> lenMustBe then
                raise EBadREsponse.Create(Format('длина ответа %d не равна %d',
                  [length(response), lenMustBe]));

            if not ParseBCD6(response, 3, _result) then
                raise EBadREsponse.Create('не верный код BCD: ' +
                  BytesToHex(response, ' ', 3, 6));
        end);
    result := _result;
end;

procedure TModbusRequest.CheckResponse(response: TBytes);
var
    crc: word;
begin
    if length(response) = 0 then
        raise EDeadlineExceeded.Create('нет ответа');

    if length(response) < 4 then
        raise EBadREsponse.Create(Format('длина ответа %d менее 9',
          [length(response)]));

    crc := CRC16(response, 0, length(response)-1);

    if crc <> 0 then
        raise EBadREsponse.Create('CRC16 не ноль');

    if response[0] <> addr then
        raise EBadREsponse.Create
          (Format('не совпадает адрес ответа %d и запроса %d',
          [response[0], addr]));

    if (length(response) = 5) ANd ((cmd or $80) = response[1]) then
        raise EBadREsponse.Create('прибор вернул код ошибки ' +
          inttostr(response[2]));

    if response[1] <> cmd then
        raise EBadREsponse.Create
          (Format('не совпадает код клманды ответа %d и запроса %d',
          [response[1], cmd]));
end;

function _GetResponse(request: TModbusRequest; w: TComportWorker;
FParse: TParseResponse): TBytes;
var
    _result: TBytes;
begin
    comport.GetResponse(request.Bytes, w,
        procedure(response: TBytes)
        begin
            request.CheckResponse(response);
            FParse(response);
            _result := response;
        end);
    result := _result;
end;

function GetResponse(addr, cmd: byte; data: TBytes; w: TComportWorker;
FParse: TParseResponse): TBytes;
var
    request: TModbusRequest;
begin
    request.addr := addr;
    request.cmd := cmd;
    request.data := data;
    result := _GetResponse(request, w, FParse);
end;

function TModbusRequest.Bytes: TBytes;
var
    i, n: integer;
    _crc16: word;
begin
    SetLength(result, 4 + length(data));
    result[0] := addr;
    result[1] := cmd;
    for i := 0 to length(data) - 1 do
        result[2 + i] := data[i];
    n := 2 + length(data);
    _crc16 := CRC16(result, 0, n - 1);
    result[n] := _crc16 shr 8;
    result[n + 1] := _crc16;
end;

const
    auchCRCHi: TBytes = [$00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80,
      $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1,
      $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00,
      $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
      $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81,
      $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0,
      $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
      $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41,
      $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80,
      $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80, $41, $00, $C1,
      $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00,
      $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40,
      $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01, $C0, $80,
      $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80, $41, $00, $C1,
      $81, $40, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40, $01,
      $C0, $80, $41, $01, $C0, $80, $41, $00, $C1, $81, $40, $00, $C1, $81, $40,
      $01, $C0, $80, $41, $00, $C1, $81, $40, $01, $C0, $80, $41, $01, $C0, $80,
      $41, $00, $C1, $81, $40];

    auchCRCLo: TBytes = [$00, $C0, $C1, $01, $C3, $03, $02, $C2, $C6, $06, $07,
      $C7, $05, $C5, $C4, $04, $CC, $0C, $0D, $CD, $0F, $CF, $CE, $0E, $0A, $CA,
      $CB, $0B, $C9, $09, $08, $C8, $D8, $18, $19, $D9, $1B, $DB, $DA, $1A, $1E,
      $DE, $DF, $1F, $DD, $1D, $1C, $DC, $14, $D4, $D5, $15, $D7, $17, $16, $D6,
      $D2, $12, $13, $D3, $11, $D1, $D0, $10, $F0, $30, $31, $F1, $33, $F3, $F2,
      $32, $36, $F6, $F7, $37, $F5, $35, $34, $F4, $3C, $FC, $FD, $3D, $FF, $3F,
      $3E, $FE, $FA, $3A, $3B, $FB, $39, $F9, $F8, $38, $28, $E8, $E9, $29, $EB,
      $2B, $2A, $EA, $EE, $2E, $2F, $EF, $2D, $ED, $EC, $2C, $E4, $24, $25, $E5,
      $27, $E7, $E6, $26, $22, $E2, $E3, $23, $E1, $21, $20, $E0, $A0, $60, $61,
      $A1, $63, $A3, $A2, $62, $66, $A6, $A7, $67, $A5, $65, $64, $A4, $6C, $AC,
      $AD, $6D, $AF, $6F, $6E, $AE, $AA, $6A, $6B, $AB, $69, $A9, $A8, $68, $78,
      $B8, $B9, $79, $BB, $7B, $7A, $BA, $BE, $7E, $7F, $BF, $7D, $BD, $BC, $7C,
      $B4, $74, $75, $B5, $77, $B7, $B6, $76, $72, $B2, $B3, $73, $B1, $71, $70,
      $B0, $50, $90, $91, $51, $93, $53, $52, $92, $96, $56, $57, $97, $55, $95,
      $94, $54, $9C, $5C, $5D, $9D, $5F, $9F, $9E, $5E, $5A, $9A, $9B, $5B, $99,
      $59, $58, $98, $88, $48, $49, $89, $4B, $8B, $8A, $4A, $4E, $8E, $8F, $4F,
      $8D, $4D, $4C, $8C, $44, $84, $85, $45, $87, $47, $46, $86, $82, $42, $43,
      $83, $41, $81, $80, $40];

function CRC16(bs: TBytes; index_from, index_to: integer): word;
var
    b: byte;
    hi, lo: word;
    n, i: integer;

begin
    hi := $FF;
    lo := $FF;
    for n := index_from to index_to do
    begin
        b := bs[n];

        i := integer(hi xor b);
        hi := lo xor auchCRCHi[i];
        lo := auchCRCLo[i];
    end;
    result := (hi shl 8) + lo;
end;

end.
