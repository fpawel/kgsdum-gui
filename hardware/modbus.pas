unit modbus;

interface

uses comport;

type
    TBytes = TArray<byte>;

    TParseBytes = reference to procedure(_: TBytes);

    TModbusRequest = record
        Addr: byte;
        Cmd: byte;
        Data: TArray<byte>;
        function Bytes: TBytes;
        procedure CheckResonse(response: TBytes);
        function GetResponse(w: TComportWorker): TBytes;
    end;

implementation

uses errors, sysutils, stringutils;

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

procedure GetResponse(request: TModbusRequest; w: TComportWorker;
  var response: TBytes);
begin
    comport.GetResponse(request.Bytes, w,
        procedure(_response: TBytes)
        begin
            request.CheckResonse(_response);
            response := _response;
        end);
end;

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

procedure TModbusRequest.CheckResonse(response: TBytes);
var
    crc: word;
begin
    if Length(response) = 0 then
        raise EDeadlineExceeded.Create('��� ������');

    if Length(response) < 4 then
        raise EBadResponse.Create(Format('����� ������ %d ����� 9',
          [Length(response)]));

    crc := CRC16(response, 0, Length(response));

    if crc <> 0 then
        raise EBadResponse.Create('CRC16 �� ����');

    if response[0] <> Addr then
        raise EBadResponse.Create
          (Format('�� ��������� ����� ������ %d � ������� %d',
          [response[0], Addr]));

    if (Length(response) = 5) ANd ((Cmd or $80) = response[1]) then
        raise EBadResponse.Create('������ ������ ��� ������ ' +
          inttostr(response[2]));

    if response[1] <> Cmd then
        raise EBadResponse.Create
          (Format('�� ��������� ��� ������� ������ %d � ������� %d',
          [response[1], Cmd]));

end;

function TModbusRequest.Bytes: TBytes;
var
    i, n: integer;
    _crc16: word;
begin
    SetLength(result, 4 + Length(Data));
    result[0] := Addr;
    result[1] := Cmd;
    for i := 0 to Length(Data) - 1 do
        result[2 + i] := Data[i];
    n := 2 + Length(Data);
    _crc16 := CRC16(result, 0, n - 1);
    result[n] := _crc16 shr 8;
    result[n + 1] := _crc16;
end;

end.
