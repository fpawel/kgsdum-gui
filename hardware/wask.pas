unit wask;

interface

uses comport;

type
    TBytes = TArray<byte>;

    TKgsRequest = record
        DeviceAddr: byte;
        ValueAddr: byte;
        Direction: byte;
        Value: double;
        function Bytes: TBytes;
        function ParseResponse(response: TBytes): double;

        function FormatResponse(response: TBytes): string;

        constructor FromBytes(b: TBytes);

        function GetResponse(ComportWorker: TComportWorker): double;
        function toString: string;

    end;

const
    KgsWrite = $A0;
    KgsRead = $B0;

implementation

uses bcd, hardware_errors, sysutils, stringutils;

function TKgsRequest.toString: string;
var
    r: TKgsRequest;
begin
    result := 'адр.' + IntToStr(DeviceAddr) + ' ';
    if Direction = KgsWrite then
        result := result + 'WRITE '
    else
        result := result + 'READ ';
    result := result + 'var.' + IntToStr(ValueAddr);
    if Direction = KgsWrite then
        result := result + '=' + floattostr(Value);
end;

function TKgsRequest.FormatResponse(response: TBytes): string;
var
    r: TKgsRequest;
begin
    result := toString;

    if length(response) = 0 then
        exit;

    if Direction = KgsRead then
        try
            result := result + ' : ' + floattostr(ParseResponse(response));
        except
            result := result + ' : ?';
        end;
end;

function CRC16(const BS: TBytes; index_from: integer; index_to: integer): word;
var
    i, index_a: integer;
    b, b1, b3, b4: byte;

begin
    result := 0;
    if index_to < 0 then
        index_to := length(BS) - 1;

    for index_a := index_from to index_to do
    begin
        b := BS[index_a];
        for i := 0 to 7 do
        begin

            if i = 0 then
                b1 := b
            else
                b1 := b1 shl 1;

            if (b1 and $80) <> 0 then
                b3 := 1
            else
                b3 := 0;

            if (result and $8000) = $8000 then
                b4 := 1
            else
                b4 := 0;

            result := result shl 1;
            if b3 <> b4 then
                result := result xor $1021;

        end;

    end;

    result := result xor $FFFF;

end;

procedure SetBit(var b: byte; pos: integer; Value: boolean);
begin
    if Value then
        b := b or (1 shl pos)
    else
        b := b and (not(1 shl pos));

end;

function GetBit(Value: byte; Index: integer): boolean;
begin
    result := ((Value shr Index) and 1) = 1;
end;

procedure pack(var BS: TBytes);
var
    i, nbit, nbyte: integer;
const
    bx: array [0 .. 3] of byte = (2, 3, 4, 5);
    bi: array [0 .. 3] of byte = (3, 2, 1, 0);
begin
    for i := 0 to 3 do
    begin
        nbyte := bx[i];
        nbit := bi[i];
        SetBit(BS[1], nbit, GetBit(BS[1 + nbyte], 7));
        BS[1 + nbyte] := BS[1 + nbyte] and $7F;
    end;
end;

procedure unpack(var BS: TBytes);
var
    i, nbit, nbyte: integer;
const
    bx: array [0 .. 3] of byte = (2, 3, 4, 5);
    bi: array [0 .. 3] of byte = (3, 2, 1, 0);
begin
    for i := 0 to 3 do
    begin
        nbyte := bx[i];
        nbit := bi[i];
        SetBit(BS[nbyte + 1], 7, GetBit(BS[1 + 0], nbit));
        SetBit(BS[1], nbit, false)
    end;
end;

function TKgsRequest.Bytes: TBytes;
var
    crc: word;
begin
    setlength(result, 9);
    result[0] := byte(DeviceAddr);
    result[1] := Direction;
    result[2] := byte(ValueAddr);
    PutBCD6(result, Value, 3);
    pack(result);
    crc := CRC16(result, 1, 6);
    result[7] := crc shr 8;
    result[8] := crc;

end;

function TKgsRequest.ParseResponse(response: TBytes): double;
var
    crc: word;
begin
    if length(response) = 0 then
        raise EDeadlineExceeded.Create('нет ответа');

    if length(response) < 9 then
        raise EBadResponse.Create(Format('длина ответа %d менее 9',
          [length(response)]));

    crc := CRC16(response, 1, 6);

    if ((crc shr 8) <> response[7]) or (byte(crc) <> response[8]) then
        raise EBadResponse.Create(Format('не совпадает CRC ответа %s = %s',
          [BytesToHex([crc shr 8, byte(crc)]),
          BytesToHex([response[7], response[8]])]));

    if response[0] <> DeviceAddr then
        raise EBadResponse.Create
          (Format('не совпадает адрес платы стенда %d != %d',
          [response[0], DeviceAddr]));

    if (response[1] and $F0) <> Direction then
        raise EBadResponse.Create
          (Format('не совпадает код направления передачи %d != %d',
          [response[1] and $F0, Direction]));

    if response[2] <> ValueAddr then
        raise EBadResponse.Create(Format('не совпадает адрес значения %d != %d',
          [response[2], ValueAddr]));

    unpack(response);

    if not ParseBCD6(response, 3, result) then
        raise EBadResponse.Create('не верный код BCD: ' + BytesToHex(response,
          ' ', 3, 6));

    if (Direction = KgsWrite) and (Value <> result) then
        raise EBadResponse.Create
          (Format('записано не правильное значение, запрос %v, ответ %v',
          [Value, result]));
end;

function TKgsRequest.GetResponse(ComportWorker: TComportWorker): double;
var
    _result: double;
    _self: TKgsRequest;
begin
    _self := self;
    comport.GetResponse(Bytes, ComportWorker,
        procedure(response: TBytes)
        begin
            _result := _self.ParseResponse(response);
        end);
    result := _result;
end;

constructor TKgsRequest.FromBytes(b: TBytes);
var
    crc: word;
    a: TBytes;
    i: integer;
begin
    if length(b) <> 9 then
        exit;
    setlength(a, 9);
    for i := 0 to 8 do
        a[i] := b[i];
    unpack(a);

    DeviceAddr := a[0];
    ValueAddr := a[2];
    Direction := a[1];
    Value := 0;
    ParseBCD6(a, 3, Value);
end;

end.
