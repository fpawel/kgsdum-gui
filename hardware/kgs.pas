unit kgs;

interface

uses comport;

type
    KgsDeviceAddr = type byte;
    KgsCommandCode = type byte;
    KgsValueAddr = type byte;
    KgsIODir = type byte;
    KgsCoefficient = type byte;
    TBytes = TArray<byte>;

function bytes_arrays_equal(a, b: TBytes): boolean;

function KgsReadCoefficient(DeviceAddr: KgsDeviceAddr;
  coefficient: KgsCoefficient; w: TComportWorker): double;

procedure KgsWriteCoefficient(DeviceAddr: KgsDeviceAddr;
  coefficient: KgsCoefficient; Value: double; w: TComportWorker);

procedure KgsWriteVar(DeviceAddr: KgsDeviceAddr; ValueAddr: KgsValueAddr;
  Value: double; w: TComportWorker);

function KgsReadVar(DeviceAddr: KgsDeviceAddr; ValueAddr: KgsValueAddr;
  w: TComportWorker): double;

implementation

uses math, sysutils, stringutils, errors;

type
    KgsRequest = record
        DeviceAddr: KgsDeviceAddr;
        ValueAddr: KgsValueAddr;
        Direction: KgsIODir;
        Value: double;
        function Bytes: TBytes;
        function ParseResponse(response: TBytes): double;

    end;

const
    KgsWrite: KgsIODir = $A0;
    KgsRead: KgsIODir = $B0;

function CRC16(const BS: TBytes; index_from: integer; index_to: integer): word;
var
    i, index_a: integer;
    b, b1, b3, b4: byte;

begin
    result := 0;
    if index_to < 0 then
        index_to := Length(BS) - 1;

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

procedure PutBCD6(var b: TBytes; x: double; index0: integer);
var
    i: integer;
    v: longint;
const
    bcdComa: array [0 .. 6] of double = (0, 1, 10, 100, 1000, 10000, 100000);
begin
    if x < 0 then
        b[index0 + 0] := b[index0 + 0] or $80;

    x := Abs(x);

    for i := 0 to 5 do
        if (x >= bcdComa[i]) and (x < bcdComa[i + 1]) then
        begin
            b[index0 + 0] := b[index0 + 0] or (6 - i);
            break
        end;

    if x < 1 then
        x := x * 1000000
    else
        while x < 100000 do
            x := x * 10;

    v := Round(x);
    b[index0 + 1] := (v div 100000) shl 4;
    v := v mod 100000;
    b[index0 + 1] := b[index0 + 1] + (v div 10000);
    v := v mod 10000;
    b[index0 + 2] := (v div 1000) shl 4;
    v := v mod 1000;
    b[index0 + 2] := b[index0 + 2] + (v div 100);
    v := v mod 100;
    b[index0 + 3] := (v div 10) shl 4;
    v := v mod 10;
    b[index0 + 3] := b[index0 + 3] + v;

end;

function dec2(b: byte; var v1: double; var v2: double): boolean;
begin
    v1 := b shr 4;
    v2 := b and $F;
    result := (v1 > -1) and (v2 > -1) and (v1 < 10) and (v2 < 10);
end;

function ParseBCD6(b: TBytes; Index: integer; var r: double): boolean;
var
    sign, coma, x, y: double;
begin
    if index + 4 >= Length(b) then
        raise Exception.Create('out of range');

    if not dec2(b[index + 1], x, y) then
        exit(false);
    r := r + 100000 * x + 10000 * y;

    if not dec2(b[index + 2], x, y) then
        exit(false);

    r := r + 1000 * x + 100 * y;

    if not dec2(b[index + 3], x, y) then
        exit(false);

    r := r + 10 * x + y;

    coma := b[index + 0] and $7;

    sign := -1;
    if b[index + 0] shr 7 = 0 then
        sign := 1;

    r := r * sign / (math.Power(10, coma));
    exit(true);

end;

function KgsRequest.Bytes: TBytes;
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

function KgsRequest.ParseResponse(response: TBytes): double;
var
    crc: word;
begin
    if Length(response) = 0 then
        raise EDeadlineExceeded.Create('нет ответа');

    if Length(response) < 9 then
        raise EBadResponse.Create(Format('длина ответа %d менее 9',
          [Length(response)]));

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
        raise EBadResponse.Create(Format('не верный код BCD %s',
          [BytesToHex([response[3], response[4], response[5], response[6]])]));

    if (Direction = KgsWrite) and (Value <> result) then
        raise EBadResponse.Create
          (Format('записано не правильное значение, запрос %v, ответ %v',
          [Value, result]));

end;

function bytes_arrays_equal(a, b: TBytes): boolean;
var
    i: integer;
begin
    if Length(a) <> Length(b) then
        exit(false);
    for i := 0 to Length(a) - 1 do
        if a[i] <> b[i] then
            exit(false);
    exit(true);

end;

function KgsGetResponse(r: KgsRequest; w: TComportWorker): double;
var
    _result: double;
begin
    comport.GetResponse(r.Bytes, w,
        procedure(response: TBytes)
        begin
            _result := r.ParseResponse(response);
        end);
    result := _result;
end;

function KgsReadVar(DeviceAddr: KgsDeviceAddr; ValueAddr: KgsValueAddr;
w: TComportWorker): double;
var
    r: KgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := ValueAddr;
    r.Direction := KgsRead;
    result := KgsGetResponse(r, w);
end;

procedure KgsWriteVar(DeviceAddr: KgsDeviceAddr; ValueAddr: KgsValueAddr;
Value: double; w: TComportWorker);
var
    r: KgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsWrite;
    r.Value := Value;
    KgsGetResponse(r, w);
end;

function KgsReadCoefficient(DeviceAddr: KgsDeviceAddr;
coefficient: KgsCoefficient; w: TComportWorker): double;
var
    r: KgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r, w);
    result := KgsReadVar(DeviceAddr, ceil(1. * coefficient - r.Value), w);
end;

procedure KgsWriteCoefficient(DeviceAddr: KgsDeviceAddr;
coefficient: KgsCoefficient; Value: double; w: TComportWorker);
var
    r: KgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r, w);

    r.ValueAddr := ceil(1. * coefficient - r.Value);
    r.Value := Value;
    r.Direction := KgsWrite;
    KgsGetResponse(r, w);

end;

procedure TestRequestBytes;
var
    r: KgsRequest;
    b: TArray<byte>;
    s: string;
begin
    b := [$64, $A9, $64, $03, $15, $65, $09, $90, $56];
    r.DeviceAddr := 100;
    r.ValueAddr := 100;
    r.Direction := KgsWrite;
    r.Value := -156.589;
    Writeln(bytes_arrays_equal(r.Bytes, b));
    Writeln(BytesToHex(r.Bytes), ' -- ', BytesToHex(b));
    Writeln(floattostr(r.ParseResponse(r.Bytes)));

end;

end.
