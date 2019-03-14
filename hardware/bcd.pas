unit bcd;

interface

procedure PutBCD6(var b: TArray<byte>; x: double; index0: integer);
function ParseBCD6(b: TArray<byte>; Index: integer; var r: double): boolean;

implementation

uses sysutils, math;

procedure PutBCD6(var b: TArray<byte>; x: double; index0: integer);
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

function ParseBCD6(b: TArray<byte>; Index: integer; var r: double): boolean;
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

end.
