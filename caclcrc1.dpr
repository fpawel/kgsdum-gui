program caclcrc1;

{$APPTYPE CONSOLE}
{$R *.res}

uses
    System.SysUtils;

var
    summa_crc: word;
    data_crc_in: TArray<byte>;
    data_crc_out: TArray<byte>;
    tmp_str: string;

procedure Find_CRC_left(data_in: byte);
// рассчет CRC-16 (сдвиг осуществляется влево)
var
    i: integer;
    bit1, bit3, bit4: byte;
begin
    i := 0;
    while i < 8 do
    begin
        if i = 0 then
            bit1 := data_in
        else
            bit1 := bit1 shl 1;
        if (bit1 and $80) = $80 then
            bit3 := 1
        else
            bit3 := 0;
        if (summa_crc and $8000) = $8000 then
            bit4 := 1
        else
            bit4 := 0;
        summa_crc := summa_crc shl 1;
        if (bit3 <> bit4) then
            summa_crc := summa_crc xor $1021; // $1021 - полином CRC
        inc(i);
    end;
end;

procedure Find_CRC_In;
var
    i: integer;
begin
    i := 0;
    summa_crc := $0000;
    while i < 6 do
    begin
        Find_CRC_left(data_crc_in[i]);
        inc(i);
    end;
    summa_crc := summa_crc xor $FFFF;
    data_crc_in[6] := summa_crc shr 8;
    data_crc_in[7] := summa_crc and $00FF;
end;

procedure Find_CRC_Out;
var
    i: integer;
begin
    i := 0;
    summa_crc := $0000;
    while i < 6 do
    begin
        Find_CRC_left(data_crc_out[i]);
        inc(i);
    end;
    summa_crc := summa_crc xor $FFFF;
    data_crc_out[6] := summa_crc shr 8;
    data_crc_out[7] := summa_crc and $00FF;
end;

procedure PrintCRC(xs: TArray<byte>);
begin
    data_crc_out := xs;
    Find_CRC_Out;
    Writeln(IntToHex(summa_crc, 4));
end;

begin
    // data_crc_out := [$B0, $3D, $05, $38, $96, $41, 0, 0];
    // data_crc_out := [$B0, $3D, $05, $38, $96, $43, 0, 0];
    PrintCRC([$B0, $48, $06, $01, $16, $57, 0, 0]);

    ReadLn(tmp_str);

end.
