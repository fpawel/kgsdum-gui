unit stringutils;

interface


function str_validate_decimal_separator(s: string): string;
function str_to_float(s: string): Double;
function str_replace_unicode_chars(s: string): string;
function inttostr2(n: integer): string;
function float_to_str(v: double): string;

function month_name(month_number:integer):string;
function try_str_to_float(s:string; var v:double):boolean;

function BytesToHex(BA: TArray<byte>; Sep: string = ' ';
    index_from : integer = -1;
    index_to : integer = -1
    ): string;

function MillisecondsToStr(ms:cardinal):string;

implementation


uses SysUtils, dateutils;

function MillisecondsToStr(ms:cardinal):string;
begin
    result := TimeToStr(IncMilliSecond(0, ms));
end;

function month_name(month_number:integer):string;
begin
    Result := FormatDateTime('mmmm', EncodeDateTime(2000, month_number, 1, 0, 0, 0, 0));
end;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

function str_replace_unicode_chars(s: string): string;
begin
    s := StringReplace(s, '₄', '_4_', [rfReplaceAll]);
    s := StringReplace(s, '₂', '_2_', [rfReplaceAll]);
    s := StringReplace(s, '₃', '_3_', [rfReplaceAll]);
    s := StringReplace(s, '₈', '_8_', [rfReplaceAll]);
    s := StringReplace(s, '∑', '_sum_', [rfReplaceAll]);
    exit( s);


end;

function try_str_to_float(s:string; var v:double):boolean;
begin
    result := TryStrToFloat(str_validate_decimal_separator(s), v);

end;

function str_to_float(s: string): Double;
begin
    exit( StrToFloat(str_validate_decimal_separator(s)) );
end;

function str_validate_decimal_separator(s: string): string;
var
    i: integer;
begin
    for i := 1 to length(s) do
        if (s[i] = '.') or (s[i] = ',') then
            s[i] := FormatSettings.DecimalSeparator;
    exit(s);
end;


function float_to_str(v: double): string;
var
    i: integer;
begin
    result := FloatToStr(v);
    for i := 1 to length(Result) do
        if result[i] = ',' then
           result[i] := '.';
end;

function BytesToHex(BA: TArray<byte>; Sep: string;
    index_from : integer;
    index_to : integer
    ): string;
var
    i, k: integer;
begin
    result := '';

    if index_from = -1 then
        index_from := low(BA);
    if index_to = -1 then
        index_to := high(BA);




    if Sep = '' then
    begin
        for i := index_from to index_to do
            result := result + IntToHex(BA[i], 2);
    end
    else
    begin
        k := index_to;
        for i := index_from to k do
        begin
            result := result + IntToHex(BA[i], 2);
            if k <> i then
                result := result + Sep;
        end;
    end;
end;

end.
