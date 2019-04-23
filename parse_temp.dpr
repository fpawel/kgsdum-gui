program parse_temp;

{$APPTYPE CONSOLE}
{$R *.res}

uses
    System.SysUtils;


function myHexToInt(s:string):integer;
begin
    if (s[1] = 'F') or (s[1] = 'f') then
        s := '$FFFF'+s
    else
        s := '$'+s;
    exit(StrToInt(s));
end;

var
    s: string;
    n:integer;

begin
    Writeln(IntToHex(-5, 2));
    Writeln(IntToHex(5,2));
    Writeln( StrToInt('$F000') );
     Writeln( myHexToInt('FFFB') );
    ReadLn(s);

end.
