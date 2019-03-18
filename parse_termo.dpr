program parse_termo;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  StrUtils, Winapi.Windows, types;

procedure waitInput;
var
    InputBuff: TInputRecord;
    IEvent: DWord;
begin
    while InputBuff.EventType <> KEY_EVENT do
    begin
        SLeep(1);
        ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), InputBuff, 1, IEvent);
    end;
end;

var i:integer;
    xs :  TStringDynArray;

begin
    xs := SplitString('01RRD,OK,AC3F,F556', ',');
    for I := 0 to 3 do
        Writeln('"', xs[i], '"');
    waitInput;
end.
