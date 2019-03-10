program test_comport;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,
  comport in 'hardware\comport.pas',
  stringutils in 'utils\stringutils.pas',
  errors in 'errors.pas',
  kgs in 'hardware\kgs.pas';

var
    hPort: THandle;
    _canceled: boolean;
    response: TArray<byte>;

procedure BackgroundWork;
begin
    if _canceled then
        raise ECanceled.Create('выполнение перервано');
    SLeep(10);
end;

function parse_response(reques: TArray<byte>): string;
begin
    exit('');
end;

procedure _do(b: TArray<byte>);
var
    t: Cardinal;
    connection_error: string;

begin
    t := GetTickCount;
    response := comport.GetResponse(b, TComportWorker.Create(hPort,
      TConfigGetResponse.Create(1000, 20, 0),
        procedure
        begin
            SLeep(1);
        end),
        procedure(response: TBytes)
        begin
            if Length(response) <> Length(b) then
                raise EBadResponse.Create('не прокатит');
        end);
    Writeln(BytesToHex(b) + ' --> ' + BytesToHex(response), ' time:',
      GetTickCount - t);
    SLeep(1);
end;

procedure waitConsoleInput;
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



begin
    try
        _canceled := false;
        hPort := OpenComport('COM3', 115200);

        _do([$11, $12, $13]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _do([$14, $15, $16, $18]);
        _canceled := true;
        _do([$14, $15, $16, $17]);

    except
        on E: Exception do
            Writeln(E.ClassName, ': ', E.Message);
    end;
    Writeln(Format('--%s--', ['']));

    waitConsoleInput;

end.
