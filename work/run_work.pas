unit run_work;

interface

uses comport, Classes;

type
    TWorkProcedure = reference to procedure;

function IsWorkRunning: boolean;
procedure RunWork(what: string; proc: TWorkProcedure);

function ComportProducts: THandle;

function ComportProductsWorker: TComportWorker;

procedure CancelExecution;

procedure Synchronize(p: TThreadProcedure); overload;
procedure Synchronize(m: TThreadMethod); overload;

var
    ComportProductsConfig, ComportGasConfig, ComportTermoConfig
      : TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, sysutils, errors,
    UnitFormProperties, UnitFormConsole;

type
    TWorkThread = class(TThread)
        FProc: TWorkProcedure;
        procedure Execute; override;
    end;

var
    _flag_running, _flag_canceled: longint;
    _hComportProducts, _hComportTermo: THandle;
    _thread: TWorkThread;
    _work: string;

procedure Synchronize(p: TThreadProcedure);
begin
    _thread.Synchronize(p);
end;

procedure Synchronize(m: TThreadMethod); overload;
begin
    _thread.Synchronize(m);
end;

procedure CancelExecution;
begin
    AtomicExchange(_flag_canceled, 1);
end;

procedure _on_comport_background;
begin
    if AtomicIncrement(_flag_canceled, 0) = 1 then
        raise EAbort.Create('выполнение прервано');
    sleep(10);
end;

function ComportProductsWorker: TComportWorker;
begin
    result := TComportWorker.Create(ComportProducts,
      TConfigGetResponse.Create(1000, 50, 3), _on_comport_background);
end;

function ComportProducts: THandle;
var
    comportName: string;
begin
    if _hComportProducts <> INVALID_HANDLE_VALUE then
        exit(_hComportProducts);

    Synchronize(
        procedure
        begin
            comportName := FormProperties.ComportProducts.Value;
        end);

    try
        _hComportProducts := comport.OpenComport(comportName, 9600);
        exit(_hComportProducts);

    except
        _hComportProducts := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function IsWorkRunning: boolean;
begin
    result := AtomicIncrement(_flag_running, 0) = 1;
end;

procedure RunWork(what: string; proc: TWorkProcedure);
begin
    if IsWorkRunning then
        raise Exception.Create('already running');
    FormConsole.AddLine(loglevInfo, what + ' : начало выполнения');
    _work := what;
    KgsdumMainForm.LabelStatusTop.Caption := what;
    _thread := TWorkThread.Create(true);
    _thread.FreeOnTerminate := true;
    _thread.FProc := proc;
    _thread.Start;

end;

procedure CloseComport(var hPort: THandle);
begin
    if hPort <> INVALID_HANDLE_VALUE then
    begin
        CloseHandle(hPort);
        hPort := INVALID_HANDLE_VALUE;
    end;
end;

procedure TWorkThread.Execute;
begin
    AtomicExchange(_flag_running, 1);
    AtomicExchange(_flag_canceled, 0);
    Synchronize(KgsdumMainForm.OnStartWork);
    try
        FProc;
    except
        on e: EAbort do
        begin
            // if _work <> 'опрос' then
            Synchronize(
                procedure
                begin
                    FormConsole.AddLine(loglevWarn,
                      format('%s: выполнение прервано', [_work]));
                end);

        end;
        on e: Exception do
        begin
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.AppException(nil, e);
                    FormConsole.AddLine(loglevError, format('%s: %s, %s',
                      [_work, e.ClassName, e.Message]));
                end);
        end;
    end;

    Synchronize(
        procedure
        begin
            KgsdumMainForm.OnStopWork;
            FormConsole.AddLine(loglevInfo, _work + ': выполнено');
        end);

    CloseComport(_hComportProducts);
    CloseComport(_hComportTermo);
    AtomicExchange(_flag_running, 0);
    _work := '';

end;

initialization

_thread := nil;
_flag_running := 0;
_hComportProducts := INVALID_HANDLE_VALUE;
_hComportTermo := INVALID_HANDLE_VALUE;
_work := '';

ComportProductsConfig := TConfigGetResponse.Create(500, 30, 2);
ComportGasConfig := TConfigGetResponse.Create(500, 30, 2);
ComportTermoConfig := TConfigGetResponse.Create(1000, 50, 3);

comport.SetcomportLogHook(
    procedure(r: TComportLogEntry)
    begin
        Synchronize(
            procedure
            var
                comport: string;
            begin
                comport := 'COM?';
                if r.HComport = _hComportProducts then
                    comport := FormProperties.ComportProducts.Value + '-стенд'
                else if r.HComport = _hComportTermo then
                    comport := FormProperties.ComportTermo.Value +
                      '-термокамера';
                FormConsole.AddComportMessage(comport, r.Request, r.Response,
                  r.Millis, r.Attempt);
            end);
    end);

end.
