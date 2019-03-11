unit run_work;

interface

uses comport,Classes;

type
    TSyncP = reference to procedure(m: TThreadProcedure);

    TWorkProcedure = reference to procedure(_: TSyncP);

    TConfigComports = record
        Products, Termo, Gas: TConfigGetResponse;
    end;

function IsWorkRunning: boolean;
procedure RunWork(what:string; proc: TWorkProcedure);

function ComportProducts(sync: TSyncP): THandle;

function ComportProductsWorker(sync: TSyncP): TComportWorker;

procedure CancelExecution;

var ComportProductsConfig, ComportTermoConfig: TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, sysutils, errors,
    UnitFormProperties;

type
    TWorkThread = class(TThread)
    private
        FProc: TWorkProcedure;
        procedure Execute; override;
    public
        procedure SyncM(m: TThreadMethod);
        procedure SyncP(m: TThreadProcedure);
    end;



var
    _flag_running: longint;
    _hComportProducts: THandle;
    _flag_canceled: longint;


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


function ComportProductsWorker(sync: TSyncP): TComportWorker;
var
    w: TComportWorker;
    proc: TBackgroundWork;
begin
    proc := _on_comport_background;
    sync(
        procedure
        begin
            w := TComportWorker.Create(ComportProducts(sync),
              TConfigGetResponse.Create(1000, 50, 3), proc);
        end);
    result := w;

end;

function ComportProducts(sync: TSyncP): THandle;
var
    comportName: string;
begin
    if _hComportProducts <> INVALID_HANDLE_VALUE then
        exit(_hComportProducts);

    sync(
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

procedure RunWork(what:string; proc: TWorkProcedure);
var
    thr: TWorkThread;
begin
    KgsdumMainForm.LabelStatusTop.Caption := what;

    thr := TWorkThread.Create(true);
    thr.FreeOnTerminate := true;
    thr.FProc := proc;
    thr.Start;
end;

procedure TWorkThread.SyncM(m: TThreadMethod);
begin
    Synchronize(m);
end;

procedure TWorkThread.SyncP(m: TThreadProcedure);
begin
    Synchronize(m);
end;



procedure TWorkThread.Execute;
begin
    if IsWorkRunning then
        raise Exception.Create('already running');
    AtomicIncrement(_flag_running);
    AtomicExchange(_flag_canceled, 0);
    Synchronize(KgsdumMainForm.OnStartWork);

    try

        FProc(SyncP);
    except
        on e: EAbort do;
        on e: Exception do
        begin
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.AppException(nil, e);
                end);

        end;
    end;
    Synchronize(KgsdumMainForm.OnStopWork);
    if _hComportProducts <> INVALID_HANDLE_VALUE then
    begin
        CloseHandle(_hComportProducts);
        _hComportProducts := INVALID_HANDLE_VALUE;

    end;


    AtomicDecrement(_flag_running);

end;

initialization

_flag_running := 0;
_hComportProducts := INVALID_HANDLE_VALUE;

ComportProductsConfig := TConfigGetResponse.Create(500, 30, 2);
ComportTermoConfig := TConfigGetResponse.Create(1000, 50, 3);

end.
