unit run_work;

interface

type
    TProcedure = reference to procedure;

function IsWorkRunning: boolean;
procedure RunWork(proc: TProcedure);

implementation

uses UnitKgsdumMainForm, windows, sysutils, Classes, errors;

type
    TWorkThread = class(TThread)
        FProc: TProcedure;
        procedure Execute; override;
    end;

var
    _flag_running: longint;

function IsWorkRunning: boolean;
begin
    result := AtomicIncrement(_flag_running, 0) = 1;
end;

procedure RunWork(proc: TProcedure);
var
    thr: TWorkThread;
begin
    thr := TWorkThread.Create(true);
    thr.FreeOnTerminate := true;
    thr.FProc := proc;
    thr.Start;
end;

procedure TWorkThread.Execute;
begin
    if IsWorkRunning then
        raise Exception.Create('already running');
    AtomicIncrement(_flag_running);
    Synchronize(KgsdumMainForm.OnStartWork);
    try

        FProc();
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
    AtomicDecrement(_flag_running);

end;

initialization

_flag_running := 0;

end.
