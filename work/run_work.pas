unit run_work;

interface

uses comport, Classes, data_model;

type
    TWorkProcedure = reference to procedure;

    TWork = record
        Name: string;
        Proc: TWorkProcedure;
        constructor Create(AName: string; AProc: TWorkProcedure);
    end;

    TWorks = TArray<TWork>;

function IsWorkRunning: boolean;
procedure RunWorks(works: TWorks);

function ComportProducts: THandle;

function ComportProductsWorker: TComportWorker;

procedure CancelExecution;

procedure Synchronize(p: TThreadProcedure); overload;
procedure Synchronize(m: TThreadMethod); overload;
procedure DoEachProduct(func: TProductProcedure);

var
    ComportProductsConfig, ComportGasConfig, ComportTermoConfig
      : TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, sysutils, errors,
    FireDAC.Comp.Client, UnitFormProperties, UnitFormLastParty, UnitKgsdumData,
    stringutils, UnitFormJournal;

type
    TWorkThread = class(TThread)
        FWorks: TWorks;
        procedure Execute; override;
    end;

var
    _flag_running, _flag_canceled: longint;
    _hComportProducts, _hComportTermo: THandle;
    _thread: TWorkThread;

constructor TWork.Create(AName: string; AProc: TWorkProcedure);
begin
    Name := AName;
    Proc := AProc;
end;

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

procedure RunWorks(works: TWorks);
begin
    if IsWorkRunning then
        raise Exception.Create('already running');
    _thread := TWorkThread.Create(true);
    _thread.FreeOnTerminate := true;
    _thread.FWorks := works;
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
var
    work: TWork;
begin
    AtomicExchange(_flag_running, 1);
    AtomicExchange(_flag_canceled, 0);
    Synchronize(KgsdumMainForm.OnStartWork);
    try
        for work in FWorks do
        begin
            if AtomicIncrement(_flag_canceled, 0) = 1 then
                raise EAbort.Create('выполнение прервано');
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.NewWork(work.Name);
                end);
            work.Proc();
        end;
    except
        on e: EAbort do
        begin
            // if _work <> 'опрос' then
            Synchronize(
                procedure
                begin
                    FormJournal.NewEntry(loglevWarn, 'выполнение прервано');
                end);

        end;
        on e: Exception do
        begin
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.AppException(nil, e);
//                    FormJournal.NewEntry(loglevError, format('%s, %s',
//                      [e.ClassName, e.Message]));
                end);
        end;
    end;

    Synchronize(KgsdumMainForm.OnStopWork);

    CloseComport(_hComportProducts);
    CloseComport(_hComportTermo);
    AtomicExchange(_flag_running, 0);
end;

procedure _do_each_product1(func: TProductProcedure);
var
    v: double;
    p: TProduct;
    Products: TArray<TProduct>;
begin
    Synchronize(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if length(Products) = 0 then
        raise Exception.Create('нет выбранных приборов для опроса');

    for p in Products do
    begin
        Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        try
            func(p);
        except
            on e: EBadResponse do
            begin
                Synchronize(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          e.Message);
                        FormJournal.NewEntry(loglevError, format('%s: %s %s',
                          [p.FormatID, e.ClassName, e.Message]));
                    end);

            end;
            on e: EDeadlineExceeded do
            begin
                Synchronize(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          'не отвечает');
                        FormJournal.NewEntry(loglevError,
                          format('%s: не отвечает', [p.FormatID]));
                    end);
            end;
        end;
    end;

end;

procedure DoEachProduct(func: TProductProcedure);
begin
    try
        _do_each_product1(func);
    finally
        Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(-1);
            end);
    end;

end;

initialization

_thread := nil;
_flag_running := 0;
_hComportProducts := INVALID_HANDLE_VALUE;
_hComportTermo := INVALID_HANDLE_VALUE;

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
                s: string;
            begin
                comport := 'COM?';
                if r.HComport = _hComportProducts then
                    comport := FormProperties.ComportProducts.Value + '-стенд'
                else if r.HComport = _hComportTermo then
                    comport := FormProperties.ComportTermo.Value +
                      '-термокамера';

                s := comport + ' : ' + BytesToHex(r.Request);
                if length(r.Response) > 0 then
                    s := s + ' --> ' + BytesToHex(r.Request);

                s := s + ' ' + Inttostr(r.millis) + ' мс';

                if r.attempt > 1 then
                    s := s + ' (' + Inttostr(r.attempt) + ')';

                FormJournal.NewEntry(loglevDebug, s);
            end);
    end);

end.
