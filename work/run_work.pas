unit run_work;

interface

uses comport, Classes, data_model, UnitFormConsole;

type
    TWorkProcedure = reference to procedure;

function IsWorkRunning: boolean;
procedure RunWork(what: string; proc: TWorkProcedure);

function ComportProducts: THandle;

function ComportProductsWorker: TComportWorker;

procedure CancelExecution;

procedure Synchronize(p: TThreadProcedure); overload;
procedure Synchronize(m: TThreadMethod); overload;
procedure DoEachProduct(func: TProductProcedure);

procedure AddWorkLog(ALevel: TLogLevel; AText: string);

var
    ComportProductsConfig, ComportGasConfig, ComportTermoConfig
      : TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, sysutils, errors,
    FireDAC.Comp.Client, UnitFormProperties, UnitFormLastParty, UnitKgsdumData,
    stringutils;

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

procedure AddWorkLog(ALevel: TLogLevel; AText: string);
begin
    FormConsole.AddLine(ALevel, _work, AText);

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        SQL.Text := 'INSERT INTO entry(work_id, created_at, level, message) VALUES ' +
          '((SELECT * FROM last_work_id), :created_at, :level, :message)';
        ParamByName('level').Value := integer(ALevel);
        ParamByName('created_at').Value := now;
        ParamByName('message').Value := AText;

        ExecSQL;
        Close;
        Free;
    end;
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

procedure RunWork(what: string; proc: TWorkProcedure);
begin
    if IsWorkRunning then
        raise Exception.Create('already running');
    _work := what;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        SQL.Text := 'INSERT INTO work(name) VALUES (:work);';
        ParamByName('work').Value := what;
        ExecSQL;
        Close;
        Free;
    end;
    AddWorkLog(loglevInfo, 'начало выполнения');
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
                    AddWorkLog(loglevWarn, 'выполнение прервано');
                end);

        end;
        on e: Exception do
        begin
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.AppException(nil, e);
                    AddWorkLog(loglevError, format('%s, %s',
                      [e.ClassName, e.Message]));
                end);
        end;
    end;

    Synchronize(
        procedure
        begin
            KgsdumMainForm.OnStopWork;
            AddWorkLog(loglevInfo, 'выполнено');
        end);

    CloseComport(_hComportProducts);
    CloseComport(_hComportTermo);
    AtomicExchange(_flag_running, 0);
    _work := '';

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
                        AddWorkLog(loglevError, format('%s: %s %s',
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
                        AddWorkLog(loglevError, format('%s: не отвечает',
                          [p.FormatID]));
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

procedure AddComportLog(AComport: string; ARequest, AResponse: TBytes;
millis, attempt: Integer);
var
    s: string;
begin
    s := AComport + ' : ' + BytesToHex(ARequest);
    if length(AResponse) > 0 then
        s := s + ' --> ' + BytesToHex(ARequest);

    s := s + ' ' + Inttostr(millis) + ' мс';

    if attempt > 1 then
        s := s + ' (' + Inttostr(attempt) + ')';
    AddWorkLog(loglevDebug, s);
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
                AddComportLog(comport, r.Request, r.Response, r.millis,
                  r.attempt);
            end);
    end);

end.
