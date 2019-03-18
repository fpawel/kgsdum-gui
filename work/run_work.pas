unit run_work;

interface

uses comport, Classes, data_model, sysutils;

type
    EConfigError = class(Exception);

    TWorkProcedure = reference to procedure;

    TWork = record
        Name: string;
        Proc: TWorkProcedure;
        constructor Create(AName: string; AProc: TWorkProcedure);
    end;

    TWorks = TArray<TWork>;

function IsWorkRunning: boolean;
procedure RunWorks(works: TWorks);

function ComportProductsWorker: TComportWorker;

procedure CancelExecution;

procedure Synchronize(p: TThreadProcedure); overload;
procedure Synchronize(m: TThreadMethod); overload;
procedure DoEachProduct(func: TProductProcedure);

procedure SwitchGasBlock6006(code: byte);

procedure TryWithErrorMessage(Proc: TWorkProcedure);

procedure RunWork(AName: string; AWork: TWorkProcedure);
procedure NewWorkLogEntry(ALevel: TLogLevel; AText: string);

function CurrentWorksCount: integer;

procedure TermochamberStart;
procedure TermochamberStop;
procedure TermochamberSetSetpoint(setpoint: double);
function TermochamberReadTemperature: double;

var
    ComportProductsConfig, ComportGasConfig, ComportTermoConfig
      : TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, hardware_errors,
    FireDAC.Comp.Client, UnitFormProperties, UnitFormLastParty, UnitKgsdumData,
    stringutils, UnitFormJournal, modbus, UnitFormConsole, termo;

type
    TWorkThread = class(TThread)
        FWorks: TWorks;
        FWork: TWork;
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
end;

function _comportProducts: THandle;
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

function ComportProductsWorker: TComportWorker;
begin
    result := TComportWorker.Create(_comportProducts, ComportProductsConfig,
      _on_comport_background);
end;

function _comportTermo: THandle;
var
    comportName: string;
begin
    if _hComportTermo <> INVALID_HANDLE_VALUE then
        exit(_hComportTermo);

    Synchronize(
        procedure
        begin
            comportName := FormProperties.ComportTermo.Value;
        end);

    try
        _hComportTermo := comport.OpenComport(comportName, 9600);
        exit(_hComportTermo);

    except
        _hComportTermo := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function _comportTermoWorker: TComportWorker;
begin
    result := TComportWorker.Create(_comportTermo, ComportTermoConfig,
      _on_comport_background);
end;

function IsWorkRunning: boolean;
begin
    result := AtomicIncrement(_flag_running, 0) = 1;
end;

procedure RunWork(AName: string; AWork: TWorkProcedure);
begin
    RunWorks([TWork.Create(AName, AWork)]);
end;

procedure RunWorks(works: TWorks);
begin
    if IsWorkRunning then
        raise EConfigError.Create('already running');
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

function CurrentWorksCount: integer;
begin
    result := length(_thread.FWorks);

end;

procedure NewWorkLogEntry(ALevel: TLogLevel; AText: string);
begin
    _thread.Synchronize(
        procedure
        begin
            if CurrentWorksCount > 1 then
            begin
                FormJournal.NewEntry(ALevel, AText);
                FormConsole.NewLine(now, ALevel, _thread.FWork.Name, AText)
            end
            else
                FormConsole.NewLine(now, ALevel, '', AText);
            with FormConsole.StringGrid1 do
                Row := RowCount - 1;

        end);

end;

procedure TWorkThread.Execute;
var
    AWork: TWork;
begin
    AtomicExchange(_flag_running, 1);
    AtomicExchange(_flag_canceled, 0);
    Synchronize(KgsdumMainForm.OnStartWork);
    try
        for AWork in FWorks do
        begin
            if AtomicIncrement(_flag_canceled, 0) = 1 then
                raise EAbort.Create('выполнение прервано');
            FWork := AWork;
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.NewWork(FWork.Name);
                    if length(FWorks) > 1 then
                        FormJournal.NewWork(FWork.Name);
                end);
            FWork.Proc();
        end;
    except

        on e: EAbort do
            NewWorkLogEntry(loglevWarn, 'выполнение перрвано');

        on e: EHardwareError do
        begin
            NewWorkLogEntry(loglevError, e.ClassName + ': ' + e.Message);
            Synchronize(
                procedure
                begin
                    KgsdumMainForm.ShowNonModalErrorMessage(e.ClassName,
                      e.Message);
                end);
        end;

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

    CloseComport(_hComportProducts);
    CloseComport(_hComportTermo);
    AtomicExchange(_flag_running, 0);
    _thread := nil;
end;

procedure _do_each_product1(func: TProductProcedure);
var
    v: double;
    p: TProduct;
    i : integer;
    Products: TArray<TProduct>;
begin
    Synchronize(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if length(Products) = 0 then
        raise EConfigError.Create('не отмечено ни одного прибора в таблице');

    for i :=0 to length(Products)-1 do
    begin
        p := Products[i];
        Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        try
            func(p);
        except
            on e: EConnectionError do
            begin
                NewWorkLogEntry(loglevError,
                  format('%s: %s, %s', [p.FormatID, e.ClassName, e.Message]));
                p.FConnection := e.Message;
                p.FConnectionFailed := true;
                Synchronize(
                    procedure
                    begin
                        FormLastParty.SetProduct(p);
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

procedure SwitchGasBlock6006(code: byte);
begin
    try
        modbus.GetResponse($20, $10, [0, $10, 0, 1, 2, 0, code],
          TComportWorker.Create(_comportProducts, ComportGasConfig,
          _on_comport_background),
            procedure(_: TBytes)
            begin
            end);
        NewWorkLogEntry(loglevInfo, 'Газовый блок 6006: ' + IntToStr(code));

    except
        on e: EHardwareError do
        begin
            e.Message := 'газовый блок: ' + e.Message;
            raise;
        end;
    end;

end;

function _message_box(AMsg: string): boolean;
var
    v: boolean;
begin
    Synchronize(
        procedure
        begin
            v := KgsdumMainForm.ErrorMessageBox(AMsg);
        end);
    result := v;

end;

procedure TryWithErrorMessage(Proc: TWorkProcedure);
var
    _continue: boolean;
begin
    try
        Proc();
    except
        on e: EHardwareError do
        begin
            Synchronize(
                procedure
                begin
                    _continue := KgsdumMainForm.ErrorMessageBox
                      (format('Нет связи с оборудованием: %s, %s'#10#13#10#13 +
                      'Ignore - продолжить выполнение настройки'#10#13#10#13 +
                      'Abort - прекратить настройку', [e.ClassName,
                      e.Message]));
                end);
        end;
    end
end;

procedure TermochamberStart;
begin
    try
        termo.TermochamberStart(_comportTermoWorker);
        NewWorkLogEntry(loglevDebug, 'термокамера: СТАРТ');
    except
        on e: EHardwareError do
        begin
            e.Message := 'не удалось выполнить команду СТАРТ: ' + e.Message;
            raise;
        end;
    end;
end;

procedure TermochamberStop;
begin
    try
        termo.TermochamberStop(_comportTermoWorker);
        NewWorkLogEntry(loglevDebug, 'термокамера: СТОП');
    except
        on e: EHardwareError do
        begin
            e.Message := 'не удалось выполнить команду СТОП: ' + e.Message;
            raise;
        end;
    end;

end;

procedure TermochamberSetSetpoint(setpoint: double);
begin
    try
        termo.TermochamberSetSetpoint(_comportTermoWorker, setpoint);
        NewWorkLogEntry(loglevDebug, 'термокамера: УСТАВКА ' +
          FloatToStr(setpoint));

    except
        on e: EHardwareError do
        begin
            e.Message := 'термокамера: не удалось задать уставку ' +
              FloatToStr(setpoint) + ': ' + e.Message;
            raise;
        end;
    end;
end;

function TermochamberReadTemperature: double;
begin
    try
        result := termo.TermochamberReadTemperature(_comportTermoWorker);
        NewWorkLogEntry(loglevDebug, 'термокамера: текущая температура=' +
          FloatToStr(result));
    except
        on e: EHardwareError do
        begin
            e.Message := 'не удалось считать текущую температуру: ' + e.Message;
            raise;
        end;
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
                    s := s + ' --> ' + BytesToHex(r.Response);

                s := s + ' ' + IntToStr(r.millis) + ' мс';

                if r.attempt > 1 then
                    s := s + ' (' + IntToStr(r.attempt) + ')';

                if r.HComport = _hComportTermo then
                begin
                    if length(r.Response) > 0 then
                        s := format('%s "%s" -> "%s"',
                          [s, TEncoding.ASCII.GetString(r.Request),
                          TEncoding.ASCII.GetString(r.Response)])
                    else
                        s := format('%s "%s"',
                          [s, TEncoding.ASCII.GetString(r.Request)]);
                end;

                NewWorkLogEntry(loglevDebug, s);
            end);
    end);

end.
