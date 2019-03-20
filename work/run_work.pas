unit run_work;

interface

uses comport, Classes, data_model, sysutils;

type

    TWorkProcedure = reference to procedure;

    TWork = record
        Name: string;
        Proc: TWorkProcedure;
        constructor Create(AName: string; AProc: TWorkProcedure);
    end;

    TWorks = TArray<TWork>;

    // ------------------------------------------------------------------------------
procedure RunWorks(withJourna: boolean; works: TWorks);
procedure RunWork(AName: string; AWork: TWorkProcedure);
function IsWorkRunning: boolean;
procedure CancelExecution;
procedure SkipDelay;
procedure Delay(what: string; DurationMS: cardinal);
// ------------------------------------------------------------------------------
procedure NewWorkLogEntry(ALevel: TLogLevel; AText: string);
// ------------------------------------------------------------------------------
function ComportProductsWorker: TComportworker;
// ------------------------------------------------------------------------------
procedure Synchronize(p: TThreadProcedure); overload;
procedure Synchronize(m: TThreadMethod); overload;
procedure DoEachProduct(func: TProductProcedure);
// ------------------------------------------------------------------------------
procedure TryWithErrorMessage(Proc: TWorkProcedure);
// ------------------------------------------------------------------------------
procedure TermochamberStart;
procedure TermochamberStop;
procedure TermochamberSetSetpoint(setpoint: double);
function TermochamberReadTemperature: double;
// ------------------------------------------------------------------------------
procedure RunKgsSetAddr(addr: byte);

// ------------------------------------------------------------------------------
var
    ComportProductsConfig, ComportGasConfig, ComportTermoConfig
      : TConfigGetResponse;

implementation

uses UnitKgsdumMainForm, windows, hardware_errors,
    FireDAC.Comp.Client,
    stringutils, UnitFormJournal, modbus, UnitFormConsole, termo,
    do_each_product, kgs, dateutils, UnitAppIni;

type
    ESkipDelay = class(Exception);

    TWorkThread = class(TThread)
        FWorks: TWorks;
        FWork: TWork;
        procedure Execute; override;
    end;

var
    _with_journal: boolean;
    _flag_running, _flag_canceled, _flag_skip_delay: longint;
    _hComportProducts, _hComportTermo: THandle;
    _thread: TWorkThread;

procedure DoEachProduct(func: TProductProcedure);
begin
    do_each_product.DoEachProduct(Synchronize, NewWorkLogEntry, func);
end;

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

procedure SkipDelay;
begin
    NewWorkLogEntry(logLevWarn, 'пропустить задержку');
    AtomicExchange(_flag_skip_delay, 1);
end;

procedure _on_comport_background;
begin
    if AtomicIncrement(_flag_canceled, 0) = 1 then
        raise EAbort.Create('выполнение прервано');

    if AtomicIncrement(_flag_skip_delay, 0) = 1 then
        raise ESkipDelay.Create('отмена задержки');
end;

function _comportProducts: THandle;
begin
    if _hComportProducts <> INVALID_HANDLE_VALUE then
        exit(_hComportProducts);

    try
        _hComportProducts := comport.OpenComport(AppIni.ComportProductsName, 9600);
        exit(_hComportProducts);

    except
        _hComportProducts := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function ComportProductsWorker: TComportworker;
begin
    result := TComportworker.Create(_comportProducts, ComportProductsConfig,
      _on_comport_background);
end;

function _comportTermo: THandle;
var
    comportName: string;
begin
    if _hComportTermo <> INVALID_HANDLE_VALUE then
        exit(_hComportTermo);

    try
        _hComportTermo := comport.OpenComport(AppIni.ComportTempName, 9600);
        exit(_hComportTermo);

    except
        _hComportTermo := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function _comportTermoWorker: TComportworker;
begin
    result := TComportworker.Create(_comportTermo, ComportTermoConfig,
      _on_comport_background);
end;

function IsWorkRunning: boolean;
begin
    result := AtomicIncrement(_flag_running, 0) = 1;
end;

procedure RunWork(AName: string; AWork: TWorkProcedure);
begin
    RunWorks(false, [TWork.Create(AName, AWork)]);
end;

procedure RunWorks(withJourna: boolean; works: TWorks);
begin
    if IsWorkRunning then
        raise EConfigError.Create('already running');
    _with_journal := withJourna;
    _thread := TWorkThread.Create(true);
    _thread.FreeOnTerminate := true;
    _thread.FWorks := works;
    _thread.Start;

end;

procedure _closeComport(var hPort: THandle);
begin
    if hPort <> INVALID_HANDLE_VALUE then
    begin
        CloseHandle(hPort);
        hPort := INVALID_HANDLE_VALUE;
    end;
end;

procedure NewWorkLogEntry(ALevel: TLogLevel; AText: string);
begin
    _thread.Synchronize(
        procedure
        begin
            if _with_journal then
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
    work_checked: boolean;
begin
    AtomicExchange(_flag_running, 1);
    AtomicExchange(_flag_canceled, 0);
    AtomicExchange(_flag_skip_delay, 0);
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
                    if Length(FWorks) > 1 then
                        FormJournal.NewWork(FWork.Name);
                end);
            FWork.Proc();
        end;
    except

        on e: EAbort do
            NewWorkLogEntry(logLevWarn, 'выполнение прервано');

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

    _closeComport(_hComportProducts);
    _closeComport(_hComportTermo);
    AtomicExchange(_flag_running, 0);
    _thread := nil;
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

procedure RunKgsSetAddr(addr: byte);
begin
    RunWork('установка адреса',
        procedure
        begin
            comport.WriteComport(_comportProducts, [0, $AA, $55, addr]);
            NewWorkLogEntry(loglevDebug, BytesToHex([0, $AA, $55, addr]));
        end);
end;

type
    TDelay = record
        FStartTimeMS, FDurationMS: cardinal;

        constructor Create(DurationMS: cardinal);
        function ExitCondition: boolean;
        procedure _read(p: TProduct; AVar: byte);
        procedure DoDelay;
    end;

constructor TDelay.Create(DurationMS: cardinal);
begin
    FStartTimeMS := GetTickCount;
    FDurationMS := DurationMS;
end;

function TDelay.ExitCondition: boolean;
begin
    result := ((GetTickCount - FStartTimeMS) >= FDurationMS);
end;

procedure TDelay._read(p: TProduct; AVar: byte);
begin
    if ExitCondition then
        exit;
    KgsReadVar(p.FAddr, AVar);
end;

procedure TDelay.DoDelay;
var
    _self: TDelay;
begin
    _self := self;
    while not ExitCondition do
        DoEachProduct(
            procedure(p: TProduct)
            var
                v: double;
                AVar: byte;
            begin
                for AVar in KgsVars do
                    _self._read(p, AVar);
            end);
end;

procedure Delay(what: string; DurationMS: cardinal);
    procedure _do_end;
    begin
        AtomicExchange(_flag_skip_delay, 0);
        Synchronize(
            procedure
            begin
                with KgsdumMainForm do
                begin
                    TimerDelay.Enabled := false;
                    PanelDelay.Hide;
                end;
            end);
    end;

begin

    NewWorkLogEntry(loglevInfo, what + ': ' + TimeToStr(IncMilliSecond(0,
      DurationMS)));
    AtomicExchange(_flag_skip_delay, 0);
    Synchronize(
        procedure
        begin
            with KgsdumMainForm do
            begin
                LabelWhatDelay.Caption := what;
                LabelDelayElepsedTime.Caption := '00:00:00';
                LabelProgress.Caption := '';
                ProgressBar1.Position := 0;
                ProgressBar1.Max := DurationMS;
                TimerDelay.Enabled := true;
                PanelDelay.Show;
            end;
        end);
    try
        TDelay.Create(DurationMS).DoDelay;
        NewWorkLogEntry(logLevWarn, what + ': ' + TimeToStr(IncMilliSecond(0,
          DurationMS)) + ': задержка выполнена');
    except
        on e: ESkipDelay do
        begin
            _do_end;
            NewWorkLogEntry(logLevWarn,
              what + ': ' + TimeToStr(IncMilliSecond(0, DurationMS)) +
              ': пропуск задержки');
            exit;
        end;
        on e: Exception do
        begin
            _do_end;
            raise;
        end;
    end;
    _do_end;
end;

procedure _onComport(r: TComportLogEntry);
var
    comport: string;
    s: string;
begin
    comport := 'COM?';
    if r.HComport = _hComportProducts then
        comport := AppIni.ComportProductsName
          + '-стенд'
    else if r.HComport = _hComportTermo then
        comport := AppIni.ComportTempName +
          '-термокамера';

    s := comport + ' : ' + BytesToHex(r.Request);
    if Length(r.Response) > 0 then
        s := s + ' --> ' + BytesToHex(r.Response);

    s := s + ' ' + IntToStr(r.millis) + ' мс';

    if r.attempt > 1 then
        s := s + ' (' + IntToStr(r.attempt) + ')';

    if r.HComport = _hComportTermo then
    begin
        if Length(r.Response) > 0 then
            s := format('%s "%s" -> "%s"',
              [s, TEncoding.ASCII.GetString(r.Request),
              TEncoding.ASCII.GetString(r.Response)])
        else
            s := format('%s "%s"', [s, TEncoding.ASCII.GetString(r.Request)]);
    end;

    NewWorkLogEntry(loglevDebug, s);
end;

initialization

_with_journal := false;
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
            begin
                _onComport(r);
            end);
    end);

end.
