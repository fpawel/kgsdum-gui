unit UnitWorker;

interface

uses
    System.SysUtils, System.Classes, comport, data_model, wask, UnitKgsdumData,
    UnitFormChartSeries;

type

    ESkipDelay = class(Exception);

    TWorker = class;

    TWorkThread = class(TThread)
        FWorks: TWorks;
        FWork: TWork;
        FWorker: TWorker;
        procedure Execute; override;
    end;

    TWorker = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
        FFlagRunning, FFlagCanceled, FFlagSkipDelay: longint;
        FHComportProducts, FHComportTermo: THandle;
        FThread: TWorkThread;

        procedure DoEachProduct1(func: TProductProcedure);
        procedure CloseComport(var hPort: THandle);
        procedure DoEndWork;
        procedure OnComportBackground;
        function KgsGetResponse(r: TKgsRequest): double;
        function TermochamberGetResponse(strRequest: string): double;
        function ComportProductsWorker: TComportworker;
        function ComportTermoWorker: TComportworker;
        function GetComportProducts: THandle;
        function GetComportTermo: THandle;

        procedure OnComport(r: TComportLogEntry);
        procedure interrogate_product(p: TProduct);

    public
        { Public declarations }
        ComportProductsConfig, ComportGasConfig, ComportTermoConfig
          : TConfigGetResponse;
        procedure DoEachProduct(func: TProductProcedure);
        procedure CancelExecution;
        procedure SkipDelay;
        procedure Delay(what: string; DurationMS: cardinal);
        procedure NewLogEntry(ALevel: TLogLevel; AText: string);

        procedure TermochamberStart;
        procedure TermochamberStop;
        procedure TermochamberSetSetpoint(setpoint: double);
        function TermochamberReadTemperature: double;

        function KgsReadVar(DeviceAddr: byte; ValueAddr: byte): double;
        procedure KgsWriteVar(DeviceAddr: byte; ValueAddr: byte; Value: double);
        function KgsReadCoefficient(DeviceAddr: byte;
          coefficient: byte): double;
        procedure KgsWriteCoefficient(DeviceAddr: byte; coefficient: byte;
          Value: double);

        procedure SwitchGasBlock6006(code: byte);

        procedure TryWithErrorMessage(Proc: TWorkProcedure);

        procedure RunWork(AName: string; AWork: TWorkProcedure);
        procedure RunWorks(withJourna: boolean; works: TWorks);
        procedure RunKgsSetAddr(addr: byte);
        procedure RunInterrogate;

        procedure Synchronize(p: TThreadProcedure); overload;
        procedure Synchronize(m: TThreadMethod); overload;

    end;

var
    Worker: TWorker;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

uses UnitFormLastParty, hardware_errors, UnitFormJournal, UnitFormConsole,
    UnitKgsdumMainForm, windows, math, UnitAppIni, termochamber, stringutils,
    dateutils,
    modbus;

{$R *.dfm}

type
    TDelay = record
        FStartTimeMS, FDurationMS: cardinal;

        FWorker: TWorker;

        constructor Create(DurationMS: cardinal; AWorker: TWorker);
        function ExitCondition: boolean;
        procedure _read(p: TProduct; AVar: byte);
        procedure DoDelay;
        procedure DoReadProduct(p: TProduct);
    end;

procedure TWorker.DataModuleCreate(Sender: TObject);
begin
    FThread := nil;
    FFlagRunning := 0;
    FHComportProducts := INVALID_HANDLE_VALUE;
    FHComportTermo := INVALID_HANDLE_VALUE;
    ComportProductsConfig := TConfigGetResponse.Create(500, 30, 2);
    ComportGasConfig := TConfigGetResponse.Create(500, 30, 2);
    ComportTermoConfig := TConfigGetResponse.Create(1000, 50, 3);

    comport.SetcomportLogHook(
        procedure(r: TComportLogEntry)
        begin
            Synchronize(
                procedure
                begin
                    OnComport(r);
                end);
        end);
end;

procedure TWorker.Synchronize(p: TThreadProcedure);
begin
    FThread.Synchronize(p);
end;

procedure TWorker.Synchronize(m: TThreadMethod);
begin
    FThread.Synchronize(m);
end;

procedure TWorker.RunWork(AName: string; AWork: TWorkProcedure);
begin
    RunWorks(false, [TWork.Create(AName, AWork)]);
end;

procedure TWorker.RunWorks(withJourna: boolean; works: TWorks);
begin
    if AtomicIncrement(FFlagRunning, 0) = 1 then
        raise EConfigError.Create('already running');
    FThread := TWorkThread.Create(true);
    FThread.FreeOnTerminate := true;
    FThread.FWorks := works;
    FThread.FWorker := self;
    FThread.Start;
end;

procedure TWorker.RunKgsSetAddr(addr: byte);
begin
    Worker.RunWork('установка адреса',
        procedure
        begin
            comport.WriteComport(ComportProductsWorker.HComport,
              [0, $AA, $55, addr]);
            NewLogEntry(loglevDebug, BytesToHex([0, $AA, $55, addr]));
        end);
end;

procedure TWorker.NewLogEntry(ALevel: TLogLevel; AText: string);
begin

    if FThread.FWork.Name = 'опрос' then
        exit;

    FThread.Synchronize(
        procedure
        begin
            FormJournal.NewEntry(ALevel, AText);
            FormConsole.NewLine(now, ALevel, FThread.FWork.Name, AText);
            with FormConsole.StringGrid1 do
                Row := RowCount - 1;
        end);
end;

procedure TWorker.CancelExecution;
begin
    AtomicExchange(FFlagCanceled, 1);
end;

procedure TWorker.SkipDelay;
begin
    NewLogEntry(logLevWarn, 'пропустить задержку');
    AtomicExchange(FFlagSkipDelay, 1);
end;

procedure TWorker.DoEachProduct1(func: TProductProcedure);
var
    p: TProduct;
    i: integer;
    Products: TArray<TProduct>;
begin
    FThread.Synchronize(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if Length(Products) = 0 then
        raise EConfigError.Create('не отмечено ни одного прибора в таблице');

    for i := 0 to Length(Products) - 1 do
    begin
        p := Products[i];
        FThread.Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        func(p);
    end;
end;

procedure TWorker.DoEachProduct(func: TProductProcedure);
begin
    try
        DoEachProduct1(func);
    finally
        FThread.Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(-1);
            end);
    end;
end;

procedure TWorker.CloseComport(var hPort: THandle);
begin
    if hPort <> INVALID_HANDLE_VALUE then
    begin
        CloseHandle(hPort);
        hPort := INVALID_HANDLE_VALUE;
    end;
end;

procedure TWorker.DoEndWork;
begin
    FThread.Synchronize(KgsdumMainForm.OnStopWork);
    CloseComport(FHComportProducts);
    CloseComport(FHComportTermo);
    AtomicExchange(FFlagRunning, 0);
    FThread := nil;
end;

procedure TWorker.OnComportBackground;
begin
    if AtomicIncrement(FFlagCanceled, 0) = 1 then
        raise EAbort.Create('выполнение прервано');

    if AtomicIncrement(FFlagSkipDelay, 0) = 1 then
        raise ESkipDelay.Create('отмена задержки');
end;

function TWorker.GetComportProducts: THandle;
begin
    if FHComportProducts <> INVALID_HANDLE_VALUE then
        exit(FHComportProducts);
    try
        FHComportProducts := comport.OpenComport
          (AppIni.ComportProductsName, 9600);
        exit(FHComportProducts);
    except
        FHComportProducts := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function TWorker.GetComportTermo: THandle;
begin
    if FHComportTermo <> INVALID_HANDLE_VALUE then
        exit(FHComportTermo);
    try
        FHComportTermo := comport.OpenComport(AppIni.ComportTempName, 9600);
        exit(FHComportTermo);
    except
        FHComportTermo := INVALID_HANDLE_VALUE;
        raise;
    end
end;

function TWorker.ComportProductsWorker: TComportworker;
begin
    result := TComportworker.Create(GetComportProducts, ComportProductsConfig,
      OnComportBackground);
end;

function TWorker.ComportTermoWorker: TComportworker;
begin
    result := TComportworker.Create(GetComportTermo, ComportTermoConfig,
      OnComportBackground);
end;

function TWorker.KgsGetResponse(r: TKgsRequest): double;
var
    _result: double;
begin
    try
        result := r.GetResponse(ComportProductsWorker);
        _result := result;
        FThread.Synchronize(
            procedure
            begin
                if r.Direction = KgsRead then
                    FormLastParty.SetAddrValue(r.DeviceAddr,
                      r.ValueAddr, _result)
                else
                    FormLastParty.SetAddrConnection(r.DeviceAddr,
                      'ok: ' + r.ToString, false);

            end);
    except
        on e: EConnectionError do
        begin
            FThread.Synchronize(
                procedure
                begin
                    FormLastParty.SetAddrConnection(r.DeviceAddr,
                      Format('%s: %s, %s', [r.ToString, e.ClassName,
                      e.Message]), true);
                end);
            e.Message := 'КГС: ' + r.ToString + ': ' + e.Message;
            raise;
        end;

    end;
end;

function TWorker.KgsReadVar(DeviceAddr: byte; ValueAddr: byte): double;
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := ValueAddr;
    r.Direction := KgsRead;
    r.Value := 0;
    result := KgsGetResponse(r)
end;

procedure TWorker.KgsWriteVar(DeviceAddr: byte; ValueAddr: byte; Value: double);
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsWrite;
    r.Value := Value;
    KgsGetResponse(r);
end;

function TWorker.KgsReadCoefficient(DeviceAddr: byte;
coefficient: byte): double;
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r);
    result := KgsReadVar(DeviceAddr, ceil(1. * coefficient - r.Value));
end;

procedure TWorker.KgsWriteCoefficient(DeviceAddr: byte; coefficient: byte;
Value: double);
var
    r: TKgsRequest;
begin
    r.DeviceAddr := DeviceAddr;
    r.ValueAddr := 97;
    r.Direction := KgsRead;
    r.Value := (coefficient div 60) * 60.;
    KgsGetResponse(r);

    r.ValueAddr := ceil(1. * coefficient - r.Value);
    r.Value := Value;
    r.Direction := KgsWrite;
    KgsGetResponse(r);

end;

function TWorker.TermochamberGetResponse(strRequest: string): double;
begin
    result := termochamber.TermochamberGetResponse(ComportTermoWorker,
      strRequest);
end;

procedure TWorker.TermochamberStart;
begin
    TermochamberGetResponse(TermochamberStartRequest);
end;

procedure TWorker.TermochamberStop;
begin
    TermochamberGetResponse(TermochamberStopRequest);
end;

procedure TWorker.TermochamberSetSetpoint(setpoint: double);
var
    s: string;
    v: integer;
begin
    setpoint := setpoint * 10.0;
    v := ceil(setpoint);

    s := IntToHex(ceil(v), 4);
    if Length(s) > 4 then
        s := Copy(s, Length(s) - 4, 4);
    TermochamberStop;
    TermochamberGetResponse(TermochamberSetpointRequest1 + s + #13#10);
end;

function TWorker.TermochamberReadTemperature: double;
begin
    result := TermochamberGetResponse(TermochamberTemperatureRequest);
end;

procedure TWorker.OnComport(r: TComportLogEntry);
var
    comport: string;
    s, strRequest, strResponse: string;
    log_level: TLogLevel;
    kgs_req: TKgsRequest;
    i:integer;
begin
    log_level := loglevDebug;
    comport := 'COM?';
    if r.HComport = FHComportProducts then
        comport := AppIni.ComportProductsName + '-стенд'
    else if r.HComport = FHComportTermo then
        comport := AppIni.ComportTempName + '-термокамера';

    s := comport + ' : ' + BytesToHex(r.Request);
    if Length(r.Response) > 0 then
        s := s + ' --> ' + BytesToHex(r.Response);

    s := s + ' ' + IntToStr(r.millis) + ' мс';

    if r.attempt > 1 then
        s := s + ' (' + IntToStr(r.attempt) + ')';

    if r.HComport = FHComportTermo then
    begin
        log_level := loglevInfo;
        strRequest := TEncoding.ASCII.GetString(r.Request);
        strResponse := TEncoding.ASCII.GetString(r.Response);
        if Length(r.Response) > 0 then
            strResponse := 'нет ответа';
        if Length(r.Response) > 0 then
            s := Format('%s "%s" : "%s" %s : %s',
              [TermochamberFormatRequest(strRequest), strRequest, strResponse,
              TermochamberFormatResponse(strResponse), s])
        else
            s := Format('%s "%s" : нет ответа : %s',
              [TermochamberFormatRequest(strRequest), strRequest, s]);
    end
    else if (r.HComport = FHComportProducts) then
    begin

        if r.Request[0] = $20 then
        begin
            s := 'газовый блок: ' + IntToStr(r.Request[8]) + ': ' + s;
            log_level := loglevInfo;
        end
        else if Length(r.Request) = 9 then
        begin
            kgs_req := TKgsRequest.fromBytes(r.Request);
            s := s + ' БО: ' + kgs_req.FormatResponse(r.Response);
            for I := 0 to Length(KgsMainVars)-1 do
            begin
                if KgsMainVars[i] = kgs_req.ValueAddr then
                    log_level := loglevTrace;
            end;
        end;
    end;

    NewLogEntry(log_level, s);
end;

procedure TWorker.TryWithErrorMessage(Proc: TWorkProcedure);
var
    hardware_error: string;
begin

    try
        Proc();
    except
        on e: EHardwareError do
        begin
            Synchronize(
                procedure
                begin

                    if KgsdumMainForm.ErrorMessageBox
                      (Format('Нет связи с оборудованием: %s, %s'#10#13#10#13 +
                      'Ignore - продолжить выполнение настройки'#10#13#10#13 +
                      'Abort - прекратить настройку', [e.ClassName, e.Message]))
                    then
                        NewLogEntry(logLevWarn, 'проигнорирована ошибка: ' +
                          e.ClassName + ', ' + e.Message)
                    else
                        hardware_error := e.Message;
                end);
        end;
    end;
    if hardware_error <> '' then
        raise EHardwareError.Create(hardware_error);
end;

procedure TWorker.Delay(what: string; DurationMS: cardinal);
    procedure _do_end;
    begin
        AtomicExchange(FFlagSkipDelay, 0);
        Synchronize(
            procedure
            begin
                KgsdumData.SaveLastSeriesBucket;
                with KgsdumMainForm do
                begin
                    TimerDelay.Enabled := false;
                    PanelDelay.Hide;
                end;
            end);
    end;

begin
    NewLogEntry(loglevInfo, what + ': ' + TimeToStr(IncMilliSecond(0,
      DurationMS)));
    AtomicExchange(FFlagSkipDelay, 0);
    Synchronize(
        procedure
        begin
            KgsdumMainForm.OnStartDelay(what, DurationMS);
        end);
    try
        TDelay.Create(DurationMS, self).DoDelay;
        NewLogEntry(logLevWarn, what + ': ' + TimeToStr(IncMilliSecond(0,
          DurationMS)) + ': задержка выполнена');
    except
        on e: ESkipDelay do
        begin
            _do_end;
            NewLogEntry(logLevWarn, what + ': ' + TimeToStr(IncMilliSecond(0,
              DurationMS)) + ': пропуск задержки');
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

procedure TWorker.SwitchGasBlock6006(code: byte);
begin
    try
        modbus.GetResponse($20, $10, [0, $10, 0, 1, 2, 0, code],
          ComportProductsWorker,
            procedure(_: TBytes)
            begin
            end);
    except
        on e: EHardwareError do
        begin
            e.Message := 'газовый блок: ' + IntToStr(code) + ': ' + e.Message;
            raise;
        end;
    end;
end;

// ------------------------------------------------------------------------------
procedure TWorker.RunInterrogate;
begin
    Worker.RunWork('опрос',
        procedure
        begin
            Worker.Synchronize(
                procedure
                begin
                    KgsdumData.NewChartSeries('опрос');
                    FormChartSeries.NewChart;
                end);
            while true do
                Worker.DoEachProduct(interrogate_product);
        end);
end;

procedure TWorker.interrogate_product(p: TProduct);
var
    AVar: byte;
begin
    try
        for AVar in KgsMainVars do
            Worker.KgsReadVar(p.FAddr, AVar);
    except
        on e: EConnectionError do
        begin
            Worker.NewLogEntry(loglevError, p.FormatID + ': ' + e.Message)
        end;
    end;

end;

// ------------------------------------------------------------------------------
procedure TWorkThread.Execute;
var
    AWork: TWork;
begin
    AtomicExchange(FWorker.FFlagRunning, 1);
    AtomicExchange(FWorker.FFlagCanceled, 0);
    AtomicExchange(FWorker.FFlagSkipDelay, 0);
    Synchronize(KgsdumMainForm.OnStartWork);
    try
        for AWork in FWorks do
        begin
            if AtomicIncrement(FWorker.FFlagCanceled, 0) = 1 then
                raise EAbort.Create('выполнение прервано');
            FWork := AWork;
            if AWork.Name <> 'опрос' then
                Synchronize(
                    procedure
                    begin
                        KgsdumMainForm.NewWork(FWork.Name);
                        FormJournal.NewWork(FWork.Name);
                    end);
            FWork.Proc();
        end;
    except

        on e: EAbort do
            FWorker.NewLogEntry(logLevWarn, e.Message);

        on e: EHardwareError do
        begin
            e.Message := 'ошибка связи с оборудованием: ' + e.Message;
            FWorker.NewLogEntry(loglevError, e.Message);
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
    FWorker.DoEndWork;

end;

// ------------------------------------------------------------------------------
constructor TDelay.Create(DurationMS: cardinal; AWorker: TWorker);
begin
    FStartTimeMS := GetTickCount;
    FDurationMS := DurationMS;
    FWorker := AWorker;
end;

function TDelay.ExitCondition: boolean;
begin
    result := ((GetTickCount - FStartTimeMS) >= FDurationMS);
end;

procedure TDelay._read(p: TProduct; AVar: byte);
begin
    if ExitCondition then
        exit;
    FWorker.KgsReadVar(p.FAddr, AVar);
end;

procedure TDelay.DoReadProduct(p: TProduct);
var
    AVar: byte;
begin
    try
        for AVar in KgsMainVars do
            _read(p, AVar);
    except
        on e: EConnectionError do
        begin
            FWorker.NewLogEntry(loglevError, p.FormatID + ': ' + e.Message)
        end;
    end;

end;

procedure TDelay.DoDelay;
var
    _self: TDelay;
    t:cardinal;
begin
    _self := self;
    while not ExitCondition do
    begin
        FWorker.DoEachProduct(_self.DoReadProduct);
        t:= GetTickCount;
        while (GetTickCount - t < 5000) and (not ExitCondition) do
        begin
            if AtomicIncrement(FWorker.FFlagSkipDelay, 0) = 1 then
                raise ESkipDelay.Create('отмена задержки');
            Sleep(10);
        end;
    end;
end;

// ------------------------------------------------------------------------------
end.
