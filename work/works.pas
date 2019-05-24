unit works;

interface

uses data_model;

procedure RunReadVar(addr: byte; AVar: byte);
procedure RunReadVars(AVar: byte);
procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
procedure RunReadCoefficients(ACoefficient: byte);
procedure RunSwitchGasBlock(code: byte);

procedure RunWriteVars(AVar: byte; AValue: double);
procedure RunWriteVar(addr: byte; AVar: byte; AValue: double);

procedure RunWriteCoefs(ACoef: byte; AValue: double);
procedure RunWriteCoef(addr: byte; ACoef: byte; AValue: double);

var
    MainWorks: TWorks;

implementation

uses sysutils, comport, UnitFormLastParty, stringutils,
    classes, windows, hardware_errors, UnitAppIni, modbus,
    crud,
    UnitKgsdumMainForm, UnitWorker, UnitFormChartSeries, UnitKgsdumData;

const
    _one_minute_ms = 1000 * 60;
    _one_hour_ms = _one_minute_ms * 60;

procedure RunWriteCoefs(ACoef: byte; AValue: double);
begin
    Worker.RunWork(Format('записать коэффициент[%d]=%s',
      [ACoef, floattostr(AValue)]),
        procedure
        begin
            Worker.DoEachProduct(
                procedure(p: TProduct)
                begin
                    Worker.KgsWriteCoefficient(p.FAddr, ACoef, AValue);
                end);

        end);
end;

procedure RunWriteCoef(addr: byte; ACoef: byte; AValue: double);
begin
    Worker.RunWork(Format('записать адрес=%d коэффициент[%d]=%s',
      [addr, ACoef, floattostr(AValue)]),
        procedure
        begin
            Worker.KgsWriteCoefficient(addr, ACoef, AValue);
        end);
end;

procedure RunWriteVars(AVar: byte; AValue: double);
begin
    Worker.RunWork(Format('записать var[%d]=%s', [AVar, floattostr(AValue)]),
        procedure
        begin
            Worker.DoEachProduct(
                procedure(p: TProduct)
                begin
                    Worker.KgsWriteVar(p.FAddr, AVar, AValue);
                end);

        end);
end;

procedure RunWriteVar(addr: byte; AVar: byte; AValue: double);
begin
    Worker.RunWork(Format('записать адрес=%d var[%d]=%s',
      [addr, AVar, floattostr(AValue)]),
        procedure
        begin
            Worker.KgsWriteVar(addr, AVar, AValue);
        end);
end;

procedure RunReadVars(AVar: byte);
begin
    Worker.RunWork(Format('считать var[%d]', [AVar]),
        procedure
        begin
            Worker.DoEachProduct(
                procedure(p: TProduct)
                begin
                    Worker.KgsReadVar(p.FAddr, AVar);
                end);

        end);
end;

procedure RunReadVar(addr: byte; AVar: byte);
begin
    Worker.RunWork(Format('считать var[%d] адрес=%d', [AVar, addr]),
        procedure
        begin
            Worker.KgsReadVar(addr, AVar);
        end);
end;

procedure RunReadCoefficients(ACoefficient: byte);
begin
    Worker.RunWork(Format('считать коэффициент[%d]', [ACoefficient]),
        procedure
        begin
            Worker.DoEachProduct(
                procedure(p: TProduct)
                begin
                    Worker.KgsReadCoefficient(p.FAddr, ACoefficient);
                end);

        end);
end;

procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
begin
    Worker.RunWork(Format('считать коэффициент[%d] адрес=%d',
      [ACoefficient, addr]),
        procedure
        begin
            Worker.KgsReadCoefficient(addr, ACoefficient);
        end);
end;

function TermochamberTryReadTemperature: double;
var
    t: double;
begin
    Worker.TryWithErrorMessage(
        procedure
        begin
            t := Worker.TermochamberReadTemperature;
        end);
    result := t;
end;

procedure TrySwitchGas(code: byte);
begin
    Worker.TryWithErrorMessage(
        procedure
        begin
            Worker.SwitchGasBlock6006(code);
        end);
end;

procedure TermochamberSetupTemperature(temperature: double);
begin
    Worker.NewLogEntry(loglevInfo, 'Перевод термокамеры на температуру ' +
      floattostr(temperature));
    TrySwitchGas(0);
    Worker.TryWithErrorMessage(
        procedure
        begin
            Worker.TermochamberSetSetpoint(temperature);
        end);

    while abs(TermochamberTryReadTemperature - temperature) > 1 do
    begin
        Worker.DoEachProduct(Worker.InterrogateProduct);
    end;
    Worker.NewLogEntry(loglevInfo, 'Термокамера вышла на температуру ' +
      floattostr(temperature));

    Worker.Delay('выдержка термокамеры при ' + floattostr(temperature) + '"C',
      _one_minute_ms * AppIni.TempTime);
end;

procedure RunSwitchGasBlock(code: byte);
begin
    Worker.RunWork('Газовый блок: ' + IntToStr(code),
        procedure
        begin
            Worker.SwitchGasBlock6006(code);
        end);
end;

procedure BlowGas(code: byte);
begin
    TrySwitchGas(code);
    Worker.Delay('продувка газа: ' + IntToStr(code), _one_minute_ms * 10);
end;

procedure BlowAir;
begin
    Worker.NewLogEntry(loglevInfo, 'продувка азотом');
    TrySwitchGas(1);
    Worker.Delay('продувка азотом', _one_minute_ms * AppIni.GasTime);
    TrySwitchGas(0);
end;

function LastParty: TParty;
var
    p: TParty;
begin
    Worker.Synchronize(
        procedure
        begin
            p := GetLastParty;
        end);
    result := p;
end;

procedure ProductsReadVar(AVar: byte);
begin
    Worker.NewLogEntry(loglevInfo, 'Считать var' + IntToStr(AVar));
    Worker.DoEachProduct(
        procedure(p: TProduct)
        begin
            Worker.KgsReadVar(p.FAddr, AVar);
        end);
end;

type
    TMeth = (MWriteKef, MReadKef, MReadVar);

    TTask = record
        FMeth: TMeth;
        FVar: byte;
        FValue: double;
        function ToString: string;
    end;

function TTask.ToString: string;
begin
    case FMeth of
        MWriteKef:
            result := Format('записать коэффициент[%d]=%s',
              [FVar, floattostr(FValue)]);
        MReadKef:
            result := Format('считать коэффициент K%d', [FVar]);
        MReadVar:
            result := Format('считать var%d', [FVar]);
    end;
end;

function DWriteKef(kef: byte; value: double): TTask;
begin
    with result do
    begin
        FMeth := MWriteKef;
        FVar := kef;
        FValue := value;
    end;
end;

function DReadKef(kef: byte): TTask;
begin
    with result do
    begin
        FMeth := MReadKef;
        FVar := kef;
    end;
end;

function DReadVar(AVar: byte): TTask;
begin
    with result do
    begin
        FMeth := MReadVar;
        FVar := AVar;
    end;
end;

procedure _do(tasks: TArray<TTask>);
var
    t: TTask;
begin
    for t in tasks do
        Worker.NewLogEntry(loglevInfo, t.ToString);

    Worker.DoEachProduct(
        procedure(p: TProduct)
        var
            t: TTask;
        begin
            for t in tasks do
            begin
                case t.FMeth of
                    MWriteKef:
                        Worker.KgsWriteCoefficient(p.FAddr, t.FVar, t.FValue);
                    MReadKef:
                        Worker.KgsReadCoefficient(p.FAddr, t.FVar);
                    MReadVar:
                        Worker.KgsReadVar(p.FAddr, t.FVar);
                end;
            end;
        end);
end;

procedure Adjust;
begin
    _do([DWriteKef(94, 0)]);
    Worker.NewLogEntry(loglevInfo, 'Корректировка нуля');
    _do([DWriteKef(95, 135), DWriteKef(55, 0)]);
    BlowGas(1);
    ProductsReadVar(100);

    Worker.NewLogEntry(loglevInfo, 'Корректировка чувствительности');
    BlowGas(3);
    _do([DWriteKef(28, LastParty.FPgs3), DReadVar(101)]);

end;

initialization

MainWorks := [TWork.Create('термоциклирование',
    procedure
    var
        i: integer;
    begin
        for i := 1 to 3 do
        begin
            TermochamberSetupTemperature(-60);
            TermochamberSetupTemperature(80);
        end;
        TermochamberSetupTemperature(20);
    end),

  TWork.Create('установка НКУ',
    procedure
    begin
        TermochamberSetupTemperature(20);
    end),

  TWork.Create('калибровка', Adjust),

  TWork.Create('линеаризация',
    procedure
    begin

        Worker.NewLogEntry(loglevInfo, 'Линеаризация ПГС4');
        BlowGas(4);
        _do([DWriteKef(44, 1), DWriteKef(48, LastParty.FPgs4), DReadVar(102)]);

        Worker.NewLogEntry(loglevInfo, 'Линеаризация ПГС2');
        BlowGas(2);
        _do([DWriteKef(44, 0), DWriteKef(29, LastParty.FPgs2), DReadVar(102)]);

        Worker.NewLogEntry(loglevInfo, 'Линеаризация ПГС1');
        BlowGas(1);
        _do([DWriteKef(44, 2), DReadVar(102)]);

        BlowAir;

    end),

  TWork.Create('термокомпенсация',
    procedure
    begin
        Worker.NewLogEntry(loglevInfo, 'Термокомпенсация +20"C');

        TermochamberSetupTemperature(20);

        BlowGas(1);

        ProductsReadVar(103);
        Worker.Pause(1000 * 60);
        ProductsReadVar(103);

        Worker.Pause(1000 * 60);
        Worker.SaveVarValue(VarWork, 'work_plus20');
        Worker.SaveVarValue(VarRef, 'ref_plus20');

        BlowGas(3);
        ProductsReadVar(107);

        Worker.Pause(1000 * 60);
        Worker.SaveVarValue(VarWork, 'work_gas3');

        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'Термокомпенсация -5"C');

        TermochamberSetupTemperature(-5);

        BlowGas(1);
        ProductsReadVar(105);

        Worker.Pause(1000 * 60);
        Worker.SaveVarValue(VarWork, 'work_minus5');
        Worker.SaveVarValue(VarRef, 'ref_minus5');

        BlowGas(3);
        ProductsReadVar(109);
        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'Термокомпенсация +50"C');

        TermochamberSetupTemperature(50);

        BlowGas(1);
        ProductsReadVar(104);

        Worker.Pause(1000 * 60);
        Worker.SaveVarValue(VarWork, 'work_plus50');
        Worker.SaveVarValue(VarRef, 'ref_plus50');

        BlowGas(3);
        ProductsReadVar(108);
        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'Окончание термокомпенсации');

        ProductsReadVar(106);
        Worker.Pause(1000 * 60);
        ProductsReadVar(110);

    end),

  TWork.Create('проверка БО',
    procedure
    begin
        Worker.NewLogEntry(loglevInfo, 'проверка БО +20"С');

        TermochamberSetupTemperature(20);
        Adjust;

        BlowGas(1);
        Worker.SaveVarValue(VarConc, 'c1_plus20');
        BlowGas(4);
        Worker.SaveVarValue(VarConc, 'c4_plus20');
        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'проверка БО 0"С');

        TermochamberSetupTemperature(0);
        BlowGas(1);
        Worker.SaveVarValue(VarConc, 'c1_zero');
        BlowGas(4);
        Worker.SaveVarValue(VarConc, 'c4_zero');
        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'проверка БО +50"С');

        TermochamberSetupTemperature(50);
        BlowGas(1);
        Worker.SaveVarValue(VarConc, 'c1_plus50');
        BlowGas(4);
        Worker.SaveVarValue(VarConc, 'c4_plus50');
        BlowAir;

        Worker.NewLogEntry(loglevInfo, 'проверка БО +20"С, повторно');

        TermochamberSetupTemperature(20);
        BlowGas(1);
        Worker.SaveVarValue(VarConc, 'c1_plus20ret');
        BlowGas(4);
        Worker.SaveVarValue(VarConc, 'c4_plus20ret');
        BlowGas(3);

        Worker.TermochamberStop;

        BlowAir;

    end)

  ];

end.
