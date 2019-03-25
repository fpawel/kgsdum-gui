unit works;

interface

uses data_model;

procedure RunReadVar(addr: byte; AVar: byte);
procedure RunReadVars(AVar: byte);
procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
procedure RunReadCoefficients(ACoefficient: byte);
procedure RunSwitchGasBlock(code: byte);

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



procedure RunReadVars(AVar: byte);
begin
    Worker.RunWork(Format('считать var%d', [AVar]),
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
    Worker.RunWork(Format('считать var%d адр.%d', [AVar, addr]),
        procedure
        begin
            Worker.KgsReadVar(addr, AVar);
        end);
end;

procedure RunReadCoefficients(ACoefficient: byte);
begin
    Worker.RunWork(Format('считать коэф.%d', [ACoefficient]),
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
    Worker.RunWork(Format('считать коэф.%d адр.%d', [ACoefficient, addr]),
        procedure
        begin
            Worker.KgsReadCoefficient(addr, ACoefficient);
        end);
end;

procedure TermochamberSetupTemperature(temperature: double);
begin
    Worker.TermochamberSetSetpoint(temperature);
    while abs(Worker.TermochamberReadTemperature - temperature) < 1 do
        Worker.DoEachProduct(
            procedure(p: TProduct)
            var
                AVar: byte;
            begin
                for AVar in KgsMainVars do
                    Worker.KgsReadVar(p.FAddr, AVar);

            end);
    Worker.Delay('выдержка термокамеры при ' + floattostr(temperature) + '\"C',
      _one_hour_ms * 3);
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
    Worker.TryWithErrorMessage(
        procedure
        begin
            Worker.SwitchGasBlock6006(code);
        end);
    Worker.Delay('продувка газа: ' + IntToStr(code), _one_minute_ms * 5);
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
begin
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
    BlowGas(1);
    ProductsReadVar(100);
    BlowGas(3);
    _do([DWriteKef(28, LastParty.Pgs3), DReadVar(101)]);
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

  TWork.Create('калибровка', Adjust),

  TWork.Create('линеаризация',
    procedure
    begin
        Adjust;

        BlowGas(4);
        _do([DWriteKef(44, 1), DWriteKef(48, LastParty.Pgs4), DReadVar(102)]);

        BlowGas(2);
        _do([DWriteKef(44, 0), DWriteKef(29, LastParty.Pgs2), DReadVar(102)]);

        BlowGas(1);
        _do([DWriteKef(44, 2), DReadVar(102)]);
    end),

  TWork.Create('термокомпенсация',
    procedure
    begin

        TermochamberSetupTemperature(20);

        BlowGas(1);
        ProductsReadVar(103);
        BlowGas(3);
        ProductsReadVar(107);
        BlowGas(1);

        TermochamberSetupTemperature(-5);

        BlowGas(1);
        ProductsReadVar(105);
        BlowGas(3);
        ProductsReadVar(109);
        BlowGas(1);

        TermochamberSetupTemperature(50);

        BlowGas(1);
        ProductsReadVar(104);
        BlowGas(3);
        ProductsReadVar(108);
        BlowGas(1);

    end)

  ];

end.
