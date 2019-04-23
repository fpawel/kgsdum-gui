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
    Worker.RunWork(Format('�������� coef%d=%s', [ACoef, floattostr(AValue)]),
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
    Worker.RunWork(Format('�������� ���.%d coef%d=%s',
      [addr, ACoef, floattostr(AValue)]),
        procedure
        begin
            Worker.KgsWriteCoefficient(addr, ACoef, AValue);
        end);
end;

procedure RunWriteVars(AVar: byte; AValue: double);
begin
    Worker.RunWork(Format('�������� var%d=%s', [AVar, floattostr(AValue)]),
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
    Worker.RunWork(Format('�������� ���.%d var%d=%s',
      [addr, AVar, floattostr(AValue)]),
        procedure
        begin
            Worker.KgsWriteVar(addr, AVar, AValue);
        end);
end;

procedure RunReadVars(AVar: byte);
begin
    Worker.RunWork(Format('������� var%d', [AVar]),
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
    Worker.RunWork(Format('������� var%d ���.%d', [AVar, addr]),
        procedure
        begin
            Worker.KgsReadVar(addr, AVar);
        end);
end;

procedure RunReadCoefficients(ACoefficient: byte);
begin
    Worker.RunWork(Format('������� ����.%d', [ACoefficient]),
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
    Worker.RunWork(Format('������� ����.%d ���.%d', [ACoefficient, addr]),
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

    Worker.Delay('�������� ����������� ��� ' + floattostr(temperature) + '"C',
      _one_hour_ms * 3);
end;

procedure RunSwitchGasBlock(code: byte);
begin
    Worker.RunWork('������� ����: ' + IntToStr(code),
        procedure
        begin
            Worker.SwitchGasBlock6006(code);
        end);
end;

procedure BlowGas(code: byte);
begin
    TrySwitchGas(code);
    Worker.Delay('�������� ����: ' + IntToStr(code), _one_minute_ms * 10);
end;

procedure BlowAir;
begin
    TrySwitchGas(1);
    Worker.Delay('�������� ��������', _one_minute_ms * 5);
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
    _do([DWriteKef(95, 135), DWriteKef(55, 0), DWriteKef(37, 0)]);

    BlowGas(1);
    ProductsReadVar(100);
    BlowGas(3);
    _do([DWriteKef(28, LastParty.Pgs3), DReadVar(101)]);
end;

initialization

MainWorks := [TWork.Create('�����������������',
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

  TWork.Create('����������', Adjust),

  TWork.Create('������������',
    procedure
    begin
        Adjust;

        BlowGas(4);
        _do([DWriteKef(44, 1), DWriteKef(48, LastParty.Pgs4), DReadVar(102)]);

        BlowGas(2);
        _do([DWriteKef(44, 0), DWriteKef(29, LastParty.Pgs2), DReadVar(102)]);

        BlowGas(1);
        _do([DWriteKef(44, 2), DReadVar(102)]);
        TrySwitchGas(0);
    end),

  TWork.Create('����������������',
    procedure
    begin

        TermochamberSetupTemperature(20);

        BlowGas(1);

        ProductsReadVar(103);
        Worker.Pause(1000 * 60);
        ProductsReadVar(103);

        BlowGas(3);
        ProductsReadVar(107);
        BlowAir;

        TermochamberSetupTemperature(-5);

        BlowGas(1);
        ProductsReadVar(105);
        BlowGas(3);
        ProductsReadVar(109);
        BlowAir;

        TermochamberSetupTemperature(50);

        BlowGas(1);
        ProductsReadVar(104);
        BlowGas(3);
        ProductsReadVar(108);
        BlowAir;

    end)

  ];

end.
