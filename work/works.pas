unit works;

interface

uses data_model;

procedure RunInterrogate;
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
    _one_minute_ms = 1000 * 60 * 60;
    _one_hour_ms = _one_minute_ms * 60;

procedure RunInterrogate;
begin
    Worker.RunWork('опрос',
        procedure
        begin
            Worker.Synchronize(procedure begin
                KgsdumData.NewChartSeries('опрос');
                FormChartSeries.NewChart;

            end );
            while True do
            begin
                Worker.DoEachProduct(
                    procedure(p: TProduct)
                    var
                        AVar: byte;
                    begin
                        try
                            for AVar in KgsVars do
                                Worker.KgsReadVar(p.FAddr, AVar);
                        except
                            on e: EConnectionError do
                            begin
                                Worker.NewLogEntry(loglevError,
                                  p.FormatID + ': ' + e.Message)
                            end;
                        end;

                    end);
            end;
        end);
end;

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
                for AVar in KgsVars do
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
    Worker.SwitchGasBlock6006(code);
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

procedure Adjust;
begin

    BlowGas(1);
    Worker.DoEachProduct(
        procedure(p: TProduct)
        begin
            Worker.KgsReadVar(p.FAddr, 100);
        end);

    BlowGas(3);
    Worker.DoEachProduct(
        procedure(p: TProduct)
        begin
            Worker.KgsWriteCoefficient(p.FAddr, 28, LastParty.Pgs[scaleConc3]);
            Worker.KgsReadVar(p.FAddr, 101);
        end);

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
        Worker.DoEachProduct(
            procedure(p: TProduct)
            begin
                Worker.KgsWriteCoefficient(p.FAddr, 44, 1);
                Worker.KgsWriteCoefficient(p.FAddr, 48,
                  LastParty.Pgs[scaleConc4]);
                Worker.KgsReadVar(p.FAddr, 102);
            end);

        BlowGas(2);
        Worker.DoEachProduct(
            procedure(p: TProduct)
            begin
                Worker.KgsWriteCoefficient(p.FAddr, 44, 0);
                Worker.KgsWriteCoefficient(p.FAddr, 29,
                  LastParty.Pgs[scaleConc2]);
                Worker.KgsReadVar(p.FAddr, 102);
            end);

        BlowGas(1);
        Worker.DoEachProduct(
            procedure(p: TProduct)
            begin
                Worker.KgsWriteCoefficient(p.FAddr, 44, 2);
                Worker.KgsReadVar(p.FAddr, 102);
            end);

    end)];

end.
