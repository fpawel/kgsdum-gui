unit works;

interface

uses data_model;

procedure RunInterrogate;
procedure RunReadVar(addr: byte; AVar: byte);
procedure RunReadVars(AVar: byte);
procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
procedure RunReadCoefficients(ACoefficient: byte);
procedure RunSwitchGasBlock(code: byte);

implementation

uses sysutils, comport, kgs, UnitFormLastParty,
    classes, windows, run_work, hardware_errors, UnitAppIni, modbus, crud;

const
    _one_minute_ms = 1000 * 60 * 60;
    _one_hour_ms = _one_minute_ms * 60;

var
    _works: TWorks;




procedure RunInterrogate;
begin
    RunWork('опрос',
        procedure
        begin
            while True do
            begin
                DoEachProduct(
                    procedure(p: TProduct)
                    var AVar:byte;
                    begin
                        for AVar in KgsVars  do
                            KgsReadVar(p.FAddr, AVar);
                    end);
            end;
        end);
end;

procedure RunReadVars(AVar: byte);
begin
    RunWork(Format('считать var%d', [AVar]),
        procedure
        begin
            DoEachProduct(
                procedure(p: TProduct)
                begin
                    KgsReadVar(p.FAddr, AVar);
                end);

        end);
end;

procedure RunReadVar(addr: byte; AVar: byte);
begin
    RunWork(Format('считать var%d адр.%d', [AVar, addr]),
        procedure
        begin
            KgsReadVar(addr, AVar);
        end);
end;

procedure RunReadCoefficients(ACoefficient: byte);
begin
    RunWork(Format('считать коэф.%d', [ACoefficient]),
        procedure
        begin
            DoEachProduct(
                procedure(p: TProduct)
                begin
                    KgsReadCoefficient(p.FAddr, ACoefficient);
                end);

        end);
end;

procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
begin
    RunWork(Format('считать коэф.%d адр.%d', [ACoefficient, addr]),
        procedure
        begin
            KgsReadCoefficient(addr, ACoefficient);
        end);
end;

procedure TermochamberSetupTemperature(temperature: double);
begin
    TermochamberSetSetpoint(temperature);
    while abs(TermochamberReadTemperature - temperature) < 1 do
        DoEachProduct(
            procedure(p: TProduct)
            var
                AVar: byte;
            begin
                for AVar in KgsVars do
                    KgsReadVar(p.FAddr, AVar);

            end);
    Delay('выдержка термокамеры при ' + floattostr(temperature) + '\"C',
      _one_hour_ms * 3);
end;

procedure SwitchGasBlock6006(code: byte);
begin
    try
        modbus.GetResponse($20, $10, [0, $10, 0, 1, 2, 0, code],
          ComportProductsWorker,
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

procedure RunSwitchGasBlock(code: byte);
begin
    RunWork('Газовый блок: ' + IntToStr(code),
        procedure
        begin
            SwitchGasBlock6006(code);
        end);
end;

procedure BlowGas(code: byte);
begin
    SwitchGasBlock6006(code);
    Delay('продувка газа: ' + IntToStr(code), _one_minute_ms * 5);
end;



initialization

_works := [TWork.Create('термоциклирование',
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

  TWork.Create('калибровка',
    procedure
    var
        party: TParty;
    begin

        Synchronize(
            procedure
            begin
                party := GetLastParty;
            end);

        BlowGas(1);
        DoEachProduct(
            procedure(p: TProduct)
            begin
                KgsReadVar(p.FAddr, 100);
            end);

        BlowGas(3);
        DoEachProduct(
            procedure(p: TProduct)
            begin
                KgsWriteCoefficient(p.FAddr, 44, 1);
                KgsWriteCoefficient(p.FAddr, 48, party.Pgs[scaleConc3]);
                KgsReadVar(p.FAddr, 102);
            end);

    end)

  ];

end.
