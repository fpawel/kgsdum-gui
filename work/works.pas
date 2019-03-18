unit works;

interface

uses data_model;

procedure RunInterrogate;
procedure RunReadVar(addr: byte; AVar: byte);
procedure RunReadVars(AVar: byte);
procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
procedure RunReadCoefficients(ACoefficient: byte);

implementation

uses sysutils, comport, UnitFormProperties, kgs, UnitFormLastParty,
    classes, windows, run_work, hardware_errors;

function ReadProductVar(p: TProduct; AVar: byte; w: TComportWorker): double;
var
    v: double;
begin
    v := KgsReadVar(p.FAddr, AVar, w);
    NewWorkLogEntry(loglevInfo, Format('%s: Var%d=%s', [p.FormatID, AVar,
      floattostr(v)]));
    p.FConnection := Format('Var%d=%s', [AVar, floattostr(v)]);

    case AVar of
        72:
            p.FValueConc := floattostr(v);
        60:
            p.FValueVar0 := floattostr(v);
        61:
            p.FValueVar1 := floattostr(v);
        63:
            p.FValueTemp := floattostr(v);

    end;

    p.FConnectionFailed := false;
    Synchronize(
        procedure
        begin
            FormLastParty.SetProduct(p);
        end);
end;

function ReadAddrVar(addr: byte; AVar: byte): double;
var
    v: double;
begin
    v := KgsReadVar(addr, AVar, ComportProductsWorker);
    if FormLastParty.FindProductWithAddr(addr,
        procedure(p: TProduct)
        begin
            NewWorkLogEntry(loglevInfo, Format('%s: var%d=%s',
              [p.FormatID, AVar, floattostr(v)]));
        end) then
        exit;
    NewWorkLogEntry(loglevInfo, Format('адр.%d: var%d=%s',
      [addr, AVar, floattostr(v)]));
end;

function ReadAddrCoefficient(addr: byte; ACoefficient: byte): double;
var
    v: double;
begin
    v := KgsReadCoefficient(addr, ACoefficient, ComportProductsWorker);
    if FormLastParty.FindProductWithAddr(addr,
        procedure(p: TProduct)
        begin
            NewWorkLogEntry(loglevInfo, Format('%s: коэф.%d=%s',
              [p.FormatID, ACoefficient, floattostr(v)]));
        end) then
        exit;
    NewWorkLogEntry(loglevInfo, Format('адр.%d: коэф.%d=%s',
      [addr, ACoefficient, floattostr(v)]));
end;

procedure RunInterrogate;
begin
    RunWork('опрос',
        procedure
        begin
            while True do
            begin
                DoEachProduct(
                    procedure(p: TProduct)
                    begin
                        ReadProductVar(p, 72, ComportProductsWorker);
                        ReadProductVar(p, 60, ComportProductsWorker);
                        ReadProductVar(p, 61, ComportProductsWorker);
                        ReadProductVar(p, 63, ComportProductsWorker);
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
                    ReadAddrVar(p.FAddr, AVar);
                end);

        end);
end;

procedure RunReadVar(addr: byte; AVar: byte);
begin
    RunWork(Format('считать var%d адр.%d', [AVar, addr]),
        procedure
        begin
            ReadAddrVar(addr, AVar);
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
                    ReadAddrCoefficient(p.FAddr, ACoefficient);
                end);

        end);
end;

procedure RunReadCoefficient(addr: byte; ACoefficient: byte);
begin
    RunWork(Format('считать коэф.%d адр.%d', [ACoefficient, addr]),
        procedure
        begin
            ReadAddrVar(addr, ACoefficient);
        end);
end;

end.
