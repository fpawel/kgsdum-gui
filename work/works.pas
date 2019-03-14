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
    classes, windows, run_work, errors, UnitFormJournal;


procedure RunWork(AName:string; AWork:TWorkProcedure);
begin
    RunWorks([TWork.Create(AName, AWork)]);
end;

function ReadProductConc(p: TProduct; w: TComportWorker): double;
var
    v: double;
begin
    v := KgsReadVar(p.FAddr, 72, w);
    Synchronize(
        procedure
        begin
            FormLastParty.SetProductConc(p.FPlace, v);
            FormJournal.NewEntry(loglevInfo, Format('%s: конц.=%s',
                  [p.FormatID, floattostr(v)]));

        end);
end;

function ReadAddrVar(addr: byte; AVar: byte): double;
var
    v: double;
begin
    v := KgsReadVar(addr, AVar, ComportProductsWorker);
    Synchronize(
        procedure
        begin
            if FormLastParty.FindProductWithAddr(addr,
                procedure(p: TProduct)
                begin
                    FormJournal.NewEntry(loglevInfo,
                      Format('%s: var%d=%s', [p.FormatID, AVar,
                      floattostr(v)]));

                end) then
                exit;
            FormJournal.NewEntry(loglevInfo,
            Format('адр.%d: var%d=%s',
              [addr, AVar, floattostr(v)]));
        end);
end;

function ReadAddrCoefficient(addr: byte; ACoefficient: byte): double;
var
    v: double;
begin
    v := KgsReadCoefficient(addr, ACoefficient, ComportProductsWorker);
    Synchronize(
        procedure
        begin
            if FormLastParty.FindProductWithAddr(addr,
                procedure(p: TProduct)
                begin
                    FormJournal.NewEntry(loglevInfo,
                      Format('%s: коэф.%d=%s', [p.FormatID, ACoefficient,
                      floattostr(v)]));

                end) then
                exit;
            FormJournal.NewEntry(loglevInfo,
                Format('адр.%d: коэф.%d=%s',
              [addr, ACoefficient, floattostr(v)]));
        end);
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
                        ReadProductConc(p, ComportProductsWorker);
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
