unit works;

interface

uses data_model;

procedure RunInterrogate;

implementation

uses sysutils, comport, UnitFormProperties, kgs, UnitFormLastParty,
    classes, windows, run_work, errors, UnitFormConsole;

type
    _FuncProduct = reference to procedure(p: TProduct);

procedure _do_each_product1(func: _FuncProduct);
var
    v: double;
    p: TProduct;
    Products: TArray<TProduct>;
begin
    Synchronize(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if length(Products) = 0 then
        raise Exception.Create('нет выбранных приборов дл€ опроса');

    for p in Products do
    begin
        Synchronize(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        try
            func(p);
        except
            on e: EBadResponse do
            begin
                Synchronize(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          e.Message);
                    end);

            end;
            on e: EDeadlineExceeded do
            begin
                Synchronize(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          'не отвечает');
                    end);
            end;
        end;
    end;

end;

procedure _do_each_product(func: _FuncProduct);
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

procedure RunInterrogate;
begin
    RunWork('ќпрос',
        procedure
        begin
            while True do
            begin
                _do_each_product(
                    procedure(p: TProduct)
                    var
                        v: double;
                    begin
                        v := KgsReadVar(p.FAddr, 72, ComportProductsWorker);
                        Synchronize(
                            procedure
                            begin
                                FormLastParty.SetProductConc(p.FPlace, v);
                            end);
                    end);
            end;
        end);
end;


function ReadVarWithLog(addr: byte; AVar: byte);
begin


end;

procedure RunReadVar(addr: byte; AVar: byte);
begin
    RunWork(Format('”правление: считать регистр %d, адрес %d', [AVar, addr]),
        procedure
        begin
            if addr = 0 then
            begin
                _do_each_product(
                    procedure(p: TProduct)
                    var
                        v: double;
                    begin
                        v := KgsReadVar(p.FAddr, AVar, ComportProductsWorker);
                        Synchronize(
                            procedure
                            begin
                                FormLastParty.SetProductConc(p.FPlace, v);
                                FormConsole.AddLine(loglevInfo,
                                  Format('адрес %d, регистр %d : %s',
                                  [addr, AVar, floattostr(v)]));
                            end);
                    end);

            end;
        end);
end;

end.
