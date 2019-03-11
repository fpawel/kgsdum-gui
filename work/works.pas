unit works;

interface

uses run_work, data_model;

procedure RunInterrogate;

implementation

uses sysutils, comport, UnitFormProperties, kgs, UnitFormLastParty,
    classes, windows, errors;

type
    _FuncProduct = reference to procedure(p: TProduct; sync: TSyncP);

procedure _do_each_product1(sync: TSyncP; func: _FuncProduct);
var
    v: double;
    p: TProduct;
    Products: TArray<TProduct>;
begin
    sync(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if length(Products) = 0 then
        raise Exception.Create('нет выбранных приборов для опроса');

    for p in Products do
    begin
        sync(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        try
            func(p, sync);
        except
            on e: EBadResponse do
            begin
                sync(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          e.Message);
                    end);

            end;
            on e: EDeadlineExceeded do
            begin
                sync(
                    procedure
                    begin
                        FormLastParty.SetProductConnectionError(p.FPlace,
                          'не отвечает');
                    end);
            end;
        end;
    end;

end;

procedure _do_each_product(sync: TSyncP; func: _FuncProduct);
begin
    try
        _do_each_product1(sync, func);
    finally
        sync(
            procedure
            begin
                FormLastParty.SetProductInterrogate(-1);
            end);
    end;

end;

procedure RunInterrogate;
begin
    RunWork('Опрос',
        procedure(sync: TSyncP)
        begin
            while True do
            begin
                _do_each_product(sync,
                    procedure(p: TProduct; sync: TSyncP)
                    var
                        v: double;
                    begin
                        v := KgsReadVar(p.FAddr, 72,
                          ComportProductsWorker(sync));
                        sync(
                            procedure
                            begin
                                FormLastParty.SetProductConc(p.FPlace, v);
                            end);
                    end);
            end;
        end);
end;

end.
