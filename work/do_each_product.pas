unit do_each_product;

interface

uses classes, data_model;

type
    TSynchronizeProcedure = reference to procedure (_: TThreadProcedure);
    TWorkLogProcedure = procedure (ALevel: TLogLevel; AText: string);

procedure DoEachProduct(sync:TSynchronizeProcedure; work_log:TWorkLogProcedure;
    func: TProductProcedure);

implementation

uses UnitFormLastParty, hardware_errors, sysutils;

procedure _do_each_product1(sync:TSynchronizeProcedure; work_log:TWorkLogProcedure;
    func: TProductProcedure);
var
    v: double;
    p: TProduct;
    i: integer;
    Products: TArray<TProduct>;
begin
    sync(
        procedure
        begin
            Products := FormLastParty.ProductionProducts;
        end);

    if length(Products) = 0 then
        raise EConfigError.Create('не отмечено ни одного прибора в таблице');

    for i := 0 to length(Products) - 1 do
    begin
        p := Products[i];
        sync(
            procedure
            begin
                FormLastParty.SetProductInterrogate(p.FPlace);
            end);

        func(p);
    end;
end;


procedure DoEachProduct(sync:TSynchronizeProcedure; work_log:TWorkLogProcedure;
    func: TProductProcedure);
begin
    try
        _do_each_product1(sync, work_log, func);
    finally
        sync(
            procedure
            begin
                FormLastParty.SetProductInterrogate(-1);
            end);
    end;
end;



end.
