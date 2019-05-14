unit data_model;

interface

uses classes, graphics;

type
    TProductField = (pcPlace, pcProductID, pcAddr, pcSerial, pcVarConc, pcVarWork,
      pcVarRef, pcVarTemp,

      pc_work_plus20,
      pc_ref_plus20,

      pc_work_gas3,

      pc_work_minus5,
      pc_ref_minus5,

      pc_work_plus50,
      pc_ref_plus50,



      pc_conc1_plus20, pc_conc4_plus20, pc_conc1_zero, pc_conc4_zero,
      pc_conc1_plus50, pc_conc4_plus50,
      pc_conc1_plus20ret, pc_conc4_plus20ret

      );

    TCheckValueResult = (cvrNone, cvrOk, cvrErr);

    TNullFloat = record
        FValue: double;
        FValid: boolean;
        function ToString: string;
    end;

    TProduct = record
        FCreatedAt: TDateTime;
        FProductID, FPartyID: int64;
        FProduction: boolean;
        FPlace, FAddr: integer;
        FSerial: string;
        FConnection: string;
        FConnectionFailed: boolean;

        FPgs1, FPgs4: double;

        FVarConc, FVarWork, FVarRef, FVarTemp: string;

        FWorkPlus20, FWorkMinus5, FWorkPlus50, FRefPlus20, FRefMinus5,
          FRefPlus50,

          FWorkGas3,

          FConc1Plus20, FConc4Plus20, FConc1Zero, FConc4Zero, FConc1Plus50,
          FConc4Plus50, FConc1Plus20ret, FConc4Plus20ret : TNullFloat;

        function FormatID: string;

    end;

    TProductProcedure = reference to procedure(_: TProduct);

    TErrorDetail = (erdtConc, erdtAbsErr, erdtErrPercent);

    TParty = record
        FPartyID: int64;
        FCreatedAt: TDateTime;

        FPgs1, FPgs2, FPgs3, FPgs4: double;
    end;

    TLogLevel = (loglevDebug, loglevInfo, loglevWarn, loglevError);

    TLogEntry = record
        FWorkID: longint;
        FCreatedAt: TDateTime;
        FLevel: TLogLevel;
        FWork, FText: string;
    end;

    TWorkEntryInfo = record
        WorkID: longint;
        CreatedAt: TDateTime;
        Name: string;
        ErrorOccurred: boolean;
    end;

    TWorkProcedure = reference to procedure;

    TWork = record
        Name: string;
        Proc: TWorkProcedure;
        constructor Create(AName: string; AProc: TWorkProcedure);
    end;

    TWorks = TArray<TWork>;

    TSeriesPointEntry = record
        StoredAt: TDateTime;
        AVar, Addr: byte;
        Value: double;
    end;

    TSeriesPointEntries = TArray<TSeriesPointEntry>;

    TSeriesBucket = record
        BucketID: longint;
        UpdatedAt, CreatedAt: TDateTime;
        Name: string;
    end;

function ProductColumnWidth(column: TProductField; canvas: TCanvas;
  prods: TArray<TProduct>; err_det: TErrorDetail): integer;

function GetProductFields(prods: TArray<TProduct>): TArray<TProductField>;
function FormatProductFieldValue(product: TProduct; field: TProductField;
  err_det: TErrorDetail): string;

function CheckProductFieldValue(product: TProduct; field: TProductField)
  : TCheckValueResult;

function ProductFieldAlignment(c: TProductField): TAlignment;

const
    VarConc = 72;
    VarRef = 60;
    VarWork = 61;
    VarTemp = 63;
    KgsMainVars: array [0 .. 3] of byte = (VarConc, VarWork, VarRef, VarTemp);

    product_column_name: array [TProductField] of string = ('№', 'ID', 'Адресс',
      'Зав.№', 'Конц.',

        'Work', 'Ref', 'T',

        'Work+20⁰C',
        'Ref+20⁰C',

        'Work_ПГС3',

        'Work-5⁰C',
        'Ref-5⁰C',

        'Work+50⁰C',
        'Ref+50⁰C',

        'ПГС1+20⁰C(1)', 'ПГС4+20⁰C(1)',
        'ПГС1 0⁰C', 'ПГС4 0⁰C',
        'ПГС1+50⁰C', 'ПГС4+50⁰C',
        'ПГС1+20⁰C(2)', 'ПГС4+20⁰C(2)'

        );

implementation

uses SysUtils, math;

function TNullFloat.ToString: string;
begin
    if not FValid then
        exit('');
    exit(floattostr(FValue));
end;

function TProduct.FormatID: string;
begin
    result := Format('№%d ID=%d заводской_номер=%s', [FPlace + 1, FProductID, FSerial]);
end;

function FormatProductFieldValue(product: TProduct; field: TProductField;
  err_det: TErrorDetail): string;
begin
    with product do
    begin
        case field of
            pcProductID:
                exit(IntToStr(FProductID));
            pcPlace:
                exit(IntToStr(FPlace + 1));
            pcAddr:
                exit(IntToStr(FAddr));
            pcSerial:
                exit(FSerial);
            pcVarConc:
                if FConnectionFailed then
                    exit('нет связи')
                else
                    exit(FVarConc);
            pcVarWork:
                exit(FVarWork);
            pcVarRef:
                exit(FVarRef);
            pcVarTemp:
                exit(FVarTemp);

            pc_work_plus20:
                exit(FWorkPlus20.ToString);
            pc_work_minus5:
                exit(FWorkMinus5.ToString);
            pc_work_plus50:
                exit(FWorkPlus50.ToString);

            pc_work_gas3:
                exit(FWorkGas3.ToString);

            pc_ref_plus20:
                exit(FRefPlus20.ToString);
            pc_ref_minus5:
                exit(FRefMinus5.ToString);
            pc_ref_plus50:
                exit(FRefPlus50.ToString);

            pc_conc1_plus20:
                exit(FConc1Plus20.ToString);
            pc_conc4_plus20:
                exit(FConc4Plus20.ToString);
            pc_conc1_zero:
                exit(FConc1Zero.ToString);
            pc_conc4_zero:
                exit(FConc4Zero.ToString);
            pc_conc1_plus50:
                exit(FConc1Plus50.ToString);
            pc_conc4_plus50:
                exit(FConc4Plus50.ToString);

            pc_conc1_plus20ret:
                exit(FConc1Plus20ret.ToString);
            pc_conc4_plus20ret:
                exit(FConc4Plus20ret.ToString);

        else

            assert(false, 'uncmown case: ' + IntToStr(integer(field)));
        end;
    end;

end;

function GetProductFields(prods: TArray<TProduct>): TArray<TProductField>;
var
    p: TProduct;
    c: TProductField;
begin
    // [pcCol0, pcSerial]
    for c := Low(TProductField) to High(TProductField) do
    begin
        for p in prods do
        begin
            if FormatProductFieldValue(p, c, erdtConc) <> '' then
            begin
                SetLength(result, Length(result) + 1);
                result[Length(result) - 1] := c;
                break;
            end;
        end;
    end;
end;

function CheckProductFieldValue(product: TProduct; field: TProductField)
  : TCheckValueResult;

    function chck(conc: TNullFloat; pgs: double): TCheckValueResult;
    var d, limit_d : double;
    begin
        if not conc.FValid then
            exit(cvrNone);
        d := abs(conc.FValue - pgs);
        limit_d := 0.1 + pgs * 0.12;
        if abs(d) < limit_d then
            exit(cvrOk)
        else
            exit(cvrErr);
    end;

begin
    with product do
    begin
        case field of
            pc_conc1_plus20:
                exit(chck(FConc1Plus20, FPgs1));
            pc_conc4_plus20:
                exit(chck(FConc4Plus20, FPgs4));

            pc_conc1_zero:
                exit(chck(FConc1Zero, FPgs1));
            pc_conc4_zero:
                exit(chck(FConc4Zero, FPgs4));

            pc_conc1_plus50:
                exit(chck(FConc1Plus50, FPgs1));
            pc_conc4_plus50:
                exit(chck(FConc4Plus50, FPgs4));

            pc_conc1_plus20ret:
                exit(chck(FConc1Plus20ret, FPgs1));
            pc_conc4_plus20ret:
                exit(chck(FConc4Plus20ret, FPgs4));

        else
            exit(cvrNone);
        end;
    end;

end;

function ProductFieldAlignment(c: TProductField): TAlignment;
begin
    case c of
        pcSerial:
            exit(taLeftJustify);
        pcAddr:
            exit(taCenter);
    else
        exit(taRightJustify);
    end;
end;

function ProductColumnWidth(column: TProductField; canvas: TCanvas;
  prods: TArray<TProduct>; err_det: TErrorDetail): integer;
var
    p: TProduct;
    w: integer;
begin

    result := 60;
    w := canvas.TextWidth(product_column_name[column]);
    if result < w + 30 then
        result := w + 30;
    for p in prods do
    begin
        w := canvas.TextWidth(FormatProductFieldValue(p, column, err_det));
        if result < w + 30 then
            result := w + 30;
    end;
end;

constructor TWork.Create(AName: string; AProc: TWorkProcedure);
begin
    Name := AName;
    Proc := AProc;
end;

end.
