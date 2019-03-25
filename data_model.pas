unit data_model;

interface

uses classes, graphics;

type
    TProductField = (pcPlace, pcAddr, pcSerial, pcConc, pcVar0, pcVar1, pcTemp,
      pcConcNorm1, pcConcNorm2, pcConcNorm3, pcConcNorm4, pcConcMinus1,
      pcConcMinus2, pcConcMinus3, pcConcMinus4, pcConcPlus1, pcConcPlus2,
      pcConcPlus3, pcConcPlus4);

    TScaleConc = (scaleConc1, scaleConc2, scaleConc3, scaleConc4);
    TScaleTemp = (scaleTempNorm, scaleTempMinus, scaleTempPlus);

    TScaleConcTemp = record
        ScaleConc: TScaleConc;
        ScaleTemp: TScaleTemp;
    end;

    TCheckValueResult = (cvrNone, cvrOk, cvrErr);

    TConcValue = record
        FConc: double;
        FAbsErr: double;
        FAbsErrLimit: double;
        FErrPercent: double;
        FValid: boolean;
        function Check: TCheckValueResult;
    end;

    TProduct = record
        FProductID, FPartyID: int64;
        FProduction: boolean;
        FPlace, FAddr: integer;
        FSerial: string;
        FConnection: string;

        FVarValue: array [byte] of string;

        FConnectionFailed: boolean;
        FConc: array [TScaleConc, TScaleTemp] of TConcValue;
        FCreatedAt: TDateTime;

        function FormatID: string;

        function GetConc(sc: TScaleConc; st: TScaleTemp): TConcValue;
        function GetConc1(f: TProductField): TConcValue;
        property Conc[sc: TScaleConc; st: TScaleTemp]: TConcValue read GetConc;
        property Conc1[f: TProductField]: TConcValue read GetConc1;
    end;

    TProductProcedure = reference to procedure(_: TProduct);

    TErrorDetail = (erdtConc, erdtAbsErr, erdtErrPercent);

    TParty = record
        FPartyID: int64;
        FCreatedAt: TDateTime;


        Pgs1, Pgs2, Pgs3, Pgs4: double;
        function Pgs(sc:TScaleConc):double;
    end;

    TLogLevel = (loglevTrace, loglevDebug, loglevInfo, loglevWarn, loglevError,
      loglevException);

    TLogEntry = record
        FWorkID : longint;
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
        BucketID : longint;
        UpdatedAt, CreatedAt: TDateTime;
        Name : string;
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
    Var0 = 60;
    Var1 = 61;
    VarTemp = 63;
    KgsMainVars: array [0 .. 3] of byte = (VarConc, Var0, Var1, VarTemp);

    product_column_name: array [TProductField] of string = ('π', '¿‰ÂÒÒ',
      '«‡‚.π', ' ÓÌˆ.', 'Var0', 'Var1', 'T,"C', 'œ√—1', 'œ√—2', 'œ√—3', 'œ√—4',
      'œ√—1-', 'œ√—2-', 'œ√—3-', 'œ√—4-', 'œ√—1+', 'œ√—2+', 'œ√—3+', 'œ√—4+');

implementation

uses SysUtils, math;

function TParty.Pgs(sc:TScaleConc):double;
begin
    case sc of
        scaleConc1: exit(Pgs1);
        scaleConc2: exit(Pgs2);
        scaleConc3: exit(Pgs3);
        scaleConc4: exit(Pgs4);
        else
        assert(false, 'wrong scale point');
    end;
end;

function TProduct.FormatID: string;
begin
    result := Format('¡Œ π%d ID%d ÒÂ.%s', [FPlace + 1, FProductID, FSerial]);
end;

function TConcValue.Check: TCheckValueResult;
begin
    if not FValid then
        exit(cvrNone)
    else
    begin
        if abs(FErrPercent) < 100 then
            exit(cvrOk)
        else
            exit(cvrErr);
    end;

end;

function fieldToConc(field: TProductField): TScaleConc;
begin
    case field of
        pcConcNorm1:
            exit(scaleConc1);
        pcConcNorm2:
            exit(scaleConc2);
        pcConcNorm3:
            exit(scaleConc3);
        pcConcNorm4:
            exit(scaleConc4);
        pcConcMinus1:
            exit(scaleConc1);
        pcConcMinus2:
            exit(scaleConc2);
        pcConcMinus3:
            exit(scaleConc3);
        pcConcMinus4:
            exit(scaleConc4);
        pcConcPlus1:
            exit(scaleConc1);
        pcConcPlus2:
            exit(scaleConc2);
        pcConcPlus3:
            exit(scaleConc3);
        pcConcPlus4:
            exit(scaleConc4);
    else
        exit(TScaleConc(-1));
    end;
end;

function fieldToTemp(field: TProductField): TScaleTemp;
begin
    case field of
        pcConcNorm1:
            exit(scaleTempNorm);
        pcConcNorm2:
            exit(scaleTempNorm);
        pcConcNorm3:
            exit(scaleTempNorm);
        pcConcNorm4:
            exit(scaleTempNorm);
        pcConcMinus1:
            exit(scaleTempMinus);
        pcConcMinus2:
            exit(scaleTempMinus);
        pcConcMinus3:
            exit(scaleTempMinus);
        pcConcMinus4:
            exit(scaleTempMinus);
        pcConcPlus1:
            exit(scaleTempPlus);
        pcConcPlus2:
            exit(scaleTempPlus);
        pcConcPlus3:
            exit(scaleTempPlus);
        pcConcPlus4:
            exit(scaleTempPlus);
    else
        exit(TScaleTemp(-1));
    end;
end;

function fieldIsConc(field: TProductField): boolean;
begin
    result := (integer(fieldToConc(field)) <> -1) and
      (integer(fieldToTemp(field)) <> -1);
end;

function TProduct.GetConc(sc: TScaleConc; st: TScaleTemp): TConcValue;
begin
    result := FConc[sc, st];
end;

function TProduct.GetConc1(f: TProductField): TConcValue;
begin
    Assert(fieldIsConc(f));
    result := FConc[fieldToConc(f), fieldToTemp(f)];
end;

function FormatProductConcValue(product: TProduct; field: TProductField;
  d: TErrorDetail): string;
begin

    Assert(fieldIsConc(field));

    with product.FConc[fieldToConc(field), fieldToTemp(field)] do
    begin
        if not FValid then
            exit('');
        case d of
            erdtConc:
                exit(floattostr(FConc));
            erdtAbsErr:
                exit(floattostr(FAbsErr));
            erdtErrPercent:
                exit(floattostr(FErrPercent));
        end;
    end;
end;

function FormatProductFieldValue(product: TProduct; field: TProductField;
  err_det: TErrorDetail): string;
begin
    with product do
        case field of
            pcPlace:
                exit(IntToStr(FPlace + 1));

            pcAddr:
                exit(IntToStr(FAddr));

            pcSerial:
                exit(FSerial);

            pcConc:
                exit(FVarValue[VarConc]);
            pcVar0:
                exit(FVarValue[Var0]);
            pcVar1:
                exit(FVarValue[Var1]);
            pcTemp:
                exit(FVarValue[VarTemp]);

        else
            if fieldIsConc(field) then
                exit(FormatProductConcValue(product, field, err_det));

            Assert(false, 'uncmown case: ' + IntToStr(integer(field)));
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
begin
    if fieldIsConc(field) then
        exit(product.Conc1[field].Check);
    exit(cvrNone);
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
