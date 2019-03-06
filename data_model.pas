unit data_model;

interface

uses  classes, graphics;

type
    TProductField = (pcPlace, pcAddr, pcSerial, pcConnection, pcBegNorm,
      pcMidNorm, pcEndNorm, pcBegMinus, pcMidMinus, pcEndMinus, pcBegPlus,
      pcMidPlus, pcEndPlus);

    TScaleConc = (scaleConcBegin, scaleConcMidle, scaleConcEnd);
    TScaleTemp = (scaleTempNorm, scaleTempMinus, scaleTempPlus);

    TScaleConcTemp = record
        ScaleConc: TScaleConc;
        ScaleTemp: TScaleTemp;
    end;

    TCheckValueResult = (cvrNone, cvrOk, cvrErr);

    TConcValue = record
        FConc: double;
        FAbsErr: double;
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
        FConc: array [TScaleConc, TScaleTemp] of TConcValue;
    end;

    TErrorDetail = (erdtConc, erdtAbsErr, erdtErrPercent);

    TParty = record
        FPartyID: int64;
        FCreatedAt: TDateTime;
        PgsBeg, PgsMid, PgsEnd: double;

    end;



function ProductColumnWidth(column: TProductField; canvas: TCanvas;
  prods: TArray<TProduct>; err_det: TErrorDetail): integer;

function GetProductFields(prods: TArray<TProduct>): TArray<TProductField>;
function FormatProductFieldValue(product: TProduct; field: TProductField;
  err_det: TErrorDetail): string;

function CheckProductFieldValue(product: TProduct;
  field: TProductField): TCheckValueResult;

function ProductFieldAlignment(c: TProductField): TAlignment;

const
    product_column_name: array [TProductField] of string = ('π', '¿‰ÂÒÒ',
      '«‡‚.π', '—‚ˇÁ¸', 'œ√—1.ÌÍÛ', 'œ√—2.ÌÍÛ', 'œ√—3.ÌÍÛ', 'œ√—1-', 'œ√—2-',
      'œ√—3-', 'œ√—1+', 'œ√—2+', 'œ√—3+'

      );

implementation

uses SysUtils, math;

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

function FormatConcValue(v: TConcValue; d: TErrorDetail): string;
begin
    if not v.FValid then
        exit('');
    case d of
        erdtConc:
            exit(floattostr(v.FConc));
        erdtAbsErr:
            exit(floattostr(v.FAbsErr));
        erdtErrPercent:
            exit(floattostr(v.FErrPercent));
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

            pcConnection:
                exit(FConnection);

            pcBegNorm:
                exit(FormatConcValue(FConc[scaleConcBegin, scaleTempNorm],
                  err_det));

            pcMidNorm:
                exit(FormatConcValue(FConc[scaleConcMidle, scaleTempNorm],
                  err_det));

            pcEndNorm:
                exit(FormatConcValue(FConc[scaleConcEnd, scaleTempNorm],
                  err_det));

            pcBegMinus:
                exit(FormatConcValue(FConc[scaleConcBegin, scaleTempMinus],
                  err_det));

            pcMidMinus:
                exit(FormatConcValue(FConc[scaleConcMidle, scaleTempMinus],
                  err_det));

            pcEndMinus:
                exit(FormatConcValue(FConc[scaleConcEnd, scaleTempMinus],
                  err_det));

            pcBegPlus:
                exit(FormatConcValue(FConc[scaleConcBegin, scaleTempPlus],
                  err_det));

            pcMidPlus:
                exit(FormatConcValue(FConc[scaleConcMidle, scaleTempPlus],
                  err_det));

            pcEndPlus:
                exit(FormatConcValue(FConc[scaleConcEnd, scaleTempPlus],
                  err_det));

        else
            assert(false, 'uncmown case: ' + IntToStr(integer(field)));
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
                SetLength(Result, Length(Result) + 1);
                Result[Length(Result) - 1] := c;
                break;
            end;
        end;
    end;
end;

function CheckProductFieldValue(product: TProduct;
  field: TProductField): TCheckValueResult;
begin

    with product do
        case field of
            pcConnection:
                if FConnection <> '' then
                    exit(cvrErr);
            pcBegNorm:
                exit(FConc[scaleConcBegin, scaleTempNorm].Color);

            pcMidNorm:
                exit(FConc[scaleConcMidle, scaleTempNorm].Color);

            pcEndNorm:
                exit(FConc[scaleConcEnd, scaleTempNorm].Color);

            pcBegMinus:
                exit(FConc[scaleConcBegin, scaleTempMinus].Color);

            pcMidMinus:
                exit(FConc[scaleConcMidle, scaleTempMinus].Color);

            pcEndMinus:
                exit(FConc[scaleConcEnd, scaleTempMinus].Color);

            pcBegPlus:
                exit(FConc[scaleConcBegin, scaleTempPlus].Color);

            pcMidPlus:
                exit(FConc[scaleConcMidle, scaleTempPlus].Color);

            pcEndPlus:
                exit(FConc[scaleConcEnd, scaleTempPlus].Color);

        else
            exit(clBlack);
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

    Result := 60;
    w := canvas.TextWidth(product_column_name[column]);
    if Result < w + 30 then
        Result := w + 30;
    for p in prods do
    begin
        w := canvas.TextWidth(FormatProductFieldValue(p, column, err_det));
        if Result < w + 30 then
            Result := w + 30;
    end;
end;

end.
