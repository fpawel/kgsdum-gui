unit crud;

interface

uses data_model, sysutils;

function GetLastPartyProducts: TArray<TProduct>;

function GetLastParty: TParty;

implementation

uses FireDAC.Comp.Client, System.Generics.collections, UnitKgsdumData, Variants;

function GetLastParty: TParty;
var
    s: string;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;

        SQL.Text :=
          'INSERT INTO party (created_at) SELECT CURRENT_TIMESTAMP WHERE NOT EXISTS(SELECT 1 FROM party)';
        ExecSQL;
        Close;

        SQL.Text := 'SELECT * FROM last_party;';
        Open;
        First;

        if eof then
            raise Exception.Create('unexpected EOF');

        with result do
        begin
            FCreatedAt := FieldValues['created_at'];
            s := DateTimeToStr(FCreatedAt);
            FPartyID := FieldValues['party_id'];
            Pgs[scaleConc1] := FieldValues['pgs1'];
            Pgs[scaleConc2] := FieldValues['pgs2'];
            Pgs[scaleConc3] := FieldValues['pgs3'];
            Pgs[scaleConc4] := FieldValues['pgs4'];
        end;

        Close;
        Free;
    end;
end;

function GetLastPartyProducts: TArray<TProduct>;
var
    party: TParty;
    _sc: TScaleConc;
    _st: TScaleTemp;

    key: string;
    abs_err, err_limit:double;


begin
    party := GetLastParty;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;

        SQL.Text :=
          'SELECT * FROM product WHERE party_id = (SELECT * FROM last_party_id) ORDER BY created_at;';
        Open;
        First;
        while not eof do
        begin
            SetLength(result, Length(result) + 1);
            with result[Length(result) - 1] do
            begin
                FPlace := Length(result) - 1;
                FAddr := FieldValues['addr'];
                FProductID := FieldValues['product_id'];
                FPartyID := FieldValues['party_id'];
                FProduction := FieldValues['production'];
                FSerial := FieldValues['serial_number'];
                FCreatedAt := FieldValues['created_at'];
                for _sc := low(TScaleConc) to High(TScaleConc) do
                    for _st := Low(TScaleTemp) to High(TScaleTemp) do
                    begin
                        case _st of
                            scaleTempNorm:
                                key := 'c_norm';
                            scaleTempMinus:
                                key := 'c_minus';
                            scaleTempPlus:
                                key := 'c_plus';
                        end;
                        key := key + IntToStr(integer(_sc) + 1);

                        with FConc[_sc, _st] do
                        begin
                            if FieldValues[key] <> Variants.Null then
                            begin
                                FConc := FieldValues[key];
                                FValid := true;
                            end
                            else
                            begin
                                FConc := 0;
                                FValid := false;
                            end;
                            err_limit := 0.1 + 0.12 * party.Pgs[_sc];
                            abs_err := FConc - party.Pgs[_sc];

                            FAbsErrLimit := err_limit;
                            FAbsErr := abs_err;
                            FErrPercent := 100.0 * FAbsErr / FAbsErrLimit;
                        end;
                    end;
            end;
            Next;
        end;
        Close;
        Free;
    end;
end;

end.
