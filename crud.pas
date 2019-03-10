unit crud;

interface

uses data_model, sysutils;

function GetLastPartyProducts: TArray<TProduct>;

function GetLastParty: TParty;

implementation

uses FireDAC.Comp.Client, System.Generics.collections, UnitKgsdumData, Variants;

function GetLastParty: TParty;
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
            FPartyID := FieldValues['party_id'];
            PgsBeg := FieldValues['pgs_beg'];
            PgsMid := FieldValues['pgs_mid'];
            PgsEnd := FieldValues['pgs_end'];
        end;

        Close;
        Free;
    end;
end;

function GetLastPartyProducts: TArray<TProduct>;
var
    p: TProduct;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'SELECT * FROM product_info WHERE party_id = (SELECT * FROM last_party_id) ORDER BY created_at;';
        Open;
        First;
        while not Eof do
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
                FConnection := FieldValues['connection_error'];

                if FieldValues['concentration_beg_norm'] <> Variants.Null then
                    with FConc[scaleConcBegin, scaleTempNorm] do
                    begin
                        FValid := true;
                        FConc := FieldValues['concentration_beg_norm'];
                        FAbsErr := FieldValues['d_beg_norm'];
                        FErrPercent := FieldValues['err_beg_norm_percent'];
                    end;
            end;
            Next;
        end;
        Close;
        Free;
    end;
end;

end.
