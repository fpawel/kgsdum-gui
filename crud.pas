unit crud;

interface

uses data_model, sysutils;

function GetLastPartyProducts: TArray<TProduct>;
procedure SaveProductValue(AProductID:int64; AField:string; AValue:double);

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

        SQL.Text := 'SELECT * FROM last_party;';
        Open;
        First;

        if eof then
        begin
            with TFDQuery.Create(nil) do
            begin
                Connection := KgsdumData.Conn;
                SQL.Text := 'INSERT INTO party DEFAULT VALUES;';
                ExecSQL;
                Free;
            end;

            Close;
            Open;
            First;

            if eof then
                raise Exception.Create('unexpected EOf');
        end;

        with result do
        begin
            FCreatedAt := FieldValues['created_at'];
            s := DateTimeToStr(FCreatedAt);
            FPartyID := FieldValues['party_id'];
            FPgs1 := FieldValues['pgs1'];
            FPgs2 := FieldValues['pgs2'];
            FPgs3 := FieldValues['pgs3'];
            FPgs4 := FieldValues['pgs4'];
        end;

        Close;
        Free;
    end;
end;

function VariantIsEmptyOrNull(const Value: Variant): Boolean;
begin
  Result := VarIsClear(Value) or VarIsEmpty(Value) or VarIsNull(Value) or (VarCompareValue(Value, Unassigned) = vrEqual);
  if (not Result) and VarIsStr(Value) then
    Result := Value = '';
end;

function FetchNullFloat(q:TFDQuery; field:string): TNullFloat;
begin
     if VariantIsEmptyOrNull(q.FieldValues[field]) then
     begin
         Result.FValid := false;
         exit;
     end;
     Result.FValid := true;
     Result.FValue := q.FieldValues[field];

end;

function GetLastPartyProducts: TArray<TProduct>;
var
    party: TParty;

    key: string;
    abs_err, err_limit: double;

    query_product:TFDQuery;

begin
    party := GetLastParty;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;

        SQL.Text :=
          'SELECT product_id FROM product WHERE party_id = (SELECT * FROM last_party_id) ORDER BY created_at;';
        Open;
        First;
        while not eof do
        begin
            SetLength(result, Length(result) + 1);
            with result[Length(result) - 1] do
            begin
                FProductID := FieldValues['product_id'];

                query_product := TFDQuery.Create(nil);
                with query_product do
                begin
                    Connection := KgsdumData.Conn;
                    SQL.Text :=
          'SELECT * FROM product WHERE product_id = :product_id;';
                    ParamByName('product_id').Value := FProductID;

                    Open;
                    First;

                    FPlace := Length(result) - 1;
                    FAddr := FieldValues['addr'];

                    FPartyID := FieldValues['party_id'];
                    FProduction := FieldValues['production'];
                    FSerial := FieldValues['serial_number'];
                    FCreatedAt := FieldValues['created_at'];

                    FPgs1 := party.FPgs1;
                    FPgs4 := party.FPgs4;

                    FPgs1 := party.FPgs1;
                    FWorkPlus20 := FetchNullFloat(query_product, 'work_plus20');
                    FWorkMinus5 := FetchNullFloat(query_product, 'work_minus5');
                    FWorkPlus50 := FetchNullFloat(query_product, 'work_plus50');

                    FWorkGas3 := FetchNullFloat(query_product, 'work_gas3');

                    FRefPlus20 := FetchNullFloat(query_product, 'ref_plus20');
                    FRefMinus5 := FetchNullFloat(query_product, 'ref_minus5');
                    FRefPlus50 := FetchNullFloat(query_product, 'ref_plus50');

                    FConc1Plus20 := FetchNullFloat(query_product, 'c1_plus20');
                    FConc4Plus20 := FetchNullFloat(query_product, 'c4_plus20');

                    FConc1Zero := FetchNullFloat(query_product, 'c1_zero');
                    FConc4Zero := FetchNullFloat(query_product, 'c4_zero');

                    FConc1Plus50 := FetchNullFloat(query_product, 'c1_plus50');
                    FConc4Plus50 := FetchNullFloat(query_product, 'c4_plus50');

                    FConc1Plus20ret := FetchNullFloat(query_product, 'c1_plus20ret');
                    FConc4Plus20ret := FetchNullFloat(query_product, 'c4_plus20ret');


                    Close;
                    Free;
                end;

            end;
            Next;
        end;
        Close;
        Free;
    end;
end;

procedure SaveProductValue(AProductID:int64; AField:string; AValue:double);
begin

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'UPDATE product SET '+ AField + ' = :value WHERE product_id = :product_id;';
        ParamByName('value').Value := AValue;
        ParamByName('product_id').Value := AProductID;
        ExecSQL;
    end;

end;

end.
