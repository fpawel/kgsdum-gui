unit UnitKgsdumData;

interface

uses
    System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG,
    FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
    FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
    FireDAC.Comp.DataSet, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
    FireDAC.Phys.SQLite, data_model;

type
    TQueryProcedure = reference to procedure(_: TFDQuery);

    TKgsdumData = class(TDataModule)
        Conn: TFDConnection;
        FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
        ConnJournal: TFDConnection;
        FDQuery1: TFDQuery;
        FDQuery2: TFDQuery;
        ConnCharts: TFDConnection;
        FDQuery3: TFDQuery;
        procedure DataModuleCreate(Sender: TObject);

    private
        { Private declarations }
        FSeriesPointEntries: TSeriesPointEntries;

    public
        { Public declarations }

        procedure SetPartyValue(propertyName: string; AValue: Variant);
        procedure AddSeriesPoint(TheAddr, TheVar: byte; TheValue: double);
        procedure NewChartSeries(name: string);

    end;

var
    KgsdumData: TKgsdumData;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses variants, dateutils, stringutils;

procedure TKgsdumData.DataModuleCreate(Sender: TObject);
var
    dir: String;

begin
    dir := GetEnvironmentVariable('APPDATA') + '\kgsdum\';
    forcedirectories(dir);

    Conn.Connected := true;
    ConnJournal.Connected := true;
    ConnCharts.Connected := true;

    FDQuery1.ExecSQL;
    FDQuery2.ExecSQL;
    FDQuery3.ExecSQL;

end;

procedure TKgsdumData.SetPartyValue(propertyName: string; AValue: Variant);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text := 'UPDATE party SET ' + propertyName +
          ' = :value WHERE party_id = (SELECT * FROM last_party_id)';
        ParamByName('value').Value := AValue;
        ExecSQL;
        Free;
    end;
end;

procedure TKgsdumData.NewChartSeries(name: string);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        SQL.Text := 'INSERT INTO bucket (name) VALUES (:name)';
        ParamByName('name').Value := name;
        ExecSQL;
        Free;
    end;

end;

procedure TKgsdumData.AddSeriesPoint(TheAddr, TheVar: byte; TheValue: double);
var
    i: integer;
    s: string;
    bucket_created_at, bucket_updated_at: TDateTime;
begin
    SetLength(FSeriesPointEntries, Length(FSeriesPointEntries) + 1);
    with FSeriesPointEntries[Length(FSeriesPointEntries) - 1] do
    begin
        StoredAt := now;
        AVar := TheVar;
        Addr := TheAddr;
        Value := TheValue;
    end;
    if SecondsBetween(now, FSeriesPointEntries[0].StoredAt) < 10 then
        exit;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        SQL.Text := 'SELECT * FROM bucket ORDER BY created_at DESC LIMIT 1';
        Open;
        First;
        if eof then
        begin
            NewChartSeries('опрос');
            bucket_updated_at := now;
            bucket_created_at := bucket_updated_at;
        end
        else
        begin
            bucket_updated_at := FieldValues['updated_at'];
            bucket_created_at := FieldValues['created_at'];
        end;

        if (bucket_updated_at <> bucket_created_at) and
          (SecondsBetween(now, bucket_updated_at) > 60) then
        begin
            NewChartSeries('опрос');
            bucket_updated_at := now;
        end;

        SQL.Text :=
          'WITH q AS (SELECT bucket_id FROM bucket ORDER BY created_at DESC LIMIT 1)'
          + 'INSERT INTO series(bucket_id, addr, var, value, stored_at)  VALUES ';
        for i := 0 to Length(FSeriesPointEntries) - 1 do
            with FSeriesPointEntries[i] do
            begin
                SQL.Text := SQL.Text + ' ( (SELECT bucket_id FROM q), ' +
                  IntToStr(Addr) + ', ' + IntToStr(AVar) + ', ' +
                  float_to_str(Value) + ', ' + 'julianday(''' +
                  FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now) + ''') )';
                if i < Length(FSeriesPointEntries) - 1 then
                    SQL.Text := SQL.Text + ', ';

            end;
        s := SQL.Text;
        Open;
        Free;
    end;

end;

end.
