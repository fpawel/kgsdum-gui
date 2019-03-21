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
        procedure NewChartSeries(AName: string);

        function GetLastSeriesBucket: TSeriesBucket;

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
    dtStr, dtStr2: string;
    dt: TDateTime;
    fs: TFormatSettings;

begin
    dir := GetEnvironmentVariable('APPDATA') + '\kgsdum\';
    forcedirectories(dir);

    Conn.Connected := true;
    ConnJournal.Connected := true;
    ConnCharts.Connected := true;

    FDQuery1.ExecSQL;
    FDQuery2.ExecSQL;
    FDQuery3.ExecSQL;

    fs := TFormatSettings.Create;
    fs.DateSeparator := '-';
    fs.ShortDateFormat := 'yyyy-MM-dd';
    fs.TimeSeparator := ':';
    fs.ShortTimeFormat := 'hh:mm:ss.zzz';
    fs.LongTimeFormat := 'hh:mm:ss.zzz';
    fs.DecimalSeparator := '.';
end;

function TKgsdumData.GetLastSeriesBucket: TSeriesBucket;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        SQL.Text := 'SELECT * FROM bucket ORDER BY created_at DESC LIMIT 1';
        Open;
        First;
        with result do
            if eof then
            begin
                BucketID := 0;
            end
            else
            begin
                BucketID := FieldValues['bucket_id'];
                UpdatedAt := FieldValues['updated_at'];
                CreatedAt := FieldValues['created_at'];
                Name := FieldValues['name'];
            end;
        Free;
    end;

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

procedure TKgsdumData.NewChartSeries(AName: string);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        SQL.Text := 'INSERT INTO bucket (created_at, name) VALUES (:created_at, :name )';
        ParamByName('name').Value := AName;
        ParamByName('created_at').Value := now;

        ExecSQL;
        Free;
    end;

end;

procedure TKgsdumData.AddSeriesPoint(TheAddr, TheVar: byte; TheValue: double);
var
    i: integer;
    s: string;
    last_bucket: TSeriesBucket;
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

    last_bucket := GetLastSeriesBucket;
    if (last_bucket.BucketID = 0) or
      (last_bucket.CreatedAt <> last_bucket.CreatedAt) and
      (SecondsBetween(now, last_bucket.UpdatedAt) > 60) then
    begin
        NewChartSeries('опрос');
        last_bucket := GetLastSeriesBucket;
    end;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;

        SQL.Text :=
          'INSERT INTO series(bucket_id, addr, var, value, stored_at)  VALUES ';
        for i := 0 to Length(FSeriesPointEntries) - 1 do
            with FSeriesPointEntries[i] do
            begin
                SQL.Text := SQL.Text + ' ( ' + IntToStr(last_bucket.BucketID) +
                  ', ' + IntToStr(Addr) + ', ' + IntToStr(AVar) + ', ' +
                  float_to_str(Value) + ', ' + 'julianday(''' +
                  FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now) + ''') )';
                if i < Length(FSeriesPointEntries) - 1 then
                    SQL.Text := SQL.Text + ', ';

            end;
        s := SQL.Text;
        ExecSQL;
        Free;
    end;

end;

end.
