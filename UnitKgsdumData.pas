unit UnitKgsdumData;

interface

uses
    System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG,
    FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
    FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
    FireDAC.Comp.DataSet, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
    FireDAC.Phys.SQLite, data_model, Vcl.ExtCtrls, FireDAC.ConsoleUI.Wait,
    FireDAC.Comp.UI;

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
        Timer1: TTimer;
        FDGUIxWaitCursor1: TFDGUIxWaitCursor;
        FDQueryProductsValues: TFDQuery;
    FDSQLiteValidate1: TFDSQLiteValidate;
        procedure DataModuleCreate(Sender: TObject);

    private
        { Private declarations }
        FSeriesPointEntries: TSeriesPointEntries;

    public
        { Public declarations }

        procedure SetPartyValue(propertyName: string; AValue: Variant);
        procedure AddSeriesPoint(TheSerial:integer; TheVar: byte; TheValue: double);
        procedure NewChartSeries(AName: string);

        function GetLastSeriesBucket: TSeriesBucket;
        procedure SaveLastSeriesBucket;

    end;

var
    KgsdumData: TKgsdumData;

function DateTimeFromDBString(dt: string): TDateTime;
function DateTimeToDBString(dt: TDateTime): string;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses variants, dateutils, stringutils, UnitFormConsole;

function DateTimeToDBString(dt: TDateTime): string;
begin
    result := 'julianday(''' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',
      dt) + ''')';
end;

function DateTimeFromDBString(dt: string): TDateTime;
var
    fs: TFormatSettings;
begin
    fs := TFormatSettings.Create;
    fs.DateSeparator := '-';
    fs.ShortDateFormat := 'yyyy-MM-dd';
    fs.TimeSeparator := ':';
    fs.ShortTimeFormat := 'hh:mm:ss.zzz';
    fs.LongTimeFormat := 'hh:mm:ss.zzz';
    fs.DecimalSeparator := '.';
    result := StrToDateTime(dt, fs);
end;

procedure TKgsdumData.DataModuleCreate(Sender: TObject);
var
    dir: String;
begin
    dir := GetEnvironmentVariable('APPDATA') + '\kgsdum\';
    forcedirectories(dir);


    FDSQLiteValidate1.Database := conn.Params.Database;
    FDSQLiteValidate1.Sweep;

    Conn.Connected := true;
    ConnJournal.Connected := true;
    ConnCharts.Connected := true;

    FDQuery1.ExecSQL;
    FDQuery2.ExecSQL;
    FDQuery3.ExecSQL;
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
        SQL.Text := 'INSERT INTO bucket (name) VALUES (:name)';
        ParamByName('name').Value := AName;
        ExecSQL;
        Free;
    end;
end;

procedure TKgsdumData.AddSeriesPoint(TheSerial: integer; TheVar: byte; TheValue: double);
var
    i: integer;
    s: string;
    seconds_elapsed: Int64;

begin
    SetLength(FSeriesPointEntries, Length(FSeriesPointEntries) + 1);
    with FSeriesPointEntries[Length(FSeriesPointEntries) - 1] do
    begin
        StoredAt := now;
        AVar := TheVar;
        ASerial := TheSerial;
        Value := TheValue;
    end;

    seconds_elapsed := SecondsBetween(now, FSeriesPointEntries[0].StoredAt);

    if seconds_elapsed > 120 then
        SaveLastSeriesBucket;

end;

procedure TKgsdumData.SaveLastSeriesBucket;
var
    i: integer;
    last_bucket: TSeriesBucket;
begin
    if Length(FSeriesPointEntries) = 0 then
        exit;

    last_bucket := GetLastSeriesBucket;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;

        SQL.Text :=
          'INSERT INTO series(bucket_id, address, variable, value, stored_at)  VALUES ';
        for i := 0 to Length(FSeriesPointEntries) - 1 do
            with FSeriesPointEntries[i] do
            begin
                SQL.Text := SQL.Text + Format(' (%d, %d, %d, %s, %s)',
                  [last_bucket.BucketID, ASerial, AVar, float_to_str(Value),
                  DateTimeToDBString(StoredAt)]);
                if i < Length(FSeriesPointEntries) - 1 then
                    SQL.Text := SQL.Text + ', ';
            end;
        ExecSQL;
        Free;
    end;
    SetLength(FSeriesPointEntries, 0);

end;

end.
