unit UnitKgsdumData;

interface

uses
    System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.PG,
    FireDAC.Phys.PGDef, FireDAC.VCLUI.Wait, Data.DB, FireDAC.Comp.Client,
    FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
    FireDAC.Comp.DataSet, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteDef,
    FireDAC.Phys.SQLite;

type
    TQueryProcedure = reference to procedure(_: TFDQuery);

    TKgsdumData = class(TDataModule)
        Conn: TFDConnection;
        FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
        ConnJournal: TFDConnection;
        FDQuery1: TFDQuery;
        FDQuery2: TFDQuery;
        procedure DataModuleCreate(Sender: TObject);

    private
        { Private declarations }
    public
        { Public declarations }
        function GetAppConfig(propertyName: string;
          ADefaultValue: Variant): Variant;
        procedure SetAppConfig(propertyName: string; AValue: Variant);

        procedure SetPartyValue(propertyName: string; AValue: Variant);

    end;

var
    KgsdumData: TKgsdumData;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

uses variants;

procedure TKgsdumData.DataModuleCreate(Sender: TObject);
var
    dir: String;

begin
    dir := GetEnvironmentVariable('APPDATA') + '\kgsdum\';
    forcedirectories(dir);
    FDQuery1.ExecSQL;
    FDQuery2.ExecSQL;
end;

procedure TKgsdumData.SetAppConfig(propertyName: string; AValue: Variant);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'INSERT OR REPLACE INTO app_config (property, value) VALUES (:property, :value);';
        ParamByName('property').Value := propertyName;
        ParamByName('value').Value := AValue;
        ExecSQL;
        Free;
    end;

end;

function TKgsdumData.GetAppConfig(propertyName: string;
  ADefaultValue: Variant): Variant;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'SELECT value FROM app_config WHERE property =:property_name';
        ParamByName('property_name').Value := propertyName;
        Open;
        First;
        if not Eof then
            result := FieldValues['value']
        else
            result := ADefaultValue;
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

end.
