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

    end;

var
    KgsdumData: TKgsdumData;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}



procedure TKgsdumData.DataModuleCreate(Sender: TObject);
var dir:String;

begin
    dir := GetEnvironmentVariable('APPDATA') + '\kgsdum\';

    forcedirectories(dir);

    FDQuery1.ExecSQL;
    FDQuery2.ExecSQL;
end;

end.
