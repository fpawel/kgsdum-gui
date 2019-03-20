unit UnitAppIni;

interface

uses
    System.SysUtils, System.Classes, inifiles;

type
    TAppIni = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
        function GetComportProductsName: string;
        function GetComportTempName: string;

        procedure SetComportProductsName(value: string);
        procedure SetComportTempName(value: string);
    public
        { Public declarations }
        Ini: TInifile;

        property ComportProductsName: string read GetComportProductsName
          write SetComportProductsName;
        property ComportTempName: string read GetComportTempName
          write SetComportTempName;

    end;

var
    AppIni: TAppIni;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TAppIni.DataModuleCreate(Sender: TObject);
begin
    Ini := TInifile.Create(ExtractFileDir(paramstr(0)) + '\main.ini');
end;

function TAppIni.GetComportProductsName: string;
begin
    result := AppIni.Ini.ReadString('work', 'comport_products', 'COM1')
end;

function TAppIni.GetComportTempName: string;
begin
    result := AppIni.Ini.ReadString('work', 'comport_temp', 'COM1')
end;

procedure TAppIni.SetComportProductsName(value: string);
begin
    AppIni.Ini.WriteString('work', 'comport_products', value);
end;

procedure TAppIni.SetComportTempName(value: string);
begin
    AppIni.Ini.WriteString('work', 'comport_temp', value)
end;

end.
