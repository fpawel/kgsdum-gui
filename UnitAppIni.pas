unit UnitAppIni;

interface

uses
    System.SysUtils, System.Classes, inifiles;

type
    TAppIni = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }


    public
        { Public declarations }
        Ini: TInifile;

        function GetComportProductsName: string;
        function GetComportTempName: string;

        procedure SetComportProductsName(value: string);
        procedure SetComportTempName(value: string);


        procedure SetGasTime(value: integer);
        procedure SetTempTime(value: integer);

        function GetGasTime: integer;
        function GetTempTime: integer;

        function GetTempLow1: double;
        function GetTempLow2: double;
        function GetTempHigh1: double;
        function GetTempHigh2: double;

        procedure SetTempLow1(value: double);
        procedure SetTempLow2(value: double);
        procedure SetTempHigh1(value: double);
        procedure SetTempHigh2(value: double);

        procedure SetTempNku(value: double);
        function GetTempNku: double;

        property ComportProductsName: string read GetComportProductsName
          write SetComportProductsName;
        property ComportTempName: string read GetComportTempName
          write SetComportTempName;

        property GasTime: integer read GetGasTime
          write SetGasTime;
        property TempTime: integer read GetTempTime
          write SetTempTime;

        property TempLow1: double read GetTempLow1
          write SetTempLow1;
        property TempLow2: double read GetTempLow2
          write SetTempLow2;
        property TempHigh1: double read GetTemphigh1
          write SetTemphigh1;
        property TempHigh2: double read GetTemphigh2
          write SetTempHigh2;
        property TempNku: double read GetTempNku
          write SetTempNku;




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

function TAppIni.GetGasTime: integer;
begin
    result := Ini.ReadInteger('work', 'gas_time', 5);
end;

function TAppIni.GetTempTime: integer;
begin
    result := Ini.ReadInteger('work', 'temp_time', 120);
end;

procedure TAppIni.SetGasTime(value: integer);
begin
    Ini.WriteInteger('work', 'gas_time', value);
end;

procedure TAppIni.SetTempTime(value: integer);
begin
    Ini.WriteInteger('work', 'temp_time', value);
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
    AppIni.Ini.WriteString('work', 'comport_temp', value);
end;

function TAppIni.GetTempLow1: double;
begin
    result := AppIni.Ini.ReadFloat('work', 'temp_low1', -60);
end;

procedure TAppIni.SetTempLow1(value: double);
begin
    AppIni.Ini.WriteFloat('work', 'temp_low1', value);
end;

function TAppIni.GetTempLow2: double;
begin
    result := AppIni.Ini.ReadFloat('work', 'temp_low2', -60);
end;

procedure TAppIni.SetTempLow2(value: double);
begin
    AppIni.Ini.WriteFloat('work', 'temp_low2', value);
end;

function TAppIni.GetTempHigh1: double;
begin
    result := AppIni.Ini.ReadFloat('work', 'temp_high1', 60);
end;

procedure TAppIni.SetTempHigh1(value: double);
begin
    AppIni.Ini.WriteFloat('work', 'temp_high1', value);
end;

function TAppIni.GetTempHigh2: double;
begin
    result := AppIni.Ini.ReadFloat('work', 'temp_high2', 60);
end;

procedure TAppIni.SetTemphigh2(value: double);
begin
    AppIni.Ini.WriteFloat('work', 'temp_high2', value);
end;

procedure TAppIni.SetTempNku(value: double);
begin
    AppIni.Ini.WriteFloat('work', 'temp_nku', value);
end;

function TAppIni.GetTempNku: double;
begin
    result := AppIni.Ini.ReadFloat('work', 'temp_nku', 20);
end;

end.
