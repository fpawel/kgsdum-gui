unit UnitFormAppConfig;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, System.Generics.collections,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    ComponentBaloonHintU;

type
    TProcFloat = procedure(v:double) of object;
    TProcbyte = procedure(v:byte) of object;
    TFormAppConfig = class(TForm)
        Panel19: TPanel;
        Panel20: TPanel;
        Panel1: TPanel;
        Shape1: TShape;
        Panel2: TPanel;
        ComboBoxComportProducts: TComboBox;
        Panel3: TPanel;
        Shape2: TShape;
        Panel4: TPanel;
        EditPgs1: TEdit;
        Panel5: TPanel;
        Shape3: TShape;
        Panel6: TPanel;
        ComboBoxComportTemp: TComboBox;
        Panel7: TPanel;
        Shape4: TShape;
        Panel8: TPanel;
        EditPgs4: TEdit;
        Panel9: TPanel;
        Shape5: TShape;
        Panel10: TPanel;
        EditPgs3: TEdit;
        Panel11: TPanel;
        Shape6: TShape;
        Panel12: TPanel;
        EditPgs2: TEdit;
        Panel13: TPanel;
        Shape7: TShape;
        Panel14: TPanel;
        EdTempTime: TEdit;
        Panel15: TPanel;
        Shape8: TShape;
        Panel16: TPanel;
        EdGasTime: TEdit;
    Panel18: TPanel;
    Shape10: TShape;
    Label2: TLabel;
    EdTempLow1: TEdit;
    Panel17: TPanel;
    Shape9: TShape;
    Label1: TLabel;
    EdTempHigh1: TEdit;
    Panel21: TPanel;
    Shape11: TShape;
    Label3: TLabel;
    EdTempNku: TEdit;
    Panel22: TPanel;
    Shape12: TShape;
    Label4: TLabel;
    EdTempLow2: TEdit;
    Panel23: TPanel;
    Shape13: TShape;
    Label5: TLabel;
    EdTempHigh2: TEdit;
    Panel24: TPanel;
    Shape14: TShape;
    Panel25: TPanel;
    EdPneumoAddr: TEdit;
        procedure ComboBoxComportProductsDropDown(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure ComboBoxComportProductsChange(Sender: TObject);
        procedure ComboBoxComportTempChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure EditPgs1Change(Sender: TObject);
        procedure EditTempNormChange(Sender: TObject);
        procedure EdTempTimeChange(Sender: TObject);
        procedure EdGasTimeChange(Sender: TObject);
    procedure EdTempLow1Change(Sender: TObject);
    procedure EdTempHigh1Change(Sender: TObject);
    procedure EdTempNkuChange(Sender: TObject);
    procedure EdTempLow2Change(Sender: TObject);
    procedure EdTempHigh2Change(Sender: TObject);
    procedure EdPneumoAddrChange(Sender: TObject);
    private
        { Private declarations }
        FUpdate: boolean;
        FKey: TDictionary<TObject, string>;
        FhWndTip: THandle;

        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

        procedure _reload;

        procedure ShowBalloonTip(c: TWinControl; Icon: TIconKind;
          Title, Text: string);

        procedure SetFloatParam(ed:TEdit; p:TProcFloat);
        procedure SetByteParam(ed:TEdit; p:TProcbyte);
    public
        { Public declarations }
    end;

var
    FormAppConfig: TFormAppConfig;

implementation

{$R *.dfm}

uses crud, data_model, FireDAC.Comp.Client, comport, UnitKgsdumData,
    stringutils, UnitAppIni;

procedure TFormAppConfig.FormCreate(Sender: TObject);
begin
    FKey := TDictionary<TObject, string>.create;
    FKey.Add(ComboBoxComportProducts, 'comport_products');
    FKey.Add(ComboBoxComportTemp, 'comport_temp');
    FKey.Add(EditPgs1, 'pgs1');
    FKey.Add(EditPgs2, 'pgs2');
    FKey.Add(EditPgs3, 'pgs3');
    FKey.Add(EditPgs4, 'pgs4');
    FKey.Add(EdGasTime, 'gas_time');
    FKey.Add(EdTempTime, 'temp_time');

    FKey.Add(EdTempLow1, 'temp_low1');
    FKey.Add(EdTempLow2, 'temp_low2');
    FKey.Add(EdTempHigh1, 'temp_high1');
    FKey.Add(EdTempHigh2, 'temp_high2');
    FKey.Add(EdTempNku, 'temp_nku');
    FKey.Add(EdPneumoAddr, 'pneumo_addr');


    _reload;


end;

procedure TFormAppConfig.FormShow(Sender: TObject);
begin
    _reload;
end;

procedure TFormAppConfig._reload;
var
    party: TParty;
begin
    party := GetLastParty;

    FUpdate := true;
    EnumComports(ComboBoxComportProducts.Items);
    EnumComports(ComboBoxComportTemp.Items);

    with ComboBoxComportProducts do
        ItemIndex := Items.IndexOf(AppIni.ComportProductsName);

    with ComboBoxComportTemp do
        ItemIndex := Items.IndexOf(AppIni.ComportTempName);

    EditPgs1.Text := FloatToStr(party.FPgs1);
    EditPgs2.Text := FloatToStr(party.FPgs2);
    EditPgs3.Text := FloatToStr(party.FPgs3);
    EditPgs4.Text := FloatToStr(party.FPgs4);

    EdGasTime.Text := inttostr(AppIni.GasTime);
    EdTempTime.Text := inttostr(AppIni.TempTime);

    EdTempLow1.Text := FloatToStr(AppIni.TempLow1);
    EdTempLow2.Text := FloatToStr(AppIni.TempLow2);
    EdTempHigh1.Text := FloatToStr(AppIni.TempHigh1);
    EdTempHigh2.Text := FloatToStr(AppIni.TempHigh2);
    EdTempNku.Text := FloatToStr(AppIni.TempNku);
    EdPneumoAddr.Text := inttostr(AppIni.PneumoAddr);

    FUpdate := false;
end;

procedure TFormAppConfig.ComboBoxComportProductsChange(Sender: TObject);
begin
    if FUpdate then
        exit;
    AppIni.ComportProductsName := ComboBoxComportProducts.Text;

end;

procedure TFormAppConfig.ComboBoxComportProductsDropDown(Sender: TObject);
var
    n: integer;
begin
    if FUpdate then
        exit;
    with Sender as TComboBox do
    begin
        n := ItemIndex;
        EnumComports(Items);
        ItemIndex := n;
    end;
end;

procedure TFormAppConfig.ComboBoxComportTempChange(Sender: TObject);
begin
    if FUpdate then
        exit;
    AppIni.ComportTempName := ComboBoxComportTemp.Text;

end;


procedure TFormAppConfig.SetByteParam(ed:TEdit; p:TProcbyte);
var v:integer;
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);
    if TryStrToInt(ed.Text, v) and (v >= 0) and (v <= $FF) then
        p(v)
    else
    begin
        ShowBalloonTip(ed as TWinControl, TIconKind.Error, '',
          '�� ���������� ��������');
        if Visible then
            (ed as TWinControl).SetFocus;
    end;
    if Visible then
        (ed as TWinControl).SetFocus;

end;

procedure TFormAppConfig.SetFloatParam(ed:TEdit; p:TProcFloat);
var v:double;
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);
    if TryStrToFloatMy(ed.Text, v) then
        p(v)
    else
    begin
        ShowBalloonTip(ed as TWinControl, TIconKind.Error, '',
          '�� ���������� ��������');
        if Visible then
            (ed as TWinControl).SetFocus;
    end;
    if Visible then
        (ed as TWinControl).SetFocus;

end;

procedure TFormAppConfig.EdTempHigh1Change(Sender: TObject);
begin
    SetFloatParam(Sender as TEdit, AppIni.SetTempHigh1);
end;

procedure TFormAppConfig.EdTempHigh2Change(Sender: TObject);
begin
    SetFloatParam(Sender as TEdit, AppIni.SetTempHigh2);
end;

procedure TFormAppConfig.EdTempLow1Change(Sender: TObject);
begin
    SetFloatParam(Sender as TEdit, AppIni.SetTempLow1);
end;

procedure TFormAppConfig.EdTempLow2Change(Sender: TObject);
begin
    SetFloatParam(Sender as TEdit, AppIni.SetTempLow2);
end;

procedure TFormAppConfig.EdTempNkuChange(Sender: TObject);
begin
    SetFloatParam(Sender as TEdit, AppIni.SetTempNku);
end;

procedure TFormAppConfig.EdTempTimeChange(Sender: TObject);
var
    n: integer;
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);

    if TryStrToInt(EdTempTime.Text, n) then
        AppIni.TempTime := n
    else
    begin
        ShowBalloonTip(Sender as TWinControl, TIconKind.Error, '',
          '�� ���������� ��������');
        if Visible then
            (Sender as TWinControl).SetFocus;
    end;
    if Visible then
        (Sender as TWinControl).SetFocus;
end;

procedure TFormAppConfig.EdGasTimeChange(Sender: TObject);
var
    n: integer;
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);

    if TryStrToInt(EdGasTime.Text, n) then
        AppIni.GasTime := n
    else
    begin
        ShowBalloonTip(Sender as TWinControl, TIconKind.Error, '',
          '�� ���������� ��������');
        if Visible then
            (Sender as TWinControl).SetFocus;
    end;
    if Visible then
        (Sender as TWinControl).SetFocus;
end;

procedure TFormAppConfig.EdPneumoAddrChange(Sender: TObject);
begin
    SetByteParam(Sender as TEdit, AppIni.SetPneumoAddr);
end;

procedure TFormAppConfig.EditPgs1Change(Sender: TObject);
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);
    try
        KgsdumData.SetPartyValue(FKey[Sender],
          str_to_float((Sender as TEdit).Text));
    except
        on e: Exception do
        begin
            ShowBalloonTip(Sender as TWinControl, TIconKind.Error, e.ClassName,
              e.Message);
        end;
    end;
    if Visible then
        (Sender as TWinControl).SetFocus;

end;

procedure TFormAppConfig.EditTempNormChange(Sender: TObject);
begin
    if FUpdate then
        exit;
    CloseWindow(FhWndTip);
    try
        AppIni.Ini.WriteFloat('work', FKey[Sender],
          str_to_float((Sender as TEdit).Text));
    except
        on e: Exception do
        begin
            ShowBalloonTip(Sender as TWinControl, TIconKind.Error, e.ClassName,
              e.Message);

        end;

    end;
    if Visible then
        (Sender as TWinControl).SetFocus;
end;

procedure TFormAppConfig.FormDeactivate(Sender: TObject);
begin
    hide;
end;

procedure TFormAppConfig.WMEnterSizeMove(var Msg: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.ShowBalloonTip(c: TWinControl; Icon: TIconKind;
  Title, Text: string);
begin
    CloseWindow(FhWndTip);
    FhWndTip := ComponentBaloonHintU.ShowBalloonTip(c, Icon, Title, Text);
end;

end.
