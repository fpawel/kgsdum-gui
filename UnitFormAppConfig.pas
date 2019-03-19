unit UnitFormAppConfig;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, System.Generics.collections,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
    TFormAppConfig = class(TForm)
        Panel1: TPanel;
        Panel2: TPanel;
        ComboBoxComportProducts: TComboBox;
        Shape1: TShape;
        Panel3: TPanel;
        Shape2: TShape;
        Panel4: TPanel;
        Panel5: TPanel;
        Shape3: TShape;
        Panel6: TPanel;
        ComboBoxComportTemp: TComboBox;
        EditPgs1: TEdit;
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
        EditTempNorm: TEdit;
        Panel15: TPanel;
        Shape8: TShape;
        Panel16: TPanel;
        EditTempPlus: TEdit;
        Panel17: TPanel;
        Shape9: TShape;
        Panel18: TPanel;
        EditTempMinus: TEdit;
        Panel19: TPanel;
        procedure ComboBoxComportProductsDropDown(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure ComboBoxComportProductsChange(Sender: TObject);
        procedure ComboBoxComportTempChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure EditPgs1Change(Sender: TObject);
    private
        { Private declarations }
        FUpdate: boolean;
        FKey: TDictionary<TWinControl, string>;

        procedure _reload;
    public
        { Public declarations }
    end;

var
    FormAppConfig: TFormAppConfig;

implementation

{$R *.dfm}

uses crud, data_model, FireDAC.Comp.Client, comport, UnitKgsdumData, stringutils;

procedure TFormAppConfig.FormCreate(Sender: TObject);
begin
    FKey := TDictionary<TWinControl, string>.create;
    FKey.Add(ComboBoxComportProducts, 'comport_products');
    FKey.Add(ComboBoxComportTemp, 'comport_temp');

    FKey.Add(EditTempNorm, 'temp_norm');
    FKey.Add(EditTempPlus, 'temp_plus');
    FKey.Add(EditTempMinus, 'temp_minus');

    FKey.Add(EditPgs1, 'pgs1');
    FKey.Add(EditPgs2, 'pgs2');
    FKey.Add(EditPgs3, 'pgs3');
    FKey.Add(EditPgs4, 'pgs4');

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
        ItemIndex := Items.IndexOf
          (KgsdumData.GetAppConfig('comport_products', 'COM1'));

    with ComboBoxComportTemp do
        ItemIndex := Items.IndexOf
          (KgsdumData.GetAppConfig('comport_temp', 'COM1'));

    EditPgs1.Text := FloatToStr(party.Pgs[scaleConc1]);
    EditPgs2.Text := FloatToStr(party.Pgs[scaleConc2]);
    EditPgs3.Text := FloatToStr(party.Pgs[scaleConc3]);
    EditPgs4.Text := FloatToStr(party.Pgs[scaleConc4]);

    EditTempNorm.Text := FloatToStr(KgsdumData.GetAppConfig('temp_norm', 20.0));
    EditTempPlus.Text := FloatToStr(KgsdumData.GetAppConfig('temp_plus', 50.0));
    EditTempMinus.Text :=
      FloatToStr(KgsdumData.GetAppConfig('temp_minus', 5.0));
    FUpdate := false;
end;

procedure TFormAppConfig.ComboBoxComportProductsChange(Sender: TObject);
begin
    KgsdumData.SetAppConfig('comport_products', ComboBoxComportProducts.Text);

end;

procedure TFormAppConfig.ComboBoxComportProductsDropDown(Sender: TObject);
var
    n: integer;
begin
    with Sender as TComboBox do
    begin
        n := ItemIndex;
        EnumComports(Items);
        ItemIndex := n;
    end;
end;

procedure TFormAppConfig.ComboBoxComportTempChange(Sender: TObject);
begin
    KgsdumData.SetAppConfig('comport_temp', ComboBoxComportTemp.Text);
end;

procedure TFormAppConfig.EditPgs1Change(Sender: TObject);
begin
    try
        if not FUpdate then
            KgsdumData.SetPartyValue(FKey[Sender as TWinControl],
              str_to_float((Sender as TEdit).Text));
    except

    end;

end;

procedure TFormAppConfig.FormDeactivate(Sender: TObject);
begin
    hide;
end;

end.
