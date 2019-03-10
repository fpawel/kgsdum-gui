unit UnitFormSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormSettings = class(TForm)
    FlowPanel1: TFlowPanel;
    Label3: TLabel;
    ComboBoxComportKgsdum: TComboBox;
    Label5: TLabel;
    ComboBoxComportGas: TComboBox;
    Label1: TLabel;
    ComboBoxComportTemperature: TComboBox;
    Label2: TLabel;
    EditSens: TEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.dfm}

end.
