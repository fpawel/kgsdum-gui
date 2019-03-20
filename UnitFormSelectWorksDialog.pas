unit UnitFormSelectWorksDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList, Vcl.ComCtrls;

type
  TFormSelectWorksDialog = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckListBox1: TCheckListBox;
    FlowPanel1: TFlowPanel;
    Label2: TLabel;
    EditAddr: TEdit;
    Label3: TLabel;
    EditVar: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Button2: TButton;
    Button3: TButton;
    CheckBoxEachAddr: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Edit2: TEdit;
    Label1: TLabel;
    Button15: TButton;
    Button1: TButton;
    Button16: TButton;
    Button17: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSelectWorksDialog: TFormSelectWorksDialog;

implementation

{$R *.dfm}

uses works, run_work, data_model, termo;

procedure TFormSelectWorksDialog.Button11Click(Sender: TObject);
begin
     RunWork('термокамера: старт',
        procedure
        begin
            TermochamberStart;
        end);
end;

procedure TFormSelectWorksDialog.Button12Click(Sender: TObject);
begin
    RunWork('термокамера: стоп',
        procedure
        begin
            TermochamberStop;
        end);
end;

procedure TFormSelectWorksDialog.Button13Click(Sender: TObject);
begin
    RunWork('термокамера: уставка ' + Edit2.Text,
        procedure
        begin
            TermochamberSetSetpoint(StrToFloat(Edit2.Text));
        end);
end;

procedure TFormSelectWorksDialog.Button14Click(Sender: TObject);
begin
    RunWork('термокамера: температура',
        procedure
        begin
            TermochamberReadTemperature;
        end);

end;

procedure TFormSelectWorksDialog.Button16Click(Sender: TObject);
begin
    RunInterrogate;
end;

procedure TFormSelectWorksDialog.Button17Click(Sender: TObject);
begin
    RunWork('задержка',
        procedure
        begin
            Delay('продувка ПГС1', 1000 * 60 );
            Delay('продувка ПГС2', 1000 * 60 );
            Delay('продувка ПГС3', 1000 * 60 );
        end);
end;

procedure TFormSelectWorksDialog.Button3Click(Sender: TObject);
begin
    if RadioButton1.Checked then
    begin
        if CheckBoxEachAddr.Checked then
            RunReadVars(StrToInt(EditVar.Text))
        else
            RunReadVar(StrToInt(EditAddr.Text), StrToInt(EditVar.Text));
    end else
    begin
        if CheckBoxEachAddr.Checked then
            RunReadCoefficients(StrToInt(EditVar.Text))
        else
            RunReadCoefficient(StrToInt(EditAddr.Text), StrToInt(EditVar.Text));
    end
end;

procedure TFormSelectWorksDialog.Button4Click(Sender: TObject);
begin
    RunKgsSetAddr(StrToInt(EditAddr.Text));
end;

procedure TFormSelectWorksDialog.Button6Click(Sender: TObject);
begin
    RunSwitchGasBlock((Sender as TComponent ).Tag);
end;

procedure TFormSelectWorksDialog.FormCreate(Sender: TObject);
begin
    SetWindowLong(Button1.Handle, GWL_STYLE, GetWindowLong(Button1.Handle,
    	GWL_STYLE) or BS_MULTILINE);
    CheckListBox1.CheckAll(cbChecked);
end;

procedure TFormSelectWorksDialog.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

end.
