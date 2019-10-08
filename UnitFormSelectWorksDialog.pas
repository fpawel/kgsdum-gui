unit UnitFormSelectWorksDialog;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
    Vcl.ExtCtrls,
    System.ImageList, Vcl.ImgList, Vcl.ComCtrls, UnitKgsdumData,
  UnitFormChartSeries;

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
        EditValue: TEdit;
        Button2: TButton;
        Button3: TButton;
        CheckBoxEachAddr: TCheckBox;
    RadioButtonVar: TRadioButton;
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
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
    private
        { Private declarations }


        function TheVar: byte;
        function TheCoef: byte;
        function TheAddr: byte;
        function TheValue: double;
    public
        { Public declarations }
    end;

var
    FormSelectWorksDialog: TFormSelectWorksDialog;

implementation

{$R *.dfm}

uses works, data_model, UnitWorker;

procedure TFormSelectWorksDialog.FormCreate(Sender: TObject);
var
    i: integer;
begin



    SetWindowLong(Button1.Handle, GWL_STYLE, GetWindowLong(Button1.Handle,
      GWL_STYLE) or BS_MULTILINE);
    // CheckListBox1.CheckAll(cbChecked);
    CheckListBox1.Clear;
    for i := 0 to length(works.MainWorks) - 1 do
        CheckListBox1.Items.Add(MainWorks[i].Name);
    CheckListBox1.CheckAll(cbChecked);

end;

procedure TFormSelectWorksDialog.Button11Click(Sender: TObject);
begin
    Worker.RunWork('термокамера: старт',
        procedure
        begin
            Worker.TermochamberStart;
        end);
end;

procedure TFormSelectWorksDialog.Button12Click(Sender: TObject);
begin
    Worker.RunWork('термокамера: стоп',
        procedure
        begin
            Worker.TermochamberStop;
        end);
end;

procedure TFormSelectWorksDialog.Button13Click(Sender: TObject);
begin
    Worker.RunWork('термокамера: уставка ' + Edit2.Text,
        procedure
        begin
            Worker.TermochamberSetSetpoint(StrToFloat(Edit2.Text));
        end);
end;

procedure TFormSelectWorksDialog.Button14Click(Sender: TObject);
begin
    Worker.RunWork('термокамера: температура',
        procedure
        begin
            Worker.TermochamberReadTemperature;
        end);

end;

procedure TFormSelectWorksDialog.Button16Click(Sender: TObject);
begin
    Worker.RunInterrogate;
end;

procedure TFormSelectWorksDialog.Button1Click(Sender: TObject);
var
    i: integer;
    xs: TWorks;
begin
    for i := 0 to length(works.MainWorks) - 1 do
    begin
        if CheckListBox1.Checked[i] then
        begin
            SetLength(xs, length(xs) + 1);
            xs[length(xs) - 1] := MainWorks[i];
        end;
    end;
    Worker.RunMainworks(xs);
end;

procedure TFormSelectWorksDialog.Button2Click(Sender: TObject);
begin
    if RadioButtonVar.Checked then
    begin
        if CheckBoxEachAddr.Checked then
            RunWriteVars(TheVar, TheValue)
        else
            RunWriteVar(TheAddr, TheVar, TheValue);
    end
    else
    begin
        if CheckBoxEachAddr.Checked then
            RunWriteCoefs(TheCoef, TheValue)
        else
            RunWriteCoef(TheAddr, TheCoef, TheValue);
    end
end;

procedure TFormSelectWorksDialog.Button3Click(Sender: TObject);
begin
    if RadioButtonVar.Checked then
    begin
        if CheckBoxEachAddr.Checked then
            RunReadVars(TheVar)
        else
            RunReadVar(TheAddr, TheVar);
    end
    else
    begin
        if CheckBoxEachAddr.Checked then
            RunReadCoefficients(TheCoef)
        else
            RunReadCoefficient(TheAddr, TheCoef);
    end
end;

procedure TFormSelectWorksDialog.Button4Click(Sender: TObject);
begin
    Worker.RunKgsSetAddr(TheAddr);
end;

procedure TFormSelectWorksDialog.Button6Click(Sender: TObject);
begin
    RunSwitchGasBlock((Sender as TComponent).Tag);
end;



procedure TFormSelectWorksDialog.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

function TFormSelectWorksDialog.TheAddr: byte;
begin
    exit( StrToInt(EditAddr.Text) );

end;

function TFormSelectWorksDialog.TheVar: byte;
begin
    exit( StrToInt(EditVar.Text) );

end;

function TFormSelectWorksDialog.TheCoef: byte;
begin
    exit( StrToInt(EditVar.Text) );

end;

function TFormSelectWorksDialog.TheValue: double;
begin
    exit( StrToFloat(EditValue.Text) );

end;

end.
