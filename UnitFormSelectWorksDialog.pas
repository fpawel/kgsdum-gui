unit UnitFormSelectWorksDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
  TFormSelectWorksDialog = class(TForm)
    Panel1: TPanel;
    CheckListBox1: TCheckListBox;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSelectWorksDialog: TFormSelectWorksDialog;

implementation

{$R *.dfm}

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
