unit UnitKgsdumMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TKgsdumMainForm = class(TForm)
    PageControlMain: TPageControl;
    TabSheetParty: TTabSheet;
    TabSheetArchive: TTabSheet;
    procedure PageControlMainDrawTab(Control: TCustomTabControl;
      TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure PageControlMainChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KgsdumMainForm: TKgsdumMainForm;

implementation

{$R *.dfm}

uses vclutils, UnitFormLastParty, UnitKgsdumData;

procedure TKgsdumMainForm.FormShow(Sender: TObject);
begin
    KgsdumData.Conn.Connected := true;
    with FormLastParty do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetParty;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;
end;

procedure TKgsdumMainForm.PageControlMainChange(Sender: TObject);
begin
    (Sender as TPageControl).Repaint;
end;

procedure TKgsdumMainForm.PageControlMainDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

end.
