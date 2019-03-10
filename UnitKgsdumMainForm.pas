unit UnitKgsdumMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    Vcl.StdCtrls,
    Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
    TKgsdumMainForm = class(TForm)
        PageControlMain: TPageControl;
        TabSheetParty: TTabSheet;
    TabSheetJournal: TTabSheet;
        ImageList4: TImageList;
        Panel3: TPanel;
        LabelStatusTop: TLabel;
        ToolBar1: TToolBar;
        ToolButtonRun: TToolButton;
        ToolBar3: TToolBar;
        ToolButton1: TToolButton;
        ToolButton4: TToolButton;
        PanelDelay: TPanel;
        LabelDelayElepsedTime: TLabel;
        LabelProgress: TLabel;
        LabelWhat: TLabel;
        ToolBar6: TToolBar;
        ToolButtonStop: TToolButton;
        Panel2: TPanel;
        ProgressBar1: TProgressBar;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        ToolBar4: TToolBar;
        ToolButton5: TToolButton;
        ToolBar5: TToolBar;
        ToolButton9: TToolButton;
        ToolButton10: TToolButton;
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: Integer; const Rect: TRect; Active: Boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ToolButtonRunClick(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure AppException(Sender: TObject; E: Exception);
        procedure OnStartWork;
        procedure OnStopWork;
    end;

var
    KgsdumMainForm: TKgsdumMainForm;

implementation

{$R *.dfm}

uses JclDebug,vclutils, UnitFormLastParty, UnitKgsdumData, UnitFormSelectWorksDialog;

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

procedure TKgsdumMainForm.ToolButtonRunClick(Sender: TObject);
begin
    with ToolButtonRun do
        with ClientToScreen(Point(0, Height)) do
        begin
            PopupMenu.CloseMenu;
            with FormSelectWorksDialog do
            begin
                Left := X + 5;
                Top := Y + 5;
                Show;
            end;
        end;
end;

procedure TKgsdumMainForm.AppException(Sender: TObject; E: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    stacktrace: string;

    FErrorLog: TextFile;
    ErrorLogFileName: string;
begin

    stackList := JclCreateStackList(false, 0, Caller(0, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, true, false, true, false);
    stacktrace := sl.Text;
    sl.Free;
    stackList.Free;
    OutputDebugStringW(PWideChar(E.Message + #10#13 + stacktrace));

    ErrorLogFileName := ExtractFileDir(paramstr(0)) + '\elcoui.errors.log';
    AssignFile(FErrorLog, ErrorLogFileName, CP_UTF8);
    if FileExists(ErrorLogFileName) then
        Append(FErrorLog)
    else
        Rewrite(FErrorLog);

    Writeln(FErrorLog, FormatDateTime('YYYY-MM-dd hh:nn:ss', now), ' ',
      E.ClassName, ' ', stringreplace(Trim(E.Message), #13, ' ',
      [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    Writeln(FErrorLog, stringreplace(Trim(stacktrace), #13, ' ',
      [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    CloseFile(FErrorLog);

    if MessageDlg(E.Message, mtError, [mbAbort, mbIgnore], 0) = mrAbort then
    begin
        Application.OnException := nil;
        Application.Terminate;
        exit;
    end;
end;

procedure TKgsdumMainForm.OnStartWork;
begin

end;

procedure TKgsdumMainForm.OnStopWork;
begin

end;

end.
