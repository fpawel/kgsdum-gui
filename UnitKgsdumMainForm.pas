unit UnitKgsdumMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    Vcl.StdCtrls,
    Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, inifiles, Vcl.Imaging.pngimage;

type
    TKgsdumMainForm = class(TForm)
        ImageList4: TImageList;
        PanelTop: TPanel;
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
        PanelMessageBox: TPanel;
        ImageError: TImage;
        ImageInfo: TImage;
        PanelMessageBoxTitle: TPanel;
        ToolBar2: TToolBar;
        ToolButton3: TToolButton;
        RichEditlMessageBoxText: TRichEdit;
        procedure FormShow(Sender: TObject);
        procedure ToolButtonRunClick(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure ToolButton3Click(Sender: TObject);
    private
        { Private declarations }
        procedure DoAppException(Sender: TObject; E: Exception);

    public
        { Public declarations }
        Ini: TIniFile;
        procedure AppException(Sender: TObject; E: Exception);
        procedure OnStartWork;
        procedure OnStopWork;
        procedure NewWork(work: string);

        function ErrorMessageBox(AMsg: string): boolean;
        procedure ShowNonModalErrorMessage(title, error: string);

    end;

var
    KgsdumMainForm: TKgsdumMainForm;

Function GetTextSize(const Text: String; Font: TFont): TSize;

implementation

{$R *.dfm}

uses FireDAC.Comp.Client, UnitKgsdumData, JclDebug, vclutils, UnitFormLastParty,
    UnitFormSelectWorksDialog,
    UnitFormProperties, works, run_work, UnitFormConsole, UnitFormJournal,
    hardware_errors;

procedure TKgsdumMainForm.FormCreate(Sender: TObject);
begin
    LabelStatusTop.Caption := '';
    PanelMessageBox.Width := 700;
    PanelMessageBox.Height := 350;
end;

procedure TKgsdumMainForm.FormResize(Sender: TObject);
begin
    if PanelMessageBox.Visible then
    begin
        PanelMessageBox.Left := ClientWidth div 2 - PanelMessageBox.Width div 2;
        PanelMessageBox.Top := ClientHeight div 2 -
          PanelMessageBox.Height div 2;
    end;
end;

procedure TKgsdumMainForm.FormShow(Sender: TObject);
begin
    KgsdumData.Conn.Connected := true;
    with FormLastParty do
    begin
        Font.Assign(self.Font);
        Parent := self;
        BorderStyle := bsNone;
        Align := alTop;
        Show;
    end;

    with FormJournal do
    begin
        Font.Assign(self.Font);
        Parent := self;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;

    PanelTop.Top := 0;
end;

procedure TKgsdumMainForm.ToolButton2Click(Sender: TObject);
begin
    CancelExecution;
end;

procedure TKgsdumMainForm.ToolButton3Click(Sender: TObject);
begin
    PanelMessageBox.Hide;
end;

procedure TKgsdumMainForm.ToolButton4Click(Sender: TObject);
begin
    FormProperties.Show;
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

procedure TKgsdumMainForm.DoAppException(Sender: TObject; E: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    stacktrace: string;

    FErrorLog: TextFile;
    ErrorLogFileName: string;
begin

    if e is EConfigError then
    begin
        MessageDlg(e.Message, mtError, [mbAbort], 0);
        exit;
    end;

    stackList := JclCreateStackList(false, 0, Caller(0, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, false, false, false, false);
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

    FormJournal.NewExceptionEntry('��������� ������',
        E.ClassName + ': ' + E.Message + #13 + stacktrace);

    if not ErrorMessageBox(E.Message +
      #10#13#10#13'Ignore - ���������� ������'#10#13#10#13'Abort - ����� �� ����������')
    then
    begin
        Application.OnException := nil;
        Application.Terminate;
    end;
end;

procedure TKgsdumMainForm.AppException(Sender: TObject; E: Exception);
begin
    try
        DoAppException(Sender, E);
    except
        on Exn: Exception do
        begin
            Application.ShowException(E);
            Application.ShowException(Exn);
            Application.Terminate;
            exit;
        end;
    end;
end;

procedure TKgsdumMainForm.OnStartWork;
begin
    with FormSelectWorksDialog do
    begin
        Button1.Enabled := false;
        CheckListBox1.Enabled := false;
        Hide;
    end;
    ToolBarStop.Visible := true;
    PanelMessageBox.Hide;

end;

procedure TKgsdumMainForm.OnStopWork;
begin
    with FormSelectWorksDialog do
    begin
        Button1.Enabled := true;
        CheckListBox1.Enabled := true;
    end;
    ToolBarStop.Visible := false;
    LabelStatusTop.Caption := LabelStatusTop.Caption + ': ���������';
end;

procedure TKgsdumMainForm.NewWork(work: string);
begin

    LabelStatusTop.Caption := work;
end;

function TKgsdumMainForm.ErrorMessageBox(AMsg: string): boolean;
begin
    result := MessageDlg(AMsg, mtError, [mbAbort, mbIgnore], 0) = mrIgnore;
end;

procedure TKgsdumMainForm.ShowNonModalErrorMessage(title, error: string);
var
    sz: TSize;
begin
    ImageInfo.Hide;
    ImageError.Show;

    RichEditlMessageBoxText.Text := '';
    PanelMessageBoxTitle.Caption := title;
    RichEditlMessageBoxText.Text := RichEditlMessageBoxText.Text + error;
    RichEditlMessageBoxText.Font.Color := clRed;

    sz := GetTextSize(RichEditlMessageBoxText.Text,
      RichEditlMessageBoxText.Font);

    PanelMessageBox.Show;
    PanelMessageBox.BringToFront;
    FormResize(self);
end;


Function GetTextSize(const Text: String; Font: TFont): TSize;
var
    LBmp: TBitmap;
begin
    LBmp := TBitmap.Create;
    try
        LBmp.Canvas.Font := Font;
        result := LBmp.Canvas.TextExtent(Text);
    finally
        LBmp.Free;
    end;
end;

end.
