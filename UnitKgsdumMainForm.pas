unit UnitKgsdumMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    Vcl.StdCtrls,
    Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, inifiles;

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
        Panel1: TPanel;
        procedure FormShow(Sender: TObject);
        procedure ToolButtonRunClick(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
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

    end;

var
    KgsdumMainForm: TKgsdumMainForm;

implementation

{$R *.dfm}

uses FireDAC.Comp.Client, UnitKgsdumData, JclDebug, vclutils, UnitFormLastParty,
    UnitFormSelectWorksDialog,
    UnitFormProperties, works, run_work, UnitFormConsole, UnitFormJournal;

procedure TKgsdumMainForm.FormCreate(Sender: TObject);
begin
    LabelStatusTop.Caption := '';
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
    Panel1.Top := 100500;

end;

procedure TKgsdumMainForm.ToolButton2Click(Sender: TObject);
begin
    CancelExecution;
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

    FormJournal.NewExceptionEntry(E.ClassName + ': ' + E.Message + ': ' +
      stacktrace);

    if not ErrorMessageBox(E.Message +
      #10#13#10#13'Ignore - продолжить работу'#10#13#10#13'Abort - выйти из приложения')
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

end;

procedure TKgsdumMainForm.OnStopWork;
begin
    with FormSelectWorksDialog do
    begin
        Button1.Enabled := true;
        CheckListBox1.Enabled := true;
    end;
    ToolBarStop.Visible := false;
    LabelStatusTop.Caption := LabelStatusTop.Caption + ': выполнено';
end;

procedure TKgsdumMainForm.NewWork(work: string);
begin
    FormJournal.NewWork(work);
    LabelStatusTop.Caption := work;
end;

function TKgsdumMainForm.ErrorMessageBox(AMsg: string): boolean;
begin
    result := MessageDlg(AMsg, mtError, [mbAbort, mbIgnore], 0) = mrIgnore;

end;

end.
