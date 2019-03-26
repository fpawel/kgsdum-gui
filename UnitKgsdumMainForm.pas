unit UnitKgsdumMainForm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    Vcl.StdCtrls,
    Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, Vcl.Imaging.pngimage,
    Vcl.Menus, UnitFormCharts;

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
        LabelWhatDelay: TLabel;
        ToolBar6: TToolBar;
        ToolButtonStop: TToolButton;
        Panel2: TPanel;
        ProgressBar1: TProgressBar;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        PanelMessageBox: TPanel;
        ImageError: TImage;
        ImageInfo: TImage;
        PanelMessageBoxTitle: TPanel;
        ToolBar2: TToolBar;
        ToolButton3: TToolButton;
        RichEditlMessageBoxText: TRichEdit;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        TimerDelay: TTimer;
        PageControlMain: TPageControl;
        TabSheetParty: TTabSheet;
        TabSheetCharts: TTabSheet;
        TabSheetJournal: TTabSheet;
        LabelDelayTotalTime: TLabel;
        procedure FormShow(Sender: TObject);
        procedure ToolButtonRunClick(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure ToolButton3Click(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure TimerDelayTimer(Sender: TObject);
        procedure ToolButtonStopClick(Sender: TObject);
        procedure PageControlMainChange(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: Integer; const Rect: TRect; Active: Boolean);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    private
        { Private declarations }
        procedure DoAppException(Sender: TObject; E: Exception);

    public
        { Public declarations }

        procedure AppException(Sender: TObject; E: Exception);
        procedure OnStartWork;
        procedure OnStopWork;
        procedure NewWork(work: string);

        function ErrorMessageBox(AMsg: string): Boolean;
        procedure ShowNonModalErrorMessage(title, error: string);

        procedure OnStartDelay(what: String; durationMs: cardinal);
    end;

var
    KgsdumMainForm: TKgsdumMainForm;

Function GetTextSize(const Text: String; Font: TFont): TSize;

implementation

{$R *.dfm}

uses uitypes, types, ShellApi, FireDAC.Comp.Client, UnitKgsdumData, JclDebug,
    vclutils,
    UnitFormLastParty, dateutils, math,
    UnitFormSelectWorksDialog,
    works, UnitFormConsole, UnitFormJournal,
    hardware_errors, UnitFormAppConfig, UnitWorker, UnitFormChartSeries;

procedure TKgsdumMainForm.FormCreate(Sender: TObject);
begin
    LabelStatusTop.Caption := '';
    PanelMessageBox.Width := 700;
    PanelMessageBox.Height := 350;
end;

procedure TKgsdumMainForm.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    FormChartSeries.ChangeAxisOrder(GetVCLControlAtPos(self, MousePos),
      WheelDelta);
    FormCharts.FFormChartSeries.ChangeAxisOrder(GetVCLControlAtPos(self,
      MousePos), WheelDelta);
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

    with FormLastParty do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetParty;
        BorderStyle := bsNone;
        Align := alTop;
        Show;
    end;

    with FormChartSeries do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetParty;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
        Hide;
    end;

    with FormJournal do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetJournal;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;

    with FormConsole do
    begin
        Font.Assign(self.Font);
        Parent := FormJournal;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
        Repaint;
    end;

    with FormCharts do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetCharts;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;

    PanelTop.Top := 0;
end;

procedure TKgsdumMainForm.TimerDelayTimer(Sender: TObject);
var
    s: string;
    elepsed_time, total_time: TDateTime;
begin

    LabelDelayElepsedTime.Caption :=
      TimeToStr(IncMilliSecond(StrToTime(LabelDelayElepsedTime.Caption), 1000));

    ProgressBar1.Position := ProgressBar1.Position + 1000;

    LabelProgress.Caption :=
      inttostr(ceil(100.0 * (ProgressBar1.Position * 1.0) / (ProgressBar1.Max *
      1.0))) + '%';
end;

procedure TKgsdumMainForm.ToolButton1Click(Sender: TObject);
begin
    with ToolButton1 do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(X, Y);
end;

procedure TKgsdumMainForm.ToolButton2Click(Sender: TObject);
begin
    Worker.CancelExecution;
end;

procedure TKgsdumMainForm.ToolButton3Click(Sender: TObject);
begin
    PanelMessageBox.Hide;
end;

procedure TKgsdumMainForm.ToolButton4Click(Sender: TObject);
begin
    with ToolButton4 do
        with ClientToScreen(Point(0, Height)) do
        begin
            with FormAppconfig do
            begin
                Left := X - 5 - Width;
                Top := Y + 5;
                Show;
            end;
        end;
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

procedure TKgsdumMainForm.ToolButtonStopClick(Sender: TObject);
begin
    Worker.SkipDelay;
end;

procedure TKgsdumMainForm.DoAppException(Sender: TObject; E: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    stacktrace: string;

    FErrorLog: TextFile;
    ErrorLogFileName: string;
begin

    if E is EConfigError then
    begin
        MessageDlg(E.Message, mtError, [mbAbort], 0);
        exit;
    end;

    stackList := JclCreateStackList(true, 0, Caller(0, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, false, false, true, false);
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

    FormJournal.NewExceptionEntry('Произошла ошибка',
      E.ClassName + ': ' + E.Message + #13 + stacktrace);

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
    LabelStatusTop.Caption := LabelStatusTop.Caption + ': выполнено';
end;

procedure TKgsdumMainForm.PageControlMainChange(Sender: TObject);
var
    PageControl: TPageControl;
begin
    PageControl := Sender as TPageControl;
    PageControl.Repaint;
    PanelMessageBox.Hide;
    if PageControl.ActivePage = TabSheetCharts then
        FormCharts.FetchDays;

end;

procedure TKgsdumMainForm.PageControlMainDrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

procedure TKgsdumMainForm.N1Click(Sender: TObject);
var
    s: string;
begin
    s := GetEnvironmentVariable('APPDATA') + '\kgsdum\';
    ShellExecute(0, nil, 'Explorer.exe', PChar(s), nil, SW_NORMAL);
end;

procedure TKgsdumMainForm.NewWork(work: string);
begin

    LabelStatusTop.Caption := work;
end;

function TKgsdumMainForm.ErrorMessageBox(AMsg: string): Boolean;
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

procedure TKgsdumMainForm.OnStartDelay(what: String; durationMs: cardinal);
begin
    FormChartSeries.NewChart;
    KgsdumData.NewChartSeries(what);
    LabelWhatDelay.Caption := what;
    LabelDelayElepsedTime.Caption := '00:00:00';
    LabelDelayTotalTime.Caption := TimeToStr(IncMilliSecond(0, durationMs));
    LabelProgress.Caption := '';
    ProgressBar1.Position := 0;
    ProgressBar1.Max := durationMs;
    TimerDelay.Enabled := true;
    PanelDelay.Show;
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
