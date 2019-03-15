unit UnitFormJournal;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    UnitFormConsole, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
    data_model;

type

    TFormJournal = class(TForm)
        Panel1: TPanel;
        StringGrid1: TStringGrid;
        Splitter1: TSplitter;
        ComboBox1: TComboBox;
        procedure Panel1Resize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: boolean);
        procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
    private
        { Private declarations }
        FWorks: TArray<TWorkEntryInfo>;
        FCurrentWork: string;
        FCurrentWorkID: longint;

        procedure _DoNewWork(work: string);
    public
        { Public declarations }
        procedure fetch_days;
        procedure NewWork(work: string);
        procedure NewEntry(ALevel: data_model.TLogLevel; AText: string);

        procedure NewExceptionEntry(AText: string);
    end;

var
    FormJournal: TFormJournal;

implementation

uses FireDAC.Comp.Client, UnitKgsdumData, dateutils,
    stringgridutils;

{$R *.dfm}

procedure TFormJournal.FormCreate(Sender: TObject);
begin
    FCurrentWork := '';
    FCurrentWorkID := 0;
    SetLength(FWorks, 1);
    StringGrid1.Cells[1, 0] := 'За сутки';
    fetch_days;

end;

procedure TFormJournal.FormShow(Sender: TObject);
begin
    with FormConsole do
    begin
        Font.Assign(self.Font);
        Parent := self;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;
    Panel1Resize(nil);
end;

procedure TFormJournal.Panel1Resize(Sender: TObject);
var
    r: TRect;
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := Panel1.Width - ColWidths[0] - 10;
        Repaint;
    end;
end;

procedure TFormJournal.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;

    case ACol of
        0:
            begin
                ta := taCenter;
                cnv.Font.Color := clGreen;
            end;
        1:
            begin
                ta := taLeftJustify;
                cnv.Font.Color := clBlack;

                if FWorks[ARow].ErrorOccurred then
                    cnv.Font.Color := clRed;
            end;

    end;

    if ARow = 0 then
    begin
        cnv.Font.Style := [fsBold];
        cnv.Font.Color := clNavy;
    end;

    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, ARow, Rect);
end;

procedure TFormJournal.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
    r: Integer;
begin
    if Key <> VK_DELETE then
        exit;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        try
            with StringGrid1.Selection do
                for r := Top to Bottom do
                begin
                    SQL.Text := 'DELETE FROM work WHERE work_id = :work_id';
                    ParamByName('work_id').Value := FWorks[r].WorkID;
                    ExecSQL;
                end;
        finally
            Free;
        end;
    end;
    ComboBox1Change(nil);

end;

procedure TFormJournal.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: boolean);
var
    created_at, combobox_date: TDateTime;
    Name, _message: string;
    level: Integer;
    r: TRect;
begin

    FormConsole.Clear;
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        try
            if ARow = 0 then
            begin
                combobox_date := StrToDate(ComboBox1.Text);
                SQL.Text := 'SELECT entry.*, work.name FROM entry INNER JOIN ' +
                  'work ON entry.work_id = work.work_id ' +
                  'WHERE CAST(STRFTIME(''%Y'', entry.created_at) AS INTEGER) = :year '
                  + '  AND CAST(STRFTIME(''%m'', entry.created_at) AS INTEGER) = :month '
                  + '  AND CAST(STRFTIME(''%d'', entry.created_at) AS INTEGER) = :day '
                  + 'ORDER BY entry.created_at ';

                ParamByName('year').Value := YearOf(combobox_date);
                ParamByName('month').Value := MonthOf(combobox_date);
                ParamByName('day').Value := DayOf(combobox_date);
            end
            else
            begin
                SQL.Text := 'SELECT entry.*, work.name FROM entry ' +
                  'INNER JOIN work ON entry.work_id = work.work_id ' +
                  'WHERE entry.work_id = :work_id';
                ParamByName('work_id').Value := FWorks[ARow].WorkID;
            end;
            Open;
            First;
            while not eof do
            begin
                created_at := FieldValues['created_at'];
                _message := FieldValues['message'];

                FormConsole.NewLine(created_at, TLogLevel(FieldValues['level']),
                  FieldValues['name'], FieldValues['message']);

                Next;
            end;
        finally
            Free;
        end;
    end;

end;

procedure TFormJournal.ComboBox1Change(Sender: TObject);
var
    combobox_date: TDateTime;
    can_select: boolean;
begin
    StringGrid1.OnSelectCell := nil;
    combobox_date := StrToDate(ComboBox1.Text);
    StringGrid1.RowCount := 1;
    SetLength(FWorks, 1);
    with FWorks[0] do
    begin
        WorkID := 0;
        Name := '';
        ErrorOccurred := false;
    end;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        try
            SQL.Text := 'SELECT *, EXISTS( SELECT * FROM entry ' +
              'WHERE entry.work_id = work.work_id ' +
              'AND level > 2) AS error_occurred ' + 'FROM work ' +
              'WHERE CAST(STRFTIME(''%Y'', created_at) AS INTEGER) = :year ' +
              'AND CAST(STRFTIME(''%m'', created_at) AS INTEGER) = :month ' +
              'AND CAST(STRFTIME(''%d'', created_at) AS INTEGER) = :day ';
            ParamByName('year').Value := YearOf(combobox_date);
            ParamByName('month').Value := MonthOf(combobox_date);
            ParamByName('day').Value := DayOf(combobox_date);

            Open;
            First;
            while not eof do
            begin
                SetLength(FWorks, Length(FWorks) + 1);
                with FWorks[Length(FWorks) - 1] do
                begin
                    CreatedAt := FieldValues['created_at'];
                    WorkID := FieldValues['work_id'];
                    Name := FieldValues['name'];
                    ErrorOccurred := FieldValues['error_occurred'];
                end;
                with StringGrid1 do
                begin
                    RowCount := RowCount + 1;
                    Cells[0, RowCount - 1] :=
                      TimeToStr(FieldValues['created_at']);
                    Cells[1, RowCount - 1] := FieldValues['name'];
                end;
                Next;
            end;
        finally
            Free;
        end;
    end;

    with StringGrid1 do
    begin
        OnSelectCell := StringGrid1SelectCell;
        Row := 0;
        StringGrid1SelectCell(nil, 0, Row, can_select);
    end;

end;

procedure TFormJournal.fetch_days;
var
    dt: TDateTime;
begin

    ComboBox1.Clear;
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        try
            SQL.Text :=
              'SELECT work.created_at, max(level) > 2 AS error_occurred FROM entry '
              + 'INNER JOIN work ON entry.work_id = work.work_id ' +
              'GROUP BY STRFTIME(''%Y-%m-%d'', work.created_at) ' +
              'ORDER BY work.created_at DESC';
            Open;
            First;
            while not eof do
            begin
                dt := FieldValues['created_at'];
                ComboBox1.Items.Add(DateToStr(dt));
                Next;
            end;
        finally
            Free;
        end;
    end;

    if (ComboBox1.Items.Count = 0) or (ComboBox1.Items[0] <> DateToStr(now))
    then
        ComboBox1.Items.Insert(0, DateToStr(now));

    if ComboBox1.Items.Count > 0 then
    begin
        ComboBox1.ItemIndex := 0;
        ComboBox1Change(nil);
    end;
end;

procedure TFormJournal._DoNewWork(work: string);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        SQL.Text :=
          'INSERT INTO work(name, created_at) VALUES (:work, :created_at );';
        ParamByName('work').Value := work;
        ParamByName('created_at').Value := now;
        ExecSQL;
        Close;

        SQL.Text := 'SELECT work_id FROM last_work';
        Open;
        First;

        SetLength(FWorks, Length(FWorks) + 1);
        with FWorks[Length(FWorks) - 1] do
        begin
            CreatedAt := now;
            WorkID := FieldValues['work_id'];
            Name := work;
            ErrorOccurred := false;
        end;

        SQL.Text := 'SELECT * FROM last_work';
        Open;
        First;
        if not eof then
            FCurrentWork := FieldValues['name'];
        FCurrentWorkID := FieldValues['work_id'];
        Close;

        Free;
    end;

    with StringGrid1 do
    begin
        RowCount := RowCount + 1;
        Cells[0, RowCount - 1] := TimeToStr(now);
        Cells[1, RowCount - 1] := work;
    end;

end;

procedure TFormJournal.NewWork(work: string);
begin
    FCurrentWork := work;
    FCurrentWorkID := 0;
    if FCurrentWork = 'опрос' then
        exit;
    _DoNewWork(work);
    NewEntry(loglevInfo, 'начало выполнения');
end;

procedure TFormJournal.NewEntry(ALevel: TLogLevel; AText: string);
begin
    with FormConsole do
    begin
        NewLine(now, ALevel, FCurrentWork, AText);
        with StringGrid1 do
            Row := RowCount - 1;
    end;

    if FCurrentWork = 'опрос' then
        exit;

    if ALevel >= loglevError then
    begin
        FWorks[Length(FWorks) - 1].ErrorOccurred := true;
        StringGrid_RedrawRow(StringGrid1, StringGrid1.RowCount - 1);
    end;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnJournal;
        SQL.Text :=
          'INSERT INTO entry(work_id, created_at, level, message) VALUES ' +
          '((SELECT work_id FROM last_work), :created_at, :level, :message)';
        ParamByName('level').Value := ALevel;
        ParamByName('created_at').Value := now;
        ParamByName('message').Value := AText;
        ExecSQL;
        Close;
        Free;
    end;
end;

procedure TFormJournal.NewExceptionEntry(AText: string);
begin
    _DoNewWork('Исключителная ситуация');
    NewEntry(loglevException, AText);

end;

end.
