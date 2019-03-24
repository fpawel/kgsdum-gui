unit UnitFormCharts;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
    data_model, UnitFormChartSeries;

type
    TFormCharts = class(TForm)
        Panel1: TPanel;
        StringGrid1: TStringGrid;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        FBuckets: TArray<TSeriesBucket>;

    public
        { Public declarations }
        FFormChartSeries: TFormChartSeries;
        procedure FetchDays;
    end;

var
    FormCharts: TFormCharts;

implementation

{$R *.dfm}

uses FireDAC.Comp.Client, FireDAC.stan.param, UnitKgsdumData, dateutils,
    stringgridutils;

procedure TFormCharts.FormCreate(Sender: TObject);
begin
    FFormChartSeries := TFormChartSeries.Create(self);
    // FetchDays;
end;

procedure TFormCharts.FormShow(Sender: TObject);
begin
    with FFormChartSeries do
    begin
        Font.Assign(self.Font);
        Parent := self;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
    end;
end;

procedure TFormCharts.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
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

    ta := taLeftJustify;
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
            end;
    end;
    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
end;

procedure TFormCharts.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    stored_at: TDateTime;
    address, variable: byte;
    value: double;
    strStoredAt: string;

begin
    FFormChartSeries.Visible := false;

    if ARow >= length(FBuckets) then
        exit;

    FFormChartSeries.NewChart;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        try
            SQL.Text :=
              'SELECT STRFTIME(''%Y-%m-%d %H:%M:%f'', stored_at) AS stored_at, '
              + 'address, variable, value ' +
              'FROM series WHERE bucket_id = :bucket_id ORDER BY stored_at';
            ParamByName('bucket_id').value := FBuckets[ARow].BucketID;
            Open;
            First;
            while not eof do
            begin
                strStoredAt := FieldValues['stored_at'];
                stored_at := DateTimeFromDBString(strStoredAt);
                value := FieldValues['value'];
                address := FieldValues['address'];
                variable := FieldValues['variable'];
                FFormChartSeries.AddValue(address, variable, value, stored_at);

                Next;
            end;
        finally
            Free;
        end;
    end;

    FFormChartSeries.Visible := true;

end;

procedure TFormCharts.ComboBox1Change(Sender: TObject);
var
    combobox_date: TDateTime;
    not_used: Boolean;
begin
    StringGrid1.OnSelectCell := nil;
    combobox_date := StrToDate(ComboBox1.Text);
    StringGrid1.RowCount := 1;
    SetLength(FBuckets, 0);

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        try
            SQL.Text := 'SELECT * FROM bucket ' +
              'WHERE CAST(STRFTIME(''%Y'', created_at) AS INTEGER) = :year ' +
              'AND CAST(STRFTIME(''%m'', created_at) AS INTEGER) = :month ' +
              'AND CAST(STRFTIME(''%d'', created_at) AS INTEGER) = :day ';
            ParamByName('year').value := YearOf(combobox_date);
            ParamByName('month').value := MonthOf(combobox_date);
            ParamByName('day').value := DayOf(combobox_date);

            Open;
            First;
            while not eof do
            begin
                SetLength(FBuckets, Length(FBuckets) + 1);
                with FBuckets[Length(FBuckets) - 1] do
                begin
                    CreatedAt := FieldValues['created_at'];
                    BucketID := FieldValues['bucket_id'];
                    Name := FieldValues['name'];
                end;
                with StringGrid1 do
                begin
                    RowCount := Length(FBuckets);
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
        StringGrid1SelectCell(nil, 0, Row, not_used);
    end;

end;

procedure TFormCharts.FetchDays;
var
    dt: TDateTime;
begin

    ComboBox1.Clear;
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        try
            SQL.Text :=
              'DELETE FROM bucket WHERE created_at = updated_at AND bucket_id <> :last_bucket_id;';
            ParamByName('last_bucket_id').value :=
              KgsdumData.GetLastSeriesBucket.BucketID;
            ExecSQL;

            SQL.Text :=
              'SELECT created_at FROM bucket GROUP BY STRFTIME(''%Y-%m-%d'', created_at)'
              + 'ORDER BY created_at DESC';
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

end.
