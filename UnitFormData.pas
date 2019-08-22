unit UnitFormData;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ComCtrls;

type
    TProduct = record
    public
        ProductID: Int64;
        Serial: Integer;
        Addr: Byte;

    end;

    TYearMonth = record
    public
        Year: Integer;
        Month: Integer;

    end;

    TFormData = class(TForm)
        StringGrid2: TStringGrid;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
    private
        { Private declarations }
        FYearMonth: TArray<TYearMonth>;
        FTable: TArray<TArray<string>>;
        procedure HandleReadOutpu1(const s: string);
    public
        { Public declarations }
        procedure FetchYearsMonths;
    end;

var
    FormData: TFormData;

implementation

{$R *.dfm}

uses FireDAC.Comp.Client, dateutils, stringgridutils, stringutils,
    UnitFormPopup,    System.NetEncoding,
    UnitKgsdumData, JclSysUtils;

procedure TFormData.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormData.FormShow(Sender: TObject);
begin
    //
    ComboBox1Change(nil);

end;

procedure TFormData.StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;

begin
    grd := Sender as TStringGrid;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;

    ta := taLeftJustify;
    DrawCellText(grd, ACol, ARow, Rect, ta, grd.Cells[ACol, ARow]);
end;

procedure TFormData.FetchYearsMonths;
var
    I: Integer;
    ym: TYearMonth;
begin
    ComboBox1.Clear;
    SetLength(FYearMonth, 0);
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.ConnCharts;
        SQL.Text := 'SELECT DISTINCT ' +
          'cast(strftime(''%Y'', created_at) AS INTEGER) AS year, ' +
          'cast(strftime(''%m'', created_at) AS INTEGER) AS month FROM party ' +
          'ORDER BY year DESC, month DESC';
        Open;
        while not eof do
        begin
            SetLength(FYearMonth, Length(FYearMonth) + 1);
            with FYearMonth[Length(FYearMonth) - 1] do
            begin
                Year := FieldValues['year'];
                Month := FieldValues['month'];
            end;
        end;
        Close;
        Free;
    end;
    if Length(FYearMonth) = 0 then
        with ym do
        begin
            Year := YearOf(now);
            Month := MonthOf(now);
            FYearMonth := [ym];
        end;

    for I := 0 to Length(FYearMonth) - 1 do
        with FYearMonth[I] do
            ComboBox1.Items.Add(Format('%d %s',
              [Year, FormatDateTime('MMMM', IncMonth(0, Month))]));

    ComboBox1.ItemIndex := 0;
    ComboBox1Change(nil);
end;

procedure TFormData.HandleReadOutpu1(const s: string);
begin
    OutputDebugStringW(PWideChar(s));

end;

procedure TFormData.ComboBox1Change(Sender: TObject);
var
    I: Integer;
    Abort: Boolean;
    StrOutput:string;
    Base64: TBase64Encoding;
begin
    KgsdumData.Conn.Connected := false;
    JclSysUtils.Execute('kgsdump d1 -y=2019 -m=4 -f=base64', StrOutput);
    KgsdumData.Conn.Connected := True;

    Base64 := TBase64Encoding.Create;
    StrOutput := TEncoding.UTF8.GetString(Base64.DecodeStringToBytes(StrOutput));
    StrOutput := '';
    Base64.Free;

end;

end.
