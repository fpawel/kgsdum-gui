unit UnitFormData;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ComCtrls, System.Generics.Collections;

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
        FCheckCells: TDictionary<TPoint, bool>;
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
    UnitFormPopup, UnitKgsdumData, FireDAC.Stan.Param, math;
// , System.NetEncoding, Grijjy.Bson.Serialization, JclSysUtils

function VariantIsEmptyOrNull(const Value: Variant): Boolean;
begin
    Result := VarIsClear(Value) or VarIsEmpty(Value) or VarIsNull(Value) or
      (VarCompareValue(Value, Unassigned) = vrEqual);
    if (not Result) and VarIsStr(Value) then
        Result := Value = '';
end;

function FetchNullFloat(q: TFDQuery; field: string): string;
var
    Value: double;
begin
    if VariantIsEmptyOrNull(q.FieldValues[field]) then
        exit('');
    Value := q.FieldByName(field).AsFloat;
    exit(Formatfloat('0.###', Value));
end;

procedure TFormData.FormCreate(Sender: TObject);
begin
    FCheckCells := TDictionary<TPoint, bool>.create;;
end;

procedure TFormData.FormShow(Sender: TObject);
begin
    //
end;

procedure TFormData.StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;
    conc, pgs, limit_d: double;

begin
    grd := Sender as TStringGrid;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if (ARow = 0) or (ACol = 0) then
        cnv.Brush.Color := cl3DLight;

    ta := taCenter;
    if (ARow > 0) AND (ACol > 4) then
        ta := taRightJustify;

    if FCheckCells.ContainsKey(Point(ACol, ARow)) then
    begin
        if FCheckCells[Point(ACol, ARow)] then
            cnv.Font.Color := clBlue
        else
            cnv.Font.Color := clRed;

    end;

    DrawCellText(grd, ACol, ARow, Rect, ta, grd.Cells[ACol, ARow]);
end;

procedure TFormData.FetchYearsMonths;
var
    I: Integer;
    ym: TYearMonth;
begin
    ComboBox1.Clear;
    SetLength(FYearMonth, 0);
    with TFDQuery.create(nil) do
    begin
        Connection := KgsdumData.Conn;
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
            Next;
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

// KgsdumData.Conn.Connected := false;
// JclSysUtils.Execute('kgsdump products.table -y=2019 -m=4 -f=base64', StrOutput);
// KgsdumData.Conn.Connected := True;
// StrOutput := TEncoding.UTF8.GetString(TNetEncoding.Base64.DecodeStringToBytes(StrOutput));
// TgoBsonSerializer.Deserialize(StrOutput, FTable);

procedure TFormData.ComboBox1Change(Sender: TObject);
var
    ACol, ARow: Integer;
const
    columns_count = 32;
    columns: array [0 .. columns_count - 1] of array [0 .. 2]
      of string = (('Номер', 'product_id', ''), ('День', 'day', ''),
      ('Загрузка', 'party_id', ''), ('Зав.номер', 'serial_number', ''),
      ('Адрес', 'addr', ''), ('Work ПГС3', 'work_gas3', ''),
      ('Work +20⁰С', 'work_plus20', ''), ('Ref +20⁰С', 'ref_plus20', ''),
      ('Work -5⁰С', 'work_minus5', ''), ('Ref -5⁰С', 'ref_minus5', ''),
      ('Work +50⁰С', 'work_plus50', ''), ('Ref +50⁰С', 'ref_plus50', ''),
      ('Погр.1 +20⁰С', 'err1_plus20', 'err1_plus20'),
      ('Погр.4 +20⁰С', 'err4_plus20', 'err4_plus20'),
      ('Погр.1 0⁰', 'err1_zero', 'err1_zero'), ('Погр.4 0⁰', 'err4_zero',
      'err4_zero'), ('Погр.1 +50⁰', 'err1_plus50', 'err1_plus50'),
      ('Погр.4 +50⁰', 'err4_plus50', 'err4_plus50'),
      ('Погр.1 +20⁰.2', 'err1_plus20ret', 'err1_plus20ret'),
      ('Погр.4 +20⁰.2', 'err4_plus20ret', 'err4_plus20ret'),

      ('Конц.1 +20⁰С', 'c1_plus20', 'err1_plus20'),
      ('Конц.4 +20⁰С', 'c4_plus20', 'err4_plus20'),
      ('Конц.1 0⁰', 'c1_zero', 'err1_zero'), ('Конц.4 0⁰', 'c4_zero',
      'err4_zero'), ('Конц.1 +50⁰', 'c1_plus50', 'err1_plus50'),
      ('Конц.4 +50⁰', 'c4_plus50', 'err4_plus50'),
      ('Конц.1 +20⁰.2', 'c1_plus20ret', 'err1_plus20ret'),
      ('Конц.4 +20⁰.2', 'c4_plus20ret', 'err4_plus20ret'),

      ('ПГС1', 'pgs1', ''), ('ПГС2', 'pgs2', ''), ('ПГС3', 'pgs3', ''),
      ('ПГС4', 'pgs4', '')

      );
begin
    FCheckCells.Clear;
    if (ComboBox1.ItemIndex < 0) or (ComboBox1.ItemIndex >= Length(FYearMonth))
    then
        exit;
    with StringGrid2, KgsdumData.FDQueryProductsValues,
      FYearMonth[ComboBox1.ItemIndex] do
    begin
        colCount := columns_count;
        RowCount := 1;
        for ACol := 0 to colCount - 1 do
            Cells[ACol, 0] := columns[ACol][0];
        ParamByName('year').Value := Year;
        ParamByName('month').Value := Month;
        Open;
        while not eof do
        begin
            RowCount := RowCount + 1;
            ARow := RowCount - 1;
            for ACol := 0 to colCount - 1 do
            begin
                if ACol < 5 then
                    Cells[ACol, ARow] :=
                      VarToStr(FieldByName(columns[ACol][1]).Value)
                else
                    Cells[ACol, ARow] :=
                      FetchNullFloat(KgsdumData.FDQueryProductsValues,
                      columns[ACol][1]);
                if (Length(columns[ACol][2]) > 0) AND
                  not(VariantIsEmptyOrNull(columns[ACol][2])) then
                    FCheckCells.Add(Point(ACol, ARow),
                      Abs(FieldByName(columns[ACol][2]).AsFloat) < 100);
            end;
            Next;
        end;
        Close;
        if RowCount > 1 then
            FixedRows := 1;
        FixedCols := 1;
    end;
    StringGrid_SetupColumnsWidth(StringGrid2);

end;

end.
