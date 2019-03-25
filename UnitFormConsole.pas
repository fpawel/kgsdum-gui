unit UnitFormConsole;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
    Vcl.Samples.Spin, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
    Vcl.Grids, data_model, Vcl.Menus, UnitAppIni;

type

    TFormConsole = class(TForm)
        ImageList4: TImageList;
        StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    N1: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1DblClick(Sender: TObject);
    procedure N1Click(Sender: TObject);

    private
        { Private declarations }
        FEntries: TArray<TLogEntry>;

    public
        { Public declarations }
        procedure NewLine(ACreatedAt: TDatetime; ALevel: data_model.TLogLevel;
          AWork, AText: string);
        procedure Clear;
    end;

var
    FormConsole: TFormConsole;

implementation

uses FireDAC.Comp.Client, Rest.Json, dateutils, richeditutils, stringutils,
    stringgridutils,
    UnitKgsdumData, comport, UnitFormPopup;

{$R *.dfm}

procedure TFormConsole.FormCreate(Sender: TObject);
begin
    SetLength(FEntries, 1);

end;

procedure TFormConsole.FormResize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := self.Width - ColWidths[0] - 30;
    end;
end;

procedure TFormConsole.StringGrid1DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
begin
    with StringGrid1 do
    begin
        FormPopup.RichEdit1.Text := Cells[1, Row];
        r := CellRect(Col, Row);
        pt := StringGrid1.ClientToScreen(r.TopLeft);
        FormPopup.Left := pt.X + 5;
        FormPopup.Top := pt.Y + 5;
        FormPopup.Show;
    end;
end;

procedure TFormConsole.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
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
                case FEntries[ARow].FLevel of
                    loglevTrace:
                        cnv.Font.Color := clGray;
                    loglevDebug:
                        cnv.Font.Color := clBlack;
                    loglevInfo:
                        cnv.Font.Color := clNavy;
                    loglevWarn:
                        cnv.Font.Color := clMaroon;
                    loglevError, loglevException:
                        cnv.Font.Color := clRed;

                end;
            end;

    end;

    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, ARow, Rect);
end;

procedure TFormConsole.Clear;
begin
    with StringGrid1 do
    begin
        Rowcount := 1;
        Cells[0, 0] := '';
        Cells[1, 0] := '';
    end;
    SetLength(FEntries, 1);
end;

procedure TFormConsole.N1Click(Sender: TObject);
begin
    StringGrid_CopytoClipboard(StringGrid1);
end;

procedure TFormConsole.NewLine(ACreatedAt: TDatetime;
  ALevel: data_model.TLogLevel; AWork, AText: string);
begin

    SetLength(FEntries, Length(FEntries) + 1);
    with FEntries[Length(FEntries) - 2] do
    begin
        FWork := AWork;
        FLevel := ALevel;
        FText := AText;
        FCreatedAt := ACreatedAt;
    end;

    with StringGrid1 do
    begin
        Cells[0, Rowcount - 1] := formatDatetime('hh:mm:ss', ACreatedAt);
        if AWork <> '' then
            Cells[1, Rowcount - 1] := AWork + ': ' + AText
        else
            Cells[1, Rowcount - 1] := AText;
        Rowcount := Rowcount + 1;
        Cells[0, Rowcount - 1] := '';
        Cells[1, Rowcount - 1] := '';
    end;
end;

end.
