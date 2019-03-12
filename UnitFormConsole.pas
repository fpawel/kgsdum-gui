unit UnitFormConsole;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
    Vcl.Samples.Spin, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
    Vcl.Grids;

type

    TLogLevel = (loglevDebug, loglevInfo, loglevWarn, loglevError);

    TFormConsole = class(TForm)
        ImageList4: TImageList;
        StringGrid1: TStringGrid;
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);

    private
        { Private declarations }
        FLevels: TArray<TLogLevel>;

    public
        { Public declarations }
        procedure AddComportMessage(AComport: string;
          ARequest, AResponse: TBytes; millis, attempt: Integer);
        procedure AddLine(ALevel: TLogLevel; AText: string);
    end;

var
    FormConsole: TFormConsole;

implementation

uses Rest.Json, dateutils, richeditutils, stringutils, stringgridutils;

{$R *.dfm}

procedure TFormConsole.FormCreate(Sender: TObject);
begin
    SetLength(FLevels, 1);

end;

procedure TFormConsole.FormResize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 100;
        ColWidths[1] := self.Width - ColWidths[0] - 30;
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

    if ACol = 0 then
    begin
        ta := taCenter;
        cnv.Font.Color := clGreen;
    end
    else
    begin
        ta := taLeftJustify;
        cnv.Font.Color := clBlack;
        case FLevels[ARow] of
            loglevDebug:
                cnv.Font.Color := clGray;
            loglevInfo:
                cnv.Font.Color := clNavy;
            loglevWarn:
                cnv.Font.Color := clMaroon;
            loglevError:
                cnv.Font.Color := clRed;
        end;
    end;

    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, ARow, Rect);
end;

procedure TFormConsole.AddLine(ALevel: TLogLevel; AText: string);
begin
    SetLength(FLevels, Length(FLevels) + 1);
    FLevels[Length(FLevels) - 2] := ALevel;

    with StringGrid1 do
    begin
        Row := RowCount - 1;
        Cells[0, RowCount - 1] := formatDatetime('hh:mm:ss.zzz', now);
        Cells[1, RowCount - 1] := AText;
        RowCount := RowCount + 1;
    end;
end;

procedure TFormConsole.AddComportMessage(AComport: string;
  ARequest, AResponse: TBytes; millis, attempt: Integer);

var
    s: string;
begin
    s := AComport + ' : ' + BytesToHex(ARequest);
    if Length(AResponse) > 0 then
        s := s + ' --> ' + BytesToHex(ARequest);

    s := s + ' ' + Inttostr(millis) + ' мс';

    if attempt > 1 then
        s := s + ' (' + Inttostr(attempt) + ')';
    AddLine(loglevDebug, s);
end;

end.
