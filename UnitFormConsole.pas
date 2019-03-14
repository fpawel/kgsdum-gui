unit UnitFormConsole;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
    Vcl.Samples.Spin, Vcl.ToolWin, System.ImageList, Vcl.ImgList, Vcl.ExtCtrls,
    Vcl.Grids, data_model;

type

    TFormConsole = class(TForm)
        ImageList4: TImageList;
        StringGrid1: TStringGrid;
        procedure FormCreate(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);

    private
        { Private declarations }
        FEntries: TArray<TLogEntry>;

    public
        { Public declarations }
        procedure AddComportMessage(AComport: string;
          ARequest, AResponse: TBytes; millis, attempt: Integer);
        procedure NewLine(ACreatedAt: TDatetime; ALevel: data_model.TLogLevel;
          AWork, AText: string);
        procedure Clear;
    end;

var
    FormConsole: TFormConsole;

implementation

uses FireDAC.Comp.Client, Rest.Json, dateutils, richeditutils, stringutils,
    stringgridutils,
    UnitKgsdumData, comport;

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
                case FEntries[ARow].Level of
                    loglevDebug:
                        cnv.Font.Color := clGray;
                    loglevInfo:
                        cnv.Font.Color := clNavy;
                    loglevWarn:
                        cnv.Font.Color := clMaroon;
                    loglevError:
                        cnv.Font.Color := clRed;
                    loglevException:
                        begin
                            cnv.Font.Color := clYellow;
                            cnv.Brush.Color := clBlack;
                        end;
                end;
            end;

    end;

    DrawCellText(StringGrid1, ACol, ARow, Rect, ta,
      StringGrid1.Cells[ACol, ARow]);
    // StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, ARow, Rect);
end;

procedure TFormConsole.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if Button = TMouseButton.mbRight then
        with StringGrid1 do
        begin
            Col := -1;
            Row := -1;
        end;

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

procedure TFormConsole.NewLine(ACreatedAt: TDatetime;
  ALevel: data_model.TLogLevel; AWork, AText: string);
begin
    SetLength(FEntries, Length(FEntries) + 1);
    with FEntries[Length(FEntries) - 2] do
    begin
        Work := AWork;
        Level := ALevel;
        Text := AText;
        Time := ACreatedAt;
    end;

    with StringGrid1 do
    begin
        Cells[0, Rowcount - 1] := formatDatetime('hh:mm:ss', ACreatedAt);
        Cells[1, Rowcount - 1] := AWork + ': ' + AText;
        Rowcount := Rowcount + 1;
        Cells[0, Rowcount - 1] := '';
        Cells[1, Rowcount - 1] := '';
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
    // NewEntry(loglevDebug, s);
end;

end.
