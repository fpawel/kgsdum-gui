﻿unit UnitFormLastParty;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
    Vcl.Imaging.pngimage, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList,
    Vcl.Menus, data_model, Vcl.ComCtrls, Vcl.ToolWin;

type
    TFormLastParty = class(TForm)
        StringGrid1: TStringGrid;
        ImageList1: TImageList;
        ToolBarParty: TToolBar;
        ToolButtonParty: TToolButton;
        ToolButtonStop: TToolButton;
        ToolButton1: TToolButton;
    ToolButton2: TToolButton;
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
          const Value: string);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure FormShow(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ToolButtonPartyClick(Sender: TObject);
        procedure ToolButtonStopClick(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
    private
        { Private declarations }
        Last_Edited_Col, Last_Edited_Row: Integer;

        FProducts: TArray<TProduct>;
        FColumns: TArray<TProductField>;

        FhWndTip: THandle;

        function GetProductValue(ColumnIndex, RowIndex: Integer): string;

        procedure DrawCellStr(ACol, ARow: Integer; Rect: TRect; ta: TAlignment;
          str: string);

        procedure DrawCellText(ACol, ARow: Integer; Rect: TRect;
          ta: TAlignment);

        procedure UpdateSerial(ACol, ARow: Integer; Value: string);
        procedure UpdateAddr(ACol, ARow: Integer; Value: string);

        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

        property ProductValues[ColumnIndex, RowIndex: Integer]: string
          read GetProductValue;

    public
        { Public declarations }
        procedure SetProductionAll(production: Boolean);

        procedure reload_data;
    end;

var
    FormLastParty: TFormLastParty;

implementation

uses FireDAC.Comp.Client, stringgridutils, stringutils, dateutils, crud,
    UnitKgsdumData, vclutils, ComponentBaloonHintU;

{$R *.dfm}

procedure TFormLastParty.FormCreate(Sender: TObject);
begin
    SetLength(FProducts, 96);
    reload_data;
end;

procedure TFormLastParty.FormShow(Sender: TObject);
begin
    //
end;

procedure TFormLastParty.WMEnterSizeMove(var Msg: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.StringGrid1SelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;
begin
    grd := Sender as TStringGrid;

    // When selecting a cell
    if grd.EditorMode then
    begin // It was a cell being edited
        grd.EditorMode := false; // Deactivate the editor
        // Do an extra check if the LastEdited_ACol and LastEdited_ARow are not -1 already.
        // This is to be able to use also the arrow-keys up and down in the Grid.
        if (Last_Edited_Col <> -1) and (Last_Edited_Row <> -1) then
            StringGrid1SetEditText(grd, Last_Edited_Col, Last_Edited_Row,
              grd.Cells[Last_Edited_Col, Last_Edited_Row]);
        // Just make the call
    end;
    // Do whatever else wanted

    if (ARow > 0) AND (FColumns[ACol] in [pcSerial, pcAddr]) then
        grd.Options := grd.Options + [goEditing]
    else
        grd.Options := grd.Options - [goEditing];
end;

procedure TFormLastParty.StringGrid1SetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
    if ARow = 0 then
        exit;
    With StringGrid1 do
        // Fired on every change
        if Not EditorMode // goEditing must be 'True' in Options
        then
        begin // Only after user ends editing the cell
            Last_Edited_Col := -1; // Indicate no cell is edited
            Last_Edited_Row := -1; // Indicate no cell is edited
            // Do whatever wanted after user has finish editing a cell
            StringGrid1.OnSetEditText := nil;
            try
                case TProductField(FColumns[ACol]) of
                    pcSerial:
                        begin
                            UpdateSerial(ACol, ARow, Value);
                        end;
                    pcAddr:
                        begin
                            UpdateAddr(ACol, ARow, Value);
                        end;
                end;
            finally
                StringGrid1.OnSetEditText := StringGrid1SetEditText;
            end;
        end
        else
        begin // The cell is being editted
            Last_Edited_Col := ACol; // Remember column of cell being edited
            Last_Edited_Row := ARow; // Remember row of cell being edited
        end;

end;

procedure TFormLastParty.ToolButton1Click(Sender: TObject);
var
    p: TProduct;
begin
    if (StringGrid1.Row - 1 < 0) or (StringGrid1.Row - 1 >= Length(FProducts))
    then
        exit;
    p := FProducts[StringGrid1.Row - 1];

    if MessageBox(Handle,
      PCHar(format
      ('Подтвердите необходимость удаления данных БО №%d-%s, место %d, адрес %d',
      [p.FProductID, p.FSerial, StringGrid1.Row, p.FAddr])),
      'Запрос подтверждения', mb_IconQuestion or mb_YesNo) <> mrYes then
        exit;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        try
            SQL.Text := 'DELETE FROM product WHERE product_id = :product_id;';
            ParamByName('product_id').Value := p.FProductID;
            ExecSQL;
        finally
            Free;
        end;
    end;
    reload_data;
end;

procedure TFormLastParty.ToolButtonPartyClick(Sender: TObject);
begin
    if not MessageBox(Handle,
      'Подтвердите необходимость создания новой партии.',
      'Запрос подтверждения', mb_IconQuestion or mb_YesNo) <> mrYes then
        exit;

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        try
            SQL.Text := 'INSERT INTO party DEFAULT VALUES;';
            ExecSQL;
            SQL.Text :=
              'INSERT INTO product (party_id, serial_number, addr) VALUES ((SELECT * FROM last_party_id), ''1'', 1);';
            ExecSQL;
        finally
            Free;
        end;
    end;
    reload_data;

end;

procedure TFormLastParty.ToolButtonStopClick(Sender: TObject);
var
    i, serial, addr: Integer;
    addrs: TArray<Integer>;
    serials: TArray<string>;

label  _l;

begin
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        try
            // SQL.Text := 'BEGIN TRANSACTION';
            // ExecSQL;

            SQL.Text :=
              'SELECT serial_number, addr FROM product WHERE party_id = (SELECT * FROM last_party_id);';
            Open;
            First;
            while not Eof do
            begin
                SetLength(serials, Length(serials) + 1);
                SetLength(addrs, Length(addrs) + 1);
                addrs[Length(addrs) - 1] := FieldValues['addr'];
                serials[Length(serials) - 1] := FieldValues['serial_number'];
                Next;
            end;
            Close;

            addr := 1;
            serial := 1;
        _l:
            for i := 0 to Length(addrs) - 1 do
            begin
                if addr = addrs[i] then
                begin
                    addr := addr + 1;
                    goto _l;
                end;
                if inttostr(serial) = serials[i] then
                begin
                    serial := serial + 1;
                    goto _l;
                end;
            end;

            SQL.Text :=
              'INSERT INTO product (party_id, serial_number, addr, production) '
              + 'VALUES ((SELECT * FROM last_party_id), :serial_number, :addr, TRUE);';
            ParamByName('serial_number').Value := inttostr(serial);
            ParamByName('addr').Value := addr;
            ExecSQL;
        finally
            Free;
        end;
    end;
    reload_data;
end;

procedure TFormLastParty.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    ACol, ARow: Integer;
    p: TProduct;
begin
    if (GetAsyncKeyState(VK_LBUTTON) >= 0) then
        exit;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if (ACol > 0) or (ARow = 0) then
        exit;
    p := FProducts[ARow - 1];
    p.FProduction := not p.FProduction;
    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'UPDATE product SET production = NOT production WHERE product_id = :product_id;';
        ParamByName('product_id').Value := p.FProductID;
        ExecSQL;
        Free;
    end;
    FProducts[ARow - 1] := p;
    StringGrid_RedrawRow(StringGrid1, ARow);

end;

procedure TFormLastParty.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    p: TProduct;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if ARow = 0 then
    begin
        cnv.Brush.Color := cl3DLight;
        DrawCellText(ACol, 0, Rect, taCenter);
        StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, 0, Rect);
        exit;
    end;

    p := FProducts[ARow - 1];
    if not p.FProduction then
        cnv.Brush.Color := $F6F7F7;

    case CheckProductFieldValue(p, FColumns[ACol]) of
        cvrOk:
            cnv.Font.Color := clBlue;
        cvrErr:
            cnv.Font.Color := clRed;
        cvrNone:
            begin

                if not p.FProduction then
                    cnv.Font.Color := clGray;

            end;

    end;

    if ACol = 0 then
    begin
        StringGrid_DrawCheckBoxCell(StringGrid1, 0, ARow, Rect, State,
          p.FProduction);
        StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
        exit;
    end;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption;
    DrawCellText(ACol, ARow, Rect, ProductFieldAlignment(FColumns[ACol]));
    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
end;

procedure TFormLastParty.DrawCellStr(ACol, ARow: Integer; Rect: TRect;
  ta: TAlignment; str: string);
var
    X, Y, txt_width, txt_height: Integer;
begin
    with StringGrid1.Canvas do
    begin

        if TextWidth(str) + 3 > Rect.Width then
            str := cut_str(str, StringGrid1.Canvas, Rect.Width);
        txt_width := TextWidth(str);
        txt_height := TextHeight(str);
        X := Rect.Left + 3;
        if ta = taRightJustify then
            X := Rect.Right - 3 - round(txt_width)
        else if ta = taCenter then
            X := Rect.Left + round((Rect.Width - txt_width) / 2.0);
        Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);
        TextRect(Rect, X, Y, str);
    end;
end;

procedure TFormLastParty.DrawCellText(ACol, ARow: Integer; Rect: TRect;
  ta: TAlignment);
begin
    DrawCellStr(ACol, ARow, Rect, ta, StringGrid1.Cells[ACol, ARow]);
end;

procedure TFormLastParty.reload_data;
var
    i: Integer;
    ARow, ACol: Integer;
    s: string;
begin
    StringGrid_Clear(StringGrid1);
    with Application.MainForm do
        with GetLastParty do
            Caption := format('Партия БО КГСДУМ № %d, создана %s',
              [FPartyID, FormatDateTime('dd MMMM yyyy hh:nn',
              IncHour(FCreatedAt, 3))]);
    FProducts := GetLastPartyProducts;
    FColumns := GetProductFields(FProducts);

    with StringGrid1 do
    begin
        colcount := Length(FColumns);
        RowCount := Length(FProducts) + 1;
        if Length(FProducts) = 0 then
            exit;

        FixedRows := 1;
        FixedCols := 1;
        ColWidths[0] := 80;

        for ACol := 0 to colcount - 1 do
        begin
            Cells[ACol, 0] := product_column_name[FColumns[ACol]];
            ColWidths[ACol] := ProductColumnWidth(FColumns[ACol],
              StringGrid1.Canvas, FProducts, erdtConc);
        end;

        for ARow := 1 to RowCount - 1 do
        begin
            for ACol := 0 to colcount - 1 do
            begin
                Cells[ACol, ARow] := ProductValues[ACol, ARow - 1];
            end;
        end;

    end;

end;

function TFormLastParty.GetProductValue(ColumnIndex, RowIndex: Integer): string;
begin
    result := formatProductfieldValue(FProducts[RowIndex],
      FColumns[ColumnIndex], erdtConc);

end;

procedure TFormLastParty.UpdateAddr(ACol, ARow: Integer; Value: string);
var
    p: TProduct;
begin
    CloseWindow(FhWndTip);
    p := FProducts[ARow - 1];

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'UPDATE product SET addr = :addr WHERE product_id = :product_id;';

        ParamByName('product_id').Value := p.FProductID;

        try
            ParamByName('addr').Value := StrToInt(Value);
            ExecSQL;
            p.FAddr := StrToInt(Value);
            FProducts[ARow - 1] := p;
        except
            on E: Exception do
            begin
                with StringGrid1 do
                begin
                    OnSetEditText := nil;
                    Cells[ACol, ARow] := inttostr(p.FAddr);
                    OnSetEditText := StringGrid1SetEditText;
                end;

                FhWndTip := StringGrid1.ShowBalloonTip(TIconKind.Error,
                  'Ошибка данных', format('%s: %s: "%s": %s',
                  [ProductValues[0, ARow - 1], product_column_name[pcSerial],
                  Value, E.Message]))

            end;
        end;
        Free;
    end;
    StringGrid_RedrawRow(StringGrid1, ARow);
end;

procedure TFormLastParty.UpdateSerial(ACol, ARow: Integer; Value: string);
var
    p: TProduct;
begin
    CloseWindow(FhWndTip);
    p := FProducts[ARow - 1];

    with TFDQuery.Create(nil) do
    begin
        Connection := KgsdumData.Conn;
        SQL.Text :=
          'UPDATE product SET serial_number = :serial_number WHERE product_id = :product_id;';
        ParamByName('serial_number').Value := Value;
        ParamByName('product_id').Value := p.FProductID;
        try
            ExecSQL;
            p.FSerial := Value;
            FProducts[ARow - 1] := p;
        except
            on E: Exception do
            begin
                with StringGrid1 do
                begin
                    OnSetEditText := nil;
                    Cells[ACol, ARow] := p.FSerial;
                    OnSetEditText := StringGrid1SetEditText;
                end;

                FhWndTip := StringGrid1.ShowBalloonTip(TIconKind.Error,
                  'Ошибка', format('%s: %s: "%s": %s',
                  [ProductValues[0, ARow - 1], product_column_name[pcSerial],
                  Value, E.Message]));

            end;
        end;
        Free;
    end;
    StringGrid_RedrawRow(StringGrid1, ARow);
end;

procedure TFormLastParty.SetProductionAll(production: Boolean);
var
    p: TProduct;
begin
    // TLastParty.SelectAll(production);
    // for p in FProducts do
    // begin
    // p.FProduction := production;
    // StringGrid_RedrawCell(StringGrid1, 0, p.FPlace + 1);
    // end;

end;

end.
