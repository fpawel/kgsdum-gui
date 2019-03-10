unit vclutils;

interface

uses CommCtrl, vcl.stdctrls, vcl.controls, vcl.ComCtrls, vcl.graphics,
    system.Types;

type
    TControlProc = reference to procedure(const AControl: TControl);

procedure SetButtonMultiline(b: TButton);

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);

procedure ConvertImagesToHighColor(ImageList: TImageList);

function cut_str(s:string;c:TCanvas; w:integer):string;

implementation

uses Winapi.Windows;

function cut_str(s:string;c:TCanvas; w:integer):string;
 var i, w1 :integer;
    s1:string;
begin
    if w > c.TextWidth( s ) then
        exit(s);
    result := s;
    w1 := c.TextWidth( '...' );
    result := '';
    for I := 1 to Length(s) do
    begin
        s1 := result + s[i];
        if c.TextWidth(s1)+w1+3 > w then
        	break;
        result := s1;
    end;
    result := result + '...';
end;

procedure ConvertImagesToHighColor(ImageList: TImageList);

// To show smooth images we have to convert the image list from 16 colors to high color.

var
    IL: TImageList;

begin
    // Have to create a temporary copy of the given list, because the list is cleared on handle creation.
    IL := TImageList.Create(nil);
    IL.Assign(ImageList);

    with ImageList do
        Handle := ImageList_Create(Width, Height, ILC_COLOR16 or ILC_MASK,
          Count, AllocBy);
    ImageList.Assign(IL);
    IL.Free;
end;

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);
var
    i: integer;
begin
    if AControl = nil then
        Exit;
    if AControl is TWinControl then
    begin
        for i := 0 to TWinControl(AControl).ControlCount - 1 do
            ModifyControl(TWinControl(AControl).controls[i], ARef);
    end;
    ARef(AControl);
end;

procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);
var
    i: integer;
    PageControl: TPageControl;
    Text: string;
    x, y: integer;
    txt_width, txt_height: double;
begin
    PageControl := Control as TPageControl;
    Active := PageControl.ActivePageIndex = TabIndex;
    Text := PageControl.Pages[TabIndex].Caption;

    txt_width := PageControl.Canvas.TextWidth(Text);
    txt_height := PageControl.Canvas.TextHeight(Text);

    x := Rect.Left + round((Rect.Width - txt_width) / 2.0);
    y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    if PageControl.ActivePageIndex = TabIndex then
    begin
        PageControl.Canvas.Brush.Color := clGradientInactiveCaption;
        PageControl.Canvas.Font.Color := clNavy;
    end
    else
    begin
        PageControl.Canvas.Brush.Color := clWindow;
        PageControl.Canvas.Font.Color := clBlack;
    end;

    PageControl.Canvas.TextRect(Rect, x, y, Text);
end;


end.
