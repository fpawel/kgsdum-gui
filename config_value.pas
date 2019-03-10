unit config_value;

interface

uses vcl.controls;

type
    TPropertyValueType = (VtInt, VtFloat, VtString, VtComportName, VtBaud,
      VtBool, VtNullFloat);

    TConfigValue = class abstract
    protected
        FError: string;
    public
        function CreateControl: TWinControl; virtual; abstract;
        procedure SetValueFromControl(Sender: TWinControl); virtual; abstract;
        function HasError: boolean;
    end;

    TConfigFloat = class(TConfigValue)
    private

        function CreateControl: TWinControl;
        procedure SetValueFromControl(Sender: TWinControl);
    public
        Min, Max: double;
        MinSet, MaxSet: boolean;
        Value: double;
    end;

    TConfigBool = class(TConfigValue)
    private
        function CreateControl: TWinControl;
        procedure SetValueFromControl(Sender: TWinControl);
        procedure OnChange(Sender: TObject);
    public
        Value: boolean;
    end;

    TConfigComportName = class(TConfigValue)
    private
        function CreateControl: TWinControl;
        procedure SetValueFromControl(Sender: TWinControl);
    public
        Value : string;
    end;

implementation

uses sysutils, stringutils, vcl.stdctrls, comport;

//------------- TConfigFloat ---------------------------------------------------
procedure TConfigFloat.SetValueFromControl(Sender: TWinControl);
var
    s: string;
begin
    s := str_validate_decimal_separator((Sender as TEdit).Text);
    if not TryStrToFloat(s, Value) then
    begin
        FError := 'не правильный синтаксис числа c плавающей точкой';
        exit;
    end;
    if MinSet and (Value < Min) then
    begin
        FError := 'меньше ' + floattostr(Min);
        exit;
    end;
    if MaxSet and (Value > Max) then
    begin
        FError := 'больше ' + floattostr(Max);
        exit;
    end;
    FError := '';
end;

function TConfigFloat.CreateControl: TWinControl;
var c : TEdit;
begin
    c := TEdit.Create(nil);
    c.Text := FloatToStr(Value);
    result := c;
end;

//------------- TConfigBool ---------------------------------------------------
procedure TConfigBool.OnChange(Sender: TObject);
begin
    Value := (Sender as TCheckBox).Checked;
end;

procedure TConfigBool.SetValueFromControl(Sender: TWinControl);
begin
    Value := (Sender as TCheckBox).Checked;
end;

function TConfigBool.CreateControl: TWinControl;
var
    cb: TCheckBox;
begin
    cb := TCheckBox.Create(nil);
    cb.OnClick := OnChange;
    cb.Checked := Value;
    result := cb;
end;

//------------- TConfigComportName ---------------------------------------------

function TConfigComportName.CreateControl: TWinControl;
var
    cb: TComboBox;
begin
    cb := TComboBox.Create(nil);
    EnumComPorts(cb.Items);
    cb.ItemIndex := cb.Items.IndexOf(Value);
    result := cb;
end;

procedure TConfigComportName.SetValueFromControl(Sender: TWinControl);
begin
    Value := (Sender as TComboBox).Text;
end;

//------------- TConfigValue ---------------------------------------------
function TConfigValue.HasError: boolean;
begin
    result := FError <> '';
end;

end.
