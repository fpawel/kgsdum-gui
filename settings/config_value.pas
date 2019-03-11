unit config_value;

interface

uses vcl.controls, inifiles;

type
    TPropertyValueType = (VtInt, VtFloat, VtString, VtComportName, VtBaud,
      VtBool, VtNullFloat);

    TConfigValue = class abstract
    public
        function AsString: string; virtual; abstract;
        function CreateControl(Parent:TWincontrol): TWinControl; virtual; abstract;
        procedure SetValueFromControl(Sender: TWinControl); virtual; abstract;
        function Error: string; virtual; abstract;
        constructor Create; virtual; abstract;
    end;

    RConfigData = record
        Name: string;
        Value: TConfigValue;
        Properties: TArray<RConfigData>;
        function HasError: boolean;
        constructor CreateProperty(AName: string;  AValue: TConfigValue);
        constructor CreateSection(AName: string; AProps: TArray<RConfigData>);
    end;

    PConfigData = ^RConfigData;

    TConfigFloat = class(TConfigValue)
    public
        FError: string;
        Min, Max: double;
        MinSet, MaxSet: boolean;
        Value: double;
        constructor Create;

        function Error: string; override;
        function CreateControl(Parent:TWincontrol): TWinControl; override;
        procedure SetValueFromControl(Sender: TWinControl); override;
        function AsString: string; override;
    end;

    TConfigBool = class(TConfigValue)
    private
        procedure OnChange(Sender: TObject);
    public
        Value: boolean;
        constructor Create;
        function Error: string; override;
        function AsString: string; override;
        function CreateControl(Parent:TWincontrol): TWinControl; override;
        procedure SetValueFromControl(Sender: TWinControl); override;
    end;

    TConfigComportName = class(TConfigValue)
    private
        FError: string;
    public
        Value: string;
        constructor Create;
        function Error: string; override;
        function AsString: string; override;
        function CreateControl(Parent:TWincontrol): TWinControl; override;
        procedure SetValueFromControl(Sender: TWinControl); override;
    end;


    TConfigComport = record
        Name: TConfigComportName;
    end;


implementation

uses sysutils, stringutils, vcl.stdctrls, comport;

// -----------------------------------------------------------------------------
// RConfigData
// -----------------------------------------------------------------------------
function RConfigData.HasError: boolean;
var
    i: integer;
begin
    if Assigned(Value) and (Value.Error <> '') then
        Exit(true);
    for i := 0 to length(Properties) - 1 do
        if (Properties[i].Value.Error <> '') then
            Exit(true);
    Exit(false);
end;

constructor RConfigData.CreateProperty(AName: string; AValue: TConfigValue);
begin
    Name := AName;
    Value := AValue;
    SetLength(Properties,0);

end;

constructor RConfigData.CreateSection(AName: string; AProps: TArray<RConfigData>);
begin
    Name := AName;
    Properties := AProps;
    Value := nil;

end;
// -----------------------------------------------------------------------------
// TConfigFloat
// -----------------------------------------------------------------------------
constructor TConfigFloat.Create;
begin

end;

function TConfigFloat.Error: string;
begin
    exit(FError);
end;

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

function TConfigFloat.CreateControl(Parent:TWincontrol): TWinControl;
var
    c: TEdit;
begin
    result := TEdit.Create(nil);
    c.Parent := Parent;
    c.Text := floattostr(Value);
    result := c;
end;

function TConfigFloat.AsString: string;
begin
    result := floattostr(Value);
end;
// -----------------------------------------------------------------------------
// TConfigBool
// -----------------------------------------------------------------------------
constructor TConfigBool.Create;
begin

end;

function TConfigBool.Error: string;
begin
    exit('');
end;

procedure TConfigBool.OnChange(Sender: TObject);
begin
    Value := (Sender as TCheckBox).Checked;
end;

procedure TConfigBool.SetValueFromControl(Sender: TWinControl);
begin
    Value := (Sender as TCheckBox).Checked;
end;

function TConfigBool.CreateControl(Parent:TWincontrol): TWinControl;
var
    cb: TCheckBox;
begin
    cb := TCheckBox.Create(nil);
    cb.Parent := Parent;
    cb.OnClick := OnChange;
    cb.Checked := Value;
    result := cb;
end;

function TConfigBool.AsString: string;
begin
    result := '';
end;

// -----------------------------------------------------------------------------
// TConfigComportName
// -----------------------------------------------------------------------------

constructor TConfigComportName.Create;
begin

end;

function TConfigComportName.Error: string;
begin
    exit(FError);
end;

function TConfigComportName.CreateControl(Parent:TWincontrol): TWinControl;
var
    cb: TComboBox;
begin
    cb := TComboBox.Create(nil);
    cb.Parent := Parent;
    EnumComPorts(cb.Items);
    cb.ItemIndex := cb.Items.IndexOf(Value);
    result := cb;
end;

procedure TConfigComportName.SetValueFromControl(Sender: TWinControl);
begin
    Value := (Sender as TComboBox).Text;
end;

function TConfigComportName.AsString: string;
begin
    result := Value;
end;

end.
