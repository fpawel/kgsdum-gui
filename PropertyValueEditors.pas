unit PropertyValueEditors;

// Utility unit for the advanced Virtual Treeview demo application which contains the implementation of edit link
// interfaces used in other samples of the demo.

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons, ExtCtrls, ComCtrls,
    Mask, config_value;

type
    TPropertyValueType = (VtInt, VtFloat, VtString, VtComportName, VtBaud,
      VtBool, VtNullFloat);

    TConfigProperty = class
    public
        Hint, Name: string;
        Value: TConfigValue;
        Children: TArray<TConfigProperty>;
    end;

    TConfigSection = class
        FName: string;
        FHint: string;
        FProperties: TArray<TConfigProperty>;
        function HasError: boolean;
    end;

    TConfigSections = TArray<TConfigSection>;

    RConfigData = record
        Prop: TConfigProperty;
        Sect: TConfigSection;
        function HasError: boolean;
    end;

    PConfigData = ^RConfigData;

    // ----------------------------------------------------------------------------------------------------------------------
    // Our own edit link to implement several different node editors.
    TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
    private
        FEdit: TWinControl; // One of the property editor classes.
        FTree: TVirtualStringTree; // A back reference to the tree calling.
        FNode: PVirtualNode; // The node being edited.
        FColumn: integer; // The column of the node being edited.
        FConfigData: PConfigData;

    protected
        procedure EditKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    public
        destructor Destroy; override;

        function BeginEdit: boolean; stdcall;
        function CancelEdit: boolean; stdcall;
        function EndEdit: boolean; stdcall;
        function GetBounds: TRect; stdcall;
        function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex): boolean; stdcall;
        procedure ProcessMessage(var Message: TMessage); stdcall;
        procedure SetBounds(R: TRect); stdcall;

    end;

    // ----------------------------------------------------------------------------------------------------------------------

type

    TPropertyTextKind = (ptkText, ptkHint);

    // The following constants provide the property tree with default data.



    // ----------------------------------------------------------------------------------------------------------------------

implementation

uses
    comport, vclutils, stringutils, RTTI;

function TConfigSection.HasError: boolean;
var
    i: integer;
begin
    for i := 0 to length(self.FProperties) - 1 do
        if FProperties[i].FValue.HasError then
            Exit(true);
    Exit(false);
end;

function RConfigData.HasError: boolean;
begin
    if Assigned(Prop) then
        exit(Prop.FValue.HasError);
    exit(Sect.HasError);

end;



// ----------------- TPropertyEditLink ----------------------------------------------------------------------------------

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TPropertyEditLink.Destroy;

begin
    // FEdit.Free; casues issue #357. Fix:
    if FEdit.HandleAllocated then
        PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
    inherited;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
    CanAdvance: boolean;

begin
    CanAdvance := true;

    case Key of
        VK_ESCAPE:
            begin
                Key := 0; // ESC will be handled in EditKeyUp()
            end;
        VK_RETURN:
            if CanAdvance then
            begin
                FTree.EndEditNode;
                Key := 0;
            end;

        VK_UP, VK_DOWN:
            begin
                // Consider special cases before finishing edit mode.
                CanAdvance := Shift = [];
                if FEdit is TComboBox then
                    CanAdvance := CanAdvance and not TComboBox(FEdit)
                      .DroppedDown;
                if FEdit is TDateTimePicker then
                    CanAdvance := CanAdvance and not TDateTimePicker(FEdit)
                      .DroppedDown;

                if CanAdvance then
                begin
                    // Forward the keypress to the tree. It will asynchronously change the focused node.
                    PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
                    Key := 0;
                end;
            end;
    end;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    case Key of
        VK_ESCAPE:
            begin
                FTree.CancelEditNode;
                Key := 0;
            end; // VK_ESCAPE
    end; // case
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: boolean;

begin
    result := true;
    FEdit.Show;
    FEdit.SetFocus;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: boolean;

begin
    result := true;
    FEdit.Hide;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: boolean;
begin
    result := true;
    FEdit.Hide;
    try
        FTree.SetFocus;
    except

    end;
    FConfigData.Prop.FValue.SetValueFromControl(FEdit);
    FTree.InvalidateNode(FNode);
    FTree.InvalidateNode(FNode.Parent);
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;

begin
    result := FEdit.BoundsRect;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure EnumBaudRates(const Items: TStrings);
begin
    Items.Add('1200');
    Items.Add('2400');
    Items.Add('4800');
    Items.Add('9600');
    Items.Add('14400');
    Items.Add('19200');
    Items.Add('38400');
    Items.Add('56000');
    Items.Add('57600');
    Items.Add('115200');
    Items.Add('128000');
    Items.Add('256000');
end;

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean;

var
    i: integer;

begin
    result := true;
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    FConfigData := FTree.GetNodeData(FNode);

    // determine what edit type actually is needed
    FEdit.Free;
    FEdit := FConfigData.Prop.FValue.CreateControl;

    if FEdit is TComboBox then
        with FEdit as TComboBox do
        begin
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            style := csDropDown;
            ItemHeight := 22;
        end
    else if FEdit is TEdit then
        with FEdit as TEdit do
        begin
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end
    else if FEdit is TCheckBox then
        with FEdit as TCheckBox do
            Caption := '---'
    else
        result := false;

    FEdit.Visible := false;
    FEdit.Parent := Tree;

end;




// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);

begin
    FEdit.WindowProc(Message);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);

var
    Dummy: integer;

begin
    // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
    // we have to set the edit's width explicitly to the width of the column.
    FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
    FEdit.BoundsRect := R;
end;
// ----------------------------------------------------------------------------------------------------------------------


end.
