unit UnitFormProperties;

// Virtual Treeview sample form demonstrating following features:
// - Property page like string tree with individual node editors.
// - Incremental search.
// Written by Mike Lischke.
{$WARN UNSAFE_CODE OFF} // Prevent warnins that are not applicable

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ImgList, ExtCtrls, UITypes, System.ImageList,
    PropertyValueEditors, config_value, comport;

const
    // Helper message to decouple node change handling from edit handling.
    WM_STARTEDITING = WM_USER + 778;

type

    TFormProperties = class(TForm)
        VST3: TVirtualStringTree;
        TreeImages: TImageList;
        procedure FormCreate(Sender: TObject);
        procedure VST3Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; out EditLink: IVTEditLink);
        procedure VST3Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var Allowed: Boolean);
        procedure VST3GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
          var HintText: string);
        procedure VST3GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var Index: TImageIndex);
        procedure VST3GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure VST3InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
          var ChildCount: Cardinal);
        procedure VST3InitNode(Sender: TBaseVirtualTree;
          ParentNode, Node: PVirtualNode;
          var InitialStates: TVirtualNodeInitStates);
        procedure VST3PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure VST3IncrementalSearch(Sender: TBaseVirtualTree;
          Node: PVirtualNode; const SearchText: string; var Result: Integer);
        procedure VST3FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
          Node: PVirtualNode; Column: TColumnIndex; const Text: string;
          const CellRect: TRect; var DefaultDraw: Boolean);
        procedure VST3BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure FormDeactivate(Sender: TObject);

    private
        FProperties: TArray<RConfigData>;

        procedure WMStartEditing(var Message: TMessage);
          message WM_STARTEDITING;

        function GetNodeData(Node: PVirtualNode): PConfigData;
    public


        ComportProducts, ComportTermo: TConfigComportName;

        property TreeData[Node: PVirtualNode]: PConfigData read GetNodeData;
    end;

var
    FormProperties: TFormProperties;

implementation

uses
    Math, stringgridutils, vclutils;

{$R *.DFM}

procedure TFormProperties.FormCreate(Sender: TObject);
begin
    // We assign these handlers manually to keep the demo source code compatible
    // with older Delphi versions after using UnicodeString instead of WideString.
    // VST3.OnGetText := VST3GetText;
    // VST3.OnGetHint := VST3GetHint;
    // VST3.OnIncrementalSearch := VST3IncrementalSearch;

    // Always tell the tree how much data space per node it must allocated for us. We can do this here, in the
    // object inspector or in the OnGetNodeDataSize event.
    VST3.NodeDataSize := SizeOf(RConfigData);
    // The VCL (D7 and lower) still uses 16 color image lists. We create a high color version explicitely because it
    // looks so much nicer.
    ConvertImagesToHighColor(TreeImages);

    ComportProducts := TConfigComportName.Create;
    ComportTermo := TConfigComportName.Create;

    ComportProducts.Value := 'COM24';

    FProperties := [RConfigData.CreateSection('Порты СОМ',
      [RConfigData.CreateProperty('Приборы', ComportProducts),
      RConfigData.CreateProperty('Термокамера', ComportTermo)])];

    VST3.RootNodeCount := Length(FProperties);

    

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
    if Node.Parent = Sender.RootNode then
        ChildCount := Length(FProperties[Node.Index].Properties)
    else
        ChildCount := 0;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

var
    section_index: Integer;
    p: PConfigData;

begin
    p := Sender.GetNodeData(Node);
    if ParentNode = nil then
    begin
        InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
        p.Value := nil;
        p.Properties := FProperties[Node.Index].Properties;
    end
    else
    begin
        p.Value := FProperties[Node.Parent.Index].Properties[Node.Index].Value;
        p.Properties := [];
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

begin
    if TextType <> ttNormal then
        exit;

    if (Node.Parent = Sender.RootNode) then
    begin
        if Column = 0 then
            CellText := FProperties[Node.Index].Name;
        exit;
    end;

    case Column of
        0:

            CellText := FProperties[Node.Parent.Index].Properties
              [Node.Index].Name;

        1:

            CellText := FProperties[Node.Parent.Index].Properties[Node.Index]
              .Value.AsString;

        2:

            CellText := FProperties[Node.Parent.Index].Properties[Node.Index]
              .Value.Error;

    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3GetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

begin

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: TImageIndex);

begin
    if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
    begin
        if Node.Parent = Sender.RootNode then
            Index := 1 // root nodes, this is an open folder
        else
        begin
            if Node.Parent.Parent = Sender.RootNode then
                Index := 2
            else
                Index := 3;
        end;
    end;
end;


// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
    with Sender do
    begin
        Allowed := (Node.Parent <> RootNode) and (Column = 1);
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
    with TargetCanvas do
    begin
        if TreeData[Node].HasError then
        begin
            Brush.Color := cl3dlight;
        end;

        FillRect(CellRect);

    end;
end;

procedure TFormProperties.VST3Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

begin
    with Sender do
    begin
        // Start immediate editing as soon as another node gets focused.
        if Assigned(Node) and (Node.Parent <> RootNode) and
          not(tsIncrementalSearching in TreeStates) then
        begin
            // We want to start editing the currently selected node. However it might well happen that this change event
            // here is caused by the node editor if another node is currently being edited. It causes trouble
            // to start a new edit operation if the last one is still in progress. So we post us a special message and
            // in the message handler we then can start editing the new node. This works because the posted message
            // is first executed *after* this event and the message, which triggered it is finished.
            PostMessage(Self.Handle, WM_STARTEDITING, WPARAM(Node), 0);
        end;
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3CreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);

// This is the callback of the tree control to ask for an application defined edit link. Providing one here allows
// us to control the editing process up to which actual control will be created.
// TPropertyEditLink implements an interface and hence benefits from reference counting. We don't need to keep a
// reference to free it. As soon as the tree finished editing the class will be destroyed automatically.
var
    x: TPropertyEditLink;
begin
    x := TPropertyEditLink.Create;
    EditLink := x;
end;

procedure TFormProperties.VST3DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
    R: TRect;
begin
    if (Column = 1) and (TreeData[Node].Value is TConfigBool) then
    begin
        R := CellRect;
        R.Left := R.Left - 9;
        R.Right := R.Right - 9;
        DrawCheckbox(Sender, TargetCanvas, R,
          (TreeData[Node].Value as TConfigBool).Value, '');
        DefaultDraw := false;
    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

begin
    // Make the root nodes underlined and draw changed nodes in bold style.
    if Node.Parent = Sender.RootNode then
        TargetCanvas.Font.Style := [fsUnderline];
    if TreeData[Node].HasError then
    begin
        TargetCanvas.Font.Color := clRed;
    end;

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3IncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);

var
    S, PropText: string;

begin
    S := SearchText;

    if Node.Parent = Sender.RootNode then
    begin
        // root nodes
        PropText := FProperties[Node.Index].Name;
    end
    else
        PropText := FProperties[Node.Parent.Index].Properties[Node.Index].Name;

    // By using StrLIComp we can specify a maximum length to compare. This allows us to find also nodes
    // which match only partially. Don't forget to specify the shorter string length as search length.
    Result := StrLIComp(PChar(S), PChar(PropText),
      Min(Length(S), Length(PropText)))
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.FormDeactivate(Sender: TObject);
begin
    Hide;
end;
// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.WMStartEditing(var Message: TMessage);

// This message was posted by ourselves from the node change handler above to decouple that change event and our
// intention to start editing a node. This is necessary to avoid interferences between nodes editors potentially created
// for an old edit action and the new one we start here.

var
    Node: PVirtualNode;

begin
    Node := Pointer(Message.WPARAM);
    // Note: the test whether a node can really be edited is done in the OnEditing event.
    VST3.EditNode(Node, 1);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TFormProperties.VST3FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    Data: PConfigData;
    I: Integer;
begin
    Data := Sender.GetNodeData(Node);
    Finalize(Data.Value);

    for I := 0 to Length(Data.Properties) - 1 do
        Data.Properties[I].Value.Free;
end;

function TFormProperties.GetNodeData(Node: PVirtualNode): PConfigData;
begin
    exit(VST3.GetNodeData(Node));
end;

end.
