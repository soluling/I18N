{
  @abstract Implements @link(TNtTreeViewTranslator) translator extension class that translates TTreeView.

  TTreeView component stores nodes as defined binary property into the form file.
  @link(TNtTranslator) can not automatically translate defined binary properties
  because it does not know the format that is used. This extension knows and it
  enables runtime language switch for nodes of TTreeView.

  To enable runtime language switch of tree views just add this unit into your project
  or add unit into any uses block.

  @longCode(#
implementation

uses
  NtTreeViewTranslator;
#)

  See @italic(Samples\Delphi\VCL\LanguageSwitch) sample to see how to use the unit.
}
unit NtTreeViewTranslator;

{$I NtVer.inc}

interface

uses
  Classes, NtBaseTranslator;

type
  { @abstract Translator extension class that translates TTreeView component. }
  TNtTreeViewTranslator = class(TNtTranslatorExtension)
  public
    { @seealso(TNtTranslatorExtension.CanTranslate) }
    function CanTranslate(obj: TObject): Boolean; override;

    { @seealso(TNtTranslatorExtension.Translate) }
    procedure Translate(
      component: TComponent;
      obj: TObject;
      const name: String;
      value: Variant;
      index: Integer); override;

    class procedure ForceUse;
  end;

implementation

uses
  SysUtils, ComCtrls, NtBase;

function TNtTreeViewTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TTreeNodes;
end;

procedure TNtTreeViewTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
const
  VERSION_32_2 = $03;
  VERSION_64_2 = $04;
  VERSION_32_3 = $05;
  VERSION_64_3 = $06;
  VERSION_32_4 = $07;
  VERSION_64_4 = $08;
var
  stream: TNtStream;

  // Child of the node, or nil if the node or the child does not exist. A translation
  // can contain more nodes than the tree, for example if nodes have been deleted at runtime.
  // The data of a missing node is still read so that the following nodes stay in sync.
  function GetChild(node: TTreeNode; index: Integer): TTreeNode;
  begin
    if (node <> nil) and (index < node.Count) then
      Result := node[index]
    else
      Result := nil;
  end;

  procedure ProcessOld(node: TTreeNode);
  var
    i, size: Integer;
    info: TNodeInfo;
  begin
    size := stream.ReadInteger;
    stream.Read(info, size);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.Text := TNtConvert.AnsiToUnicode(info.Text);
    end;

    for i := 0 to info.Count - 1 do
      ProcessOld(GetChild(node, i));
  end;

{$IFDEF DELPHI2006}
  procedure Process(node: TTreeNode);
  var
    i: Integer;
    info: TNodeDataInfo;
    str: UnicodeString;
  begin
    stream.ReadInteger; // size
    stream.Read(info, SizeOf(info));

    SetLength(str, info.TextLen);

    if info.TextLen > 0 then
      stream.Read(str[1], 2*info.TextLen);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.Text := str;
    end;

    for i := 0 to info.Count - 1 do
      Process(GetChild(node, i));
  end;
{$ENDIF}

{$IFDEF UNICODE}
  procedure Process2(node: TTreeNode);
  var
    i: Integer;
    info: {$IFDEF DELPHIXE2}TNodeDataInfo2x86{$ELSE}TNodeDataInfo2{$ENDIF};
    str: UnicodeString;
  begin
    stream.ReadInteger;
    stream.Read(info, SizeOf(info));

    SetLength(str, info.TextLen);

    if info.TextLen > 0 then
      stream.Read(str[1], 2*info.TextLen);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.Text := str;
    end;

    for i := 0 to info.Count - 1 do
      Process2(GetChild(node, i));
  end;
{$ENDIF}

{$IFDEF DELPHIXE2}
  procedure Process264(node: TTreeNode);
  var
    i: Integer;
    info: TNodeDataInfo2x64;
    str: UnicodeString;
  begin
    stream.ReadInteger;
    stream.Read(info, SizeOf(info));

    SetLength(str, info.TextLen);

    if info.TextLen > 0 then
      stream.Read(str[1], 2*info.TextLen);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.Text := str;
    end;

    for i := 0 to info.Count - 1 do
      Process264(GetChild(node, i));
  end;
{$ENDIF}

{$IFDEF DELPHI12}
  procedure ReadNodeClass(Stream: TStream);
  var
    LSize: Byte;
    LInd: Byte;
    LName: string;
  begin
    Stream.ReadBuffer(LSize, SizeOf(LSize));

    if LSize = 0 then
    begin
      Stream.ReadBuffer(LInd, SizeOf(LInd));
    end
    else
    begin
      SetLength(LName, LSize);
      Stream.ReadBuffer(LName[1], LSize * SizeOf(WideChar));
    end;
  end;

  procedure Process332(node: TTreeNode; version: Integer);
  var
    i: Integer;
    info: TNodeDataInfo3x86;
    str: UnicodeString;
  begin
    if version = VERSION_32_4 then
      ReadNodeClass(stream);

    stream.ReadInteger;
    stream.Read(info, SizeOf(info));

    SetLength(str, info.TextLen);

    if info.TextLen > 0 then
      stream.Read(str[1], 2*info.TextLen);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.CheckState := info.CheckState;
      node.Text := str;
    end;

    for i := 0 to info.Count - 1 do
      Process332(GetChild(node, i), version);
  end;

  procedure Process364(node: TTreeNode; version: Integer);
  var
    i: Integer;
    info: TNodeDataInfo3x64;
    str: UnicodeString;
  begin
    if version = VERSION_64_4 then
      ReadNodeClass(stream);

    stream.ReadInteger;
    stream.Read(info, SizeOf(info));

    SetLength(str, info.TextLen);

    if info.TextLen > 0 then
      stream.Read(str[1], 2*info.TextLen);

    if node <> nil then
    begin
      node.ImageIndex := info.ImageIndex;
      node.SelectedIndex := info.SelectedIndex;
      node.OverlayIndex := info.OverlayIndex;
      node.CheckState := info.CheckState;
      node.Text := str;
    end;

    for i := 0 to info.Count - 1 do
      Process364(GetChild(node, i), version);
  end;
{$ENDIF}

var
  i, count: Integer;
  node: TTreeNode;
  nodes: TTreeNodes;
{$IFDEF UNICODE}
  version: Byte;
{$ENDIF}
begin //FI:C101
  nodes := obj as TTreeNodes;

{$IFDEF DELPHIXE}
  stream := TNtStream.Create(TBytes(value));
{$ELSE}
  stream := TNtStream.Create(AnsiString(value));
{$ENDIF}
  try
    if name = 'Items.Data' then
    begin
      count := stream.ReadInteger;
      node := nodes.GetFirstNode;

      for i := 0 to count - 1 do  //FI:W528
      begin
        ProcessOld(node);
        if node <> nil then
          node := node.GetNextSibling;
      end;
    end
{$IFDEF DELPHI2006}
    else
    begin
      {$IFDEF UNICODE}version := {$ENDIF}stream.ReadByte;
      count := stream.ReadInteger;
      node := nodes.GetFirstNode;

      for i := 0 to count - 1 do  //FI:W528
      begin
  {$IFDEF DELPHI12}
        if version >= VERSION_64_4 then
          Process364(node, version)
        else if version = VERSION_32_4 then
          Process332(node, version)
        else if version >= VERSION_64_3 then
          Process364(node, version)
        else if version = VERSION_32_3 then
          Process332(node, version)
        else
  {$ENDIF}
  {$IFDEF DELPHIXE2}
        if version >= VERSION_64_2 then
          Process264(node)
        else
  {$ENDIF}
  {$IFDEF UNICODE}
        if version = VERSION_32_2 then
          Process2(node)
        else
  {$ENDIF}
          Process(node);

        if node <> nil then
          node := node.GetNextSibling;
      end;
    end;
{$ENDIF}
  finally
    stream.Free;
  end;
end;

class procedure TNtTreeViewTranslator.ForceUse;
begin //FI:W519
end;

initialization
  NtTranslatorExtensions.Register(TNtTreeViewTranslator);
end.
