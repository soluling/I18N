{
  @abstract Implements @link(TNtTreeViewTranslator) translator extension class that translates TTreeView.

  TTreeView component stores nodes as defined binary property into the form file.
  @link(TNtTranslator) can not automatically translate defined binary properties
  because it does not know the format that is used. This extension knows and it
  enables runtime language switch for nodes of TTreeView.

  To enabled runtime language switch of tree views just add this unit into your project
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
var
  stream: TNtStream;

  procedure ProcessOld(node: TTreeNode);
  var
    i, size: Integer;
    info: TNodeInfo;
  begin
    size := stream.ReadInteger;
    stream.Read(info, size);

    node.ImageIndex := info.ImageIndex;
    node.SelectedIndex := info.SelectedIndex;
    node.OverlayIndex := info.OverlayIndex;
    node.Text := TNtConvert.AnsiToUnicode(info.Text);

    for i := 0 to info.Count - 1 do
      ProcessOld(node[i]);
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

    node.ImageIndex := info.ImageIndex;
    node.SelectedIndex := info.SelectedIndex;
    node.OverlayIndex := info.OverlayIndex;
    node.Text := str;

    for i := 0 to info.Count - 1 do
      Process(node[i]);
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

    node.ImageIndex := info.ImageIndex;
    node.SelectedIndex := info.SelectedIndex;
    node.OverlayIndex := info.OverlayIndex;
    node.Text := str;

    for i := 0 to info.Count - 1 do
      Process2(node[i]);
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

    node.ImageIndex := info.ImageIndex;
    node.SelectedIndex := info.SelectedIndex;
    node.OverlayIndex := info.OverlayIndex;
    node.Text := str;

    for i := 0 to info.Count - 1 do
      Process2(node[i]);
  end;
{$ENDIF}

const
  VERSION_32_2 = $03;
  VERSION_64_2 = $04;
var
  i, count: Integer;
  node: TTreeNode;
  nodes: TTreeNodes;
{$IFDEF UNICODE}
  version: Byte;
{$ENDIF}
begin
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
  {$IFDEF UNICODE}
    {$IFDEF DELPHIXE2}
        if version >= VERSION_64_2 then
          Process264(node)
        else
    {$ENDIF}
        if version = VERSION_32_2 then
          Process2(node)
        else
  {$ENDIF}
          Process(node);

        node := node.GetNextSibling;
      end;
    end;
{$ENDIF}
  finally
    stream.Free;
  end;
end;

initialization
  NtTranslatorExtensions.Register(TNtTreeViewTranslator);
end.
