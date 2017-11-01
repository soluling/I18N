{
  @abstract Implements @link(TNtListViewTranslator) translator extension class that translates TListView.

  To enabled runtime language switch of list views just add this unit into your project
  or add unit into any uses block.

  @longCode(#
implementation

uses
  NtListViewTranslator;
#)

  See @italic(Samples\Delphi\VCL\LanguageSwitch) sample to see how to use the unit.
}
unit NtListViewTranslator;

{$I NtVer.inc}

interface

uses
  Classes, NtBaseTranslator;

type
  { @abstract Translator extension class that translates TListView component. }
  TNtListViewTranslator = class(TNtTranslatorExtension)
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
  ComCtrls, NtBase;

type
  TItemInfo = packed record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    Data: Pointer;
  end;

  TItemInfo2 = packed record
    ImageIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    SubItemCount: Integer;
    GroupID: Integer;
    Data: Pointer;
  end;

function TNtListViewTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TListItems;
end;

procedure TNtListViewTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
const
  VERSION_32_2 = $03;  // 32-bit struct size version 2
  VERSION_32_3 = $05;  // 32-bit struct size version 3
var
  stream: TNtStream;
  items: TListItems;

  procedure ProcessOld;
  var
    i, j, count: Integer;
    item: TListItem;
    info: TItemInfo;
  begin
    stream.ReadInteger;
    count := stream.ReadInteger;

    for i := 0 to count - 1 do
    begin
      item := items[i];

      stream.Read(info, Sizeof(info));
      item.ImageIndex := info.ImageIndex;
      item.StateIndex := info.StateIndex;
      item.OverlayIndex := info.OverlayIndex;
      item.Data := info.Data;
{$IFDEF DELPHIXE}
      item.Caption := TNtConvert.BytesToUnicode(stream.ReadShortString);
{$ELSE}
      item.Caption := TNtConvert.AnsiToUnicode(stream.ReadShortString);
{$ENDIF}

      for j := 0 to info.SubItemCount - 1 do
{$IFDEF DELPHIXE}
        item.SubItems[j] := TNtConvert.BytesToUnicode(stream.ReadShortString);
{$ELSE}
        item.SubItems[j] := TNtConvert.AnsiToUnicode(stream.ReadShortString);
{$ENDIF}
    end;
  end;

{$IFDEF DELPHI2006}
  procedure ProcessNew;
  var
    item: TListItem;

    procedure ProcessItemInfo;
    var
      i: Integer;
      info: TItemInfo;
    begin
      stream.Read(info, Sizeof(info));
      item.ImageIndex := info.ImageIndex;
      item.StateIndex := info.StateIndex;
      item.OverlayIndex := info.OverlayIndex;
      item.Data := info.Data;
      item.Caption := stream.ReadShortUnicodeString;

      for i := 0 to info.SubItemCount - 1 do
        item.SubItems[i] := stream.ReadShortUnicodeString;
    end;

{$IFDEF UNICODE}
    procedure ProcessItemInfo2(version: Byte);
    var
      i: Integer;
      info: TItemInfo2;
    begin
      stream.Read(info, Sizeof(info));
      item.ImageIndex := info.ImageIndex;
      item.StateIndex := info.StateIndex;
      item.OverlayIndex := info.OverlayIndex;
      item.Data := info.Data;
      item.Caption := stream.ReadShortUnicodeString;

      for i := 0 to info.SubItemCount - 1 do
      begin
        item.SubItems[i] := stream.ReadShortUnicodeString;

        if version >= VERSION_32_3 then
          stream.ReadPointer;
      end;
    end;
  {$ENDIF}

  var
    i, count: Integer;
{$IFDEF UNICODE}
    version: Byte;
{$ENDIF}
  begin
    {$IFDEF UNICODE}version := {$ENDIF}stream.ReadByte;
    stream.ReadInteger;
    count := stream.ReadInteger;

    for i := 0 to count - 1 do
    begin
      item := items[i];

{$IFDEF UNICODE}
      if version >= VERSION_32_2 then
        ProcessItemInfo2(version)
      else
{$ENDIF}
        ProcessItemInfo;
    end;
  end;
{$ENDIF}

begin
  items := obj as TListItems;

{$IFDEF DELPHIXE}
  stream := TNtStream.Create(TNtConvert.RawByteStringToBytes(AnsiString(value)));
{$ELSE}
  stream := TNtStream.Create(AnsiString(value));
{$ENDIF}
  try
    if name = 'Items.Data' then
      ProcessOld
{$IFDEF DELPHI2006}
    else
      ProcessNew
{$ENDIF}
  finally
    stream.Free;
  end;
end;

initialization
  NtTranslatorExtensions.Register(TNtListViewTranslator);
end.
