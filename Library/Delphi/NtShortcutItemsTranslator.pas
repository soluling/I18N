{
  @abstract Implements @link(TNtShortcutItemsTranslator) translator extension class that translates TdxShortcusItems from DevExpress

  To enable runtime language switch of shortcut list just add this unit into your project
  or add unit into any uses block.

  @longCode(#
implementation

uses
  NtShortcutItemsTranslator;
#)

  See @italic(Samples\Delphi\VCL\3rdParty\ShortcutBar) sample to see how to use the unit.
}
unit NtShortcutItemsTranslator;

{$I NtVer.inc}

interface

uses
  SysUtils, Classes, NtBaseTranslator;

type
  { @abstract Translator extension class that translates TdxShortcusItems component. }
  TNtShortcutItemsTranslator = class(TNtTranslatorExtension)
  private
    FVersion: String;

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
  Variants, ShortcutList, NtBase;

function TNtShortcutItemsTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TdxShortcutItems;
end;

procedure TNtShortcutItemsTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
var
  i, itemIndex, propertyCount: Integer;
  items: TdxShortcutItems;
begin
  // We will process only the string types
  if not TNtBaseTranslator.IsString(Vartype(value)) then
    Exit;

  if index = 0 then
    FVersion := value;

  if FVersion = '1.7' then
    propertyCount := 4
  else if FVersion = '1.13' then
    propertyCount := 3
  else
    propertyCount := 2;

  // Skip the version and item count
  i := index - 2;

  // There are four values for each item. The Caption value is the first.
  if (i mod propertyCount = 0) then
  begin
    items := obj as TdxShortcutItems;
    itemIndex := i div propertyCount;

    if itemIndex < items.Count then
      items[itemIndex].Caption := value;
  end;
end;

initialization
  NtTranslatorExtensions.Register(TNtShortcutItemsTranslator);
end.
