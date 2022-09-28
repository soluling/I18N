{
  @abstract Implements @link(TNtVirtualTreeViewTranslator) extension class that translates columns of TVirtualTreeView.

  TVirtualTreeView is an old control that was designed before Delphi properly supported Unicode.
  This is why the makers of TVirtualTreeView decided to stored Text and Hint properties
  of the column item in a defined custom property. @link(TNtTranslator) can not
  automatically translate defined properties because there is no direct mapping
  between form properties and runtime properties. This extension enables
  runtime language switch for columns of TVirtualTreeView.

  To enable runtime language switch of TVirtualTreeView just add this unit into
  your project or add unit into any uses block.

  @longCode(#
implementation

uses
  NtVirtualTreeTranslator;
#)

  See @italic(Samples\Delphi\VCL\VirtualTree) sample to see how to use the unit.
}
unit NtVirtualTreeTranslator;

interface

uses
  Classes, NtBaseTranslator;

type
  { @abstract Translator extension class that translates TVirtualTreeView component. }
  TNtVirtualTreeViewTranslator = class(TNtTranslatorExtension)
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

    { @seealso(TNtTranslatorExtension.GetActualObject) }
    function GetActualObject(obj: TObject; const propName: String): TObject; override;

    { @seealso(TNtTranslatorExtension.GetActualName) }
    function GetActualName(obj: TObject; const propName: String): String; override;
  end;

implementation

uses
  VirtualTrees;

function TNtVirtualTreeViewTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TVirtualTreeColumn;
end;

procedure TNtVirtualTreeViewTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
var
  item: TVirtualTreeColumn;
begin
  if obj is TVirtualTreeColumn then
  begin
    item := TVirtualTreeColumn(obj);

    if name = 'WideText' then
      item.Text := value
    else if name = 'WideHint' then
      item.Hint := value
  end
end;

function TNtVirtualTreeViewTranslator.GetActualObject(
  obj: TObject;
  const propName: String): TObject;
begin
  if (obj is TVirtualStringTree) and (propName = 'Columns') then
    Result := TVirtualStringTree(obj).Header
  else
    Result := nil;
end;

function TNtVirtualTreeViewTranslator.GetActualName(
  obj: TObject;
  const propName: String): String;
begin
  if (obj is TVirtualTreeColumn) and (propName = 'WideText') then
    Result := 'Text'
  else if (obj is TVirtualTreeColumn) and (propName = 'WideHint') then
    Result := 'Hint'
  else
    Result := '';
end;

initialization
  NtTranslatorExtensions.Register(TNtVirtualTreeViewTranslator);
end.
