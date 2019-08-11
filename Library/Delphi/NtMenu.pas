{
  @abstract Implements routines that help you internationalize menus.

  If you use runtime language switch the short cut text of menu items do
  not change. This is because VCL stores the initial resource string in an array
  and if you change the language the array still holds the resource strings in
  default language. This unit makes the array to change it value after a language switch.
  The unit does not expose any functions but you just add this into your project.

  @longCode(#
implementation

uses
  NtMenu;
#)

  See @italic(Samples\Delphi\VCL\Menu) sample to see how to use the unit.

  This unit can translste the shortcut only on 32-bit projects.
  On 64-bit platform it does nothing.
}

unit NtMenu;

{$I NtVer.inc}

interface

implementation

uses
  Windows, Classes, Consts, Menus, SysUtils, NtLocalization;

type
  TMenuKeyCap =
  (
    mkcBkSp,
    mkcTab,
    mkcEsc,
    mkcEnter,
    mkcSpace,
    mkcPgUp,
    mkcPgDn,
    mkcEnd,
    mkcHome,
    mkcLeft,
    mkcUp,
    mkcRight,
    mkcDown,
    mkcIns,
    mkcDel,
    mkcShift,
    mkcCtrl,
    mkcAlt
  );

function MenuKeyCaps(value: TMenuKeyCap): String;
begin
  case value of
    mkcBkSp: Result := SmkcBkSp;
    mkcTab: Result := SmkcTab;
    mkcEsc: Result := SmkcEsc;
    mkcEnter: Result := SmkcEnter;
    mkcSpace: Result := SmkcSpace;
    mkcPgUp: Result := SmkcPgUp;
    mkcPgDn: Result := SmkcPgDn;
    mkcEnd: Result := SmkcEnd;
    mkcHome: Result := SmkcHome;
    mkcLeft: Result := SmkcLeft;
    mkcUp: Result := SmkcUp;
    mkcRight: Result := SmkcRight;
    mkcDown: Result := SmkcDown;
    mkcIns: Result := SmkcIns;
    mkcDel: Result := SmkcDel;
    mkcShift: Result := SmkcShift;
    mkcCtrl: Result := SmkcCtrl;
    mkcAlt: Result := SmkcAlt;
  else
    Result := '';
  end;
end;

function GetSpecialName(ShortCut: TShortCut): String;
var
  scanCode: Integer;
  keyName: array[0..255] of Char;
begin
  Result := '';

  scanCode := MapVirtualKey(WordRec(shortCut).Lo, 0) shl 16;

  if scanCode <> 0 then
  begin
    GetKeyNameText(scanCode, keyName, SizeOf(keyName));
    Result := keyName;
  end;
end;

function ShortCutToText(shortCut: TShortCut): String;
var
  key: Byte;
  name: String;
begin  //FI:C101
  key := LoByte(Word(shortCut));

  case key of
    $08, $09:
      name := MenuKeyCaps(TMenuKeyCap(Ord(mkcBkSp) + key - $08));

    $0D:
      name := MenuKeyCaps(mkcEnter);

    $1B:
      name := MenuKeyCaps(mkcEsc);

    $20..$28:
      name := MenuKeyCaps(TMenuKeyCap(Ord(mkcSpace) + key - $20));

    $2D..$2E:
      name := MenuKeyCaps(TMenuKeyCap(Ord(mkcIns) + key - $2D));

    $30..$39:
      name := Chr(key - $30 + Ord('0'));

    $41..$5A:
      name := Chr(key - $41 + Ord('A'));

    $60..$69:
      name := Chr(key - $60 + Ord('0'));

    $70..$87:
      name := 'F' + IntToStr(key - $6F);
  else
    name := GetSpecialName(shortCut);
  end;

  if name <> '' then
  begin
    Result := '';

    if shortCut and scShift <> 0 then
      Result := Result + MenuKeyCaps(mkcShift);

    if shortCut and scCtrl <> 0 then
      Result := Result + MenuKeyCaps(mkcCtrl);

    if shortCut and scAlt <> 0 then
      Result := Result + MenuKeyCaps(mkcAlt);

    Result := Result + name;
  end
  else
    Result := '';
end;

function TextToShortCut(text: String): TShortCut;  //FI:O801

  function CompareFront(var text: String; const front: String): Boolean;
  begin
    Result := False;

    if (Length(text) >= Length(front)) and (AnsiStrLIComp(PChar(text), PChar(front), Length(front)) = 0) then
    begin
      Result := True;
      Delete(text, 1, Length(Front));
    end;
  end;

var
  key: TShortCut;
  shift: TShortCut;
begin
  Result := 0;
  shift := 0;

  while True do
  begin
    if CompareFront(Text, MenuKeyCaps(mkcShift)) then
      shift := shift or scShift
    else if CompareFront(Text, '^') then
      shift := shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps(mkcCtrl)) then
      shift := shift or scCtrl
    else if CompareFront(Text, MenuKeyCaps(mkcAlt)) then
      shift := shift or scAlt
    else
      Break;
  end;

  if text = '' then
    Exit;

  for key := $08 to $255 do
    if AnsiCompareText(text, ShortCutToText(key)) = 0 then
    begin
      Result := key or shift;
      Exit;
    end;
end;

var
  ShortCutToTextCode: TNtProcedureData;
  TextToShortCutCode: TNtProcedureData;
initialization
  TNtMap.RemapProcedure(@Menus.ShortCutToText, @ShortCutToText, ShortCutToTextCode);
  TNtMap.RemapProcedure(@Menus.TextToShortCut, @TextToShortCut, TextToShortCutCode);
end.
