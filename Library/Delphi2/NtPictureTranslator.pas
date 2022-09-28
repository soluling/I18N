{
  @abstract Implements @link(TNtPictureTranslator) translator extension class that translates
  TPicture (e.g. TImage.Picture).

  To enable runtime language switch of images just add this unit into your project
  or add unit into any uses block.

  @longCode(#
implementation

uses
  NtPictureTranslator;
#)

  See @italic(Samples\Delphi\VCL\LanguageSwitch) sample to see how to use the unit.
}
unit NtPictureTranslator;

{$I NtVer.inc}

interface

uses
  Classes, NtBaseTranslator;

type
  { @abstract Translator extension class that translates TPicture objects. }
  TNtPictureTranslator = class(TNtTranslatorExtension)
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
  SysUtils, Variants, Graphics;

function TNtPictureTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := obj is TPicture;
end;

procedure TNtPictureTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
var
  stream: TMemoryStream;

  procedure ProcessSize;
  var
    size: Longint;
  begin
    stream.Read(size, SizeOf(size));
  end;

var
  nameLen: Byte;
  pictureClassName: String;
  ansi: AnsiString;
{$IFDEF DELPHIXE}
  data: TBytes;
{$ELSE}
  data: AnsiString;
{$ENDIF}
  picture: TPicture;
  currentGraphic, newGraphic: TGraphic;
begin
  if (VarType(value) <> (varArray or varByte)) and (VarType(value) <> (varString)) then
    Exit;

  picture := obj as TPicture;
  currentGraphic := picture.Graphic;
{$IFDEF DELPHIXE}
  data := value;
{$ELSE}
  data := AnsiString(value);
{$ENDIF}

  stream := nil;
  newGraphic := nil;
  try
    stream := TMemoryStream.Create;
{$IFDEF DELPHIXE}
    stream.Write(data[0], Length(data));
{$ELSE}
    stream.Write(data[1], Length(data));
{$ENDIF}
    stream.Seek(0, soFromBeginning);

    stream.Read(nameLen, 1);
    SetLength(ansi, nameLen);
    stream.Read(ansi[1], nameLen);
    pictureClassName := String(ansi);

    if SameText(currentGraphic.ClassName, pictureClassName) then
    begin
      newGraphic := TGraphicClass(currentGraphic.ClassType).Create;

      if SameText(pictureClassName, 'TBitmap') or SameText(pictureClassName, 'TJpegImage') then
        ProcessSize;

      newGraphic.LoadFromStream(stream);
      picture.Graphic := newGraphic;
    end;
  finally
    newGraphic.Free;
    stream.Free;
  end;
end;

initialization
  NtTranslatorExtensions.Register(TNtPictureTranslator);
end.
