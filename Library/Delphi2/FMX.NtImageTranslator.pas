unit FMX.NtImageTranslator;

interface

uses
  Classes, NtBaseTranslator;

type
  { @abstract Extension class that translates TPicture objects. }
  TNtImageTranslator = class(TNtTranslatorExtension)
  public
    { @seealso(TNtExtension.CanTranslate) }
    function CanTranslate(obj: TObject): Boolean; override;

    { @seealso(TNtExtension.Translate) }
    procedure Translate(
      component: TComponent;
      obj: TObject;
      const name: String;
      value: Variant;
      index: Integer); override;
  end;

implementation

uses
  SysUtils,
  Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.MultiResBitmap;

function TNtImageTranslator.CanTranslate(obj: TObject): Boolean;
begin
  Result := (obj is TBitmap) or (obj is TFixedBitmapItem);
end;

procedure TNtImageTranslator.Translate(
  component: TComponent;
  obj: TObject;
  const name: String;
  value: Variant;
  index: Integer);
var
  data: TBytes;
  stream: TMemoryStream;
  bitmap: TBitmap;
  vt: TVarType;
begin
  vt := VarType(value);

  if ((vt and varByte) <> 0) and ((vt and varArray) <> 0) then
  begin
    data := value;

    stream := TMemoryStream.Create;
    try
      stream.Write(data[0], Length(data));
      stream.Seek(0, TSeekOrigin.soBeginning);

      if obj is TBitmap then
      begin
        bitmap := TBitmap.CreateFromStream(stream);
        try
          TBitmap(obj).Assign(bitmap);
        finally
          bitmap.Free;
        end;
      end
      else if obj is TFixedBitmapItem then
        TFixedBitmapItem(obj).Bitmap.LoadFromStream(stream)
    finally
      stream.Free;
    end;
  end;
end;

initialization
  NtTranslatorExtensions.Register(TNtImageTranslator);
end.
