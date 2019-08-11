unit NtGraphic;

interface

uses
  Graphics;

type
  TNtGraphicResoure = class helper for TGraphic
    procedure LoadResource(
      resType, resName: PChar;
      instance: THandle = 0); overload;

    procedure LoadResource(
      const resType, resName: String;
      instance: THandle = 0); overload;
  end;

implementation

uses
  Classes,
  NtBase;

procedure TNtGraphicResoure.LoadResource(
  resType, resName: PChar;
  instance: THandle);
var
  stream: TResourceStream;
begin
  stream := TNtResources.GetResourceStream(resType, resName, instance);
  try
    Self.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TNtGraphicResoure.LoadResource(
  const resType, resName: String;
  instance: THandle);
begin
  LoadResource(PChar(resType), PChar(resName), instance);
end;

end.
