unit NtResourceString;

interface

implementation

uses
  NtResource;

function TranslateResourceString(resStringRec: PResStringRec): String;
var
  oldLoadResStringFunc: function (ResStringRec: PResStringRec): string;
begin
  Result := _T(resStringRec);

  if Result <> '' then
    Exit;

  oldLoadResStringFunc := LoadResStringFunc;
  try
    LoadResStringFunc := nil;
    Result := LoadResString(resStringRec);
  finally
    LoadResStringFunc := oldLoadResStringFunc;
  end;
end;

initialization
  // Enable resource string translation
  LoadResStringFunc := TranslateResourceString;
end.
