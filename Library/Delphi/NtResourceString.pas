unit NtResourceString;

interface

implementation

uses
  System.SyncObjs,
  NtResourceEx;

var
  cs: TCriticalSection;

function TranslateResourceString(resStringRec: PResStringRec): String;
var
  oldLoadResStringFunc: function (ResStringRec: PResStringRec): string;
begin
  cs.Acquire;
  try
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
  finally
    cs.Release;
  end;
end;

initialization
  // Enable resource string translation
  LoadResStringFunc := TranslateResourceString;

  cs := TCriticalSection.Create;
finalization
  cs.Free;
end.
