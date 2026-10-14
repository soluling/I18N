unit NtResourceString;

{ Enables resource string translation. Add this unit to the uses clause of the application. }

interface

implementation

uses
  NtResourceEx;

initialization
  // Enable resource string translation
  LoadResStringFunc := TranslateResourceString;
end.
