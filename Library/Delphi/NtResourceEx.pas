unit NtResourceEx;

{$I NtVer.inc}

interface

{$IFDEF DELPHIDX4}
function _T(
  resStringRec: PResStringRec;
  const language: String = ''): String; overload;

{ Translate a resource string. If the resource string has no translation the
  original string is returned. NtResourceString.pas sets this as LoadResStringFunc.
  @param resStringRec Resource string.
  @return Translated or original string. }
function TranslateResourceString(resStringRec: PResStringRec): String;
{$ENDIF}

{ Get the string value in the current language or in the specific language.
  @param original Original string value.
  @param id       Optional id value. If not present the original value is used.
  @param group    String group.
  @param language Optional language id. If not present the current language is used.
  @return String value in the current languages.}
function _T(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String; overload;

{ Get the string value in the current language or in the specific language.
  @param original Original string value.
  @param group    String group.
  @param language Optional language id. If not present the current language is used.
  @return String value in the current languages.}
function _TG(
  const original: String;
  const group: String;
  const language: String = ''): String;

implementation

uses
{$IFDEF DELPHIDX4}
  System.SyncObjs,
{$ENDIF}
  NtResource;

{$IFDEF DELPHIDX4}
function _T(
  resStringRec: PResStringRec;
  const language: String = ''): String;
begin
  if language <> '' then
    Result := NtResources.GetStringInLanguage(language, resStringRec)
  else
    Result := NtResources.GetString(resStringRec);
end;

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
{$ENDIF}

function _T(
  const original: String;
  const id: String = '';
  const group: String = '';
  const language: String = ''): String;
begin
  if language <> '' then
    Result := NtResources.GetStringInLanguage(language, original, id, group)
  else
    Result := NtResources.GetString(original, id, group);
end;

function _TG(
  const original: String;
  const group: String;
  const language: String = ''): String;
begin
  if language <> '' then
    Result := NtResources.GetStringInLanguage(language, original, '', group)
  else
    Result := NtResources.GetString(original, '', group);
end;

{$IFDEF DELPHIDX4}
initialization
  cs := TCriticalSection.Create;
finalization
  cs.Free;
{$ENDIF}
end.
