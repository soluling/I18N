unit NtResourceEx;

{$I NtVer.inc}

interface

{$IFDEF DELPHIDX4}
function _T(
  resStringRec: PResStringRec;
  const language: String = ''): String; overload;
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

implementation

uses
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

end.
