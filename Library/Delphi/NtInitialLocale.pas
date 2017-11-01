{
  @abstract Implements routine that makes the default locale to match the locale of the user instead the language of the operating system.

  The default locale of a Delphi application depends on the Delphi version. Up to
  Delphi 2009 the default locale was locale set in the Regional Settings of
  Control Panel. Starting from Delphi 2010 this no longer the case. The default
  locale is the language of the operating system itself.

  If you want to have the old behaviour with Delphi 2010 or later use this unit.
  The unit does not expose any functions but you just add this into your project such
  way that the unit is called first. A suitable place is the first item in the uses
  clause of the program.

  @longCode(#
program Project1;

uses
  NtInitialLocale,
  Forms,
  Unit1 in 'Unit1.pas';
#)

  This makes sure that the initialization block of the unit is called before calling
  any other initialization block.

  See @italic(Samples\Delphi\VCL\LanguageSwitch) sample to see how to use the unit.
}
unit NtInitialLocale;

{$I NtVer.inc}

interface

implementation

{$IFDEF DELPHI2010}
uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  NtBase;

function DoesResourceFileExist(const fileName, locale: String): Boolean;

  function FileExists(const fileName: String): Boolean;
{$IFDEF MSWINDOWS}
  var
    handle: THandle;
  begin
    handle := CreateFile(
      PChar(fileName),
      GENERIC_READ,
      FILE_SHARE_READ,
      nil,
      OPEN_EXISTING,
      0,
      0);

    Result := handle <> INVALID_HANDLE_VALUE;

    if Result then
      CloseHandle(handle);
  end;
{$ENDIF}
{$IFDEF POSIX}
  begin
    Result := False;
  end;
{$ENDIF}

var
  resourceFileName: String;
begin
  resourceFileName := fileName;

  while (resourceFileName <> '') and (fileName[Length(resourceFileName)] <> '.') do
    Delete(resourceFileName, Length(resourceFileName), 1);

  Result := FileExists(resourceFileName + locale);
end;

{$IFDEF MSWINDOWS}
function GetString(lcType: LCTYPE): String;
var
  len: Integer;
begin
  SetLength(Result, 7);
  len := GetLocaleInfo(GetThreadLocale, lcType, @Result[1], Length(Result));
  Result := Copy(Result, 1, len);

  while (Result <> '') and (Result[Length(Result)] = #0) do
    Delete(Result, Length(Result), 1);
end;
{$ENDIF}

procedure SetLocale(const value: String);
begin
{$IFDEF DELPHIXE}
  {$IFDEF DELPHIDX2}
  TNtBase.LoadNew(value);
  {$ELSE}
  SetLocaleOverride(value);
  {$ENDIF}
{$ELSE}
  TNtBase.LoadNew(value);
{$ENDIF}
end;

var
  fileName: String;

function ProcessLocale(const value: String): Boolean;
begin
  if DoesResourceFileExist(fileName, value) or (value = DefaultLocale) then
  begin
    SetLocale(value);
    Result := True;
  end
  else
    Result := False;
end;

{$IFDEF MSWINDOWS}
var
  id, language, country: String;
{$ENDIF}
initialization
  fileName := ParamStr(0);

  if GetLocaleOverride(fileName) <> '' then
    Exit;

{$IFDEF MSWINDOWS}
  language := GetString(LOCALE_SISO639LANGNAME);
  country := GetString(LOCALE_SISO3166CTRYNAME);

  if ProcessLocale(language + '-' + country) then
    Exit;

  if ProcessLocale(language) then
    Exit;

  id := GetString(LOCALE_SABBREVLANGNAME);

  if ProcessLocale(id) then
    Exit;

  Delete(id, 3, 1);
  ProcessLocale(id);
{$ENDIF}
{$ENDIF}
end.
