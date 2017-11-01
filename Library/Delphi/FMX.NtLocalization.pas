{
  @abstract Contains localization routines.
}
unit FMX.NtLocalization;

{$I NtVer.inc}

interface

uses
  System.Classes,
  NtBase;

type
  TNtLocale = class
  public
    class function IsPreviousLocaleBidi: Boolean; static;
    class function ExtensionToLocale(const value: String): Integer; static;
    class function GetDefaultLanguage: String; static;
  end;

implementation

uses
  SysUtils,
{$IFDEF MACOS}
  {$IFDEF IOS}
  iOSapi.Foundation,
  {$ELSE}
  Macapi.Foundation,
  {$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  FMX.Platform,
  FMX.NtTranslator;


// TNtLocale

class function TNtLocale.IsPreviousLocaleBidi: Boolean;
begin
  Result := False;
end;

{$IFDEF MSWINDOWS}
var
  enumLocale: Integer;
  enumExtension: String;

function LocaleEnumProc(localeString: PChar): Integer; stdcall;
var
  locale: Integer;
  str, extension1:String;
{$IFDEF DELPHI2010}
  extension2: String;
{$ENDIF}
begin
  str := localeString;
  locale := StrToInt('$' + str);

  extension1 := GetLocaleStr(locale, LOCALE_SABBREVLANGNAME, '');
{$IFDEF DELPHI2010}
  extension2 := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '') + '-' + GetLocaleStr(locale, LOCALE_SISO3166CTRYNAME, '');
{$ENDIF}

  if SameText(extension1, enumExtension) {$IFDEF DELPHI2010}or SameText(extension2, enumExtension){$ENDIF} then
  begin
    enumLocale := locale;
    Result := 0;
  end
  else
    Result := 1;
end;

function LanguageEnumProc(localeString: PChar): Integer; stdcall;
var
  locale: Integer;
  str, extension1:String;
{$IFDEF DELPHI2010}
  extension2: String;
{$ENDIF}
begin
  str := localeString;
  locale := StrToInt('$' + str);

  extension1 := GetLocaleStr(locale, LOCALE_SABBREVLANGNAME, '');
  Delete(extension1, Length(extension1), 1);

{$IFDEF DELPHI2010}
  extension2 := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '');
{$ENDIF}

  if SameText(extension1, enumExtension) {$IFDEF DELPHI2010}or SameText(extension2, enumExtension){$ENDIF} then
  begin
    enumLocale := TNtBase.LocaleToPrimary(locale);
    Result := 0;
  end
  else
    Result := 1;
end;
{$ENDIF}

class function TNtLocale.ExtensionToLocale(const value: String): Integer;
begin
  Result := 0;

  if value = '' then
    Exit;

{$IFDEF MSWINDOWS}
  enumExtension := value;

  if enumExtension[1] = '.' then
    Delete(enumExtension, 1, 1);

  enumLocale := 0;
  EnumSystemLocales(@LocaleEnumProc, LCID_SUPPORTED);

  if enumLocale = 0 then
    EnumSystemLocales(@LanguageEnumProc, LCID_SUPPORTED);

  Result := enumLocale;
{$ENDIF}
end;

procedure SetInitialResourceLocale;

  function CheckLocale(fileName: String; locale: String; tryPrimary: Boolean): String;
  var
    ext: String;
  begin
    ext := '.' + locale;

    if FileExists(ChangeFileExt(fileName, ext)) then
      Result := locale
    else if (Length(locale) > 2) or tryPrimary then
    begin
      Delete(ext, Length(ext), 1);

      if FileExists(ChangeFileExt(fileName, ext)) then
      begin
        Result := locale;
        Delete(Result, Length(Result), 1);
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

  function CheckLocales(fileName: String; locales: String): String;
  var
    p, lang: PChar;
  begin
    p := PChar(locales);

    while p^ <> #0 do
    begin
      lang := p;

      while (p^ <> ',') and (p^ <> #0) do
        Inc(p);

      if p^ = ',' then
      begin
        p^ := #0;
        Inc(p);
      end;

      Result := CheckLocale(fileName, lang, False);

      if Result <> '' then
      begin
        Result := UpperCase(Result);
        Exit;
      end;
    end;
  end;

var
  buffer: array[0..260] of Char;
begin
  PreviouslyLoadedResourceLocale := '';

  if (LoadedResourceLocale = '') and
    (GetModuleFileName(LibModuleList.Instance, buffer, SizeOf(buffer)) > 0) then
  begin
    LoadedResourceLocale := CheckLocale(buffer, TNtLocaleRegistry.GetCurrentDefaultLocale, False);

    if LoadedResourceLocale <> '' then
      Exit;

    LoadedResourceLocale := CheckLocales(buffer, GetUILanguages(TNtBase.GetUserLanguage));

    if LoadedResourceLocale <> '' then
      Exit;

{$IFDEF MSWINDOWS}
    if (GetVersion and $000000FF) < 6 then
      LoadedResourceLocale := CheckLocales(buffer, GetUILanguages(TNtBase.GetSystemLanguage));
{$ENDIF}

    if LoadedResourceLocale <> '' then
      Exit;

    LoadedResourceLocale := CheckLocale(buffer, TNtBase.LocaleToIsoCode(TNtBase.GetUserLanguage), True);
  end;
end;

class function TNtLocale.GetDefaultLanguage: String;

{$IFDEF MACOS}
  function GetMac: String;
  var
    languages: NSArray;
  begin
    languages := TNSLocale.OCClass.preferredLanguages;

    if languages.count > 0 then
      Result := String(TNSString.Wrap(languages.objectAtIndex(0)).UTF8String)
    else
      Result := '';
  end;
{$ENDIF}

{$IFDEF ANDROID}
  function GetAndroid: String;
  var
    localeServ: IFMXLocaleService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(localeServ)) then
      Result := localeServ.GetCurrentLangID
    else
      Result := '';
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  function GetWindows: String;
  begin
    Result := GetLocaleStr(GetUserDefaultLCID, LOCALE_SISO639LANGNAME, 'en');
  end;
{$ENDIF}

begin
{$IFDEF MACOS}
  Result := GetMac;
{$ENDIF}
{$IFDEF ANDROID}
  Result := GetAndroid;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := GetWindows;
{$ENDIF}
end;

initialization
  SystemLanguage := TNtLocale.GetDefaultLanguage;
  DefaultLocale := SystemLanguage;

  SetInitialResourceLocale;
end.

