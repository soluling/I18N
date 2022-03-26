{
  @abstract Implements @link(NtWindows) that contains some help functions.
}
unit NtWindows;

{$I NtVer.inc}

interface

uses
  NtBase;

type
  { @abstract Helper class that contains version, file and locale functions. }
  TNtWindows = class
  public
    { Checks if the operating system is at least Windows NT.
      @return @true if the operating system is at least Windows NT. }
    class function IsNt: Boolean;

    { Checks if the operating system is at least Windows Vista.
      @return @true if the operating system is at least Windows Vista. }
    class function IsVista: Boolean;

    { Checks if the operating system is at least Windows 7.
      @return @true if the operating system is at least Windows 7. }
    class function Is7: Boolean;

    { Checks if the operating system is at least Windows 8.
      @return @true if the operating system is at least Windows 8. }
    class function Is8: Boolean;

    { Checks if the operating system is at least Windows 10.
      @return @true if the operating system is at least Windows 10. }
    class function Is10: Boolean;

    { Checks if the file name is a sub sub path (e.g. Files\Sample.exe)
      @param fileName  File name to be checked.
      @return @true if the file name is a sub path. }
    class function IsSubPath(const fileName: String): Boolean;

    { Converts locale code (e.g. en-US) to a Windows locale id (e.g. 1033).
      @param code  ISO locale id.
      @return Windows locale id. }
    class function CodeToId(const code: String): Integer;

    { Gets a locale string.
      @param id          ISO locale id.
      @param locale      Windows locale id.
      @param localeType  Type of locale string to retrieve.
      @param default     Default value to be used if no string was found.
      @return Locale string.}
    class function GetLocaleStr(
      const id: String;
      locale, localeType: Integer;
      const default: string): String;

    { Gets a display name of a language.
      @param id            ISO locale id.
      @param locale        Windows locale id.
      @param languageName  Language name type.
      @return Display name. }
    class function GetDisplayName(
      const id: String;
      locale: Integer;
      languageName: TNtLanguageName): String;

    { Gets a list of available languages.
      @param language        Language list.
      @param exeFileName     Application filename.
      @param compatibleOnly  Include only compatible languages. Not used with Unicode enabled Delphi (Delphi 2009 or later).
      @param checkVersions   If true only resource DLLs where version matches the EXE are included.
      @param dir             If not empty specifies the directory where resource DLLs are.
      @return Language cout. }
    class function GetAvailable(
      languages: TNtLanguages;
      exeFileName: String;
      compatibleOnly: Boolean;
      checkVersions: Boolean;
      const dir: String = ''): Integer;

    { Gets a display name of a variant.
      @param language      Language id.
      @param language      Variant id.
      @return Display name of the variant. }
    class function GetVariantName(const language, variant: String): String;
  end;

implementation

uses
  Windows,
  SysUtils,
  Classes,
{$IFDEF DELPHIXE}
  NtResource,
{$ENDIF}
  NtLocalization;

const
  LOCALE_SLOCALIZEDDISPLAYNAME = $00000002;
  //LOCALE_SLOCALIZEDLANGUAGENAME = $0000006f;
  //LOCALE_SLOCALIZEDCOUNTRYNAME = $00000006;

  LOCALE_SNATIVEDISPLAYNAME = $00000073;

  LOCALE_SENGLISHDISPLAYNAME = $00000072;

type
  TEnumSystemLocalesEx = function(
    lpLocaleEnumProc: TFarProc;
    dwFlags: DWord;
    lParam: LParam;
    lpReserver: Pointer): BOOL; stdcall;

  TLocaleNameToLCID = function(
    lpName: PChar;
    dwFlags: DWord): LCID; stdcall;

  TGetLocaleInfoEx = function(
    lpLocaleName: PChar;
    LCType: LCTYPE;
    lpLCData: PChar;
    cchData: Integer): Integer; stdcall;

resourcestring
  STraditionalSort = 'traditional sort';
  SInternationalSort = 'international sort';

var
  enumSystemLocalesEx: TEnumSystemLocalesEx;
  localeNameToLCID: TLocaleNameToLCID;
  getLocaleInfoEx: TGetLocaleInfoEx;
  FVersionInfo: TOSVersionInfo;

class function TNtWindows.IsNt: Boolean;
begin
  Result := FVersionInfo.dwPlatformId = 2;
end;

class function TNtWindows.IsVista: Boolean;
begin
  Result := FVersionInfo.dwMajorVersion >= 6;
end;

class function TNtWindows.Is7: Boolean;
begin
  Result :=
    (FVersionInfo.dwMajorVersion > 6) or
    ((FVersionInfo.dwMajorVersion = 6) and (FVersionInfo.dwMinorVersion >= 1));
end;

class function TNtWindows.Is8: Boolean;
begin
  Result :=
    (FVersionInfo.dwMajorVersion > 6) or
    ((FVersionInfo.dwMajorVersion = 6) and (FVersionInfo.dwMinorVersion >= 2));
end;

class function TNtWindows.Is10: Boolean;
begin
  Result := FVersionInfo.dwMajorVersion >= 10;
end;

class function TNtWindows.IsSubPath(const fileName: String): Boolean;
begin
  if (fileName = '') or (fileName[1] = '\') or
    ((Length(fileName) >= 2) and (fileName[2] = ':')) then
  begin
    Result := False
  end
  else
    Result := True;
end;

class function TNtWindows.CodeToId(const code: String): Integer;
begin
  Result := localeNameToLCID(PChar(code), 0);

  if Pos('-', code) = 0 then
    Result := TNtBase.LocaleToPrimary(Result);
end;

class function TNtWindows.GetLocaleStr(
  const id: String;
  locale, localeType: Integer;
  const default: string): String;
var
  l: Integer;
  buffer: array[0..255] of Char;
begin
  if IsVista then
  begin
    l := getLocaleInfoEx(PChar(id), localeType, buffer, SizeOf(buffer));

    if l > 0 then
      SetString(Result, buffer, l - 1)
    else
      Result := default;
  end
  else
    Result := SysUtils.GetLocaleStr(locale, localeType, default);
end;

class function TNtWindows.GetDisplayName(
  const id: String;
  locale: Integer;
  languageName: TNtLanguageName): String;

  function LoadResString(id: Integer): String;
  var
    buffer: array[0..4095] of Char;
  begin
    Result := '';

    SetString(
      Result,
      buffer,
      LoadString(TNtBase.GetResourceInstance, id, buffer, Length(buffer)))
  end;

  function ProcessNew(localeType: Integer): String; overload;
  begin
    Result := GetLocaleStr(id, locale, localeType, '');
  end;

  function ProcessLegacy(languageLocaleType, countryLocaleType: Integer): String; overload;
  var
    str: String;
  begin
    Result := GetLocaleStr(id, locale, languageLocaleType, '');

    if TNtBase.LocaleToSub(locale) <> SUBLANG_NEUTRAL then
    begin
      str := GetLocaleStr(id, locale, countryLocaleType, '');

      if str <> '' then
      begin
        if locale = $040A then
        begin
          if languageName = lnNative then
            str := str + ', Alfabetización tradicional'
          else
            str := str + ', ' + STraditionalSort;
        end
        else if locale = $0C0A then
        begin
          if languageName = lnNative then
            str := str + ', Alfabetización internacional'
          else
            str := str + ', ' + SInternationalSort;
        end;

        Result := Result + ' (' + str + ')';
      end;
    end;
  end;

  function GetEnglish: String;
  begin
    if Is7 then
      Result := ProcessNew(LOCALE_SENGLISHDISPLAYNAME)
    else
      Result := ProcessLegacy(LOCALE_SENGLANGUAGE, LOCALE_SENGCOUNTRY);
  end;

  function GetNative: String;
  begin
    if Is7 then
      Result := ProcessNew(LOCALE_SNATIVEDISPLAYNAME)
    else
      Result := ProcessLegacy(LOCALE_SNATIVELANGNAME, LOCALE_SNATIVECTRYNAME);
  end;

  function GetLocalized: String;
  begin
    Result := LoadResString(locale);

    if Result = '' then
      Result := GetEnglish;
  end;

begin
  case languageName of
    lnNative: Result := GetNative;
    lnLocalized: Result := GetLocalized;
    lnBoth: Result := TNtLanguage.GetBoth(GetNative, GetLocalized);
    lnEnglish: Result := GetEnglish;

    lnSystem:
      if IsVista then
        Result := ProcessNew(LOCALE_SLOCALIZEDDISPLAYNAME)
      else
        Result := ProcessLegacy(LOCALE_SLANGUAGE, LOCALE_SCOUNTRY);
  end;
end;

var
  enumLanguages: TNtLanguages;
  enumExeFileName: String;
  enumCompatibleOnly: Boolean;
  enumCheckVersions: Boolean;

{$IFDEF DELPHI2009}
function CheckStamp(
  stream: TStream;
  const stamp: TBytes;
  start: Integer): Boolean; overload;

  function GetByte: Byte;
  begin
    stream.Read(Result, Sizeof(Result));
  end;

var
  i: Integer;
  position: Integer;
begin
  Result := True;
  position := stream.Position;

  try
    for i := 0 to start - 1 do
      GetByte;

    for i := 0 to Length(stamp) - 1 do
      if GetByte <> stamp[i] then
        Exit(False);
  finally
    stream.Position := position;
  end;
end;

function CheckStamp(
  const fileName: String;
  const stamp: TBytes;
  start: Integer = 0): Boolean; overload;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CheckStamp(stream, stamp, start);
  finally
    stream.Free;
  end;
end;

function IsPeFile(const fileName: String): Boolean;
const
  IMAGE_DOS_SIGNATURE_C = #$4D#$5A; // MZ
begin
  Result := CheckStamp(fileName, BytesOf(IMAGE_DOS_SIGNATURE_C));
end;
{$ENDIF}

function EnumLocalesEx(localeStr: PChar; flags: DWord; param: LParam): Integer; stdcall;  //FI:O804
var
  code: String;
  thisFileName: String;
begin
  code := localeStr;
  thisFileName := ChangeFileExt(enumExeFileName, '.' + code);

  if FileExists(thisFileName) {$IFDEF DELPHI2009}and IsPeFile(thisFileName){$ENDIF} then
  begin
    enumLanguages.Add(code, TNtWindows.CodeToId(code));
  end;

  Result := 1;
end;

function EnumLocales(localeStr: PAnsiChar): Integer; stdcall;

  procedure Process(
    const code: String;
    id: Integer;
    checkIfExists: Boolean);
  var
    i: Integer;
    ext, fileName, resourceFileName: String;
  begin
    ext := '.' + code;
    fileName := ChangeFileExt(enumExeFileName, ext);

    if FileExists(fileName) and
      (not enumCompatibleOnly or TNtBase.IsLocaleCompatible(id)) and
      (not enumCheckVersions or TNtResource.DoesLocaleVersionMatchFile(fileName)) then
    begin
      if checkIfExists then
        for i := 0 to enumLanguages.Count - 1 do
          if enumLanguages[i].Code = code then
            Exit;

      enumLanguages.Add(code, id, fileName);
      Exit;
    end;

    if ResourceDllDir <> '' then
    begin
      resourceFileName := ChangeFileExt(ExtractFileName(enumExeFileName), ext);
      fileName := ResourceDllDir + '\' + resourceFileName;

      if not FileExists(fileName) then
        fileName :=  ExtractFileDir(enumExeFileName) + '\' + ResourceDllDir + '\' + resourceFileName;

      if FileExists(fileName) and
        (not enumCompatibleOnly or TNtBase.IsLocaleCompatible(id)) and
        (not enumCheckVersions or TNtResource.DoesLocaleVersionMatchFile(fileName)) then
      begin
        if checkIfExists then
          for i := 0 to enumLanguages.Count - 1 do
            if enumLanguages[i].Code = code then
              Exit;

        enumLanguages.Add(code, id, fileName);
      end;
    end;
  end;

var
  id, primary: Integer;
  code: String;
{$IFDEF UNICODE}
  ansiStr: AnsiString;
{$ENDIF}
{$IFDEF DELPHI2010}
  languageCode: String;
{$ENDIF}
begin
{$IFDEF UNICODE}
  ansiStr := localeStr;
  id := StrToInt('$' + String(ansiStr));
{$ELSE}
  id := StrToInt('$' + localeStr);
{$ENDIF}

  primary := TNtBase.LocaleToPrimary(id);

{$IFDEF DELPHI2010}
  languageCode := GetLocaleStr(id, LOCALE_SISO639LANGNAME, '');
  Process(languageCode, primary, True);

  code := languageCode + '-' + GetLocaleStr(id, LOCALE_SISO3166CTRYNAME, '');
  Process(code, id, False);
{$ENDIF}

  code := GetLocaleStr(id, LOCALE_SABBREVLANGNAME, '');
  Process(code, id, False);

  Delete(code, Length(code), 1);

{$IFDEF DELPHI2010}
  if not SameText(code, languageCode) then
{$ENDIF}
    Process(code, primary, True);

  Result := 1;
end;

class function TNtWindows.GetAvailable(
  languages: TNtLanguages;
  exeFileName: String;
  compatibleOnly: Boolean;
  checkVersions: Boolean;  //FI:O804
  const dir: String = ''): Integer;
begin
  if exeFileName = '' then
    exeFileName := TNtBase.GetRunningFileName;

  enumLanguages := languages;
  enumCompatibleOnly := compatibleOnly;

  if dir <> '' then
  begin
    if IsSubPath(dir) then
      enumExeFileName := ExtractFileDir(exeFileName) + '\' + dir + '\' + ExtractFileName(exeFileName)
    else
      enumExeFileName := dir + '\' + ExtractFileName(exeFileName)
  end
  else
    enumExeFileName := exeFileName;

  if Assigned(enumSystemLocalesEx) then
  begin
    enumSystemLocalesEx(@EnumLocalesEx, LOCALE_ALL, 0, nil);

    if languages.Count = 0 then
    begin
      enumExeFileName := TNtBase.GetFolderPath(CSIDL_PERSONAL) + '\' + APPLICATION_DIR + '\' + ExtractFileName(exeFileName);
      enumSystemLocalesEx(@EnumLocalesEx, LOCALE_ALL, 0, nil);
    end;
  end;

  if languages.Count = 0 then
  begin
    enumExeFileName := exeFileName;
    EnumSystemLocalesA(@EnumLocales, LCID_SUPPORTED);

    if languages.Count = 0 then
    begin
      enumExeFileName := TNtBase.GetFolderPath(CSIDL_PERSONAL) + '\' + APPLICATION_DIR + '\' + ExtractFileName(exeFileName);
      EnumSystemLocales(@EnumLocales, LCID_SUPPORTED);
    end;
  end;

  Result := languages.Count;
end;

class function TNtWindows.GetVariantName(const language, variant: String): String;  //FI:O804
resourcestring
  SModern = 'modern';
  SValencia = 'Valencia';
  STechnicalSort = 'technical sort';
  SPhoneBookSort = 'phone book sort';
  SStrokeSort = 'stroke sort';
  SRadicalSort = 'radical/stroke sort';
  SBopomofoSort = 'Bopomofo sort';
begin
  if SameText(variant, 'modern') then
    Result := SModern
  else if SameText(variant, 'technl') then
    Result := STechnicalSort
  else if SameText(variant, 'valencia') then
    Result := SValencia
  else if SameText(variant, 'phoneb') then
    Result := SPhoneBookSort
  else if SameText(variant, 'tradnl') then
    Result := STraditionalSort
  else if SameText(variant, 'stroke') then
    Result := SStrokeSort
  else if SameText(variant, 'radstr') then
    Result := SRadicalSort
  else if SameText(variant, 'pronun') then
    Result := SBopomofoSort
  else
    Result := variant
end;

var
  module: THandle;
initialization
  FVersionInfo.dwOSVersionInfoSize := Sizeof(FVersionInfo);
  GetVersionEx(FVersionInfo);

  if TNtWindows.IsVista then
  begin
    module := LoadLibrary('Kernel32.dll');
    enumSystemLocalesEx := GetProcAddress(module, 'EnumSystemLocalesEx');
    localeNameToLCID := GetProcAddress(module, 'LocaleNameToLCID');
    getLocaleInfoEx := GetProcAddress(module, 'GetLocaleInfoEx');
  end;
end.
