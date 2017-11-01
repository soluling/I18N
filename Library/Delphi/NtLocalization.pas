{
  @abstract Implements localization routines.

  The routines are grouped into following static classes: @link(TNtResource),
  @link(TNtLocale) and @link(TNtMap).

  These routines are low level routines. Normally you do not call them but
  you use @link(TNtLanguageDialog.Select) to change a language. However if you
  want to build your own user interface to select a new language you have to
  use routines of this unit.

  See @italic(Samples\Delphi\VCL\CustomSelect) or @italic(Samples\Delphi\FMX\CustomSelect)
  samples to see how to use the unit.
}
unit NtLocalization;

{$I NtVer.inc}

interface

uses
  Windows, Classes, NtBase;

const
  { Procedure address data size. }
  PROCEDURE_DATA_SIZE = 6;

type
  { @abstract Static class that manipulates resource DLLs. }
  TNtResource = class
  public
    { Get the locale of the currently loaded resources.
      @return The locale code. }
    class function GetActiveLocale: String;

    { Check if the version of the loaded resource DLL matches the version of the application.
      @return @true if the version match, @false if not. }
    class function DoesVersionMatch: Boolean; overload;

    { Check if the version of given resource instance matches the version of the application.
      @return @true if the version match, @false if not. }
    class function DoesVersionMatch(resInstance: LongWord): Boolean; overload;

    { Check if the version of resource DLL of the give language code matches the version of the application.
      @return @true if the version match, @false if not. }
    class function DoesLocaleVersionMatch(const code: String): Boolean;

    { Check if the version of resource DLL of the given DLL file matches the version of the application.
      @return @true if the version match, @false if not. }
    class function DoesLocaleVersionMatchFile(const fileName: String): Boolean;
  end;

  { @abstract Static class that contains locale routines. }
  TNtLocale = class
  private
    class function GetPreviousLocale: String;

  public
    class procedure CheckLocaleVariables;
    class procedure UpdateFormatSettings(locale: Integer);

    { Convert resource DLL extension into to locale id.
      @param value Extension to be converted. It can be with or without leading period.
      @return Locale id. }
    class function ExtensionToLocale(const value: String): Integer;

    { Get the code page matching the given locale id.
      @param locale Locale to be checked.
      @return Code page. }
    class function LocaleToCodePage(locale: Integer): Integer;

    { Convert locale id into ISO code.
      @param locale Locale to be converted.
      @return ISO code. }
    class function LocaleToIso639(locale: Integer): String;

    { Check if the active locale is Asian.
      @return @true if the previous active is Asian, @false if not. }
    class function IsActiveLocaleAsian: Boolean;

    { Check if the active locale is bi-directional.
      @return @true if the previous active is bi-directional, @false if not. }
    class function IsActiveLocaleBidi: Boolean;

    { Check if the previous locale was bi-directional.
      @return @true if the previous locale was bi-directional, @false if not. }
    class function IsPreviousLocaleBidi: Boolean;

    { Get the language or locale of the form.
      @return Language or locale id. If 0 the form locale is neutral. }
    class function GetFormLocale: Integer;

    { Check if the locale is Asian.
      @param locale Locale to be checked.
      @return @true if the locale is Asian, @false if not. }
    class function IsLocaleAsian(value: String): Boolean;

    { Check if the locale is bi-directional.
      @param locale Locale to be checked.
      @return @true if the locale is bi-directional, @false if not. }
    class function IsLocaleBidi(value: String): Boolean;

    { Convert locale id into ISO code. It contains language code and optional
      country code separated by - character.
      @param locale Locale to be converted.
      @return ISO code. }
    class function LocaleToIso(locale: Integer): String;
  end;

  { @abstract Record that stores information about function pointer. }
  TNtProcedureData = record
    Address: Pointer;
    Data: array[0..PROCEDURE_DATA_SIZE] of Byte;
  end;

  { @abstract Static class that contains routines for function mapping. }
  TNtMap = class
  public
    { Replace a function with another.
      This has effection only on 32-bit applications.
      @param oldProc Pointer to exisiting function.
      @param newProc Pointer to a new function that replaces the old one.
      @param data    Structure that stores information of the replaced function. }
    class procedure RemapProcedure(oldProc, newProc: Pointer; var data: TNtProcedureData);

    { Restores a replaced function.
      This has effection only on 32-bit applications.
      @param originalProc Pointer to original function.
      @param data         Structure that stores information of the replaced function. }
    class procedure RestoreProcedure(originalProc: Pointer; data: TNtProcedureData);
  end;

implementation

uses
  SysUtils, Registry;

var
  FFormResourceLocale: Integer;

type
  TVersionInfo = record
    wLength: Word;
    wValueLength: Word;
    wType: Word;
    szKey: array[0..14] of WideChar;
    padding1: array[0..1] of Word;
    value: TVSFixedFileInfo;
    padding2: array[0..1] of Word;
  end;

  PVersionInfo = ^TVersionInfo;


{$IFNDEF DELPHIXE5}
function EnumEraNames(names: PChar): Integer; stdcall;
var
  i: Integer;
begin
  Result := 0;

  i := Low(EraNames);

  while EraNames[I] <> '' do
  begin
    if (i = High(EraNames)) then
      Exit
    else
      Inc(i);
  end;

  EraNames[i] := names;
  Result := 1;
end;

function EnumEraYearOffsets(yearOffsets: PChar): Integer; stdcall;
var
  i: Integer;
begin
  Result := 0;

  i := Low(EraYearOffsets);

  while EraYearOffsets[i] <> -1 do
  begin
    if (i = High(EraYearOffsets)) then
      Exit
    else
      Inc(I);
  end;

  EraYearOffsets[i] := StrToIntDef(yearOffsets, 0);
  Result := 1;
end;
{$ENDIF}

class function TNtLocale.LocaleToCodePage(locale: Integer): Integer;
begin
  if locale = LANG_CHINESE then
    Result := 936
  else
    Result := StrToInt(GetLocaleStr(locale, LOCALE_IDEFAULTANSICODEPAGE, '0'));
end;

class function TNtLocale.LocaleToIso639(locale: Integer): String;
begin
  Result := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '');

  if TNtBase.LocaleToSub(locale) <> SUBLANG_NEUTRAL then
    Result := Result + LOCALE_SEPARATOR + GetLocaleStr(locale, LOCALE_SISO3166CTRYNAME, '');
end;

{$IFNDEF UNICODE}
function CharInSet(c: AnsiChar; const charSet: TSysCharSet): Boolean;
begin
  Result := c in charSet;
end;
{$ENDIF}

function TranslateDateFormat(
  locale: Integer;
  const formatStr: String): String;
var
  i, l: Integer;
  calendarType: CALTYPE;
begin
  i := 1;
  Result := '';

  calendarType := StrToIntDef(GetLocaleStr(locale, LOCALE_ICALENDARTYPE, '1'), 1);

  if not (calendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA]) then
  begin
    if SysLocale.PriLangID in [LANG_JAPANESE, LANG_CHINESE, LANG_KOREAN] then
    begin
      while i <= Length(formatStr) do
      begin
        if not CharInSet(formatStr[i], ['g', 'G']) then
          Result := Result + formatStr[i];

        Inc(i);
      end;
    end
    else
      Result := formatStr;

    Exit;
  end;

  while i <= Length(formatStr) do
  begin
    if CharInSet(formatStr[i], LeadBytes) then
    begin
      l := CharLength(formatStr, i);
      Result := Result + Copy(formatStr, i, l);
      Inc(i, l);
    end
    else
    begin
      if StrLIComp(@formatStr[i], 'gg', 2) = 0 then
      begin
        Result := Result + 'ggg';
        Inc(i, 1);
      end
      else if StrLIComp(@formatStr[i], 'yyyy', 4) = 0 then
      begin
        Result := Result + 'eeee';
        Inc(i, 4 - 1);
      end
      else if StrLIComp(@formatStr[i], 'yy', 2) = 0 then
      begin
        Result := Result + 'ee';
        Inc(i, 2 - 1);
      end
      else if CharInSet(formatStr[i], ['y', 'Y']) then
        Result := Result + 'e'
      else
        Result := Result + formatStr[i];

      Inc(i);
    end;
  end;
end;

{$IFNDEF DELPHI2006}
function LcidToCodePage(locale: LCID): Integer;
var
  buffer: array [0..6] of Char;
begin
  GetLocaleInfo(locale, LOCALE_IDEFAULTANSICODEPAGE, buffer, SizeOf(buffer));
  Result:= StrToIntDef(buffer, GetACP);
end;

procedure InitSysLocale(locale: Integer);
var
  i: Integer;
  defaultLangID: LANGID;
  ansiCPInfo: TCPInfo;
  pcharA: PAnsiChar;
  bufferA: array[128..255] of Char;
  bufferW: array[128..256] of Word;

  procedure InitLeadBytes;
  var
    i: Integer;
    j: Byte;
  begin
    GetCPInfo(LcidToCodePage(SysLocale.DefaultLCID), ansiCPInfo);

    with ansiCPInfo do
    begin
      i := 0;
      while (i < MAX_LEADBYTES) and ((LeadByte[i] or LeadByte[i + 1]) <> 0) do
      begin
        for j := LeadByte[i] to LeadByte[i + 1] do
          Include(LeadBytes, Char(j));
        Inc(i, 2);
      end;
    end;
  end;

  function IsWesternGroup: Boolean;
  type
    TLanguage = $00..$1D;
  const
    NEUTRAL = TLanguage($00);
    DANISH = TLanguage($06);
    DUTCH = TLanguage($13);
    ENGLISH  = TLanguage($09);
    FINNISH = TLanguage($0B);
    FRENCH = TLanguage($0C);
    GERMAN = TLanguage($07);
    ITALIAN  = TLanguage($10);
    NORWEGIAN = TLanguage($14);
    PORTUGUSE = TLanguage($16);
    SPANISH  = TLanguage($0A);
    SWEDISH  = TLanguage($1D);
  begin
    Result := SysLocale.PriLangID in
    [
      NEUTRAL,
      DANISH,
      DUTCH,
      ENGLISH,
      FINNISH,
      FRENCH,
      GERMAN,
      ITALIAN,
      NORWEGIAN,
      PORTUGUSE,
      SPANISH,
      SWEDISH
    ];
  end;

begin
  SysLocale.DefaultLCID := $0409;
  SysLocale.PriLangID := LANG_ENGLISH;
  SysLocale.SubLangID := SUBLANG_ENGLISH_US;

  if locale <> 0 then
    SysLocale.DefaultLCID := locale;

  defaultLangID := Word(locale);

  if defaultLangID <> 0 then
  begin
    SysLocale.PriLangID := defaultLangID and $03FF;
    SysLocale.SubLangID := defaultLangID shr 10;
  end;

  LeadBytes := [];

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if IsWesternGroup then
    begin
      SysLocale.MiddleEast := False;
      SysLocale.FarEast := False;
    end
    else
    begin
      InitLeadBytes;
      SysLocale.FarEast := LeadBytes <> [];

      if SysLocale.FarEast then
      begin
        SysLocale.MiddleEast := False;
        Exit;
      end;

      for i := Low(bufferA) to High(bufferA) do
        bufferA[I] := Char(i);

      pcharA := @bufferA;

      GetStringTypeExA(
        SysLocale.DefaultLCID,
        CT_CTYPE2,
        pcharA,
        High(bufferA) - Low(bufferA) + 1,
        bufferW);

      for i := Low(bufferA) to High(bufferA) do
      begin
        SysLocale.MiddleEast := bufferW[i] = C2_RIGHTTOLEFT;

        if SysLocale.MiddleEast then
          Exit;
      end;
    end;
  end
  else
  begin
    SysLocale.MiddleEast := GetSystemMetrics(SM_MIDEASTENABLED) <> 0;
    SysLocale.FarEast := GetSystemMetrics(SM_DBCSENABLED) <> 0;

    if SysLocale.FarEast then
      InitLeadBytes;
  end;
end;
{$ENDIF}

class procedure TNtLocale.CheckLocaleVariables;
begin
  if LoadedResourceLocale <> '' then
    UpdateFormatSettings(ExtensionToLocale(LoadedResourceLocale));
end;

class procedure TNtLocale.UpdateFormatSettings(locale: Integer);

  function LocalGetLocaleStr(localeType: Integer; const default: String): String;
  begin
    Result := GetLocaleStr(locale, localeType, default);
  end;

var
  i, day: Integer;
  hourFormat, timePrefix, timePostfix: String;
{$IFNDEF DELPHIXE5}
  j: Integer;
  calendarType: CALTYPE;
{$ENDIF}
begin
  if locale = LANG_CHINESE then
    locale := TNtBase.MakeLangId(LANG_CHINESE, SUBLANG_CHINESE_SIMPLIFIED);

{$IFNDEF DELPHI2006}
  InitSysLocale(locale);
{$ENDIF}

{$IFNDEF DELPHIXE5}
  if SysLocale.FarEast then
  begin
    calendarType := StrToIntDef(GetLocaleStr(
      locale,
      LOCALE_IOPTIONALCALENDAR, '1'),
      1);

    if calendarType in [CAL_JAPAN, CAL_TAIWAN, CAL_KOREA] then
    begin
      EnumCalendarInfo(@EnumEraNames, locale, calendarType, CAL_SERASTRING);

      for j := Low(EraYearOffsets) to High(EraYearOffsets) do
        EraYearOffsets[j] := -1;

      EnumCalendarInfo(
        @EnumEraYearOffsets,
        locale,
        CalendarType,
        CAL_IYEAROFFSETRANGE);
    end;
  end;
{$ENDIF}

  for i := 1 to 12 do
  begin
    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortMonthNames[i] := LocalGetLocaleStr(
      LOCALE_SABBREVMONTHNAME1 + i - 1,
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortMonthNames[i]);

    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongMonthNames[i] := LocalGetLocaleStr(
      LOCALE_SMONTHNAME1 + i - 1,
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongMonthNames[i]);
  end;

  for i := 1 to 7 do
  begin
    day := (I + 5) mod 7;

    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDayNames[i] := LocalGetLocaleStr(
      LOCALE_SABBREVDAYNAME1 + day,
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDayNames[i]);

    {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongDayNames[i] := LocalGetLocaleStr(
      LOCALE_SDAYNAME1 + day,
      {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongDayNames[i]);
  end;

  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}CurrencyString := GetLocaleStr(locale, LOCALE_SCURRENCY, '');
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}CurrencyFormat := StrToIntDef(GetLocaleStr(locale, LOCALE_ICURRENCY, '0'), 0);
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}NegCurrFormat := StrToIntDef(GetLocaleStr(locale, LOCALE_INEGCURR, '0'), 0);
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ThousandSeparator := GetLocaleChar(locale, LOCALE_STHOUSAND, ',');
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DecimalSeparator := GetLocaleChar(locale, LOCALE_SDECIMAL, '.');
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}CurrencyDecimals := StrToIntDef(GetLocaleStr(locale, LOCALE_ICURRDIGITS, '0'), 0);
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}DateSeparator := GetLocaleChar(locale, LOCALE_SDATE, '/');

  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortDateFormat := TranslateDateFormat(
    locale,
    GetLocaleStr(locale, LOCALE_SSHORTDATE, 'm/d/yy'));

  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongDateFormat := TranslateDateFormat(
    locale,
    GetLocaleStr(locale, LOCALE_SLONGDATE, 'mmmm d, yyyy'));

  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimeSeparator := GetLocaleChar(locale, LOCALE_STIME, ':');
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimeAMString := GetLocaleStr(locale, LOCALE_S1159, 'am');
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}TimePMString := GetLocaleStr(locale, LOCALE_S2359, 'pm');
  TimePrefix := '';
  TimePostfix := '';

  if StrToIntDef(GetLocaleStr(locale, LOCALE_ITLZERO, '0'), 0) = 0 then
    HourFormat := 'h'
  else
    HourFormat := 'hh';

  if StrToIntDef(GetLocaleStr(locale, LOCALE_ITIME, '0'), 0) = 0 then
  begin
    if StrToIntDef(GetLocaleStr(locale, LOCALE_ITIMEMARKPOSN, '0'), 0) = 0 then
      TimePostfix := ' AMPM'
    else
      TimePrefix := 'AMPM ';
  end;

  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ShortTimeFormat := TimePrefix + HourFormat + ':mm' + TimePostfix;
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}LongTimeFormat := TimePrefix + HourFormat + ':mm:ss' + TimePostfix;
  {$IFDEF DELPHIXE}FormatSettings.{$ENDIF}ListSeparator := GetLocaleChar(locale, LOCALE_SLIST, ',');

  FirstDayOfWeek := TNtDayOfWeek(StrToInt(GetLocaleStr(
    locale,
    LOCALE_IFIRSTDAYOFWEEK,
    '0')));

  FirstWeekOfYear := TNtFirstWeekOfYear(StrToInt(GetLocaleStr(
    locale,
    LOCALE_IFIRSTWEEKOFYEAR,
    '0')));
end;

function GetLocaleStr(locale, localeType: Integer; const default: UnicodeString): UnicodeString;
var
  l: Integer;
  buffer: array[0..255] of WideChar;
begin
  l := GetLocaleInfoW(locale, localeType, buffer, SizeOf(buffer));

  if l > 0 then
    SetString(Result, buffer, l - 1)
  else
    Result := default;
end;

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

class function TNtLocale.ExtensionToLocale(const value: String): Integer;
begin
  Result := 0;

  if value = '' then
    Exit;

  enumExtension := value;

  if enumExtension[1] = '.' then
    Delete(enumExtension, 1, 1);

  enumLocale := 0;
  EnumSystemLocales(@LocaleEnumProc, LCID_SUPPORTED);

  if enumLocale = 0 then
    EnumSystemLocales(@LanguageEnumProc, LCID_SUPPORTED);

  Result := enumLocale;
end;

class function TNtResource.GetActiveLocale: String;
begin
  if TNtBase.IsLoaded then
    Result := LoadedResourceLocale
  else
    Result := TNtLocale.LocaleToIso(TNtLocale.GetFormLocale);

  if Result = '' then
    Result := DefaultLocale;
end;

var
  languageIds: TList;

function EnumResourceLanguagesProc(
  instance: THandle;
  resType: PChar;
  resName: PChar;
  languageId: Word;
  param: DWord): Bool; stdcall;
begin
  languageIds.Add(Pointer(languageId));
  Result := True;
end;

function EnumResourceNamesProc(
  instance: THandle;
  resType: PChar;
  resName: PChar;
  param: DWord): Bool; stdcall;
var
  i, id, windowsLocale: Integer;
  reader: TReader;
  stream: TResourceStream;
  header: array[0..3] of AnsiChar;
begin
  Result := True;
  stream := TResourceStream.Create(instance, resName, resType);
  reader := TReader.Create(stream, stream.Size);
  try
    if stream.Size < 4 then
      Exit;

    reader.Read(header, 4);

    if header = VCL_FORM_HEADER then
    begin
      languageIds := TList.Create;
      try
        EnumResourceLanguages(HInstance, resType, resName, @EnumResourceLanguagesProc, 0);

        if languageIds.Count > 0 then
        begin
          Result := False;

          if languageIds.Count = 1 then
            FFormResourceLocale := Integer(languageIds[0])
          else
          begin
            windowsLocale := TNtBase.GetUserLanguage;

            for i  := 0 to languageIds.Count - 1 do
            begin
              id := Integer(languageIds[0]);

              if windowsLocale = id then
              begin
                FFormResourceLocale := id;
                Exit;
              end;
            end;

            windowsLocale := TNtBase.LocaleToPrimary(TNtBase.GetUserLanguage);

            for i  := 0 to languageIds.Count - 1 do
            begin
              id := Integer(languageIds[0]);

              if windowsLocale = id then
              begin
                FFormResourceLocale := id;
                Exit;
              end;
            end;
          end;
        end;
      finally
        languageIds.Free;
      end;
    end;
  finally
    reader.Free;
    stream.Free;
  end;
end;

class function TNtLocale.GetFormLocale: Integer;
begin
  if FFormResourceLocale = 0 then
    EnumResourceNames(HInstance, PChar(RT_RCDATA), @EnumResourceNamesProc, 0);

  Result := FFormResourceLocale;
end;

class function TNtLocale.IsActiveLocaleBidi: Boolean;
begin
  Result := IsLocaleBiDi(TNtResource.GetActiveLocale);
end;

class function TNtLocale.GetPreviousLocale: String;
begin
  if PreviouslyLoadedResourceLocale <> '' then
    Result := PreviouslyLoadedResourceLocale
  else
    Result := TNtbase.LocaleToExtension(GetFormLocale);
end;

class function TNtLocale.IsPreviousLocaleBidi: Boolean;
begin
  Result := IsLocaleBiDi(GetPreviousLocale);
end;

class function TNtLocale.IsActiveLocaleAsian: Boolean;
begin
  Result := IsLocaleAsian(TNtResource.GetActiveLocale);
end;

var
  enumFound: Boolean;
  enumVersion1, enumVersion2, enumVersion3, enumVersion4: Integer;

function EnumEnumVersionProcLanguagesProc(
  module: Integer;
  resType: PChar;
  resName: PChar;
  language: Word;
  param: Integer): Bool; stdcall;
var
  resource: HRSRC;
  versionInfo: PVersionInfo;
begin
  resource := FindResource(module, resName, resType);
  versionInfo := LockResource(LoadResource(module, resource));

  enumVersion1 := versionInfo.value.dwFileVersionMS shr 16;
  enumVersion2 := $FF and versionInfo.value.dwFileVersionMS;
  enumVersion3 := versionInfo.value.dwFileVersionLS shr 16;
  enumVersion4 := $FF and versionInfo.value.dwFileVersionLS;

  enumFound := True;

  Result := Bool(0);
end;

function EnumVersionProc(
  module: Integer;
  resType: PChar;
  resName: PChar;
  param: Integer): Bool; stdcall;
begin
  EnumResourceLanguages(module, resType, resName, @EnumEnumVersionProcLanguagesProc, 0);

  Result := Bool(0);
end;

class function TNtResource.DoesVersionMatch(resInstance: LongWord): Boolean;
var
  exeVersion1, exeVersion2, exeVersion3, exeVersion4: Integer;
begin
  Result := True;

  enumFound := False;
  EnumResourceNames(HInstance, RT_VERSION, @EnumVersionProc, 0);

  if not enumFound then
    Exit;

  exeVersion1 := enumVersion1;
  exeVersion2 := enumVersion2;
  exeVersion3 := enumVersion3;
  exeVersion4 := enumVersion4;

  enumFound := False;
  EnumResourceNames(resInstance, RT_VERSION, @EnumVersionProc, 0);

  if not enumFound then
    Exit;

  Result :=
    (exeVersion1 = enumVersion1) and
    (exeVersion2 = enumVersion2) and
    (exeVersion3 = enumVersion3) and
    (exeVersion4 = enumVersion4);
end;

class function TNtResource.DoesVersionMatch: Boolean;
begin
  if TNtBase.IsLoaded then
    Result := DoesVersionMatch(LibModuleList.ResInstance)
  else
    Result := True;
end;

class function TNtResource.DoesLocaleVersionMatch(const code: String): Boolean;
var
  instance: HModule;
begin
  instance := LoadLibraryEx(
    PChar(TNtBase.GetLanguageFile(TNtBase.GetRunningFileName, code)),
    0,
    LOAD_LIBRARY_AS_DATAFILE);
  try
    Result := DoesVersionMatch(instance);
  finally
    FreeLibrary(instance);
  end;
end;

class function TNtResource.DoesLocaleVersionMatchFile(const fileName: String): Boolean;
var
  instance: HModule;
begin
  instance := LoadLibraryEx(
    PChar(fileName),
    0,
    LOAD_LIBRARY_AS_DATAFILE);
  try
    Result := DoesVersionMatch(instance);
  finally
    FreeLibrary(instance);
  end;
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

{$IFDEF DELPHI2010}
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
{$ENDIF}

var
  buffer: array[0..260] of Char;
begin
  PreviouslyLoadedResourceLocale := '';

  if (LoadedResourceLocale = '') and
    (GetModuleFileName(LibModuleList.Instance, buffer, SizeOf(buffer)) > 0) then
  begin
{$IFDEF DELPHIXE}
    LoadedResourceLocale := GetLocaleOverride(buffer);
{$ELSE}
    LoadedResourceLocale := CheckLocale(buffer, TNtLocaleRegistry.GetCurrentDefaultLocale, False);

    if LoadedResourceLocale <> '' then
      Exit;

  {$IFDEF DELPHI2010}
    LoadedResourceLocale := CheckLocales(buffer, GetUILanguages(GetUserDefaultUILanguage));

    if LoadedResourceLocale <> '' then
      Exit;

    if (GetVersion and $000000FF) < 6 then
      LoadedResourceLocale := CheckLocales(buffer, GetUILanguages(GetSystemDefaultUILanguage));

    if LoadedResourceLocale <> '' then
      Exit;

    LoadedResourceLocale := CheckLocale(buffer, TNtBase.LocaleToExtension(GetUserDefaultUILanguage), True);
  {$ELSE}
    LoadedResourceLocale := CheckLocale(buffer, TNtBase.LocaleToExtension(GetThreadLocale), True);
  {$ENDIF}
{$ENDIF}
  end;
end;

class function TNtLocale.IsLocaleBidi(value: String): Boolean;
begin
  if Length(value) > 2 then
    value := Copy(value, 1, 2);

  value := LowerCase(value);

  Result := (value = 'ar') or (value = 'he') or (value = 'fa');
end;

class function TNtLocale.IsLocaleAsian(value: String): Boolean;
begin
  if Length(value) > 2 then
    value := Copy(value, 1, 2);

  value := LowerCase(value);

  Result := (value = 'jp') or (value = 'ch') or (value = 'ko');
end;

class function TNtLocale.LocaleToIso(locale: Integer): String;
begin
  if locale = 0 then
    Result := ''
  else
  begin
    Result := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '');

    if TNtBase.LocaleToSub(locale) <> SUBLANG_NEUTRAL then
      Result := Result + '_' + GetLocaleStr(locale, LOCALE_SISO3166CTRYNAME, '');
  end;
end;


// TNtMap

class procedure TNtMap.RemapProcedure(oldProc, newProc: Pointer; var data: TNtProcedureData);
{$IFDEF WIN32}
var
  i: Integer;
  p: Pointer;
  protect1, protect2: DWord;
  address: PAnsiChar;
{$ENDIF}
begin
{$IFDEF WIN32}
  if data.Address <> nil then
    Exit;

  if not VirtualProtect(oldProc, PROCEDURE_DATA_SIZE, PAGE_EXECUTE_READWRITE, @protect1) then
    RaiseLastOSError;

  p := oldProc;

  if Word(p^) = $25FF then
  begin
    Inc(Integer(p), 2);
    p := Pointer(Pointer(p^)^);

    if not VirtualProtect(oldProc, PROCEDURE_DATA_SIZE, protect1, @protect2) then
      RaiseLastOSError;

    oldProc := p;

    if not VirtualProtect(oldProc, PROCEDURE_DATA_SIZE, PAGE_EXECUTE_READWRITE, @protect1) then
      RaiseLastOSError;
  end;

  Move(oldProc^, data.Data, PROCEDURE_DATA_SIZE);
  data.Address := oldProc;

  i := Integer(newProc) - Integer(p) - PROCEDURE_DATA_SIZE + 1;

  address := PAnsiChar(oldProc);
  address[0] := AnsiChar($E9);
  address[1] := AnsiChar(i and 255);
  address[2] := AnsiChar((i shr 8) and 255);
  address[3] := AnsiChar((i shr 16) and 255);
  address[4] := AnsiChar((i shr 24) and 255);

  if not VirtualProtect(address, PROCEDURE_DATA_SIZE, protect1, @protect2) then
    RaiseLastOSError;
{$ENDIF}
end;

class procedure TNtMap.RestoreProcedure(originalProc: Pointer; data: TNtProcedureData);
{$IFDEF WIN32}
var
  protect1, protect2: Cardinal;
{$ENDIF}
begin
{$IFDEF WIN32}
  if data.Address = nil then
    Exit;

  if not VirtualProtect(data.Address, PROCEDURE_DATA_SIZE, PAGE_EXECUTE_READWRITE, @protect1) then
    RaiseLastOSError;

  Move(data.Data, data.Address^, PROCEDURE_DATA_SIZE);

  if not VirtualProtect(data.Address, PROCEDURE_DATA_SIZE, protect1, @protect2) then
    RaiseLastOSError;
{$ENDIF}
end;


initialization
  FFormResourceLocale := 0;
  SetInitialResourceLocale;
end.

