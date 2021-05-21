{
  @abstract Implements resource and locale routines that are common to VCL and FMX.

  The unit contains following classes: @link(TNtLanguage) and @link(TNtLanguages).
  They are used to store information about resource DLL languages.

  In addition it contains routines that are grouped into three static classes:
  @link(TNtBase), @link(TNtLocaleRegistry) and @link(TNtConvert).

  @link(TNtBase.GetAvailable) gets the available resource languages. Use it
  to populate the available language list in your user interface. After you
  have selected the language call @link(TNtTranslator.SetNew) to turn on that language.

  See @italic(Samples\Delphi\VCL\CustomSelect) or @italic(Samples\Delphi\FMX\CustomSelect)
  samples to see how to use the unit.
}

unit NtBase;

{$I NtVer.inc}

interface

uses
{$IFDEF AUTOREFCOUNT}
  System.Generics.Collections,
{$ENDIF}
  SysUtils,
  Classes;

const
  APPLICATION_RESOURCE = 'SOLULING';
  APPLICATION_DIR = 'Soluling';

  { Character that is used to separate country part from language part in the locale id. }
  LOCALE_SEPARATOR = '-';

  LOCALE_ALL = 0;          // enumerate all named based locales

  { Form header. }
  VCL_FORM_HEADER: array[0..3] of Byte = ($54, $50, $46, $30);  // 'TPF0'

  { The virtual folder that represents the My Documents desktop item. }
  CSIDL_PERSONAL = $0005;

{$IFNDEF MSWINDOWS}
  { Neutral sub language id. }
  SUBLANG_NEUTRAL = 0;
{$ENDIF}

{$IFDEF DELPHIXE}
  { Current locale override key (Embarcadero era) }
  LOCALE_OVERRIDE_KEY = 'Software\Embarcadero\Locales';
  { Old locale override key (CodeGear era) }
  OLD_LOCALE_OVERRIDE_KEY = 'Software\CodeGear\Locales';
  { Oldest locale override key (Borland era) }
  OLDEST_LOCALE_OVERRIDE_KEY = 'Software\Borland\Locales';
{$ELSE}
  {$IFDEF UNICODE}
  LOCALE_OVERRIDE_KEY = 'Software\CodeGear\Locales';
  OLD_LOCALE_OVERRIDE_KEY = 'Software\Borland\Locales';
  {$ELSE}
  LOCALE_OVERRIDE_KEY = 'Software\Borland\Locales';
  OLD_LOCALE_OVERRIDE_KEY = '';
  {$ENDIF}
  OLDEST_LOCALE_OVERRIDE_KEY = '';
{$ENDIF}

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
  RawByteString = AnsiString;
{$ENDIF}

  { Day of the week. }
  TNtDayOfWeek =
  (
    wdMonday,     //< Monday
    wdTuesday,    //< Tuesday
    wdWednesday,  //< Wednesday
    wdThursday,   //< Thursday
    wdFriday,     //< Friday
    wdSaturday,   //< Saturday
    wdSunday      //< wdSunday
  );

  { The first day of the week. }
  TNtFirstWeekOfYear =
  (
    fwFirstPart,  //< First week that has at least one day in January.
    fwFirstFull,  //< First full week of January.
    fwFirst4      //< First week that has at least four days in January.
  );

  { User interface layout. }
  TNtLayout =
  (
    laLeftToRight,  //< User interface layout is Western (e.g. from left to right).
    laRightToLeft   //< User interface layout is Middle-Eastern (e.g. from right to left).
  );

  // Specifies resource options.
  TNtResourceOption =
  (
    roNoThreadLocale,     //< Do not update the thread locle.
    roNoLocaleVariables,  //< Do not update the locale variables.
    roNoUpdateBidiMode,   //< Do not update the TApplication.BiDiMode.
    roFlipChildren,       //< Do not flip the controls on forms.
    roSaveLocale          //< Save the active locale.
  );

  // Set of resource options.
  TNtResourceOptions = set of TNtResourceOption;

  { Options for extrating embedded resource files. }
  TExtractOption =
  (
    eoCheckDate,   //< Application check the date of exisisting resource files (if any). If the date is same or newer as the embedded file the file is not extracted.
    eoRemoveFiles  //< Extracted files are removed after application closes.
  );

  { Set of extracting options. }
  TExtractOptions = set of TExtractOption;

  { Specifies in what language language, country and locale names are given. }
  TNtLanguageName =
  (
    lnNative,     //< Language's own language is used: English, Deutsch, Français, ...
    lnLocalized,  //< Language of the application is used.
    lnBoth,       //< Native + localize is used.
    lnEnglish,    //< English is used.
    lnSystem      //< Lanugage of the operation system is used.
  );

  { @abstract Contains infomation about resource DLL language. }
  TNtLanguage = class(TObject)
  private
    FCode: String;
    FId: Integer;
    FFileName: String;

    function GetName(i: TNtLanguageName): String;

  public
    { Get language name that contains both native and localized names.
      @param native       Language name in the language itself.
      @param localized    Language name in the current language.
      @return Display name. }
    class function GetBoth(const native, localize: String): String;

    { Get the display name of a language or locale.
      @param locale       Windows language or locale id.
      @param languageName Specifies what kind of name is returned.
      @return Display name such as English or English (United States). }
    class function GetDisplayName(
      const id: String;
      locale: Integer = 0;
      languageName: TNtLanguageName = lnSystem): String;

    property Code: String read FCode write FCode;                  //< ISO language or locale code.
    property Id: Integer read FId write FId;                       //< Windows locale id.
    property FileName: String read FFileName write FFileName;      //< Resource DLL file name.
    property NativeName: String index lnNative read GetName;       //< Native name.  Uses language of the language itself.
    property LocalizedName: String index lnLocalized read GetName; //< Localized name. Uses language of the application.
    property EnglishName: String index lnEnglish read GetName;     //< English name.
    property SystemName: String index lnSystem read GetName;       //< System name. Uses language of the operating system.
    property Names[i: TNtLanguageName]: String read GetName;       //< Array of names.
  end;

  { Specifies how the default locale is selected. }
  TLocaleSelect =
  (
    lsUI,       //< Locale matching the UI language of OS. This is the default behaviour in Delphi 2010 or later.
    lsSettings  //< Locale matching the regional settings of Control Panel. This is the default behaviour in Delphi 2009 or earlier.
  );

  { @abstract List of resource DLL languages. }
  TNtLanguages = class(TObject)
  private
{$IFDEF AUTOREFCOUNT}
    FItems: TList<TNtLanguage>;
{$ELSE}
    FItems: TList;
{$ENDIF}

    function GetCount: Integer;
    function GetItem(i: Integer): TNtLanguage;

  public
    constructor Create;
    destructor Destroy; override;

    { Add a new language to the list.
      @param code     Language or locale code.
      @param id       Windows language or locale id.
      @param fileName Resource DLL file name. }
    function Add(
      const code: String;
      id: Integer = 0;
      const fileName: String = ''): TNtLanguage; overload;

    { Add a language to the list.
      @param language Language to be added }
    procedure Add(language: TNtLanguage); overload;

    procedure AddDefault;

    property Count: Integer read GetCount;                          //< Language count.
    property Items[i: Integer]: TNtLanguage read GetItem; default;  //< Languages.

  end;

  { @abstract Static class that contains localization routines. }
  TNtBase = class
  private
    class procedure DeleteExtractedFiles;
{$IFDEF MSWINDOWS}
    class function HasWriteAccess(const dir: String): Boolean;
{$ENDIF}

  public
    { Checks the resource DLL count. If this is 0 raises an exception. }
    class procedure CheckThatDllsExist;

    { Check if the given locale is compatible to the system.
      @param locale Locale to be checked.
      @return @true if the locale is compatible, @false if not. On Delphi 2009 later return always @true.}
    class function IsLocaleCompatible(locale: Integer): Boolean;

    { Get a list of available resource DLL languages.
      @param languages      Returns a list of available languages.
      @param exeFileName    Specifies the name of the application. If empty the current application is used.
      @param compatibleOnly If @true only those resource DLL languages are added that are compatible to the current system settings. Not used in Delphi 2009 or later.
      @param checkVersions  If @true only those resource DLL languages are added where the version resource of the DLL matches the EXE.
      @return Amount of lanugages. }
    class function GetAvailable(
      languages: TNtLanguages;
      exeFileName: String = '';
      compatibleOnly: Boolean = False;
      checkVersions: Boolean = False): Integer;

    { Get the number of the available resource DLLs.
      @param exeFileName    Specifies the name of the application. If empty the current application is used.
      @param compatibleOnly If @true only those resource DLL languages are added that are compatible to the current system settings. Not used in Delphi 2009 or later.
      @param checkVersions  If @true only those resource DLL languages are added where the version resource of the DLL matches the EXE.
      @return Amount of lanugages. }
    class function GetAvailableCount(
      exeFileName: String = '';
      compatibleOnly: Boolean = False;
      checkVersions: Boolean = False): Integer;

    { Get the resource DLL file for the given locale.
      @param fileName
      @param id
      @return Resource DLL file name. Empty if not found. }
    class function GetResourceFile(const fileName, id: String): String;

    { Check if a resource DLL file for the given locale exists.
      @param fileName
      @param id
      @return True if a resource file exists. }
    class function ResourceFileExist(const fileName, id: String): Boolean;

    { Set the initial locale to match the current settings in the Regional Settings of Control Panel.
      @param localeSelect   Specifies how to select the dfault locale.
      @param defaultLocale  Specifies the default locale. If empty the default locale is not set. }
    class procedure SetInitialLocale(
      localeSelect: TLocaleSelect;
      const defaultLocale: String = '');

    { Sets the directory where the resource DLL file locates.
      @longCode(#
program CustomDir;

uses
  NtBase,
  Forms,
  Unit1 in 'Unit1.pas';

begin
  TNtBase.SetResourceDllDir('Locales');
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
#)
      @param value         Directory. Can be abosulte or relative to the application file.
      @param localeSelect  Specifies how to select the dfault locale. }
    class procedure SetResourceDllDir(const value: String; localeSelect: TLocaleSelect = lsUI);

    { Load a new resource DLL file. If you call @link(TNtTranslator.SetNew) this
      should not be called.
      @param code      The locale code of the resource to be loaded.
      @param fileName  The file to be loaded. If empty then function looks the standard resource file name and location.
      @return The module handle of the loaded resource DLL. }
    class function LoadNew(
      code: String = '';
      fileName: String = ''): HModule;

    { Extracts embedded resource DLLs from EXE to the application directory.
      If the user has write access to the directory where the EXE locates, then
      resource files are exteracted there. Otherwise the files are extracted
      to the user's personal directory.
      @param options       Specifies the extract options
      @param resourceName  Specifies the resource type name of the resource DLLs. If empty SOLULING is used. }
    class function ExtractFiles(
      options: TExtractOptions = [eoCheckDate];
      resourceName: String = ''): Integer;

    { @abstract Disables the resource DLLs.
      If you call this application does not load any resource DLL.
      See Samples\Delphi\VCL\IgnoreResourceDll to see how to use this procedure. }
    class procedure DisableResourceDlls;

    { Get a special folder.
      @return Full path name to the folder. }
    class function GetFolderPath(nFolder: Integer): String;

    { Parse locale into language, country and variant parts.
      @param value    Locale to be parsed.
      @param language Returns the language part.
      @param country  Returns the country part.
      @param variant  Returns the variant part. }
    class procedure ParseLocaleString(value: String; var language, country, variant: String);

    { Parse language id into language, script, country and variant parts.
      @param value    Locale to be parsed.
      @param language Returns the language part.
      @param script   Returns the script part.
      @param country  Returns the country part.
      @param variant  Returns the variant part. }
    class procedure ParseLocaleId(
      id: String;
      var language: String;
      var script: String;
      var country, variant: String);

    { Check if a resource DLL has been loaded.
      @return @true if a resource DLL has been loaded. @false if no resource DLL has been loaded and the application uses the EXE resources. }
    class function IsLoaded: Boolean;

    { Get the locale code of the currently loaded resource DLL.
      @return Language or locale code. This is either code of the loaded resource DLL or default language. }
    class function GetActiveLocale: String;

    { Combine primary and sub language ids into a language id.
      @param primaryLanguage Primary language id.
      @param subLanguage     Sub language id.
      @return Language id.
      @seealso(LocaleToPrimary) }
    class function MakeLangId(primaryLanguage, subLanguage: Integer): Integer;

    { Get primary language id part from locale id.
      @param locale Language id.
      @return Primary language id.
      @seealso(MakeLangId)
      @seealso(LocaleToSub) }
    class function LocaleToPrimary(locale: Integer): Integer;

    { Get sub language id part from locale id.
      @param locale Language id.
      @return Sub language id.
      @seealso(MakeLangId)
      @seealso(LocaleToPrimary) }
    class function LocaleToSub(locale: Integer): Integer;

    { Get file name of the currently running application.
      @return Full file name of the application. }
    class function GetRunningFileName: String;

    { Get the resource file name for the given application or library file.
      @param exeFileName Application (.exe) or library (.dll, .bpl) file.
      @param code        Language or locale of the resource file that is needed.
      @return Resource file name. }
    class function GetLanguageFile(const exeFileName: String; const code: String): String;

    { Get the currently loaded resource DLL file of the current application.
      @return Resource DLL file. }
    class function GetCurrentLanguageFile: String;

    { Get the resource instance.
      @return Resource instance. If a resource DLL has been loaded then return the resource handle of that DLL. If no resource DLL has been loaded returns the resource handle of the application itself. }
    class function GetResourceInstance: LongWord;

    { Get the default language of the user.
      @return Language or locale id.
      @seealso(GetSystemLanguage) }
    class function GetUserLanguage: Integer;

    { Get the default language of the system.
      @return Language or locale id.
      @seealso(GetUserLanguage) }
    class function GetSystemLanguage: Integer;

    class function GetDefaultLanguage: String;
    class function GetDefaultLocale: String;

    { Convert locale id into locale code.
      @param locale Language or locale id
      @return Language or locale code. }
    class function LocaleToIsoCode(locale: Integer): String;

    { Convert ISO locale into locale id.
      @param locale Locale code.
      @return Locale id. }
    class function IsoToLocale(const locale: String): Integer;

    { Convert ISO language and country into locale id.
      @param language Language code.
      @param country  Country code.
      @return Locale id. }
    class function IsoLanguageToLocale(const language: String; const country: String = ''): Integer;

    { Convert locale id into Windows language extension.
      This is two or three characters.
      @param locale Locale to be converted.
      @return Extension without period. }
{$IFDEF MSWINDOWS}
    class function LocaleToExtension(locale: Integer): String;
{$ENDIF}
  end;

  { @abstract Static class that contains resource routines. }
  TNtResources = class
  public
    class function DoesExist(
      resType, resName: PChar;
      instance: THandle = 0): Boolean;

    class function GetResourceStream(
      resType, resName: PChar;
      instance: THandle = 0): TResourceStream;

{$IFDEF DELPHI2007}
    class function LoadResource(
      resType, resName: PChar;
      instance: THandle = 0): TBytes;
{$ENDIF}
  end;

  { @abstract Static class that contains registry routines to manipulate locale override values.
    Locale override are registry key entries that specify the default locale of
    a Delphi application. }
  TNtLocaleRegistry = class
  public
    { Save the current locale of the current application as locale override. }
    class procedure SetCurrentDefaultLocale;

    { Get the locale override of the current application.
      @return The locale override value.}
    class function GetCurrentDefaultLocale: String;

    { Clear the locale override value of the current application in the registry.
      @return @true if succesful, @false if failed. }
    class function ClearCurrentDefaultLocale: Boolean;

    { Set the locale override value of the given application.
      @param fileName Application.
      @param code     Locale override value.}
    class procedure SetDefaultLocale(const fileName: String; code: String);

    { Get the locale override value of the given application.
      @param fileName Application.
      @return Locale override value.}
    class function GetDefaultLocale(const fileName: String): String; overload;

    { Get the locale override value of the given application.
      @param fileName Application.
      @param keyExists @true if the locale override value exists, @false if it does not exist.
      @return Locale override value.}
    class function GetDefaultLocale(const fileName: String; out keyExists: Boolean): String; overload;

    { Clear the locale override value of the given application.
      @return @true if succesful, @false if failed. }
    class function ClearDefaultLocale(const fileName: String): Boolean;
  end;

  { @abstract Static class that contains routines to convert between Ansi and Unicode. }
  TNtConvert = class
  public
    { Converts an Ansi string into a Unicode string.
      @param str      Ansi string to be converted.
      @param codePage Code page that the Ansi string uses.
      @return String as a Unicode string. }
    class function AnsiToUnicode(const str: RawByteString; codePage: Integer = 0): UnicodeString;

    { Converts a Unicode string into an Ansi string.
      @param str      Unicode string to be converted.
      @param codePage Code page that the Ansi string uses.
      @return String as an Ansi string. }
    class function UnicodeToAnsi(const str: UnicodeString; codePage: Integer = 0): RawByteString;

{$IFDEF DELPHI2007}
    class function BytesToUnicode(str: TBytes; codePage: Integer = 0): UnicodeString;
    class function UnicodeToBytes(const str: UnicodeString; codePage: Integer = 0): TBytes;

    class function BytesToRawByteString(bytes: TBytes): RawByteString;
    class function RawByteStringToBytes(str: RawByteString): TBytes;
{$ENDIF}
  end;

  { @abstract Abstract extension class. }
  TNtExtension = class(TObject)
  end;

  { Class type of the extension class. }
  TNtExtensionClass = class of TNtExtension;

  { @abstract Class that stores installed extension. }
  TNtExtensions = class(TObject)
  private
{$IFDEF AUTOREFCOUNT}
    FItems: TList<TNtExtension>;
{$ELSE}
    FItems: TList;
{$ENDIF}

    function GetCount: Integer;
    function GetItem(i: Integer): TNtExtension;

    procedure ClearItems;

  protected
    constructor Create; virtual;

  public
    destructor Destroy; override;

    { Registers an extension.
      @param extensionClass Extension class to be registered. }
    procedure Register(extensionClass: TNtExtensionClass);

    { Count of the registered extensions. }
    property Count: Integer read GetCount;
    { Array of the registered extension. }
    property Items[i: Integer]: TNtExtension read GetItem; default;
  end;

{$IFDEF MSWINDOWS}
const
  KERNEL = 'kernel32.dll';

function GetUserDefaultUILanguage: WORD; stdcall; external KERNEL name 'GetUserDefaultUILanguage';
function GetSystemDefaultUILanguage: WORD; stdcall; external KERNEL name 'GetSystemDefaultUILanguage';
{$ENDIF}

var
  ResourceDllDir: String;

  // Currently loaded locale.
  LoadedResourceLocale: String;

  // Previously loaded locale. If empty the current locale is the only locale that has been loaded.
  PreviouslyLoadedResourceLocale: String;

  // Resource options.
  ResourceOptions: TNtResourceOptions;

  { Variable that stores the first day of the week of the current locale. }
  FirstDayOfWeek: TNtDayOfWeek;

  { Variable that stores the first week of the year of the current locale. }
  FirstWeekOfYear: TNtFirstWeekOfYear;

  { Variable that stores current layout of the user interface. }
  UiLayout: TNtLayout;

  { Variable that stores the original language of the application. }
  OriginalLanguage: String;

  { Variable that stores the default language of the application. }
  DefaultLocale: String;

  { Variable that stores the language of the operating system. }
  SystemLanguage: String;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
  Registry,
  NtWindows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Unistd,
  Posix.SysStat,
  Posix.Dlfcn,
{$ENDIF}
{$IFDEF MACOS}
  Macapi.CoreFoundation,
{$ENDIF}
{$IFDEF DELPHI2010}
  IOUtils,
{$ENDIF}
{$IFDEF DELPHIXE}
  NtResource,
{$ENDIF}
  RTLConsts;

var
  FExtractedResourceFiles: TStringList;


// TNtLanguage

function TNtLanguage.GetName(i: TNtLanguageName): String;
begin
  Result := GetDisplayName(FCode, FId, i);
end;

class function TNtLanguage.GetBoth(const native, localize: String): String;
begin
  Result := Format('%s - %s', [native, localize]);
end;

class function TNtLanguage.GetDisplayName(const id: String; locale: Integer; languageName: TNtLanguageName): String;
{$IFDEF DELPHIXE}
  function GetNative: String;
  begin
    if id = OriginalLanguage then
      Result := NtResources.Originals[id]
    else
      Result := NtResources.GetStringInLanguage(id, '', id, '');
  end;

  function GetLocalized: String;
  begin
    if LoadedResourceLocale = '' then
      Result := NtResources.Originals[id]
    else
      Result := NtResources.GetString('', id, '');
  end;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  if NtResources.Enabled then
  begin
    case languageName of
      lnNative: Result := GetNative;
      lnLocalized: Result := GetLocalized;
      lnBoth: Result := GetBoth(GetNative, GetLocalized);
      lnEnglish: Result := NtResources.Originals[id];
      lnSystem: Result := NtResources.GetStringInLanguage(SystemLanguage, '', id, '');
    else
      raise Exception.Create('Not implemented');
    end;
  end
  else
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Result := TNtWindows.GetDisplayName(id, locale, languageName);
{$ENDIF}
  end;
end;


// TNtLanguages

constructor TNtLanguages.Create;
begin
  inherited;
{$IFDEF AUTOREFCOUNT}
  FItems := TList<TNtLanguage>.Create;
{$ELSE}
  FItems := TList.Create;
{$ENDIF}
end;

destructor TNtLanguages.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  while FItems.Count > 0 do
  begin
    TObject(FItems[0]).Free;
    FItems.Delete(0);
  end;
{$ENDIF}

  FItems.Free;

  inherited;
end;

function TNtLanguages.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TNtLanguages.GetItem(i: Integer): TNtLanguage;
begin
  Result := FItems[i];
end;

procedure TNtLanguages.AddDefault;
begin
  Add(DefaultLocale);
end;

function TNtLanguages.Add(const code: String; id: Integer; const fileName: String): TNtLanguage;
begin
  Result := TNtLanguage.Create;
  Result.Code := code;

  if id > 0 then
    Result.Id := id
  else
    Result.Id := TNtBase.IsoToLocale(code);

  Result.FileName := fileName;

  Add(Result);
end;

procedure TNtLanguages.Add(language: TNtLanguage);
begin
  FItems.Add(language);
end;


// TNtBase

class procedure TNtBase.DisableResourceDlls;
begin
  LoadNew('');
end;

{$IFNDEF UNICODE}
function LocaleToCodePage(locale: Integer): Integer;
begin
  if locale = LANG_CHINESE then
    Result := 936
  else
    Result := StrToInt(GetLocaleStr(locale, LOCALE_IDEFAULTANSICODEPAGE, '0'));
end;
{$ENDIF}

class function TNtBase.IsLocaleCompatible(locale: Integer): Boolean;  //FI:O804
begin
{$IFDEF UNICODE}
  Result := True;
{$ELSE}
  Result := LocaleToCodePage(locale) = Integer(GetACP);
{$ENDIF}
end;

class function TNtBase.GetAvailable(
  languages: TNtLanguages;
  exeFileName: String;
  compatibleOnly: Boolean;
  checkVersions: Boolean): Integer;
{$IFDEF DELPHIXE}
var
  i: Integer;
{$ENDIF}
begin
  if exeFileName = '' then
    exeFileName := TNtBase.GetRunningFileName;

{$IFDEF DELPHIXE}
  if NtResources.Count > 0 then
  begin
    for i := 0 to NtResources.Count - 1 do
      languages.Add(NtResources[i].Id);

    Result := NtResources.Count;
  end
  else
{$ENDIF}
  begin
{$IFDEF MSWINDOWS}
    Result := TNtWindows.GetAvailable(languages, exeFileName, compatibleOnly, checkVersions);
{$ENDIF}
{$IFDEF POSIX}
    Result := 0;
{$ENDIF}
  end;
end;

class function TNtBase.GetAvailableCount(
  exeFileName: String;
  compatibleOnly: Boolean;
  checkVersions: Boolean): Integer;
var
  languages: TNtLanguages;
begin
  if exeFileName = '' then
    exeFileName := TNtBase.GetRunningFileName;

  languages := TNtLanguages.Create;
  try
    GetAvailable(languages, exeFileName, compatibleOnly, checkVersions);
    Result := languages.Count;
  finally
    languages.Free;
  end;
end;

class procedure TNtBase.CheckThatDllsExist;
begin
  if TNtBase.GetAvailableCount = 0 then
    raise Exception.Create('There is no available resource file. You must created them before switching a language');
end;

class function TNtBase.GetFolderPath(nFolder: Integer): String;
{$IFDEF MSWINDOWS}
type
  TSHGetFolderPath = function(hwndOwner: HWnd; nFolder: Integer; hToken: THandle; dwFlags: DWord; lpszPath: PChar): HResult; stdcall;
var
  handle: THandle;
  proc: TSHGetFolderPath;
  buffer: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  handle := LoadLibrary('shfolder.dll');
  try
{$IFDEF UNICODE}
    proc := GetProcAddress(handle, 'SHGetFolderPathW');
{$ELSE}
    proc := GetProcAddress(handle, 'SHGetFolderPathA');
{$ENDIF}

    if Assigned(proc) and (proc(0, nFolder, 0, 0, buffer) = S_OK) then
      Result := buffer;
  finally
    FreeLibrary(handle);
  end;
{$ENDIF}
end;

class function TNtBase.GetCurrentLanguageFile: String;
begin
  Result := GetLanguageFile(GetRunningFileName, LoadedResourceLocale);
end;

class function TNtBase.GetLanguageFile(const exeFileName: String; const code: String): String;
begin
  Result := ChangeFileExt(exeFileName, '.' + code);
end;

class function TNtBase.GetRunningFileName: String;
var
  len: Integer;
begin
  SetLength(Result, MAX_PATH);
  len := GetModuleFileName(HInstance, PChar(Result), MAX_PATH);

  if len > 0 then
    SetLength(Result, len)
  else
    Result := '';
end;

class function TNtBase.MakeLangId(primaryLanguage, subLanguage: Integer): Integer;
begin
  Result := (subLanguage shl 10) or primaryLanguage;
end;

class function TNtBase.LocaleToPrimary(locale: Integer): Integer;
begin
  Result := locale and $3FF;
end;

class function TNtBase.LocaleToSub(locale: Integer): Integer;
begin
  Result := (locale shr 10) and $3F;
end;

class procedure TNtBase.ParseLocaleId(
  id: String;  //FI:O801
  var language: String;
  var script: String;
  var country, variant: String);

  function GetString: String;
  var
    p1, p2: Integer;
  begin
    p1 := Pos(LOCALE_SEPARATOR, id);
    p2 := Pos('_', id);

    if (p1 > 0) and ((p2 = 0) or (p1 < p2)) then
    begin
      Result := Copy(id, 1, p1 - 1);
      Delete(id, 1, p1);
    end
    else if (p2 > 0) and ((p1 = 0) or (p2 < p1)) then
    begin
      Result := Copy(id, 1, p2 - 1);
      Delete(id, 1, p2);
    end
    else
    begin
      Result := id;
      id := '';
    end
  end;

  function CharUpperCase(c: Char): Char;
  var
    str: String;
  begin
    str := c;
    Result := UpperCase(str)[1];
  end;

  function MakeCapital(const str: String): String;
  begin
    Result := LowerCase(str);

    if Length(Result) > 0 then
      Result[1] := CharUpperCase(Result[1]);
  end;

var
  str: String;
begin
  // la[-Scri][-CO[-variant]]
  language := LowerCase(GetString);
  str := GetString;

  if Length(str) = 4 then
  begin
    country := GetString;
    variant := GetString;
    script := MakeCapital(str);
  end
  else
  begin
    country := str;
    variant := GetString;
    script := '';
  end;

  country := UpperCase(country);
end;

class procedure TNtBase.ParseLocaleString(
  value: String;  //FI:O801
  var language, country, variant: String);

  function GetString: String;
  var
    p: Integer;
  begin
    p := Pos(LOCALE_SEPARATOR, value);

    if p > 0 then
    begin
      Result := Copy(value, 1, p - 1);
      Delete(value, 1, p);
    end
    else
    begin
      Result := value;
      value := '';
    end;
  end;

begin
  language := GetString;
  country := GetString;
  variant := GetString;
end;

class function TNtBase.IsLoaded: Boolean;
begin
  Result :=
    (LibModuleList <> nil) and
    (LibModuleList.ResInstance <> 0) and
    (LibModuleList.ResInstance <> LibModuleList.Instance);
end;

class function TNtBase.GetActiveLocale: String;
{$IFDEF MSWINDOWS}
  {$IFDEF DELPHI2010}
var
  hostAppName: array [0..MAX_PATH] of Char;
  {$ENDIF}
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  if NtResources.Enabled then
    Result := NtResources.LanguageId
  else
{$ENDIF}
  if IsLoaded then
  begin
{$IFDEF MSWINDOWS}
  {$IFDEF DELPHI2010}
    if LoadedResourceLocale = '' then
    begin
      GetModuleFileName(0, hostAppName, Length(hostAppName));
      LoadedResourceLocale := ExtractFileExt(GetResourceModuleName(hostAppName, hostAppName));
      Delete(LoadedResourceLocale, 1, 1);
    end;
  {$ENDIF}
{$ENDIF}
    Result := LoadedResourceLocale;
  end
  else
    Result := DefaultLocale;
end;

class function TNtBase.GetResourceInstance: LongWord;
begin
  if LibModuleList <> nil then
    Result := LibModuleList.ResInstance
  else
    Result := HInstance;
end;

class function TNtBase.GetDefaultLanguage: String;
begin
{$IFDEF MSWINDOWS}
  Result := GetLocaleStr(GetUserDefaultLCID, LOCALE_SISO639LANGNAME, '');
{$ELSE}
  Result := '';
{$ENDIF}
end;

class function TNtBase.GetDefaultLocale: String;
begin
{$IFDEF MSWINDOWS}
  Result := GetLocaleStr(GetUserDefaultLCID, LOCALE_SISO639LANGNAME, '') + '-' + GetLocaleStr(GetUserDefaultLCID, LOCALE_SISO3166CTRYNAME, '');
{$ELSE}
  Result := '';
{$ENDIF}
end;

class function TNtBase.GetUserLanguage: Integer;
begin
{$IFDEF MSWINDOWS}
  Result := GetUserDefaultUILanguage;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

class function TNtBase.GetSystemLanguage: Integer;
begin
{$IFDEF MSWINDOWS}
  Result := GetSystemDefaultUILanguage;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

class function TNtBase.LocaleToIsoCode(locale: Integer): String;
begin
{$IFDEF MSWINDOWS}
  Result := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '');

  if TNtBase.LocaleToSub(locale) <> SUBLANG_NEUTRAL then
    Result := Result + LOCALE_SEPARATOR + GetLocaleStr(locale, LOCALE_SISO3166CTRYNAME, '');
{$ELSE}
  Result := '';
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
var
  enumLanguage: String;
  enumCountry: String;
  enumLocale: Integer;

function Iso639EnumProc(localeString: PChar): Integer; stdcall;
var
  locale: Integer;
  str, language, country: String;
begin
  str := localeString;
  locale := StrToInt('$' + str);
  Result := 1;

  language := GetLocaleStr(locale, LOCALE_SISO639LANGNAME, '');
  country := GetLocaleStr(locale, LOCALE_SISO3166CTRYNAME, '');

  if (CompareText(language, enumLanguage) = 0) and ((enumCountry = '') or SameText(country, enumCountry)) then
  begin
    if enumCountry = '' then
      enumLocale := TNtBase.LocaleToPrimary(locale)
    else
      enumLocale := locale;

    Result := 0;
  end;
end;
{$ENDIF}

class function TNtBase.IsoLanguageToLocale(
  const language: String;
  const country: String): Integer;
begin
{$IFDEF MSWINDOWS}
  enumLanguage := language;
  enumCountry := country;
  enumLocale := 0;

  EnumSystemLocales(@Iso639EnumProc, LCID_SUPPORTED);

  Result := enumLocale;
{$ELSE}
  Result := 0;
{$ENDIF}
end;

class function TNtBase.IsoToLocale(const locale: String): Integer;
begin
{$IFDEF MSWINDOWS}
  Result := TNtWindows.CodeToId(locale)
{$ELSE}
  Result := 0;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
class function TNtBase.LocaleToExtension(locale: Integer): String;
begin
  if locale = 0 then
    Result := ''
  else
  begin
    Result := GetLocaleStr(locale, LOCALE_SABBREVLANGNAME, '');

    if TNtBase.LocaleToSub(locale) = SUBLANG_NEUTRAL then
      Delete(Result, Length(Result), 1);
  end;
end;
{$ENDIF}

{$IFDEF POSIX}
function LoadModule(moduleName, resModuleName: string; checkOwner: Boolean): LongWord;
var
  st1, st2: _stat;
  moduleFileName, resModuleFileName: UTF8String;
begin
  Result := 0;
  moduleFileName := UTF8Encode(moduleName);

  if checkOwner and (stat(MarshaledAString(moduleFileName), st1) = -1) then
    Exit;

  resModuleFileName := UTF8Encode(resModuleName);

  if (not checkOwner) or
    ((stat(MarshaledAString(resModuleFileName), st2) <> -1) and (st1.st_uid = st2.st_uid) and (st1.st_gid = st2.st_gid)) then
  begin
    Result := dlopen(MarshaledAString(resModuleFileName), RTLD_LAZY);
  end;
end;
{$ENDIF}

class procedure TNtBase.SetResourceDllDir(const value: String; localeSelect: TLocaleSelect);
begin
  ResourceDllDir := value;
  SetInitialLocale(localeSelect);
end;

class procedure TNtBase.SetInitialLocale(
  localeSelect: TLocaleSelect;
  const defaultLocale: String);
{$IFDEF MSWINDOWS}
var
  fileName: String;

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

  procedure SetLocale(const value: String);
  begin
    TNtBase.LoadNew(value);
  end;

  function ProcessLocale(const value: String): Boolean;
  begin
    if TNtBase.ResourceFileExist(fileName, value) or (value = DefaultLocale) then
    begin
      SetLocale(value);
      Result := True;
    end
    else
      Result := False;
  end;

  function ProcessLocales(const value: String): Boolean;
  var
    i: Integer;
    id: String;
    ids: TStringList;
  begin
    ids := TStringList.Create;
    try
      ids.Delimiter := ',';
      ids.DelimitedText := value;

      for i := 0 to ids.Count - 1 do
      begin
        id := ids[i];

        if (id <> '') and (TNtBase.ResourceFileExist(fileName, id) or (id = DefaultLocale)) then
        begin
          SetLocale(id);
          Result := True;
          Exit;
        end
      end;

      Result := False;
    finally
      ids.Free;
    end;
  end;

var
  id, language, country: String;
{$ENDIF}
begin  //FI:C101
{$IFDEF MSWINDOWS}
  fileName := ParamStr(0);

  if defaultLocale <> '' then
    NtBase.DefaultLocale := defaultLocale;
{$IFDEF DELPHI2010}
  // Check if there is a resource DLL matching the locale override.
  id := GetLocaleOverride(fileName);

  if id <> '' then
  begin
    if ResourceDllDir = '' then
      Exit;

    if ProcessLocale(id) then
      Exit;
  end;

  // Check if there is a resource DLL matching the default locale.
  if localeSelect = lsUI then
  begin
    // UI match
    if ProcessLocales(GetUILanguages(GetUserDefaultUILanguage)) then
      Exit;

    if ProcessLocales(GetUILanguages(GetSystemDefaultUILanguage)) then
      Exit;

    id := GetLocaleStr(GetUserDefaultUILanguage, LOCALE_SABBREVLANGNAME, '');

    if ProcessLocale(id) then
      Exit;

    Delete(id, 2, 1);

    if ProcessLocale(id) then
      Exit;
  end
  else
{$ENDIF}
  begin
    // Regionale settings match
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
  end;
{$ENDIF}
end;

class function TNtBase.ResourceFileExist(const fileName, id: String): Boolean;
begin
  Result := GetResourceFile(fileName, id) <> '';
end;

class function TNtBase.GetResourceFile(const fileName, id: String): String;
var
  ext: String;
begin
  ext := '.' + id;

  // Look first from resource DLL dir (if specified)
  if ResourceDllDir <> '' then
  begin
    Result := ResourceDllDir + '\' + ChangeFileExt(ExtractFileName(fileName), ext);

    if FileExists(Result) then
      Exit;

    Result := ExtractFileDir(fileName) + '\' + ResourceDllDir + '\' + ChangeFileExt(ExtractFileName(fileName), ext);

    if FileExists(Result) then
      Exit;
  end;

  // Look then from the application or library dir
  Result := ChangeFileExt(fileName, ext);

  if FileExists(Result) then
    Exit;

  // Look finally from Soluling's personal directory such as C:\Users\<user>\Documents\Soluling
{$IFDEF MSWINDOWS}
  Result := GetFolderPath(CSIDL_PERSONAL) + '\' + APPLICATION_DIR + '\' + ChangeFileExt(ExtractFileName(fileName), ext);

  if FileExists(Result) then
    Exit;
{$ENDIF}

  Result := '';
end;

class function TNtBase.LoadNew(code: String; fileName: String): HModule;

  function LoadResourceDll(const fileName, ext: String): HModule;
  var
    dllFileName: String;
  begin
    dllFileName := GetResourceFile(fileName, ext);

{$IFDEF MSWINDOWS}
    Result := LoadLibraryEx(PChar(dllFileName), 0, LOAD_LIBRARY_AS_DATAFILE);
{$ENDIF}
{$IFDEF POSIX}
    Result := LoadModule(fileName, dllFileName, True);
{$ENDIF}
  end;

var
  p: Integer;
  newInst: HMODULE;
  resourceFileNameGiven: Boolean;
  newResourceLocale: String;
  module: PLibModule;
  buffer: array[0..260] of Char;
begin  //FI:C101
  // ResStringCleanupCache was added in Delphi 10.4.2
{$IF CompilerVersion = 34}
  {$IF Declared(RTLVersion1042)}
  ResStringCleanupCache;
  {$IFEND}
{$IFEND}
{$IFDEF DELPHIDX5}
  ResStringCleanupCache;
{$ENDIF}

  Result := 0;
  module := LibModuleList;
  newResourceLocale := '';

  while module <> nil do
  begin
    if code = '' then
    begin
      if module.ResInstance <> module.Instance then
        FreeLibrary(module.ResInstance);

      module.ResInstance := module.Instance;

      if Result = 0 then
        Result := module.Instance;
    end
    else
    begin
      if fileName = '' then
      begin
        GetModuleFileName(module.Instance, buffer, SizeOf(buffer));
        fileName := buffer;
        resourceFileNameGiven := False;
      end
      else
        resourceFileNameGiven := True;

      if FileExists(fileName) then
      begin
        if resourceFileNameGiven then
          newInst := LoadResourceDll(fileName, '')
        else
        begin
          newInst := LoadResourceDll(fileName, code);

          if newInst = 0 then
          begin
            p := Pos(LOCALE_SEPARATOR, code);

            if p > 0 then
            begin
              Delete(code, p, Length(code));
              newInst := LoadResourceDll(fileName, code);
            end
            else if Length(code) = 3 then
            begin
              Delete(code, Length(code), 1);
              newInst := LoadResourceDll(fileName, code);
            end
          end;
        end;

        if newInst <> 0 then
        begin
          if module.ResInstance <> module.Instance then
            FreeLibrary(module.ResInstance);

          module.ResInstance := newInst;

          if Result = 0 then
            Result := newInst;

          newResourceLocale := code;
        end
        else
        begin
          module.ResInstance := module.Instance;
        end;
      end;
    end;

    module := module.Next;
    fileName := '';
  end;

  PreviouslyLoadedResourceLocale := LoadedResourceLocale;
  LoadedResourceLocale := newResourceLocale;
end;

function GetFileDateTime(const fileName: String): TDateTime;
{$IFNDEF DELPHI2006}
var
  time: Integer;
{$ENDIF}
begin
{$IFDEF DELPHI2006}
  FileAge(fileName, Result);
{$ELSE}
  time := FileAge(fileName);

  if time > 0 then
    Result := FileDateToDateTime(time)
  else
    Result := 0.0;
{$ENDIF}
end;

procedure SetFileDateTime(const fileName: String; date: TDateTime);
begin
  FileSetDate(fileName, DateTimeToFileDate(date));
end;

{$IFDEF MSWINDOWS}
var
  enumResourceFileCount: Integer;
  enumFileName: String;
  enumOptions: TExtractOptions;
  enumFileDate: TDateTime;

function EnumExtractResourceNamesProc(
  instance: THandle;
  resType: PChar;
  resName: PChar;
  param: DWord): Bool; stdcall;  //FI:O804
var
  size: Integer;
  unload: Boolean;
  str, fileName: String;
  pointer: PByte;
  resource: THandle;
  stream: TFileStream;
begin
  str := resName;
  fileName := ChangeFileExt(enumFileName, '.' + str);

  if (not FileExists(fileName) or
    not (eoCheckDate in enumOptions) or
    (GetFileDateTime(fileName) <> enumFileDate)) then
  begin
    unload := resName = LoadedResourceLocale;

    if unload then
      TNtBase.LoadNew;

    resource := FindResource(instance, PChar(resName), PChar(resType));
    pointer := LockResource(LoadResource(instance, resource));
    size := SizeofResource(instance, resource);

    SysUtils.DeleteFile(fileName);

    stream := TFileStream.Create(fileName, fmCreate);
    try
      stream.Write(pointer^, size);
    finally
      stream.Free;
    end;

    SetFileDateTime(fileName, enumFileDate);

    if unload then
      TNtBase.LoadNew(resName);

    if eoRemoveFiles in enumOptions then
      FExtractedResourceFiles.Add(fileName);

    Inc(enumResourceFileCount);
  end;

  Result := True;
end;
{$ENDIF}

class procedure TNtBase.DeleteExtractedFiles;
var
  libModule: PLibModule;
begin
  if (FExtractedResourceFiles <> nil) and (FExtractedResourceFiles.Count > 0) then
  begin
    libModule := LibModuleList;

    while libModule <> nil do
    begin
      if libModule.ResInstance <> libModule.Instance then
        FreeLibrary(libModule.ResInstance);

      libModule.ResInstance := libModule.Instance;
      libModule := libModule.Next;
    end;

    while FExtractedResourceFiles.Count > 0 do
    begin
      SysUtils.DeleteFile(FExtractedResourceFiles[0]);
      FExtractedResourceFiles.Delete(0);
    end;
  end;
end;

class function TNtBase.ExtractFiles(
  options: TExtractOptions;
  resourceName: String): Integer;
{$IFDEF MSWINDOWS}
var
  dir: String;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if HasWriteAccess(ExtractFileDir(TNtBase.GetRunningFileName)) then
    enumFileName := TNtBase.GetRunningFileName
  else
  begin
    dir := TNtBase.GetFolderPath(CSIDL_PERSONAL) + '\' + APPLICATION_DIR;
    CreateDir(dir);
    enumFileName := dir + '\' + ExtractFileName(TNtBase.GetRunningFileName);
  end;

  if resourceName = '' then
    resourceName := APPLICATION_RESOURCE;

  enumResourceFileCount := 0;
  enumFileDate := GetFileDateTime(TNtBase.GetRunningFileName);
  enumOptions := options;

  EnumResourceNames(HInstance, PChar(resourceName), @EnumExtractResourceNamesProc, 0);

  Result := enumResourceFileCount;
{$ENDIF}
{$IFDEF POSIX}
  Result := 0;
{$ENDIF}

  if not TNtBase.IsLoaded or (FExtractedResourceFiles.Count > 0) then
    LoadNew(GetDefaultLocale);
end;

{$IFDEF MSWINDOWS}
class function TNtBase.HasWriteAccess(const dir: String): Boolean;
var
  f: TextFile;
  fileName, parentDir: String;
{$IFNDEF DELPHI2010}
  buffer: array[0..MAX_PATH] of Char;
{$ENDIF}
begin
  try
    if DirectoryExists(dir) then
    begin
{$IFDEF DELPHI2010}
      fileName := TPath.GetTempFileName;
{$ELSE}
      GetTempFileName(PChar(dir), 'nt', 0, buffer);
      fileName := buffer;
{$ENDIF}

      AssignFile(f, fileName);
      Rewrite(f);
      try
        Write(f, 10);
      finally
        Close(f);
        SysUtils.DeleteFile(fileName);
      end;
    end
    else
    begin
      parentDir := ExtractFileDir(dir);
      try
        ChDir(parentDir);
        CreateDir(dir);
      finally
        RemoveDir(dir);
      end;
    end;

    Result := True;
  except
    Result := False;
  end;
end;
{$ENDIF}


// TNtResources

class function TNtResources.DoesExist(
  resType, resName: PChar;
  instance: THandle): Boolean;
begin
  if instance = 0 then
    instance := TNtBase.GetResourceInstance;

  try
    Result := FindResource(instance, resName, resType) <> 0;
  except
    Result := False;
  end;
end;

class function TNtResources.GetResourceStream(
  resType, resName: PChar;
  instance: THandle): TResourceStream;
begin
  if instance = 0 then
    instance := TNtBase.GetResourceInstance;

  if DoesExist(resType, resName, instance) then
    Result := TResourceStream.Create(instance, resName, resType)
  else
    Result := nil;
end;

{$IFDEF DELPHI2007}
class function TNtResources.LoadResource(
  resType, resName: PChar;
  instance: THandle = 0): TBytes;
var
  stream: TResourceStream;
begin
  if instance = 0 then
    instance := TNtBase.GetResourceInstance;

  if DoesExist(resType, resName, instance) then
  begin
    stream := TResourceStream.Create(instance, resName, resType);
    try
      SetLength(Result, stream.Size);
      stream.Read(Result[0], stream.Size);
    finally
      stream.Free;
    end;
  end
  else
    SetLength(Result, 0);
end;
{$ENDIF}


// TNtLocaleRegistry

class procedure TNtLocaleRegistry.SetCurrentDefaultLocale;
var
  fileName: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileName(0, fileName, SizeOf(fileName)) > 0 then
    SetDefaultLocale(fileName, LoadedResourceLocale);
end;

class function TNtLocaleRegistry.GetCurrentDefaultLocale: String;
var
  fileName: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileName(0, fileName, SizeOf(fileName)) > 0 then
    Result := GetDefaultLocale(fileName)
  else
    Result := '';
end;

class procedure TNtLocaleRegistry.SetDefaultLocale(const fileName: String; code: String);
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(LOCALE_OVERRIDE_KEY, True) then
    begin
      if code = '' then
        code := ' ';

      reg.WriteString(fileName, code);
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
end;

class function TNtLocaleRegistry.GetDefaultLocale(const fileName: String): String;
{$IFNDEF DELPHIXE}
var
  keyExists: Boolean;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  Result := GetLocaleOverride(fileName);
{$ELSE}
  Result := GetDefaultLocale(fileName, keyExists);
{$ENDIF}
end;

class function TNtLocaleRegistry.GetDefaultLocale(const fileName: String; out keyExists: Boolean): String;
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;

  function Open(const key: String): Boolean;
  begin
    if key <> '' then
    begin
      reg.RootKey := HKEY_CURRENT_USER;
      Result := reg.OpenKey(key, False);

      if not Result then
      begin
        reg.RootKey := HKEY_LOCAL_MACHINE;
        Result := reg.OpenKey(key, False);
      end;
    end
    else
      Result := False;
  end;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := '';
  keyExists := False;

  reg := TRegistry.Create;
  try
    if Open(LOCALE_OVERRIDE_KEY) or
      Open(OLD_LOCALE_OVERRIDE_KEY) or
      Open(OLDEST_LOCALE_OVERRIDE_KEY) then
    begin
      if reg.ValueExists(fileName) then
      begin
        Result := reg.ReadString(fileName);
        keyExists := True;
      end;
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
end;

class function TNtLocaleRegistry.ClearCurrentDefaultLocale: Boolean;
var
  fileName: array[0..MAX_PATH] of Char;
begin
  if GetModuleFileName(0, fileName, SizeOf(fileName)) = 0 then
    Result := False
  else
    Result := ClearDefaultLocale(fileName);
end;

class function TNtLocaleRegistry.ClearDefaultLocale(const fileName: String): Boolean;
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;
{$ENDIF}
begin
  Result := False;
{$IFDEF MSWINDOWS}
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;

    if reg.OpenKey(LOCALE_OVERRIDE_KEY, False) then
    begin
      Result := reg.ValueExists(fileName);
      reg.DeleteValue(fileName);
    end;
  finally
    reg.Free;
  end;
{$ENDIF}
end;


// TNtConvert

class function TNtConvert.AnsiToUnicode(const str: RawByteString; codePage: Integer): UnicodeString;
var
  len: Integer;
begin
{$IFDEF UNICODE}
  if codePage = 0 then
    codePage := StringCodePage(str);

  if codePage = 0 then
    codePage := DefaultSystemCodePage;
{$ENDIF}

  if str = '' then
    Result := ''
  else
  begin
{$IFDEF DELPHIXE3}
    len := UnicodeFromLocaleChars(codePage, 0, MarshaledAString(str), -1, nil, 0);
{$ELSE}
    len := MultiByteToWideChar(codePage, 0, PAnsiChar(str), -1, nil, 0);
{$ENDIF}

    if len > 1 then
    begin
      SetLength(Result, len - 1);
{$IFDEF DELPHIXE3}
      UnicodeFromLocaleChars(codePage, 0, MarshaledAString(str), -1, PWideChar(Result), len);
{$ELSE}
      MultiByteToWideChar(codePage, 0, PAnsiChar(str), -1, PWideChar(Result), len);
{$ENDIF}
    end
    else
      Result := '';
  end;
end;

class function TNtConvert.UnicodeToAnsi(const str: UnicodeString; codePage: Integer): RawByteString;
var
  len: Integer;
begin
{$IFDEF UNICODE}
  if codePage = 0 then
    codePage := DefaultSystemCodePage;
{$ENDIF}

  if str = '' then
    Result := ''
  else
  begin
{$IFDEF DELPHIXE3}
    len := LocaleCharsFromUnicode(codePage, 0, PWideChar(str), -1, nil, 0, nil, nil);
{$ELSE}
    len := WideCharToMultiByte(codePage, 0, PWideChar(str), -1, nil, 0, nil, nil);
{$ENDIF}

    if len > 1 then
    begin
      SetLength(Result, len - 1);
{$IFDEF DELPHIXE3}
      LocaleCharsFromUnicode(codePage, 0, PWideChar(str), -1, MarshaledAString(Result), len, nil, nil);
{$ELSE}
      WideCharToMultiByte(codePage, 0, PWideChar(str), -1, PAnsiChar(Result), len, nil, nil);
{$ENDIF}
    end
    else
      Result := '';
  end;

{$IFDEF UNICODE}
  SetCodePage(Result, codePage, False);
{$ENDIF}
end;

{$IFDEF DELPHI2007}
class function TNtConvert.BytesToRawByteString(bytes: TBytes): RawByteString;
begin
  SetLength(Result, Length(bytes));
  Move(bytes[0], Result[1], Length(bytes));
end;

class function TNtConvert.RawByteStringToBytes(str: RawByteString): TBytes;
begin
  SetLength(Result, Length(str));
  Move(str[1], Result[0], Length(str));
end;

class function TNtConvert.BytesToUnicode(str: TBytes; codePage: Integer = 0): UnicodeString;
begin
  Result := AnsiToUnicode(BytesToRawByteString(str), codePage);
end;

class function TNtConvert.UnicodeToBytes(const str: UnicodeString; codePage: Integer = 0): TBytes;
begin
  Result := RawByteStringToBytes(UnicodeToAnsi(str, codePage));
end;
{$ENDIF}


// TNtExtensions

constructor TNtExtensions.Create;
begin
  inherited;
{$IFDEF AUTOREFCOUNT}
  FItems := TList<TNtExtension>.Create;
{$ELSE}
  FItems := TList.Create;
{$ENDIF}
end;

destructor TNtExtensions.Destroy;
begin
  ClearItems;
  FItems.Free;
  inherited;
end;

procedure TNtExtensions.ClearItems;
begin
{$IFDEF AUTOREFCOUNT}
  FItems.Clear;
{$ELSE}
  while FItems.Count > 0 do
  begin
    TObject(FItems[0]).Free;
    FItems.Delete(0);
  end;
{$ENDIF}
end;

function TNtExtensions.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TNtExtensions.GetItem(i: Integer): TNtExtension;
begin
  Result := FItems[i];
end;

procedure TNtExtensions.Register(extensionClass: TNtExtensionClass);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].ClassType = extensionClass then
      Exit;

  FItems.Add(extensionClass.Create);
end;


initialization
  FExtractedResourceFiles := TStringList.Create;

  // Workaround for the resource string cache bug of Delphi 10.4.1
{$IF CompilerVersion = 34}
  {$IF Declared(RTLVersion1041)}
    {$IF not Declared(RTLVersion1042)}
    LoadResStringFunc := nil;
    {$IFEND}
  {$IFEND}
{$IFEND}

  UiLayout := laLeftToRight;
  DefaultLocale := 'en';
  OriginalLanguage := 'en';
finalization
  TNtBase.DeleteExtractedFiles;
  FExtractedResourceFiles.Free;
end.
