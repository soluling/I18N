// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtBase.pas' rev: 35.00 (Windows)

#ifndef NtbaseHPP
#define NtbaseHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntbase
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtLanguage;
class DELPHICLASS TNtLanguages;
class DELPHICLASS TNtBase;
class DELPHICLASS TNtResources;
class DELPHICLASS TNtLocaleRegistry;
class DELPHICLASS TNtConvert;
class DELPHICLASS TNtExtension;
class DELPHICLASS TNtExtensions;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNtDayOfWeek : unsigned char { wdMonday, wdTuesday, wdWednesday, wdThursday, wdFriday, wdSaturday, wdSunday };

enum DECLSPEC_DENUM TNtFirstWeekOfYear : unsigned char { fwFirstPart, fwFirstFull, fwFirst4 };

enum DECLSPEC_DENUM TNtLayout : unsigned char { laLeftToRight, laRightToLeft };

enum DECLSPEC_DENUM TNtResourceOption : unsigned char { roNoThreadLocale, roNoLocaleVariables, roNoUpdateBidiMode, roFlipChildren, roSaveLocale };

typedef System::Set<TNtResourceOption, TNtResourceOption::roNoThreadLocale, TNtResourceOption::roSaveLocale> TNtResourceOptions;

enum DECLSPEC_DENUM TExtractOption : unsigned char { eoCheckDate, eoRemoveFiles };

typedef System::Set<TExtractOption, TExtractOption::eoCheckDate, TExtractOption::eoRemoveFiles> TExtractOptions;

enum DECLSPEC_DENUM TNtLanguageName : unsigned char { lnNative, lnLocalized, lnBoth, lnEnglish, lnSystem };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtLanguage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FCode;
	int FId;
	System::UnicodeString FFileName;
	System::UnicodeString __fastcall GetName(TNtLanguageName i);
	
public:
	__classmethod System::UnicodeString __fastcall GetBoth(const System::UnicodeString native, const System::UnicodeString localize);
	__classmethod System::UnicodeString __fastcall GetDisplayName(const System::UnicodeString id, int locale = 0x0, TNtLanguageName languageName = (TNtLanguageName)(0x4));
	__property System::UnicodeString Code = {read=FCode, write=FCode};
	__property int Id = {read=FId, write=FId, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property System::UnicodeString NativeName = {read=GetName, index=0};
	__property System::UnicodeString LocalizedName = {read=GetName, index=1};
	__property System::UnicodeString EnglishName = {read=GetName, index=3};
	__property System::UnicodeString SystemName = {read=GetName, index=4};
	__property System::UnicodeString Names[TNtLanguageName i] = {read=GetName};
public:
	/* TObject.Create */ inline __fastcall TNtLanguage() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtLanguage() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TLocaleSelect : unsigned char { lsUI, lsSettings };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtLanguages : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TNtLanguage* operator[](int i) { return this->Items[i]; }
	
private:
	System::Classes::TList* FItems;
	int __fastcall GetCount();
	TNtLanguage* __fastcall GetItem(int i);
	
public:
	__fastcall TNtLanguages();
	__fastcall virtual ~TNtLanguages();
	TNtLanguage* __fastcall Add(const System::UnicodeString code, int id = 0x0, const System::UnicodeString fileName = System::UnicodeString())/* overload */;
	void __fastcall Add(TNtLanguage* language)/* overload */;
	void __fastcall AddDefault();
	__property int Count = {read=GetCount, nodefault};
	__property TNtLanguage* Items[int i] = {read=GetItem/*, default*/};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtBase : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__classmethod void __fastcall DeleteExtractedFiles();
	__classmethod bool __fastcall HasWriteAccess(const System::UnicodeString dir);
	
public:
	__classmethod void __fastcall CheckThatDllsExist();
	__classmethod bool __fastcall IsLocaleCompatible(int locale);
	__classmethod int __fastcall GetAvailable(TNtLanguages* languages, System::UnicodeString exeFileName = System::UnicodeString(), bool compatibleOnly = false, bool checkVersions = false);
	__classmethod int __fastcall GetAvailableCount(System::UnicodeString exeFileName = System::UnicodeString(), bool compatibleOnly = false, bool checkVersions = false);
	__classmethod System::UnicodeString __fastcall GetResourceFile(const System::UnicodeString fileName, const System::UnicodeString id);
	__classmethod bool __fastcall ResourceFileExist(const System::UnicodeString fileName, const System::UnicodeString id);
	__classmethod void __fastcall SetInitialLocale(TLocaleSelect localeSelect, const System::UnicodeString defaultLocale = System::UnicodeString());
	__classmethod void __fastcall SetResourceDllDir(const System::UnicodeString value, TLocaleSelect localeSelect = (TLocaleSelect)(0x0));
	__classmethod NativeUInt __fastcall LoadNew(System::UnicodeString code = System::UnicodeString(), System::UnicodeString fileName = System::UnicodeString());
	__classmethod int __fastcall ExtractFiles(TExtractOptions options = (TExtractOptions() << TExtractOption::eoCheckDate ), System::UnicodeString resourceName = System::UnicodeString());
	__classmethod void __fastcall DisableResourceDlls();
	__classmethod System::UnicodeString __fastcall GetFolderPath(int nFolder);
	__classmethod void __fastcall ParseLocaleString(System::UnicodeString value, System::UnicodeString &language, System::UnicodeString &country, System::UnicodeString &variant);
	__classmethod void __fastcall ParseLocaleId(System::UnicodeString id, System::UnicodeString &language, System::UnicodeString &script, System::UnicodeString &country, System::UnicodeString &variant);
	__classmethod bool __fastcall IsLoaded();
	__classmethod System::UnicodeString __fastcall GetActiveLocale();
	__classmethod int __fastcall MakeLangId(int primaryLanguage, int subLanguage);
	__classmethod int __fastcall LocaleToPrimary(int locale);
	__classmethod int __fastcall LocaleToSub(int locale);
	__classmethod System::UnicodeString __fastcall GetRunningFileName();
	__classmethod System::UnicodeString __fastcall GetLanguageFile(const System::UnicodeString exeFileName, const System::UnicodeString code);
	__classmethod System::UnicodeString __fastcall GetCurrentLanguageFile();
	__classmethod unsigned __fastcall GetResourceInstance();
	__classmethod int __fastcall GetUserLanguage();
	__classmethod int __fastcall GetSystemLanguage();
	__classmethod System::UnicodeString __fastcall GetDefaultLanguage();
	__classmethod System::UnicodeString __fastcall GetDefaultLocale();
	__classmethod System::UnicodeString __fastcall LocaleToIsoCode(int locale);
	__classmethod int __fastcall IsoToLocale(const System::UnicodeString locale);
	__classmethod int __fastcall IsoLanguageToLocale(const System::UnicodeString language, const System::UnicodeString country = System::UnicodeString());
	__classmethod System::UnicodeString __fastcall LocaleToExtension(int locale);
public:
	/* TObject.Create */ inline __fastcall TNtBase() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtBase() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtResources : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall DoesExist(System::WideChar * resType, System::WideChar * resName, NativeUInt instance = (NativeUInt)(0x0));
	__classmethod System::Classes::TResourceStream* __fastcall GetResourceStream(System::WideChar * resType, System::WideChar * resName, NativeUInt instance = (NativeUInt)(0x0));
	__classmethod System::DynamicArray<System::Byte> __fastcall LoadResource(System::WideChar * resType, System::WideChar * resName, NativeUInt instance = (NativeUInt)(0x0));
public:
	/* TObject.Create */ inline __fastcall TNtResources() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtResources() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtLocaleRegistry : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall SetCurrentDefaultLocale();
	__classmethod System::UnicodeString __fastcall GetCurrentDefaultLocale();
	__classmethod bool __fastcall ClearCurrentDefaultLocale();
	__classmethod void __fastcall SetDefaultLocale(const System::UnicodeString fileName, System::UnicodeString code);
	__classmethod System::UnicodeString __fastcall GetDefaultLocale(const System::UnicodeString fileName)/* overload */;
	__classmethod System::UnicodeString __fastcall GetDefaultLocale(const System::UnicodeString fileName, /* out */ bool &keyExists)/* overload */;
	__classmethod bool __fastcall ClearDefaultLocale(const System::UnicodeString fileName);
public:
	/* TObject.Create */ inline __fastcall TNtLocaleRegistry() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtLocaleRegistry() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtConvert : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::UnicodeString __fastcall AnsiToUnicode(const System::RawByteString str, int codePage = 0x0);
	__classmethod System::RawByteString __fastcall UnicodeToAnsi(const System::UnicodeString str, int codePage = 0x0);
	__classmethod System::UnicodeString __fastcall BytesToUnicode(System::DynamicArray<System::Byte> str, int codePage = 0x0);
	__classmethod System::DynamicArray<System::Byte> __fastcall UnicodeToBytes(const System::UnicodeString str, int codePage = 0x0);
	__classmethod System::RawByteString __fastcall BytesToRawByteString(System::DynamicArray<System::Byte> bytes);
	__classmethod System::DynamicArray<System::Byte> __fastcall RawByteStringToBytes(System::RawByteString str);
public:
	/* TObject.Create */ inline __fastcall TNtConvert() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtConvert() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtExtension : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	/* TObject.Create */ inline __fastcall TNtExtension() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtExtension() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TNtExtensionClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtExtensions : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TNtExtension* operator[](int i) { return this->Items[i]; }
	
private:
	System::Classes::TList* FItems;
	int __fastcall GetCount();
	TNtExtension* __fastcall GetItem(int i);
	void __fastcall ClearItems();
	
protected:
	__fastcall virtual TNtExtensions();
	
public:
	__fastcall virtual ~TNtExtensions();
	void __fastcall Register(TNtExtensionClass extensionClass);
	__property int Count = {read=GetCount, nodefault};
	__property TNtExtension* Items[int i] = {read=GetItem/*, default*/};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define APPLICATION_RESOURCE L"SOLULING"
#define APPLICATION_DIR L"Soluling"
static const System::WideChar LOCALE_SEPARATOR = (System::WideChar)(0x2d);
static const System::Int8 LOCALE_ALL = System::Int8(0x0);
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 4> VCL_FORM_HEADER;
static const System::Int8 CSIDL_PERSONAL = System::Int8(0x5);
#define LOCALE_OVERRIDE_KEY L"Software\\Embarcadero\\Locales"
#define OLD_LOCALE_OVERRIDE_KEY L"Software\\CodeGear\\Locales"
#define OLDEST_LOCALE_OVERRIDE_KEY L"Software\\Borland\\Locales"
#define KERNEL L"kernel32.dll"
extern "C" System::Word __stdcall GetUserDefaultUILanguage();
extern "C" System::Word __stdcall GetSystemDefaultUILanguage();
extern DELPHI_PACKAGE System::UnicodeString ResourceDllDir;
extern DELPHI_PACKAGE System::UnicodeString LoadedResourceLocale;
extern DELPHI_PACKAGE System::UnicodeString PreviouslyLoadedResourceLocale;
extern DELPHI_PACKAGE TNtResourceOptions ResourceOptions;
extern DELPHI_PACKAGE TNtDayOfWeek FirstDayOfWeek;
extern DELPHI_PACKAGE TNtFirstWeekOfYear FirstWeekOfYear;
extern DELPHI_PACKAGE TNtLayout UiLayout;
extern DELPHI_PACKAGE System::UnicodeString OriginalLanguage;
extern DELPHI_PACKAGE System::UnicodeString DefaultLocale;
extern DELPHI_PACKAGE System::UnicodeString SystemLanguage;
}	/* namespace Ntbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTBASE)
using namespace Ntbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtbaseHPP
