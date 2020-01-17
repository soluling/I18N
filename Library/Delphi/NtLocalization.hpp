// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtLocalization.pas' rev: 33.00 (Windows)

#ifndef NtlocalizationHPP
#define NtlocalizationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <NtBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntlocalization
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtResource;
class DELPHICLASS TNtLocale;
struct TNtProcedureData;
class DELPHICLASS TNtMap;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtResource : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod System::UnicodeString __fastcall GetActiveLocale();
	__classmethod bool __fastcall DoesVersionMatch()/* overload */;
	__classmethod bool __fastcall DoesVersionMatch(unsigned resInstance)/* overload */;
	__classmethod bool __fastcall DoesLocaleVersionMatch(const System::UnicodeString code);
	__classmethod bool __fastcall DoesLocaleVersionMatchFile(const System::UnicodeString fileName);
public:
	/* TObject.Create */ inline __fastcall TNtResource() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtResource() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtLocale : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	__classmethod System::UnicodeString __fastcall GetPreviousLocale();
	
public:
	__classmethod void __fastcall CheckLocaleVariables();
	__classmethod void __fastcall UpdateFormatSettings(System::UnicodeString locale)/* overload */;
	__classmethod void __fastcall UpdateFormatSettings(int locale)/* overload */;
	__classmethod int __fastcall ExtensionToLocale(const System::UnicodeString value);
	__classmethod int __fastcall LocaleToCodePage(int locale);
	__classmethod System::UnicodeString __fastcall LocaleToIso639(int locale);
	__classmethod bool __fastcall IsActiveLocaleAsian();
	__classmethod bool __fastcall IsActiveLocaleBidi();
	__classmethod bool __fastcall IsPreviousLocaleBidi();
	__classmethod int __fastcall GetFormLocale();
	__classmethod bool __fastcall IsLocaleAsian(System::UnicodeString value);
	__classmethod bool __fastcall IsLocaleBidi(System::UnicodeString value);
	__classmethod System::UnicodeString __fastcall LocaleToIso(int locale);
public:
	/* TObject.Create */ inline __fastcall TNtLocale() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtLocale() { }
	
};

#pragma pack(pop)

struct DECLSPEC_DRECORD TNtProcedureData
{
public:
	void *Address;
	System::StaticArray<System::Byte, 7> Data;
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtMap : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall RemapProcedure(void * oldProc, void * newProc, TNtProcedureData &data);
	__classmethod void __fastcall RestoreProcedure(const TNtProcedureData &data);
public:
	/* TObject.Create */ inline __fastcall TNtMap() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtMap() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 PROCEDURE_DATA_SIZE = System::Int8(0x6);
}	/* namespace Ntlocalization */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTLOCALIZATION)
using namespace Ntlocalization;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtlocalizationHPP
