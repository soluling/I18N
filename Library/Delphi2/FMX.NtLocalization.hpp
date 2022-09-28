// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.NtLocalization.pas' rev: 35.00 (iOS)

#ifndef Fmx_NtlocalizationHPP
#define Fmx_NtlocalizationHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <NtBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Ntlocalization
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtLocale;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TNtLocale : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	static bool __fastcall IsPreviousLocaleBidi();
	static int __fastcall ExtensionToLocale(const System::UnicodeString value);
	static System::UnicodeString __fastcall GetDefaultLanguage();
public:
	/* TObject.Create */ inline __fastcall TNtLocale() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtLocale() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntlocalization */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_NTLOCALIZATION)
using namespace Fmx::Ntlocalization;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_NtlocalizationHPP
