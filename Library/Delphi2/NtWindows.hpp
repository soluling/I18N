// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtWindows.pas' rev: 35.00 (Windows)

#ifndef NtwindowsHPP
#define NtwindowsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <NtBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntwindows
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtWindows;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtWindows : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall IsNt();
	__classmethod bool __fastcall IsVista();
	__classmethod bool __fastcall Is7();
	__classmethod bool __fastcall Is8();
	__classmethod bool __fastcall Is10();
	__classmethod bool __fastcall IsSubPath(const System::UnicodeString fileName);
	__classmethod int __fastcall CodeToId(const System::UnicodeString code);
	__classmethod System::UnicodeString __fastcall GetLocaleStr(const System::UnicodeString id, int locale, int localeType, const System::UnicodeString Default);
	__classmethod System::UnicodeString __fastcall GetDisplayName(const System::UnicodeString id, int locale, Ntbase::TNtLanguageName languageName);
	__classmethod int __fastcall GetAvailable(Ntbase::TNtLanguages* languages, System::UnicodeString exeFileName, bool compatibleOnly, bool checkVersions, const System::UnicodeString dir = System::UnicodeString());
	__classmethod System::UnicodeString __fastcall GetVariantName(const System::UnicodeString language, const System::UnicodeString variant);
public:
	/* TObject.Create */ inline __fastcall TNtWindows() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtWindows() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntwindows */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTWINDOWS)
using namespace Ntwindows;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtwindowsHPP
