// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtOrdinal.pas' rev: 33.00 (Windows)

#ifndef NtordinalHPP
#define NtordinalHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <NtPattern.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntordinal
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtOrdinal;
//-- type declarations -------------------------------------------------------
typedef int TOrdinal;

typedef System::Int8 TSmallOrdinal;

enum DECLSPEC_DENUM TOrdinalForm : unsigned char { ofShort, ofLong };

typedef System::UnicodeString __fastcall (*TOrdinalShortProc)(TOrdinal ordinal, Ntpattern::TPlural plural, Ntpattern::TGender gender);

typedef System::UnicodeString __fastcall (*TOrdinalLongProc)(TSmallOrdinal ordinal, Ntpattern::TPlural plural, Ntpattern::TGender gender);

typedef System::StaticArray<System::UnicodeString, 10> TOrdinalArray;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtOrdinal : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall IsShortOrdinal(TOrdinal value);
	__classmethod System::UnicodeString __fastcall Format(TOrdinalForm form, TOrdinal ordinal, Ntpattern::TPlural plural = (Ntpattern::TPlural)(0x1), Ntpattern::TGender gender = (Ntpattern::TGender)(0x2));
	__classmethod System::UnicodeString __fastcall FormatShort(TOrdinal ordinal, Ntpattern::TPlural plural = (Ntpattern::TPlural)(0x1), Ntpattern::TGender gender = (Ntpattern::TGender)(0x2));
	__classmethod System::UnicodeString __fastcall FormatLong(TOrdinal ordinal, Ntpattern::TPlural plural = (Ntpattern::TPlural)(0x1), Ntpattern::TGender gender = (Ntpattern::TGender)(0x2));
	__classmethod void __fastcall Register(const System::UnicodeString id, TOrdinalLongProc longProc, TOrdinalShortProc shortProc);
public:
	/* TObject.Create */ inline __fastcall TNtOrdinal() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtOrdinal() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntordinal */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTORDINAL)
using namespace Ntordinal;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtordinalHPP
