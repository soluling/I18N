// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtNumber.pas' rev: 33.00 (Windows)

#ifndef NtnumberHPP
#define NtnumberHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <NtPattern.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntnumber
{
//-- forward type declarations -----------------------------------------------
struct TAbbreviationRule;
class DELPHICLASS TAbbreviatedNumber;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TAbbreviatedNumberForm : unsigned char { anLong, anShort, anCurrency };

struct DECLSPEC_DRECORD TAbbreviationRule
{
public:
	unsigned __int64 Range;
	Ntpattern::TPlural Plural;
	System::UnicodeString Value;
};


typedef System::DynamicArray<TAbbreviationRule> TAbbreviationRules;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TAbbreviatedNumber : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod bool __fastcall CanBeAbbreviated(TAbbreviatedNumberForm form, double value);
	__classmethod System::UnicodeString __fastcall Format(TAbbreviatedNumberForm form, double value, int precision = 0x2);
	__classmethod System::UnicodeString __fastcall FormatLong(double value, int precision = 0x2);
	__classmethod System::UnicodeString __fastcall FormatShort(double value, int precision = 0x2);
	__classmethod System::UnicodeString __fastcall FormatCurrency(double value, int precision = 0x2);
	__classmethod void __fastcall Register(const System::UnicodeString id, TAbbreviationRule *longRules, const int longRules_High, TAbbreviationRule *shortRules, const int shortRules_High, TAbbreviationRule *currencyRules, const int currencyRules_High);
public:
	/* TObject.Create */ inline __fastcall TAbbreviatedNumber() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TAbbreviatedNumber() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 DEFAULT_PRECISION = System::Int8(0x2);
}	/* namespace Ntnumber */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTNUMBER)
using namespace Ntnumber;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtnumberHPP
