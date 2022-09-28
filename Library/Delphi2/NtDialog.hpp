// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtDialog.pas' rev: 35.00 (Windows)

#ifndef NtdialogHPP
#define NtdialogHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntdialog
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtDialogFilter;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNtDialogFilterPosition : unsigned char { fpNone, fpFirst, fpLast };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtDialogFilter : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TNtDialogFilterPosition FAllSupportedPosition;
	System::UnicodeString FFirstPart;
	System::UnicodeString FSecondPart;
	System::UnicodeString FAllSupportedPattern;
	System::Classes::TStringList* FMasks;
	System::UnicodeString __fastcall GetValue();
	void __fastcall DoAdd(const System::UnicodeString value);
	
public:
	__fastcall TNtDialogFilter();
	__fastcall virtual ~TNtDialogFilter();
	void __fastcall All(const System::UnicodeString pattern);
	void __fastcall Supported(const System::UnicodeString pattern);
	void __fastcall Add(const System::UnicodeString pattern, System::UnicodeString mask);
	__property System::UnicodeString Value = {read=GetValue};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntdialog */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTDIALOG)
using namespace Ntdialog;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtdialogHPP
