// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtListViewTranslator.pas' rev: 35.00 (Windows)

#ifndef NtlistviewtranslatorHPP
#define NtlistviewtranslatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <NtBaseTranslator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntlistviewtranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtListViewTranslator;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtListViewTranslator : public Ntbasetranslator::TNtTranslatorExtension
{
	typedef Ntbasetranslator::TNtTranslatorExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj);
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index);
public:
	/* TObject.Create */ inline __fastcall TNtListViewTranslator() : Ntbasetranslator::TNtTranslatorExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtListViewTranslator() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntlistviewtranslator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTLISTVIEWTRANSLATOR)
using namespace Ntlistviewtranslator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtlistviewtranslatorHPP
