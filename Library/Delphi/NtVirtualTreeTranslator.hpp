// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtVirtualTreeTranslator.pas' rev: 33.00 (Windows)

#ifndef NtvirtualtreetranslatorHPP
#define NtvirtualtreetranslatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <NtBaseTranslator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntvirtualtreetranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtVirtualTreeViewTranslator;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtVirtualTreeViewTranslator : public Ntbasetranslator::TNtTranslatorExtension
{
	typedef Ntbasetranslator::TNtTranslatorExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj);
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index);
	virtual System::TObject* __fastcall GetActualObject(System::TObject* obj, const System::UnicodeString propName);
	virtual System::UnicodeString __fastcall GetActualName(System::TObject* obj, const System::UnicodeString propName);
public:
	/* TObject.Create */ inline __fastcall TNtVirtualTreeViewTranslator() : Ntbasetranslator::TNtTranslatorExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtVirtualTreeViewTranslator() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntvirtualtreetranslator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTVIRTUALTREETRANSLATOR)
using namespace Ntvirtualtreetranslator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtvirtualtreetranslatorHPP
