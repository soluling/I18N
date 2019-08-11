// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtTreeViewTranslator.pas' rev: 33.00 (Windows)

#ifndef NttreeviewtranslatorHPP
#define NttreeviewtranslatorHPP

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

namespace Nttreeviewtranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtTreeViewTranslator;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtTreeViewTranslator : public Ntbasetranslator::TNtTranslatorExtension
{
	typedef Ntbasetranslator::TNtTranslatorExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj);
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index);
public:
	/* TObject.Create */ inline __fastcall TNtTreeViewTranslator() : Ntbasetranslator::TNtTranslatorExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtTreeViewTranslator() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Nttreeviewtranslator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTTREEVIEWTRANSLATOR)
using namespace Nttreeviewtranslator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NttreeviewtranslatorHPP
