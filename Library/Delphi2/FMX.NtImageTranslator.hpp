// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.NtImageTranslator.pas' rev: 35.00 (iOS)

#ifndef Fmx_NtimagetranslatorHPP
#define Fmx_NtimagetranslatorHPP

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

namespace Fmx
{
namespace Ntimagetranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtImageTranslator;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TNtImageTranslator : public Ntbasetranslator::TNtTranslatorExtension
{
	typedef Ntbasetranslator::TNtTranslatorExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj);
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index);
public:
	/* TObject.Create */ inline __fastcall TNtImageTranslator() : Ntbasetranslator::TNtTranslatorExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtImageTranslator() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntimagetranslator */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_NTIMAGETRANSLATOR)
using namespace Fmx::Ntimagetranslator;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_NtimagetranslatorHPP
