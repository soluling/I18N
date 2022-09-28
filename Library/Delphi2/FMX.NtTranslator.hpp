// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.NtTranslator.pas' rev: 35.00 (iOS)

#ifndef Fmx_NttranslatorHPP
#define Fmx_NttranslatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Types.hpp>
#include <NtBase.hpp>
#include <NtBaseTranslator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Nttranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtTranslator;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TNtTranslator : public Ntbasetranslator::TNtBaseTranslator
{
	typedef Ntbasetranslator::TNtBaseTranslator inherited;
	
private:
	void __fastcall TranslateForm(Fmx::Forms::TCommonCustomForm* form);
	void __fastcall TranslateDelta(Fmx::Types::TFmxObject* component);
	
protected:
	virtual void __fastcall AfterProcessComponent(System::Classes::TComponent* component);
	virtual void __fastcall Translate(System::Classes::TComponent* component);
	
public:
	__classmethod bool __fastcall SetNew(const System::UnicodeString code = System::UnicodeString(), Ntbase::TNtResourceOptions options = Ntbase::TNtResourceOptions() , const System::UnicodeString originalCode = System::UnicodeString());
	__classmethod void __fastcall TranslateForms();
	__classmethod int __fastcall GetDevices(Fmx::Types::TFmxObject* component, System::Classes::TStrings* devices);
	__classmethod System::UnicodeString __fastcall GetDevice(Fmx::Types::TFmxObject* component);
protected:
	/* TNtBaseTranslator.Create */ inline __fastcall TNtTranslator() : Ntbasetranslator::TNtBaseTranslator() { }
	
public:
	/* TNtBaseTranslator.Destroy */ inline __fastcall virtual ~TNtTranslator() { }
	
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall _T(Fmx::Forms::TCustomForm* form)/* overload */;
extern DELPHI_PACKAGE void __fastcall Translate(Fmx::Forms::TCustomForm* form)/* overload */;
}	/* namespace Nttranslator */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_NTTRANSLATOR)
using namespace Fmx::Nttranslator;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_NttranslatorHPP
