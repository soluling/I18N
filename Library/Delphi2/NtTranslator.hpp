// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtTranslator.pas' rev: 35.00 (Windows)

#ifndef NttranslatorHPP
#define NttranslatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Forms.hpp>
#include <NtBase.hpp>
#include <NtBaseTranslator.hpp>

//-- user supplied -----------------------------------------------------------

namespace Nttranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtTranslator;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtTranslator : public Ntbasetranslator::TNtBaseTranslator
{
	typedef Ntbasetranslator::TNtBaseTranslator inherited;
	
private:
	void __fastcall TranslateForm(Vcl::Forms::TCustomForm* form);
	
protected:
	virtual void __fastcall AfterProcessComponent(System::Classes::TComponent* component);
	virtual void __fastcall Translate(System::Classes::TComponent* component);
	
public:
	__classmethod void __fastcall InitializeApplication(Ntbase::TNtLayout layout = (Ntbase::TNtLayout)(0x0));
	__classmethod void __fastcall InitializeForm(Vcl::Forms::TCustomForm* form);
	__classmethod bool __fastcall SetNew(const System::UnicodeString code = System::UnicodeString(), Ntbase::TNtResourceOptions options = Ntbase::TNtResourceOptions() , const System::UnicodeString originalCode = System::UnicodeString(), const System::UnicodeString fileName = System::UnicodeString());
	__classmethod void __fastcall TranslateForms();
protected:
	/* TNtBaseTranslator.Create */ inline __fastcall TNtTranslator() : Ntbasetranslator::TNtBaseTranslator() { }
	
public:
	/* TNtBaseTranslator.Destroy */ inline __fastcall virtual ~TNtTranslator() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE void __fastcall _T(Vcl::Forms::TCustomForm* form)/* overload */;
}	/* namespace Nttranslator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTTRANSLATOR)
using namespace Nttranslator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NttranslatorHPP
