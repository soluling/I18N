// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtRaizeChecker.pas' rev: 35.00 (Windows)

#ifndef NtraizecheckerHPP
#define NtraizecheckerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Vcl.Controls.hpp>
#include <NtChecker.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntraizechecker
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtRaizeChecker;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtRaizeChecker : public Ntchecker::TNtCheckerExtension
{
	typedef Ntchecker::TNtCheckerExtension inherited;
	
public:
	virtual bool __fastcall Show(Ntchecker::TFormChecker* checker, Vcl::Controls::TControl* control, Vcl::Controls::TControl* &restoreControl);
	virtual bool __fastcall Restore(Vcl::Controls::TControl* control, Vcl::Controls::TControl* restoreControl);
	virtual bool __fastcall Ignore(Vcl::Controls::TControl* control, Ntchecker::TFormIssueTypes issueTypes);
public:
	/* TObject.Create */ inline __fastcall TNtRaizeChecker() : Ntchecker::TNtCheckerExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtRaizeChecker() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntraizechecker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTRAIZECHECKER)
using namespace Ntraizechecker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtraizecheckerHPP
