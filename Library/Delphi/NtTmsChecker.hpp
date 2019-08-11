// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtTmsChecker.pas' rev: 33.00 (Windows)

#ifndef NttmscheckerHPP
#define NttmscheckerHPP

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

namespace Nttmschecker
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtTmsChecker;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtTmsChecker : public Ntchecker::TNtCheckerExtension
{
	typedef Ntchecker::TNtCheckerExtension inherited;
	
public:
	virtual bool __fastcall Show(Ntchecker::TFormChecker* checker, Vcl::Controls::TControl* control, Vcl::Controls::TControl* &restoreControl);
	virtual bool __fastcall Restore(Vcl::Controls::TControl* control, Vcl::Controls::TControl* restoreControl);
	virtual bool __fastcall Ignore(Vcl::Controls::TControl* control, Ntchecker::TFormIssueTypes issueTypes);
public:
	/* TObject.Create */ inline __fastcall TNtTmsChecker() : Ntchecker::TNtCheckerExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtTmsChecker() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Nttmschecker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTTMSCHECKER)
using namespace Nttmschecker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NttmscheckerHPP
