// CodeGear C++Builder
// Copyright (c) 1995, 2017 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtDatabaseUtils.pas' rev: 33.00 (Windows)

#ifndef NtdatabaseutilsHPP
#define NtdatabaseutilsHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Data.DB.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntdatabaseutils
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtDatabaseUtils;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtDatabaseUtils : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod void __fastcall HideField(Data::Db::TDataSet* dataSet, const System::UnicodeString name);
	__classmethod void __fastcall ShowField(Data::Db::TDataSet* dataSet, const System::UnicodeString name, const System::UnicodeString caption, int width = 0x0);
public:
	/* TObject.Create */ inline __fastcall TNtDatabaseUtils() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtDatabaseUtils() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntdatabaseutils */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTDATABASEUTILS)
using namespace Ntdatabaseutils;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtdatabaseutilsHPP
