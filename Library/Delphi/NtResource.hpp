// CodeGear C++Builder
// Copyright (c) 1995, 2016 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtResource.pas' rev: 32.00 (Windows)

#ifndef NtresourceHPP
#define NtresourceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntresource
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtResourceLanguage;
class DELPHICLASS TNtDelphiResource;
class DELPHICLASS TNtDelphiResources;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtResourceLanguage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FOriginal;
	System::UnicodeString FId;
	System::UnicodeString FImage;
	
public:
	TNtResourceLanguage* __fastcall AddImage(const System::UnicodeString value);
	__property System::UnicodeString Original = {read=FOriginal};
	__property System::UnicodeString Id = {read=FId};
	__property System::UnicodeString Image = {read=FImage};
public:
	/* TObject.Create */ inline __fastcall TNtResourceLanguage(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtResourceLanguage(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtDelphiResource : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FOffset;
	System::Classes::TResourceStream* FStream;
	System::UnicodeString FId;
	int FFormCount;
	int FFormNameOffset;
	int FStringGroupCount;
	int FStringGroupNameOffset;
	int FResourceCount;
	int FResourceNameOffset;
	void __fastcall SetOffset(int value);
	
public:
	System::Classes::TStream* __fastcall FindForm(const System::UnicodeString name);
	System::UnicodeString __fastcall FindString(const System::UnicodeString original, System::UnicodeString id, const System::UnicodeString group);
	System::Classes::TStream* __fastcall FindResource(const System::UnicodeString id);
	__property System::UnicodeString Id = {read=FId};
	__property int Offset = {read=FOffset, write=SetOffset, nodefault};
public:
	/* TObject.Create */ inline __fastcall TNtDelphiResource(void) : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtDelphiResource(void) { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtDelphiResources : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TNtDelphiResource* operator[](int i) { return this->Languages[i]; }
	
private:
	bool FLoaded;
	System::UnicodeString FResourceName;
	System::Classes::TResourceStream* FStream;
	bool FCascadingEnabled;
	int FLanguageIndex;
	System::Generics::Collections::TList__1<TNtDelphiResource*>* FLanguages;
	System::Generics::Collections::TList__1<TNtResourceLanguage*>* FLanguageNames;
	TNtDelphiResource* __fastcall GetCurrent(void);
	int __fastcall GetCount(void);
	bool __fastcall GetEnabled(void);
	TNtDelphiResource* __fastcall GetLanguage(int i);
	System::UnicodeString __fastcall GetLanguageId(void);
	System::UnicodeString __fastcall GetOriginal(const System::UnicodeString id);
	System::UnicodeString __fastcall GetLanguageImage(const System::UnicodeString id);
	void __fastcall SetLanguageId(const System::UnicodeString value);
	void __fastcall CheckLoad(void);
	
public:
	__fastcall TNtDelphiResources(void);
	__fastcall virtual ~TNtDelphiResources(void);
	void __fastcall Load(void);
	int __fastcall Find(const System::UnicodeString id);
	System::Classes::TStream* __fastcall FindForm(const System::UnicodeString name);
	bool __fastcall FormExists(const System::UnicodeString name);
	System::UnicodeString __fastcall GetString(const System::UnicodeString original, const System::UnicodeString id, const System::UnicodeString group);
	System::UnicodeString __fastcall GetStringInLanguage(const System::UnicodeString language, const System::UnicodeString original, const System::UnicodeString id, const System::UnicodeString group);
	System::Classes::TStream* __fastcall GetResource(const System::UnicodeString id, System::WideChar * resType = (System::WideChar *)(0xa));
	TNtResourceLanguage* __fastcall _T(const System::UnicodeString original, const System::UnicodeString id);
	__classmethod TNtDelphiResources* __fastcall GetResources();
	__property bool CascadingEnabled = {read=FCascadingEnabled, write=FCascadingEnabled, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property TNtDelphiResource* Current = {read=GetCurrent};
	__property bool Enabled = {read=GetEnabled, nodefault};
	__property System::UnicodeString LanguageId = {read=GetLanguageId, write=SetLanguageId};
	__property TNtDelphiResource* Languages[int i] = {read=GetLanguage/*, default*/};
	__property System::UnicodeString ResourceName = {read=FResourceName, write=FResourceName};
	__property System::Classes::TResourceStream* Stream = {read=FStream};
	__property System::UnicodeString Originals[const System::UnicodeString id] = {read=GetOriginal};
	__property System::UnicodeString LanguageImages[const System::UnicodeString id] = {read=GetLanguageImage};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define NTRES_RESOURCE_NAME_C L"NtLangRes"
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 4> NTRES_MAGIC_C;
extern DELPHI_PACKAGE System::StaticArray<System::Byte, 4> NTLANG_MAGIC_C;
extern DELPHI_PACKAGE TNtDelphiResources* NtResources;
extern DELPHI_PACKAGE System::UnicodeString __fastcall _T(const System::UnicodeString original, const System::UnicodeString id = System::UnicodeString(), const System::UnicodeString group = System::UnicodeString())/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall Translate(const System::UnicodeString original, const System::UnicodeString id = System::UnicodeString(), const System::UnicodeString group = System::UnicodeString())/* overload */;
extern DELPHI_PACKAGE System::UnicodeString __fastcall _TG(const System::UnicodeString original, const System::UnicodeString group);
extern DELPHI_PACKAGE System::UnicodeString __fastcall TranslateGroup(const System::UnicodeString original, const System::UnicodeString group);
}	/* namespace Ntresource */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTRESOURCE)
using namespace Ntresource;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtresourceHPP
