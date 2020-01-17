// CodeGear C++Builder
// Copyright (c) 1995, 2018 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtBaseTranslator.pas' rev: 33.00 (MacOS)

#ifndef NtbasetranslatorHPP
#define NtbasetranslatorHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.TypInfo.hpp>
#include <NtBase.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntbasetranslator
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtBaseTranslator;
class DELPHICLASS TNtTranslatorExtension;
class DELPHICLASS TNtStringsTranslator;
class DELPHICLASS TNtTranslatorExtensions;
class DELPHICLASS TNtStream;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (*TNtBeforeTranslateEvent)(System::Classes::TComponent* host, System::TObject* obj, System::Typinfo::PPropInfo propertyInfo, const System::Variant &currentValue, System::Variant &newValue, bool &cancel);

typedef void __fastcall (*TNtAfterTranslateEvent)(System::Classes::TComponent* host, System::TObject* obj, System::Typinfo::PPropInfo propertyInfo);

class PASCALIMPLEMENTATION TNtBaseTranslator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Classes::TComponent* FCurrent;
	System::Classes::TComponent* FHost;
	int FIndex;
	System::UnicodeString FName;
	System::TObject* FObj;
	System::UnicodeString FPropertyName;
	System::Typinfo::TPropInfo *FPropInfo;
	System::Classes::TReader* FReader;
	System::UnicodeString FSubName;
	System::Classes::TStringList* FUnnamedTypes;
	bool FTranslateLayout;
	System::Typinfo::PTypeInfo __fastcall GetTypeInfo();
	System::DynamicArray<System::Byte> __fastcall ProcessBinary();
	void __fastcall ProcessBytes(int count);
	void __fastcall ProcessCollection();
	void __fastcall ProcessComponent(System::Classes::TComponent* parent, bool root);
	void __fastcall ProcessList();
	void __fastcall ProcessProperty(System::TObject* obj);
	void __fastcall ProcessSet();
	void __fastcall ProcessPropertyValue();
	System::Variant __fastcall GetPropValue(System::TObject* instance);
	void __fastcall SetPropValue(System::TObject* instance, const System::Variant &value);
	__property System::Typinfo::PTypeInfo TypeInfo = {read=GetTypeInfo};
	
protected:
	virtual void __fastcall AfterProcessComponent(System::Classes::TComponent* component);
	bool __fastcall DoTranslate(System::Classes::TComponent* component, System::UnicodeString resourceName = System::UnicodeString());
	virtual void __fastcall Translate(System::Classes::TComponent* component);
	__fastcall TNtBaseTranslator();
	__property bool TranslateLayout = {read=FTranslateLayout, write=FTranslateLayout, nodefault};
	
public:
	__fastcall virtual ~TNtBaseTranslator();
};


class PASCALIMPLEMENTATION TNtTranslatorExtension : public Ntbase::TNtExtension
{
	typedef Ntbase::TNtExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj) = 0 ;
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index) = 0 ;
	virtual System::TObject* __fastcall GetActualObject(System::TObject* obj, const System::UnicodeString propName);
	virtual System::UnicodeString __fastcall GetActualName(System::TObject* obj, const System::UnicodeString propName);
public:
	/* TObject.Create */ inline __fastcall TNtTranslatorExtension() : Ntbase::TNtExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtTranslatorExtension() { }
	
};


_DECLARE_METACLASS(System::TMetaClass, TNtTranslatorExtensionClass);

class PASCALIMPLEMENTATION TNtStringsTranslator : public TNtTranslatorExtension
{
	typedef TNtTranslatorExtension inherited;
	
public:
	virtual bool __fastcall CanTranslate(System::TObject* obj);
	virtual void __fastcall Translate(System::Classes::TComponent* component, System::TObject* obj, const System::UnicodeString name, const System::Variant &value, int index);
public:
	/* TObject.Create */ inline __fastcall TNtStringsTranslator() : TNtTranslatorExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtStringsTranslator() { }
	
};


class PASCALIMPLEMENTATION TNtTranslatorExtensions : public Ntbase::TNtExtensions
{
	typedef Ntbase::TNtExtensions inherited;
	
public:
	TNtTranslatorExtension* operator[](int i) { return this->Items[i]; }
	
private:
	HIDESBASE TNtTranslatorExtension* __fastcall GetItem(int i);
	bool __fastcall CanTranslate(System::TObject* obj, TNtTranslatorExtension* &extension);
	System::TObject* __fastcall GetActualObject(System::TObject* obj, const System::UnicodeString propName);
	System::UnicodeString __fastcall GetActualName(System::TObject* obj, const System::UnicodeString propName);
	
public:
	__property TNtTranslatorExtension* Items[int i] = {read=GetItem/*, default*/};
protected:
	/* TNtExtensions.Create */ inline __fastcall virtual TNtTranslatorExtensions() : Ntbase::TNtExtensions() { }
	
public:
	/* TNtExtensions.Destroy */ inline __fastcall virtual ~TNtTranslatorExtensions() { }
	
};


class PASCALIMPLEMENTATION TNtStream : public System::Classes::TMemoryStream
{
	typedef System::Classes::TMemoryStream inherited;
	
public:
	__fastcall TNtStream(System::DynamicArray<System::Byte> value);
	System::Byte __fastcall ReadByte();
	System::Word __fastcall ReadWord();
	int __fastcall ReadInteger();
	void * __fastcall ReadPointer();
	System::DynamicArray<System::Byte> __fastcall ReadShortString();
	System::UnicodeString __fastcall ReadShortUnicodeString();
public:
	/* TMemoryStream.Destroy */ inline __fastcall virtual ~TNtStream() { }
	
};


//-- var, const, procedure ---------------------------------------------------
#define STRING_TYPES (System::Set<System::TTypeKind, System::TTypeKind::tkUnknown, System::TTypeKind::tkMRecord>() << System::TTypeKind::tkString << System::TTypeKind::tkLString << System::TTypeKind::tkWString << System::TTypeKind::tkUString )
extern DELPHI_PACKAGE System::Typinfo::TTypeKinds NtEnabledProperties;
extern DELPHI_PACKAGE bool NtFormPositionTranslationEnabled;
extern DELPHI_PACKAGE TNtBeforeTranslateEvent NtBeforeTranslate;
extern DELPHI_PACKAGE TNtAfterTranslateEvent NtAfterTranslate;
extern DELPHI_PACKAGE TNtTranslatorExtensions* NtTranslatorExtensions;
}	/* namespace Ntbasetranslator */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTBASETRANSLATOR)
using namespace Ntbasetranslator;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtbasetranslatorHPP
