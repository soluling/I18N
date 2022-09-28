// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtPattern.pas' rev: 35.00 (Windows)

#ifndef NtpatternHPP
#define NtpatternHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntpattern
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TPattern;
class DELPHICLASS TNumberPattern;
class DELPHICLASS TPluralPattern;
class DELPHICLASS TOperatorPattern;
class DELPHICLASS TGenderPattern;
class DELPHICLASS TSelectPattern;
struct TFormatParameter;
class DELPHICLASS TFormatPartEnumerator;
class DELPHICLASS TFormatPart;
class DELPHICLASS TFormatStringEnumerator;
class DELPHICLASS TFormatString;
class DELPHICLASS TPluralInfo;
class DELPHICLASS TMultiPattern;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TPlural : unsigned char { pfZero, pfOne, pfTwo, pfFew, pfMany, pfOther };

typedef System::Set<TPlural, TPlural::pfZero, TPlural::pfOther> TPlurals;

enum DECLSPEC_DENUM TGender : unsigned char { geMale, geFemale, geNeutral };

typedef System::Set<TGender, TGender::geMale, TGender::geNeutral> TGenders;

enum DECLSPEC_DENUM TOperatorKind : unsigned char { okEqual, okAround, okLess, okLessOrEqual, okGreater, okGreaterOrEqual, okRange };

typedef TPlural __fastcall (*TPluralProc)(double n, int i, int v, int w, int f, int t);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPattern : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FValue;
	
public:
	__property System::UnicodeString Value = {read=FValue, write=FValue};
public:
	/* TObject.Create */ inline __fastcall TPattern() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TPattern() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNumberPattern : public TPattern
{
	typedef TPattern inherited;
	
public:
	/* TObject.Create */ inline __fastcall TNumberPattern() : TPattern() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNumberPattern() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPluralPattern : public TNumberPattern
{
	typedef TNumberPattern inherited;
	
private:
	TPlural FPlural;
	
public:
	__fastcall TPluralPattern();
	virtual bool __fastcall Equals(System::TObject* obj);
	virtual System::UnicodeString __fastcall ToString();
	__property TPlural Plural = {read=FPlural, write=FPlural, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TPluralPattern() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TOperatorPattern : public TNumberPattern
{
	typedef TNumberPattern inherited;
	
private:
	TOperatorKind FKind;
	int FOperand;
	int FOperand2;
	
public:
	__fastcall TOperatorPattern();
	virtual bool __fastcall Equals(System::TObject* obj);
	virtual System::UnicodeString __fastcall ToString();
	__property TOperatorKind Kind = {read=FKind, write=FKind, nodefault};
	__property int Operand = {read=FOperand, write=FOperand, nodefault};
	__property int Operand2 = {read=FOperand2, write=FOperand2, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TOperatorPattern() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TGenderPattern : public TPattern
{
	typedef TPattern inherited;
	
private:
	TGender FGender;
	
public:
	__fastcall TGenderPattern();
	virtual bool __fastcall Equals(System::TObject* obj);
	virtual System::UnicodeString __fastcall ToString();
	__property TGender Gender = {read=FGender, write=FGender, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TGenderPattern() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TSelectPattern : public TPattern
{
	typedef TPattern inherited;
	
private:
	System::UnicodeString FSelect;
	
public:
	__fastcall TSelectPattern();
	virtual bool __fastcall Equals(System::TObject* obj);
	virtual System::UnicodeString __fastcall ToString();
	__property System::UnicodeString Select = {read=FSelect, write=FSelect};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TSelectPattern() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TFormatParameterKind : unsigned char { fpPlural, fpOperator, fpGender, fpSelect };

typedef System::Set<TFormatParameterKind, TFormatParameterKind::fpPlural, TFormatParameterKind::fpSelect> TFormatParameterKinds;

struct DECLSPEC_DRECORD TFormatParameter
{
	
public:
	TFormatParameterKind Kind;
	union
	{
		struct 
		{
			System::WideChar *Select;
		};
		struct 
		{
			TGender Gender;
			System::WideChar *Value;
		};
		struct 
		{
			int Plural;
		};
		
	};
};


enum DECLSPEC_DENUM TPluralUsage : unsigned char { puInteger, puDecimal };

typedef System::Set<TPluralUsage, TPluralUsage::puInteger, TPluralUsage::puDecimal> TPluralUsages;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormatPartEnumerator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFormatPart* FFormatPart;
	int FIndex;
	
public:
	__fastcall TFormatPartEnumerator(TFormatPart* formatPart);
	TPattern* __fastcall GetCurrent();
	bool __fastcall MoveNext();
	__property TPattern* Current = {read=GetCurrent};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFormatPartEnumerator() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormatPart : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TPattern* operator[](int i) { return this->Items[i]; }
	
private:
	System::Classes::TList* FItems;
	System::UnicodeString FName;
	System::UnicodeString FParameterType;
	TFormatPart* FParent;
	TFormatParameterKind FUsedKind;
	TPlural FUsedPlural;
	TOperatorKind FUsedOperator;
	int FUsedOperand;
	int FUsedOperand2;
	TGender FUsedGender;
	System::UnicodeString FUsedSelect;
	TFormatParameterKind __fastcall GetKind();
	int __fastcall GetCount();
	System::UnicodeString __fastcall GetFirstValue();
	System::UnicodeString __fastcall GetOtherValue();
	TPattern* __fastcall GetItem(int i);
	System::UnicodeString __fastcall GetDefaultValue();
	TGender __fastcall GetDefaultGender();
	System::UnicodeString __fastcall GetDefaultSelect();
	System::UnicodeString __fastcall GetPluralValue(TPlural plural);
	System::UnicodeString __fastcall GetMatchingValue(int value);
	System::UnicodeString __fastcall GetOperatorValue(TOperatorKind kind, int operand, int operand2);
	System::UnicodeString __fastcall GetEqualOperatorValue(int operand);
	System::UnicodeString __fastcall GetGenderValue(TGender gender);
	System::UnicodeString __fastcall GetSelectValue(const System::UnicodeString select);
	void __fastcall SetPluralValue(TPlural plural, const System::UnicodeString value);
	void __fastcall SetOperatorValue(TOperatorKind kind, int operand, int operand2, const System::UnicodeString value);
	void __fastcall SetEqualOperatorValue(int operand, const System::UnicodeString value);
	void __fastcall SetGenderValue(TGender gender, const System::UnicodeString value);
	void __fastcall SetSelectValue(const System::UnicodeString select, const System::UnicodeString value);
	
public:
	__fastcall TFormatPart();
	__fastcall virtual ~TFormatPart();
	TFormatPartEnumerator* __fastcall GetEnumerator();
	void __fastcall Clear();
	void __fastcall Delete(int index);
	TPattern* __fastcall Find(TPattern* pattern)/* overload */;
	TPattern* __fastcall FindAny(TPattern* pattern);
	TPattern* __fastcall FindOther();
	TGenderPattern* __fastcall Find(TGender gender)/* overload */;
	bool __fastcall Exists(TGender gender)/* overload */;
	TGenderPattern* __fastcall Add(const System::UnicodeString value, TGender gender)/* overload */;
	TGenderPattern* __fastcall FindGender();
	TSelectPattern* __fastcall Find(const System::UnicodeString select)/* overload */;
	bool __fastcall Exists(const System::UnicodeString select)/* overload */;
	TSelectPattern* __fastcall Add(const System::UnicodeString value, const System::UnicodeString select)/* overload */;
	TSelectPattern* __fastcall Insert(int index, const System::UnicodeString value, const System::UnicodeString select)/* overload */;
	TPluralPattern* __fastcall Find(TPlural plural)/* overload */;
	bool __fastcall Exists(TPlural plural)/* overload */;
	TPluralPattern* __fastcall Add(const System::UnicodeString value, TPlural plural)/* overload */;
	TOperatorPattern* __fastcall Find(TOperatorKind kind, int operand, int operand2)/* overload */;
	bool __fastcall Exists(TOperatorKind kind, int operand, int operand2)/* overload */;
	TOperatorPattern* __fastcall Add(const System::UnicodeString value, TOperatorKind kind, int operand, int operand2 = 0x0)/* overload */;
	TOperatorPattern* __fastcall FindOperator(int value);
	TPluralPattern* __fastcall FindMatching(int value);
	bool __fastcall ExistsMatching(int value, TPlural &plural);
	bool __fastcall IsNumber();
	bool __fastcall IsGender();
	bool __fastcall IsSelect();
	System::UnicodeString __fastcall GetPluralPattern(TPlural plural, unsigned count);
	System::UnicodeString __fastcall GetGenderPattern(TGender gender);
	__property TFormatParameterKind Kind = {read=GetKind, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property System::UnicodeString FirstValue = {read=GetFirstValue};
	__property System::UnicodeString OtherValue = {read=GetOtherValue};
	__property TPattern* Items[int i] = {read=GetItem/*, default*/};
	__property System::UnicodeString Name = {read=FName, write=FName};
	__property TGender DefaultGender = {read=GetDefaultGender, nodefault};
	__property System::UnicodeString DefaultSelect = {read=GetDefaultSelect};
	__property System::UnicodeString ParameterType = {read=FParameterType, write=FParameterType};
	__property System::UnicodeString OperatorValues[TOperatorKind kind][int operand][int operand2] = {read=GetOperatorValue, write=SetOperatorValue};
	__property System::UnicodeString EqualOperatorValues[int operand] = {read=GetEqualOperatorValue, write=SetEqualOperatorValue};
	__property System::UnicodeString PluralValues[TPlural plural] = {read=GetPluralValue, write=SetPluralValue};
	__property System::UnicodeString MatchingValues[int plural] = {read=GetMatchingValue};
	__property System::UnicodeString GenderValues[TGender gender] = {read=GetGenderValue, write=SetGenderValue};
	__property System::UnicodeString SelectValues[const System::UnicodeString select] = {read=GetSelectValue, write=SetSelectValue};
	__property TFormatPart* Parent = {read=FParent, write=FParent};
	__property System::UnicodeString DefaultValue = {read=GetDefaultValue};
	__property TFormatParameterKind UsedKind = {read=FUsedKind, nodefault};
	__property TPlural UsedPlural = {read=FUsedPlural, nodefault};
	__property TOperatorKind UsedOperator = {read=FUsedOperator, nodefault};
	__property int UsedOperand = {read=FUsedOperand, nodefault};
	__property int UsedOperand2 = {read=FUsedOperand2, nodefault};
	__property TGender UsedGender = {read=FUsedGender, nodefault};
	__property System::UnicodeString UsedSelect = {read=FUsedSelect};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormatStringEnumerator : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFormatString* FFormatString;
	int FIndex;
	
public:
	__fastcall TFormatStringEnumerator(TFormatString* formatString);
	TFormatPart* __fastcall GetCurrent();
	bool __fastcall MoveNext();
	__property TFormatPart* Current = {read=GetCurrent};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TFormatStringEnumerator() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TPlaceholderKind : unsigned char { pkPrintf, pkDoubleBrace, pkSingleBrace, pkHashtag };

enum DECLSPEC_DENUM TIcuMessageEscape : unsigned char { ieDefault, ieReact, ieOriginal };

enum DECLSPEC_DENUM TFormatStringSyntax : unsigned char { fsIcu, fsLegacy };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormatString : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TFormatPart* operator[](int i) { return this->Items[i]; }
	
private:
	System::Classes::TList* FItems;
	System::UnicodeString FStartPattern;
	TPlaceholderKind FPlaceholderKind;
	TIcuMessageEscape FEscape;
	TFormatStringSyntax FSyntax;
	int __fastcall GetCount();
	TFormatPart* __fastcall GetItem(int i);
	System::UnicodeString __fastcall GetText();
	System::UnicodeString __fastcall GetOperatorValue(TOperatorKind kind, int operand, int operand2);
	System::UnicodeString __fastcall GetEqualOperatorValue(int operand);
	System::UnicodeString __fastcall GetPluralValue(TPlural value);
	System::UnicodeString __fastcall GetMatchingValue(int value);
	System::UnicodeString __fastcall GetGenderValue(TGender value);
	System::UnicodeString __fastcall GetSelectValue(const System::UnicodeString value);
	bool __fastcall ParseIcuPattern(const System::UnicodeString pattern, int &index);
	
public:
	__fastcall TFormatString();
	__fastcall virtual ~TFormatString();
	TFormatStringEnumerator* __fastcall GetEnumerator();
	void __fastcall Clear();
	TPattern* __fastcall Find(TPattern* pattern)/* overload */;
	TPattern* __fastcall FindAny(TPattern* pattern);
	TGenderPattern* __fastcall Find(TGender gender, bool all = true)/* overload */;
	bool __fastcall Exists(TGender gender)/* overload */;
	TGenderPattern* __fastcall AddValue(TGender gender, const System::UnicodeString value)/* overload */;
	TPluralPattern* __fastcall Find(TPlural plural, bool all = true)/* overload */;
	bool __fastcall Exists(TPlural plural)/* overload */;
	TPluralPattern* __fastcall AddValue(TPlural plural, const System::UnicodeString value)/* overload */;
	TOperatorPattern* __fastcall Find(TOperatorKind kind, int operand, int operand2, bool all = true)/* overload */;
	bool __fastcall Exists(TOperatorKind kind, int operand, int operand2)/* overload */;
	TPluralPattern* __fastcall FindMatching(int operand, bool all = true);
	bool __fastcall ExistsMatching(int operand);
	TFormatPart* __fastcall AddParameter(const System::UnicodeString name = System::UnicodeString());
	void __fastcall ParsePattern(const System::UnicodeString pattern);
	bool __fastcall ParseLegacy(System::UnicodeString pattern);
	bool __fastcall ParseIcu(System::UnicodeString pattern);
	__classmethod bool __fastcall IsPattern _DEPRECATED_ATTRIBUTE1("Use IsMultiPattern instead") (const System::UnicodeString pattern);
	__classmethod bool __fastcall IsMultiPattern(const System::UnicodeString pattern);
	__property int Count = {read=GetCount, nodefault};
	__property TFormatPart* Items[int i] = {read=GetItem/*, default*/};
	__property System::UnicodeString StartPattern = {read=FStartPattern, write=FStartPattern};
	__property System::UnicodeString Text = {read=GetText};
	__property System::UnicodeString OperatorValues[TOperatorKind kind][int operand][int operand2] = {read=GetOperatorValue};
	__property System::UnicodeString EqualOperatorValues[int value] = {read=GetEqualOperatorValue};
	__property System::UnicodeString PluralValues[TPlural value] = {read=GetPluralValue};
	__property System::UnicodeString MatchingValues[int value] = {read=GetMatchingValue};
	__property System::UnicodeString GenderValues[TGender value] = {read=GetGenderValue};
	__property System::UnicodeString SelectValues[const System::UnicodeString value] = {read=GetSelectValue};
	__property TPlaceholderKind PlaceholderKind = {read=FPlaceholderKind, write=FPlaceholderKind, nodefault};
	__property TIcuMessageEscape Escape = {read=FEscape, write=FEscape, nodefault};
	__property TFormatStringSyntax Syntax = {read=FSyntax, write=FSyntax, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TPluralInfo : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FId;
	TPluralProc FProc;
	int FIntegerCount;
	TPlurals FIntegerPlurals;
	TPlural FDefaultInteger;
	int FDecimalCount;
	TPlurals FDecimalPlurals;
	TPlural FDefaultDecimal;
	System::UnicodeString FExpression;
	TPlural __fastcall GetInteger(int i);
	TPlural __fastcall GetDecimal(int i);
	void __fastcall SetIntegerPlurals(TPlurals value);
	void __fastcall SetDecimalPlurals(TPlurals value);
	
public:
	__fastcall TPluralInfo();
	__classmethod TPluralInfo* __fastcall Get(System::UnicodeString id = System::UnicodeString());
	__classmethod TPluralInfo* __fastcall Find(const System::UnicodeString id);
	__property System::UnicodeString Id = {read=FId, write=FId};
	__property TPluralProc Proc = {read=FProc, write=FProc};
	__property int IntegerCount = {read=FIntegerCount, nodefault};
	__property TPlurals IntegerPlurals = {read=FIntegerPlurals, write=SetIntegerPlurals, nodefault};
	__property TPlural Integers[int i] = {read=GetInteger};
	__property TPlural DefaultInteger = {read=FDefaultInteger, nodefault};
	__property int DecimalCount = {read=FDecimalCount, nodefault};
	__property TPlurals DecimalPlurals = {read=FDecimalPlurals, write=SetDecimalPlurals, nodefault};
	__property TPlural Decimals[int i] = {read=GetDecimal};
	__property TPlural DefaultDecimal = {read=FDefaultDecimal, nodefault};
	__property System::UnicodeString Expression = {read=FExpression, write=FExpression};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TPluralInfo() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TMultiPattern : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	__classmethod TPluralProc __fastcall GetProc()/* overload */;
	__classmethod TPluralProc __fastcall GetProc(const System::UnicodeString id)/* overload */;
	__classmethod bool __fastcall IsSingleFormLanguage(const System::UnicodeString locale = System::UnicodeString());
	__classmethod bool __fastcall IsZeroLikeOne(const System::UnicodeString locale = System::UnicodeString());
	__classmethod TPlural __fastcall GetPlural(int count);
	__classmethod void __fastcall GetMatchingPlural(int count, const System::UnicodeString format, TPlural &plural, TPlural &customPlural);
	__classmethod System::UnicodeString __fastcall GetPluralName(TPlural plural);
	__classmethod System::UnicodeString __fastcall GetGenderName(TGender gender);
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, int count)/* overload */;
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, int count, System::UnicodeString &startPattern)/* overload */;
	__classmethod TPlurals __fastcall GetPlurals(TPluralUsage usage);
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, TGender gender)/* overload */;
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, TGender gender, System::UnicodeString &startPattern)/* overload */;
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, const System::UnicodeString select)/* overload */;
	__classmethod System::UnicodeString __fastcall GetPattern(const System::UnicodeString patterns, const System::UnicodeString select, System::UnicodeString &startPattern)/* overload */;
	__classmethod TGender __fastcall GetGender(const System::UnicodeString patterns, TGender gender);
	__classmethod System::UnicodeString __fastcall Format(const System::UnicodeString pattern, int count)/* overload */;
	__classmethod System::UnicodeString __fastcall Format(const System::UnicodeString pattern, int count, const System::TVarRec *args, const int args_High)/* overload */;
	__classmethod System::UnicodeString __fastcall Format(const System::UnicodeString pattern, TGender gender, const System::TVarRec *args, const int args_High)/* overload */;
	__classmethod System::UnicodeString __fastcall Format(const System::UnicodeString pattern, const System::UnicodeString select, const System::TVarRec *args, const int args_High)/* overload */;
	__classmethod System::UnicodeString __fastcall Format(const System::UnicodeString pattern, const System::TVarRec *counts, const int counts_High)/* overload */;
	__classmethod void __fastcall GetNumber(const System::UnicodeString pattern, int count, TFormatParameterKind &kind, TPlural &plural, TOperatorKind &operatorKind, int &operand, int &operand2);
	__classmethod System::UnicodeString __fastcall FormatMulti(const System::UnicodeString pattern, const TFormatParameter *args, const int args_High)/* overload */;
	__classmethod TPlural __fastcall StringToPlural(const System::UnicodeString value);
	__classmethod int __fastcall StringToOperator(const System::UnicodeString value, /* out */ TOperatorKind &kind, /* out */ int &operand2)/* overload */;
	__classmethod int __fastcall StringToOperator(const System::UnicodeString value, /* out */ TOperatorKind &kind)/* overload */;
	__classmethod TGender __fastcall StringToGender(const System::UnicodeString value);
	__classmethod bool __fastcall TryStringToPlural(const System::UnicodeString value, /* out */ TPlural &plural);
	__classmethod bool __fastcall TryStringToOperator(System::UnicodeString value, /* out */ TOperatorKind &kind, /* out */ int &operand, /* out */ int &operand2);
	__classmethod bool __fastcall TryStringToGender(const System::UnicodeString value, /* out */ TGender &gender);
	__classmethod System::UnicodeString __fastcall PluralToString(TPlural value);
	__classmethod System::UnicodeString __fastcall OperatorToString(TOperatorKind kind, int operand, int operand2);
	__classmethod System::UnicodeString __fastcall GenderToString(TGender value);
	__classmethod System::UnicodeString __fastcall SelectToString(const System::UnicodeString value);
	__classmethod bool __fastcall IsNumber(const System::UnicodeString value);
	__classmethod bool __fastcall IsPlural(const System::UnicodeString value);
	__classmethod bool __fastcall IsOperator(const System::UnicodeString value);
	__classmethod bool __fastcall IsGender(const System::UnicodeString value);
	__classmethod bool __fastcall IsOther(const System::UnicodeString value);
	__classmethod bool __fastcall IsNeutral(const System::UnicodeString value);
	__classmethod bool __fastcall IsLanguageInArray(const System::UnicodeString language, System::UnicodeString *languages, const int languages_High);
	__classmethod void __fastcall Register(const System::UnicodeString id, TPluralProc proc)/* overload */;
	__classmethod void __fastcall Register(const System::UnicodeString id, TPlurals integerPlurals, TPlurals decimalPlurals, const System::UnicodeString expression)/* overload */;
public:
	/* TObject.Create */ inline __fastcall TMultiPattern() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TMultiPattern() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
#define OTHER_C L"other"
extern DELPHI_PACKAGE int OperatorDelta;
extern DELPHI_PACKAGE bool RaiseExceptionOnInvalidPattern;
}	/* namespace Ntpattern */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTPATTERN)
using namespace Ntpattern;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtpatternHPP
