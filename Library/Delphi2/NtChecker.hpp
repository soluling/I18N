// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtChecker.pas' rev: 35.00 (Windows)

#ifndef NtcheckerHPP
#define NtcheckerHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <System.Classes.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <System.JSON.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <NtBase.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntchecker
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TFormIssue;
class DELPHICLASS TTruncationIssue;
class DELPHICLASS TOverlapIssue;
class DELPHICLASS TOutOfBoundsIssue;
class DELPHICLASS TFormChecker;
class DELPHICLASS TNtCheckerExtension;
class DELPHICLASS TNtTabControlChecker;
class DELPHICLASS TNtCheckerExtensions;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TFormIssueType : unsigned char { itTruncation, itOverlap, itOutOfBounds };

typedef System::Set<TFormIssueType, TFormIssueType::itTruncation, TFormIssueType::itOutOfBounds> TFormIssueTypes;

typedef System::TMetaClass* TFormIssueClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TFormIssue : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TFormChecker* FChecker;
	Vcl::Controls::TControl* FControl;
	Vcl::Controls::TWinControl* FForm;
	Vcl::Imaging::Pngimage::TPngImage* FScreenshot;
	System::UnicodeString __fastcall GetScreenshotFileName();
	
protected:
	virtual TFormIssueType __fastcall GetIssueType() = 0 ;
	virtual System::UnicodeString __fastcall GetName() = 0 ;
	virtual void __fastcall WriteJson(System::Json::TJSONObject* json);
	
public:
	__fastcall virtual TFormIssue();
	__fastcall virtual ~TFormIssue();
	void __fastcall TakeScreenshot(const System::Types::TRect &highlightRect);
	void __fastcall SaveScreenshot();
	void __fastcall RemoveScreenshot();
	bool __fastcall ShouldWriteIssue();
	__property TFormIssueType IssueType = {read=GetIssueType, nodefault};
	__property Vcl::Controls::TControl* Control = {read=FControl, write=FControl};
	__property Vcl::Controls::TWinControl* Form = {read=FForm, write=FForm};
	__property System::UnicodeString Name = {read=GetName};
	__property Vcl::Imaging::Pngimage::TPngImage* Screenshot = {read=FScreenshot};
	__property System::UnicodeString ScreenshotFileName = {read=GetScreenshotFileName};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TTruncationIssue : public TFormIssue
{
	typedef TFormIssue inherited;
	
private:
	int FTextWidth;
	
protected:
	virtual TFormIssueType __fastcall GetIssueType();
	virtual System::UnicodeString __fastcall GetName();
	
public:
	__property int TextWidth = {read=FTextWidth, write=FTextWidth, nodefault};
public:
	/* TFormIssue.Create */ inline __fastcall virtual TTruncationIssue() : TFormIssue() { }
	/* TFormIssue.Destroy */ inline __fastcall virtual ~TTruncationIssue() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TOverlapIssue : public TFormIssue
{
	typedef TFormIssue inherited;
	
public:
	Vcl::Controls::TControl* operator[](int i) { return this->Controls[i]; }
	
private:
	System::Classes::TList* FControls;
	int __fastcall GetCount();
	Vcl::Controls::TControl* __fastcall GetControl(int i);
	
protected:
	virtual TFormIssueType __fastcall GetIssueType();
	virtual System::UnicodeString __fastcall GetName();
	virtual void __fastcall WriteJson(System::Json::TJSONObject* json);
	
public:
	__fastcall virtual TOverlapIssue();
	__fastcall virtual ~TOverlapIssue();
	void __fastcall Add(Vcl::Controls::TControl* value);
	bool __fastcall Exists(Vcl::Controls::TControl* control);
	__property int Count = {read=GetCount, nodefault};
	__property Vcl::Controls::TControl* Controls[int i] = {read=GetControl/*, default*/};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TOutOfBoundsIssue : public TFormIssue
{
	typedef TFormIssue inherited;
	
protected:
	virtual TFormIssueType __fastcall GetIssueType();
	virtual System::UnicodeString __fastcall GetName();
public:
	/* TFormIssue.Create */ inline __fastcall virtual TOutOfBoundsIssue() : TFormIssue() { }
	/* TFormIssue.Destroy */ inline __fastcall virtual ~TOutOfBoundsIssue() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TFormControlEvent)(TFormChecker* checker, Vcl::Controls::TControl* control, bool &skip);

typedef void __fastcall (__closure *TFormIssueEvent)(TFormChecker* checker, TFormIssue* issue, bool &ignore);

typedef System::StaticArray<System::Uitypes::TColor, 3> THighlightColors;

class PASCALIMPLEMENTATION TFormChecker : public System::TObject
{
	typedef System::TObject inherited;
	
public:
	TFormIssue* operator[](int i) { return this->Issues[i]; }
	
private:
	System::Classes::TList* FIssues;
	Vcl::Controls::TWinControl* FControl;
	TFormIssueTypes FDisabledTypes;
	Vcl::Forms::TCustomForm* FForm;
	THighlightColors FHighlightColors;
	bool FHighlightIssues;
	int FHighlightMargin;
	int FHighlightWidth;
	System::UnicodeString FOutputDir;
	System::UnicodeString FActiveOutputDir;
	bool FScreenshots;
	Vcl::Extctrls::TTimer* FTimer;
	System::Json::TJSONArray* FResults;
	System::Json::TJSONObject* FResultsForm;
	System::Json::TJSONArray* FResultsIssues;
	System::UnicodeString FResultsFileName;
	TFormControlEvent FOnControl;
	TFormIssueEvent FOnIssue;
	System::Uitypes::TFontCharset __fastcall GetCharset();
	int __fastcall GetCount();
	TFormIssue* __fastcall GetIssue(int i);
	Vcl::Controls::TWinControl* __fastcall GetScreenshotControl(Vcl::Controls::TControl* control);
	TFormIssue* __fastcall AllocIssue(TFormIssueClass issueClass, Vcl::Controls::TControl* control, const System::Types::TRect &hightlightRect);
	void __fastcall AddIssue(TFormIssue* issue);
	bool __fastcall IsOverlapIssueUnique(TOverlapIssue* issue);
	bool __fastcall IsTruncationDisabled(Vcl::Controls::TControl* control);
	void __fastcall ProcessTruncation(Vcl::Controls::TControl* control, int width);
	void __fastcall CheckOverlap(Vcl::Controls::TControl* control);
	void __fastcall CheckOutOfBounds(Vcl::Controls::TControl* control);
	void __fastcall CheckLabel(Vcl::Stdctrls::TCustomLabel* control);
	void __fastcall CheckCheckBox(Vcl::Stdctrls::TCheckBox* control);
	void __fastcall CheckRadioButton(Vcl::Stdctrls::TRadioButton* control);
	void __fastcall CheckButton(Vcl::Stdctrls::TButton* control);
	void __fastcall CheckComboBox(Vcl::Stdctrls::TCustomComboBox* control);
	void __fastcall CheckListBox(Vcl::Stdctrls::TCustomListBox* control);
	void __fastcall Process(Vcl::Controls::TControl* control);
	int __fastcall CalculateWidthInPixels(Vcl::Graphics::TCanvas* canvas, const System::UnicodeString str, System::Uitypes::TFontCharset charset = (System::Uitypes::TFontCharset)(0x0));
	void __fastcall ProcessForm(System::TObject* Sender);
	void __fastcall Open();
	void __fastcall Close();
	
public:
	__fastcall TFormChecker();
	__fastcall virtual ~TFormChecker();
	void __fastcall Check(Vcl::Controls::TWinControl* control);
	void __fastcall LanguageChanged();
	Vcl::Controls::TControl* __fastcall GetParent(Vcl::Controls::TControl* control, Vcl::Controls::TControlClass parentClass);
	__property System::UnicodeString ActiveOutputDir = {read=FActiveOutputDir};
	__property System::Uitypes::TFontCharset Charset = {read=GetCharset, nodefault};
	__property int Count = {read=GetCount, nodefault};
	__property TFormIssueTypes DisabledTypes = {read=FDisabledTypes, write=FDisabledTypes, nodefault};
	__property TFormIssue* Issues[int i] = {read=GetIssue/*, default*/};
	__property THighlightColors HighlightColors = {read=FHighlightColors, write=FHighlightColors};
	__property bool HighlightIssues = {read=FHighlightIssues, write=FHighlightIssues, nodefault};
	__property int HighlightMargin = {read=FHighlightMargin, write=FHighlightMargin, nodefault};
	__property int HighlightWidth = {read=FHighlightWidth, write=FHighlightWidth, nodefault};
	__property System::UnicodeString OutputDir = {read=FOutputDir, write=FOutputDir};
	__property bool Screenshots = {read=FScreenshots, write=FScreenshots, nodefault};
	__property TFormControlEvent OnControl = {read=FOnControl, write=FOnControl};
	__property TFormIssueEvent OnIssue = {read=FOnIssue, write=FOnIssue};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtCheckerExtension : public Ntbase::TNtExtension
{
	typedef Ntbase::TNtExtension inherited;
	
public:
	virtual bool __fastcall Show(TFormChecker* checker, Vcl::Controls::TControl* control, Vcl::Controls::TControl* &restoreControl);
	virtual bool __fastcall Restore(Vcl::Controls::TControl* control, Vcl::Controls::TControl* restoreControl);
	virtual bool __fastcall Ignore(Vcl::Controls::TControl* control, TFormIssueTypes issueTypes);
public:
	/* TObject.Create */ inline __fastcall TNtCheckerExtension() : Ntbase::TNtExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtCheckerExtension() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TNtCheckerExtensionClass;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtTabControlChecker : public TNtCheckerExtension
{
	typedef TNtCheckerExtension inherited;
	
public:
	virtual bool __fastcall Show(TFormChecker* checker, Vcl::Controls::TControl* control, Vcl::Controls::TControl* &restoreControl);
	virtual bool __fastcall Restore(Vcl::Controls::TControl* control, Vcl::Controls::TControl* restoreControl);
	virtual bool __fastcall Ignore(Vcl::Controls::TControl* control, TFormIssueTypes issueTypes);
public:
	/* TObject.Create */ inline __fastcall TNtTabControlChecker() : TNtCheckerExtension() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TNtTabControlChecker() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TNtCheckerExtensions : public Ntbase::TNtExtensions
{
	typedef Ntbase::TNtExtensions inherited;
	
public:
	TNtCheckerExtension* operator[](int i) { return this->Items[i]; }
	
private:
	Vcl::Controls::TControl* FRestoreControl;
	HIDESBASE TNtCheckerExtension* __fastcall GetItem(int i);
	bool __fastcall Show(TFormChecker* checker, Vcl::Controls::TControl* control);
	bool __fastcall Restore(Vcl::Controls::TControl* control);
	bool __fastcall Ignore(Vcl::Controls::TControl* control, TFormIssueTypes issueTypes);
	
public:
	__property TNtCheckerExtension* Items[int i] = {read=GetItem/*, default*/};
protected:
	/* TNtExtensions.Create */ inline __fastcall virtual TNtCheckerExtensions() : Ntbase::TNtExtensions() { }
	
public:
	/* TNtExtensions.Destroy */ inline __fastcall virtual ~TNtCheckerExtensions() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE TFormChecker* NtFormChecker;
extern DELPHI_PACKAGE TNtCheckerExtensions* NtCheckerExtensions;
}	/* namespace Ntchecker */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTCHECKER)
using namespace Ntchecker;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtcheckerHPP
