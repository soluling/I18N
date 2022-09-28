// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'NtLanguageDlg.pas' rev: 35.00 (Windows)

#ifndef NtlanguagedlgHPP
#define NtlanguagedlgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <NtBase.hpp>
#include <NtLocalization.hpp>

//-- user supplied -----------------------------------------------------------

namespace Ntlanguagedlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtLanguageDialog;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNtLanguageDialogOption : unsigned char { ldShowErrorIfNoDll, ldCompatible, ldCheckVersion };

typedef System::Set<TNtLanguageDialogOption, TNtLanguageDialogOption::ldShowErrorIfNoDll, TNtLanguageDialogOption::ldCheckVersion> TNtLanguageDialogOptions;

class PASCALIMPLEMENTATION TNtLanguageDialog : public Vcl::Forms::TForm
{
	typedef Vcl::Forms::TForm inherited;
	
__published:
	Vcl::Stdctrls::TListBox* LanguageList;
	Vcl::Stdctrls::TButton* OkButton;
	Vcl::Stdctrls::TButton* CancelButton;
	void __fastcall FormCreate(System::TObject* sender);
	void __fastcall FormShow(System::TObject* sender);
	void __fastcall LanguageListDblClick(System::TObject* sender);
	void __fastcall FormDestroy(System::TObject* Sender);
	
private:
	bool FCheckVersion;
	bool FCompatible;
	Ntbase::TNtLanguageName FLanguageName;
	System::UnicodeString FOriginalLanguage;
	System::UnicodeString FOriginalLanguageName;
	System::UnicodeString __fastcall GetLanguage(int i);
	int __fastcall GetLanguageCount();
	System::UnicodeString __fastcall GetSelectedLanguage();
	void __fastcall SetSelectedLanguage(const System::UnicodeString value);
	
public:
	__classmethod bool __fastcall Select(Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Vcl::Forms::TPosition position = (Vcl::Forms::TPosition)(0x6))/* overload */;
	__classmethod bool __fastcall Select(System::UnicodeString &language, const System::UnicodeString originalLanguage, const System::UnicodeString originalLanguageName = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Vcl::Forms::TPosition position = (Vcl::Forms::TPosition)(0x6))/* overload */;
	__classmethod bool __fastcall Select(const System::UnicodeString originalLanguage, const System::UnicodeString originalLanguageName = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Vcl::Forms::TPosition position = (Vcl::Forms::TPosition)(0x6))/* overload */;
	__classmethod bool __fastcall Choose(System::UnicodeString &language, const System::UnicodeString originalLanguage = System::UnicodeString(), const System::UnicodeString originalLanguageName = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions options = TNtLanguageDialogOptions() , Vcl::Forms::TPosition position = (Vcl::Forms::TPosition)(0x6));
	__property bool CheckVersion = {read=FCheckVersion, write=FCheckVersion, nodefault};
	__property bool Compatible = {read=FCompatible, write=FCompatible, nodefault};
	__property int LanguageCount = {read=GetLanguageCount, nodefault};
	__property System::UnicodeString Languages[int i] = {read=GetLanguage};
	__property System::UnicodeString OriginalLanguage = {read=FOriginalLanguage, write=FOriginalLanguage};
	__property System::UnicodeString OriginalLanguageName = {read=FOriginalLanguageName, write=FOriginalLanguageName};
	__property System::UnicodeString SelectedLanguage = {read=GetSelectedLanguage, write=SetSelectedLanguage};
	__property Ntbase::TNtLanguageName LanguageName = {read=FLanguageName, write=FLanguageName, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TNtLanguageDialog(System::Classes::TComponent* AOwner) : Vcl::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TNtLanguageDialog(System::Classes::TComponent* AOwner, int Dummy) : Vcl::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TNtLanguageDialog() { }
	
public:
	/* TWinControl.CreateParented */ inline __fastcall TNtLanguageDialog(HWND ParentWindow) : Vcl::Forms::TForm(ParentWindow) { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntlanguagedlg */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_NTLANGUAGEDLG)
using namespace Ntlanguagedlg;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// NtlanguagedlgHPP
