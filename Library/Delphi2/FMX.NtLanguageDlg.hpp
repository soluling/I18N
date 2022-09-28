// CodeGear C++Builder
// Copyright (c) 1995, 2021 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.NtLanguageDlg.pas' rev: 35.00 (iOS)

#ifndef Fmx_NtlanguagedlgHPP
#define Fmx_NtlanguagedlgHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.SysUtils.hpp>
#include <System.Types.hpp>
#include <System.UITypes.hpp>
#include <System.Classes.hpp>
#include <System.Variants.hpp>
#include <FMX.Types.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Forms.hpp>
#include <FMX.Dialogs.hpp>
#include <FMX.Layouts.hpp>
#include <FMX.ListBox.hpp>
#include <FMX.StdCtrls.hpp>
#include <FMX.NtLocalization.hpp>
#include <FMX.Controls.Presentation.hpp>
#include <NtBase.hpp>
#include <System.Generics.Collections.hpp>
#include <System.Generics.Defaults.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Ntlanguagedlg
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TNtLanguageDialog;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TNtLanguageDialogOption : unsigned char { ldShowErrorIfNoResource };

typedef System::Set<TNtLanguageDialogOption, TNtLanguageDialogOption::ldShowErrorIfNoResource, TNtLanguageDialogOption::ldShowErrorIfNoResource> TNtLanguageDialogOptions;

class PASCALIMPLEMENTATION TNtLanguageDialog : public Fmx::Forms::TForm
{
	typedef Fmx::Forms::TForm inherited;
	
__published:
	Fmx::Listbox::TListBox* LanguageList;
	Fmx::Stdctrls::TButton* OkButton;
	Fmx::Stdctrls::TButton* CancelButton;
	Fmx::Controls::TStyleBook* Resources1;
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall FormActivate(System::TObject* Sender);
	void __fastcall FormClose(System::TObject* Sender, System::Uitypes::TCloseAction &Action);
	void __fastcall LanguageListDblClick(System::TObject* Sender);
	
private:
	System::UnicodeString FOriginalLanguage;
	Ntbase::TNtLanguageName FLanguageName;
	TNtLanguageDialogOptions FOptions;
	System::UnicodeString __fastcall GetLanguage(int i);
	int __fastcall GetLanguageCount();
	System::UnicodeString __fastcall GetSelectedLanguage();
	void __fastcall SetSelectedLanguage(const System::UnicodeString value);
	
public:
	__classmethod bool __fastcall Select(System::UnicodeString &language, System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__classmethod bool __fastcall Select(System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__classmethod void __fastcall Select(System::DelphiInterface<System::Sysutils::TProc__1<System::UnicodeString> > done, System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__classmethod void __fastcall Select(System::Sysutils::_di_TProc done, System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions dialogOptions = TNtLanguageDialogOptions() , Ntbase::TNtResourceOptions languageOptions = Ntbase::TNtResourceOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__classmethod bool __fastcall Choose(System::UnicodeString &language, System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions options = TNtLanguageDialogOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__classmethod void __fastcall Choose(System::DelphiInterface<System::Sysutils::TProc__1<System::UnicodeString> > done, System::UnicodeString originalLanguage = System::UnicodeString(), Ntbase::TNtLanguageName languageName = (Ntbase::TNtLanguageName)(0x0), TNtLanguageDialogOptions options = TNtLanguageDialogOptions() , Fmx::Forms::TFormPosition position = (Fmx::Forms::TFormPosition)(0x6))/* overload */;
	__property int LanguageCount = {read=GetLanguageCount, nodefault};
	__property System::UnicodeString Languages[int i] = {read=GetLanguage};
	__property System::UnicodeString OriginalLanguage = {read=FOriginalLanguage, write=FOriginalLanguage};
	__property System::UnicodeString SelectedLanguage = {read=GetSelectedLanguage, write=SetSelectedLanguage};
	__property Ntbase::TNtLanguageName LanguageName = {read=FLanguageName, write=FLanguageName, nodefault};
public:
	/* TCustomForm.Create */ inline __fastcall virtual TNtLanguageDialog(System::Classes::TComponent* AOwner) : Fmx::Forms::TForm(AOwner) { }
	/* TCustomForm.CreateNew */ inline __fastcall virtual TNtLanguageDialog(System::Classes::TComponent* AOwner, NativeInt Dummy) : Fmx::Forms::TForm(AOwner, Dummy) { }
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TNtLanguageDialog() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Ntlanguagedlg */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_NTLANGUAGEDLG)
using namespace Fmx::Ntlanguagedlg;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_NtlanguagedlgHPP
