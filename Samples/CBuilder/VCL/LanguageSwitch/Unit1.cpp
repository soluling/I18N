//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Strings.hpp"
#include "NtLanguageDlg.hpp"
#include "NtPictureTranslator.hpp"
#include "NtListViewTranslator.hpp"
#include "NtTreeViewTranslator.hpp"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
  TNtPictureTranslator::ForceUse();   // Enables runtime language switch of TImage.Picture
  TNtListViewTranslator::ForceUse();  // Enables runtime language switch of TListView
  TNtTreeViewTranslator::ForceUse();  // Enables runtime language switch of TTreeView
}

// This procedure initializes the properties that are set on run time
void TForm1::UpdateItems()
{
  Label2->Caption = System::LoadResourceString(&Strings::_SSample);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
  // Set the properties for first time
  UpdateItems();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::LanguageButtonClick(TObject *Sender)
{
  // Show a language select dialog and turn on the selected language
  if (TNtLanguageDialog::Select("en", "", lnBoth))
  {
	// Language has been changed.
	// Properties that were set on run time must be reset.
	UpdateItems();
  }
}
//---------------------------------------------------------------------------

