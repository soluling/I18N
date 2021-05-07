//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DerivedFrm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseFrm"
#pragma resource "*.dfm"
TDerivedForm *DerivedForm;
//---------------------------------------------------------------------------
__fastcall TDerivedForm::TDerivedForm(TComponent* Owner)
	: TBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
