//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Unit2.hpp"
#include "BaseFrm.h"
#include "DerivedFrm.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  TBaseForm* baseForm = new TBaseForm(this);
  baseForm->Show();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  TDerivedForm* derivedForm = new TDerivedForm(this);
  derivedForm->Show();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  Label1->Caption = System::LoadResourceString(&Unit2::_SStr1);
  Label2->Caption = System::LoadResourceString(&Unit2::_SStr2);
}
//---------------------------------------------------------------------------

