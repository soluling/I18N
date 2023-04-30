#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"

#pragma package(smart_init)
#pragma resource "*.dfm"

TForm1 *Form1;

__fastcall TForm1::TForm1(TComponent* Owner): TForm(Owner)
{
}

Double TForm1::GetDistance()
{
  return StrToFloatDef(DistanceEdit->Text, 0);
}

Double TForm1::GetSpeed()
{
  return StrToFloatDef(SpeedEdit->Text, 0);
}

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  EditChange(this);
}

void __fastcall TForm1::EditChange(TObject *Sender)
{
  CalculateAction->Enabled = (Distance > 0) && (Speed > 0);
}

void __fastcall TForm1::CalculateActionExecute(TObject *Sender)
{
  if (Speed == 0)
	return;

  Double time = Distance/Speed;
  int hours = trunc(time);
  int minutes = round(60*(time - hours));

  HoursLabel->Caption = IntToStr(hours) + " hours";
  MinutesLabel->Caption = IntToStr(minutes) + " minutes";
}

void __fastcall TForm1::Exit1Click(TObject *Sender)
{
  Close();
}

void __fastcall TForm1::About1Click(TObject *Sender)
{
  ShowMessage("Driving time calculator");
}

