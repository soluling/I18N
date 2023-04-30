//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>
#include <Vcl.Menus.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *DistanceLabel;
	TEdit *DistanceEdit;
	TLabel *SpeedLabel;
	TEdit *SpeedEdit;
	TButton *CalculateButton;
	TImage *CarImage;
	TActionList *ActionList1;
	TAction *CalculateAction;
	TMainMenu *MainMenu1;
	TMenuItem *File1;
	TMenuItem *Calculate1;
	TMenuItem *N1;
	TMenuItem *Exit1;
	TMenuItem *Help1;
	TMenuItem *About1;
	TImage *FlagImage;
	TLabel *ResultLabel;
	TMenuItem *Language1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall EditChange(TObject *Sender);
	void __fastcall CalculateActionExecute(TObject *Sender);
	void __fastcall Exit1Click(TObject *Sender);
	void __fastcall About1Click(TObject *Sender);
	void __fastcall Language1Click(TObject *Sender);
private:	// User declarations
	Double GetDistance();
	Double GetSpeed();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);

	__property Double Distance = {read=GetDistance};
	__property Double Speed = {read=GetSpeed};
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
