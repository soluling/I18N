//---------------------------------------------------------------------------

#ifndef Unit1H
#define Unit1H
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Imaging.pngimage.hpp>
//---------------------------------------------------------------------------
class TForm1 : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label2;
	TListView *ListView1;
	TListBox *ListBox1;
	TImage *Image1;
	TButton *LanguageButton;
	TTreeView *TreeView1;
	TLabel *Label1;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall LanguageButtonClick(TObject *Sender);
private:	// User declarations
	void UpdateItems();
public:		// User declarations
	__fastcall TForm1(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TForm1 *Form1;
//---------------------------------------------------------------------------
#endif
