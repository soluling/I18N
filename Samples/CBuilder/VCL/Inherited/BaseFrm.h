//---------------------------------------------------------------------------

#ifndef BaseFrmH
#define BaseFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
//---------------------------------------------------------------------------
class TBaseForm : public TForm
{
__published:	// IDE-managed Components
	TLabel *Label1;
private:	// User declarations
public:		// User declarations
	__fastcall TBaseForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBaseForm *BaseForm;
//---------------------------------------------------------------------------
#endif
