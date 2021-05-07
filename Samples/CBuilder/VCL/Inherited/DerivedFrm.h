//---------------------------------------------------------------------------

#ifndef DerivedFrmH
#define DerivedFrmH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>
#include "BaseFrm.h"
//---------------------------------------------------------------------------
class TDerivedForm : public TBaseForm
{
__published:	// IDE-managed Components
	TCheckBox *CheckBox1;
private:	// User declarations
public:		// User declarations
	__fastcall TDerivedForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDerivedForm *DerivedForm;
//---------------------------------------------------------------------------
#endif
