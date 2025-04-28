unit SharedInheritedFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SharedFrm, Vcl.StdCtrls;

type
  TSharedInheritedForm = class(TSharedForm)
    CheckBox1: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
