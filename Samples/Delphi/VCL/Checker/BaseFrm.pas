unit BaseFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TBaseForm = class(TForm)
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BaseForm: TBaseForm;

implementation

{$R *.dfm}

{$IFDEF CHECKUI}
uses
  NtChecker;
{$ENDIF}

procedure TBaseForm.FormActivate(Sender: TObject);
begin
  // Check layout issues such as truncations and overlappings
{$IFDEF CHECKUI}
  NtFormChecker.Check(Self);
{$ENDIF}
end;

end.
