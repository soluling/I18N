{
  This sample shows how to make a single file deployment.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtBaseTranslator,
  NtLanguageDlg;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TNtLanguageDialog.Select('en');
end;

initialization
  NtEnabledProperties := STRING_TYPES;

  // Extract resource DLL files from resources into external files so VCL can
  // use them.
  TNtBase.ExtractFiles;
end.
