unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    LanguageButton: TButton;
    LanguageNameGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
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
  NtBase, NtLanguageDlg;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LanguageNameGroup.ItemIndex := 0;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  TNtLanguageDialog.Select('en', '', TNtLanguageName(LanguageNameGroup.ItemIndex));
end;

end.
