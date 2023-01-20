unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LanguageButton: TButton;
    LoadedLabel: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtBase,
  NtResource,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;
var
  i: Integer;
  str: String;
  directories: TStringDynArray;
begin
  Label2.Text := _T('This is a sample');  //loc This is a comment

  directories := NtResources.ResourceDirectories;
  str := directories[0];

  for i := 1 to Length(directories) - 1 do
    str := str + sLineBreak + directories[i];

  Label3.Text := str;

  case NtResources.TranslationSource of
    tsNone: str := _T('None');
    tsResource: str := _T('Resource:');
    tsFile: str := _T('File:');
    tsDirectory: str := _T('Directory:');
  end;

  LoadedLabel.Text := Format('%s %s', [str, NtResources.TranslationSourceValue]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  if ParamCount > 0 then
    NtResources.LanguageId := ParamStr(1);

  _T(Self);

  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en', lnBoth) then
    UpdateStrings;
end;

initialization
  // Here you can specify a custom directory where you translation file(s) is located
  //NtResources.ResourcePath := 'D:\NT\Deploy\Samples\Delphi\FMX\ExternalFile\NtLangRes.ntres';
end.
