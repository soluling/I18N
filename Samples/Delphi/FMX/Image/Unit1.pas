unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    FormImageLabel: TLabel;
    FormImage: TImage;
    ResourceImageLabel: TLabel;
    ResourceImage: TImage;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateValues;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtResource,
  NtResourceString,
  FMX.NtImageTranslator,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateValues;
var
  stream: TStream;
begin
  stream := NtResources.GetResource('Image1');
  try
    ResourceImage.Bitmap.LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  SEnglish = 'English';
  SFinnish = 'Finnish';
  SGerman = 'German';
  SFrench = 'French';
  SJapanese = 'Japanese';
begin
  // Add language names so they can be localized
  // Attach a flag image to each name so the language dialog can show the flag with language name
  NtResources.Add('English', 'English', SEnglish, 'en').AddImage('en');
  NtResources.Add('Finnish', 'suomi', SFinnish, 'fi').AddImage('fi');
  NtResources.Add('German', 'Deutsch', SGerman, 'de').AddImage('de');
  NtResources.Add('French', 'français', SFrench, 'fr').AddImage('fr');
  NtResources.Add('Japanese', '日本語', SJapanese, 'ja').AddImage('ja');

  _T(Self);

  UpdateValues;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  TNtLanguageDialog.Select(
    procedure
    begin
      UpdateValues;
    end);
end;

end.
