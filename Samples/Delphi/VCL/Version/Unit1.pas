unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
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
  NtLocalization,
  NtTranslator,
  NtLanguageDlg;

procedure TForm1.FormCreate(Sender: TObject);
resourcestring
  STwo = 'Two';
begin
  Label2.Caption := STwo;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en') then
    FormCreate(Self);
end;

initialization
  // If a resource DLL has been loaded then check if the version of DLL and application match.
  // If not then make application not to use the DLL.
  if LoadedResourceLocale <> '' then
  begin
    if not TNtResource.DoesVersionMatch then
    begin
      // Inform user that versions do not match
      ShowMessage(Format(
        'The version of "%0:s" resource DLL file does not match to "%1:s" application file.'#13#10 +
        'The application can not run correctly because the old resource DLL may cause the application to crash.'#13#10 +
        'The application rejected the DLL and is now using resource of EXE file.',
        [TNtBase.GetLanguageFile(Application.ExeName, LoadedResourceLocale), Application.ExeName]));

      // Force application not to use the resource DLL
      TNtTranslator.SetNew('');
    end;
  end;
end.
