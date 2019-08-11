unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Label0: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label11: TLabel;
    Label21: TLabel;
    Label101: TLabel;
    Label111: TLabel;
    ZeroCheck: TCheckBox;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);
    procedure ZeroCheckChange(Sender: TObject);

  private
    procedure UpdateStrings;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  NtPattern,
  NtResource,
  FMX.NtLanguageDlg,
  FMX.NtTranslator;

procedure TForm1.UpdateStrings;

  function Process(count: Integer): String;
  begin
    if not ZeroCheck.IsChecked then
    begin
      // Contains two patterns: one and other.
      Result := TMultiPattern.Format(_T('{plural, one {%d file} other {%d files}}', 'MessagePlural'), count, [count]);
    end
    else
    begin
      // Contains three patterns: zero, one and other.
      Result := TMultiPattern.Format(_T('{plural, zero {No files} one {%d file} other {%d files}}', 'ZeroMessagePlural'), count, [count]);
    end;
  end;

begin
  label0.Text := Process(0);
  label1.Text := Process(1);
  label2.Text := Process(2);
  label3.Text := Process(3);
  label4.Text := Process(4);
  label5.Text := Process(5);
  label11.Text := Process(11);
  label21.Text := Process(21);
  label101.Text := Process(101);
  label111.Text := Process(111);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NtResources._T('English', 'en');
  NtResources._T('Finnish', 'fi');
  NtResources._T('German', 'de');
  NtResources._T('French', 'fr');
  NtResources._T('Japanese', 'ja');

  _T(Self);
  UpdateStrings;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select then
    UpdateStrings;
end;

procedure TForm1.ZeroCheckChange(Sender: TObject);
begin
  UpdateStrings;
end;

end.
