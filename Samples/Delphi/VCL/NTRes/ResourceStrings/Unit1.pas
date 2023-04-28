unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    procedure Translate;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase,
  NtResource,
  NtResourceEx,
  NtTranslatorEx;

procedure TForm1.Translate;
resourcestring
  SHello = 'Hello';
  SSample = 'This is a sample';
  SStr1 = 'Str1';
  SStr2 = 'Str2';
  SHelloName = 'Hello %s';
begin
  _T(Self);

  Label2.Caption := SHello;
  Label3.Caption := SSample;
  Label4.Caption := SStr1;
  Label5.Caption := SStr2;
  Label6.Caption := Format(SHelloName, ['John']);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if ParamCount > 0 then
    NtResources.LanguageId := ParamStr(1);

  Translate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  NtResources.LanguageId := 'fi';
  Translate;
end;

function TranslateResourceString(resStringRec: PResStringRec): String;
begin
  Result := _T(resStringRec);
end;

initialization
  LoadResStringFunc := TranslateResourceString;
  DefaultLocale := 'en';
end.
