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
  NtTranslator;

procedure TForm1.Translate;
begin
  _T(Self);

  Label2.Caption := _T('Hello');
  Label3.Caption := _T('This is a sample', 'Unit1');
  Label4.Caption := _T('Another group', 'Str1', 'Another');
  Label5.Caption := _T('Another group', 'Str2', 'Another');
  Label6.Caption := Format(_T('Hello %s'), ['John']);
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

initialization
  DefaultLocale := 'en';
end.
