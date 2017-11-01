{
  This applications shows how you should replace string string arrays (if any)
  in the case you plan to support runtime language switch.
}

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LanguageButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure LanguageButtonClick(Sender: TObject);

  private
    procedure UpdateItems;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtLanguageDlg;

resourcestring
  SOne = 'One';
  STwo = 'Two';

const
  // This is a bad code because Delphi initializes the array when application
  // starts and the values are whatever resource is loaded on that time.
  // If you change the language the values in the array do not change.
  // Do not use code like this if you use runtime language change.
  VALUES: array[0..1] of String = (SOne, STwo);

  // You have two choices:

  // 1)
  // This is a solution that VCL uses on some cases. The array does not contain
  // the resource string value but pointer to the resource string.
  POINTERS: array[0..1] of Pointer = (@SOne, @STwo);


// 2)
// This is another solution. You replace array but a function that returns the
// right resource string.
// We at NewTool recommend this.
function GetValue(i: Integer): String;
begin
  case i of
    0: Result := SOne;
    1: Result := STwo;
  else
    Result := '';
  end;
end;

procedure TForm1.UpdateItems;
begin
  // Bad code
  Label1.Caption := VALUES[0];
  Label2.Caption := VALUES[1];

  // Good code #1
  Label3.Caption := LoadResString(POINTERS[0]);
  Label4.Caption := LoadResString(POINTERS[1]);

  // Good code #2
  Label5.Caption := GetValue(0);
  Label6.Caption := GetValue(1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateItems;
end;

procedure TForm1.LanguageButtonClick(Sender: TObject);
begin
  if TNtLanguageDialog.Select('en') then
    UpdateItems;
end;

end.
