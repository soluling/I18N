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
    Label7: TLabel;
    Label8: TLabel;
    Label0_0: TLabel;
    Label0_1: TLabel;
    Label0_2: TLabel;
    Label1_0: TLabel;
    Label1_1: TLabel;
    Label1_2: TLabel;
    Label2_0: TLabel;
    Label2_1: TLabel;
    Label2_2: TLabel;
    Label3_0: TLabel;
    Label3_1: TLabel;
    Label3_2: TLabel;
    Label0_3: TLabel;
    Label1_3: TLabel;
    Label2_3: TLabel;
    Label3_3: TLabel;
    Label9: TLabel;
    Label0_4: TLabel;
    Label1_4: TLabel;
    Label2_4: TLabel;
    Label3_4: TLabel;
    Label10: TLabel;
    Label4_0: TLabel;
    Label4_1: TLabel;
    Label4_2: TLabel;
    Label4_3: TLabel;
    Label4_4: TLabel;
    Label11: TLabel;
    Label5_0: TLabel;
    Label5_1: TLabel;
    Label5_2: TLabel;
    Label5_3: TLabel;
    Label5_4: TLabel;
    procedure FormCreate(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  NtBase, NtPattern;

procedure TForm1.FormCreate(Sender: TObject);

  procedure Process(count: Integer; label1, label2, label3, label4, label5: TLabel);
  resourcestring
    // Contains the standard two patterns: one and other.
    SMessagePlural = '{plural, one {%d file} other {%d files}}';  //loc 0: Amount of files
    // Contains three patterns: zero, one and other.
    SZeroMessagePlural = '{plural, zero {No files} one {One file} other {%d files}}';  //loc 0: Amount of files
    // Contains four patterns: zero, one, two and other.
    STwoMessagePlural = '{plural, zero {No files} one {One file} two {Two files} other {%d files}}';  //loc 0: Amount of files
    // Contains four patterns: =0, one, =2 and other.
    S2MessagePlural = '{plural, =0 {No files} one {One file} =2 {Two files} other {%d files}}';  //loc 0: Amount of files
    // Contains fixed patterns: =0, =1, =2, <6, <=10, ~12 and other.
    STextMessagePlural = '{plural, =0 {No files} =1 {One file} =2 {Two files} <6 {Few files} <=10 {Several files} ~12 {A dozen files} other {Lots of files}}';  //loc 0: Amount of files
  begin
    label1.Caption := TMultiPattern.Format(SMessagePlural, [count]);
    label2.Caption := TMultiPattern.Format(SZeroMessagePlural, [count]);
    label3.Caption := TMultiPattern.Format(STwoMessagePlural, [count]);
    label4.Caption := TMultiPattern.Format(S2MessagePlural, [count]);
    label5.Caption := TMultiPattern.Format(STextMessagePlural, [count]);
  end;

begin
  Process(0, label0_0, label0_1, label0_2, label0_3, label0_4);
  Process(1, label1_0, label1_1, label1_2, label1_3, label1_4);
  Process(2, label2_0, label2_1, label2_2, label2_3, label2_4);
  Process(3, label3_0, label3_1, label3_2, label3_3, label3_4);
  Process(11, label4_0, label4_1, label4_2, label4_3, label4_4);
  Process(20, label5_0, label5_1, label5_2, label5_3, label5_4);
end;

// Plural engine needs to know the language that the application uses.
// Compiled EXE file does not contain this information. This is why we create
// a resource string that contains the locale code.
// The default locale is set to this code.
// If you name the resource string to SNtLocale NewTool automatically translates
// it to using the locale code of the target language.
resourcestring
  SNtLocale = 'en';
initialization
  DefaultLocale := SNtLocale;
end.
