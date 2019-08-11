unit MessagesFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, BaseFrm;

type
  TMessagesForm = class(TBaseForm)
    StaticLabel: TLabel;
    RuntimeLabel: TLabel;
    SingularLabel: TLabel;
    FewLabel: TLabel;
    ManyLabel: TLabel;
    OtherLabel: TLabel;
    DynamicLabel: TLabel;
    Dynamic2Label: TLabel;
    GenderLabel: TLabel;
    SelectLabel: TLabel;
    procedure FormCreate(Sender: TObject);

  public
    procedure UpdateStrings;
  end;

var
  MessagesForm: TMessagesForm;

implementation

{$R *.dfm}

uses
  NtPattern;

procedure TMessagesForm.FormCreate(Sender: TObject);
begin
  UpdateStrings;
end;

procedure TMessagesForm.UpdateStrings;
resourcestring
  SRuntime = 'Runtime message'; //loc Message text
  SDynamic = 'Hello %s!'; //loc 0: Name of the user
  SDynamic2 = 'Hello %s and %s!'; //loc 0: Name of the first user, 1: Name of the second user
  SMessagePlural = '{plural, one {%d file} other {%d files}}'; //loc 0: File count
  SMessageGender = '{gender, male {%s will bring his bicycle} female {%s will bring her bicycle}}';  //loc 0: Name of a person
  SMessageSelect = '{select, soccer {%s is the best soccer player} hockey {%s is the best ice hockey player} basketball {%s is the best basketball player}}'; //loc 0: Name of the player
var
  str, str2, player: String;
begin
  str := 'John';
  str2 := 'Jill';
  player := 'Wayne Gretzky';

  // Runtime messages
  RuntimeLabel.Caption := SRuntime;
  DynamicLabel.Caption := Format(SDynamic, [str]);
  Dynamic2Label.Caption := Format(SDynamic2, [str, str2]);

  // Plural enabled messages
  SingularLabel.Caption := TMultiPattern.Format(SMessagePlural, 1);
  FewLabel.Caption := TMultiPattern.Format(SMessagePlural, 2);
  ManyLabel.Caption := TMultiPattern.Format(SMessagePlural, 5);
  OtherLabel.Caption := TMultiPattern.Format(SMessagePlural, 10);

  // Gender enabled messages
  GenderLabel.Caption := TMultiPattern.Format(SMessageGender, 'male', [str]);

  // Select enabled messages
  SelectLabel.Caption := TMultiPattern.Format(SMessageSelect, 'hockey', [player]);
end;

end.
