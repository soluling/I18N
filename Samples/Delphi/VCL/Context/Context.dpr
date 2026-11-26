program Context;

// Context sample: ambiguous short strings, //loc comments, length limits, placeholder
// descriptions and patterns. See Unit1.pas and README.md.

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
