program Tests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  DUnitTestRunner,
  NtPattern in '..\NtPattern.pas',
  PatternTests in 'PatternTests.pas',
  NumberTests in 'NumberTests.pas',
  NtNumberData in '..\NtNumberData.pas',
  NtHiddenId in '..\NtHiddenId.pas',
  HiddenTests in 'HiddenTests.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
end.

