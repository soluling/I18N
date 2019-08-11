{
  @abstract Implements routine that makes the default locale to match the locale of the user instead the language of the operating system.

  The default locale of a Delphi application depends on the Delphi version. Up to
  Delphi 2009 the default locale was locale set in the Regional Settings of
  Control Panel. Starting from Delphi 2010 this no longer the case. The default
  locale is the language of the operating system itself.

  If you want to have the old behaviour with Delphi 2010 or later use this unit.
  The unit does not expose any functions but you just add this into your project such
  way that the unit is called first. A suitable place is the first item in the uses
  clause of the program.

  @longCode(#
program Project1;

uses
  NtInitialLocale,
  Forms,
  Unit1 in 'Unit1.pas';
#)

  This makes sure that the initialization block of the unit is called before calling
  any other initialization block.

  See @italic(Samples\Delphi\VCL\LanguageSwitch) sample to see how to use the unit.
}
unit NtInitialLocale;

{$I NtVer.inc}

interface

implementation

{$IFDEF DELPHI2010}
uses
  NtBase;

initialization
  TNtBase.SetInitialLocale(lsSettings);
{$ENDIF}
end.
