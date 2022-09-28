unit NtTest;

interface

implementation

uses
  TypInfo;

var
  tt: TTypeKind;
initialization
  tt := tkString;
  tt := tkLString;
  tt := tkWString;
{$IFDEF UNICODE}
  tt := tkUString;
{$ENDIF}
end.