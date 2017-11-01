//  DELPHI6     Delphi 6 or higher
//  DELPHI7     Delphi 7 or higher
//  DELPHI2005  Delphi 2005 or higher
//  DELPHI2006  Delphi 2006 or higher
//  DELPHI2007  Delphi 2007 or higher

//  DELPHI2009  Delphi 2009 or higher
//  DELPHI2010  Delphi 2010 or higher
//  DELPHIXE    Delphi XE or higher
//  DELPHIXE2   Delphi XE2 or higher
//  DELPHIXE3   Delphi XE3 or higher
//  DELPHIXE4   Delphi XE4 or higher
//  DELPHIXE5   Delphi XE5 or higher
//  DELPHIXE6   Delphi XE6 or higher
//  DELPHIXE7   Delphi XE7 or higher
//  DELPHIXE8   Delphi XE8 or higher
//  DELPHIDX    Delphi 10 Seattle or higher
//  DELPHIDX1   Delphi 10.1 Berlin or higher
//  DELPHIDX2   Delphi 10.2 Tokyo or higher
//  DELPHIDX3   Delphi 10.3 xxx or higher

{$IF CompilerVersion >= 24}
  {$LEGACYIFEND ON}
{$IFEND}

{$IF CompilerVersion >= 14}
  {$DEFINE DELPHI6}
{$IFEND}

{$IF CompilerVersion >= 15}
  {$DEFINE DELPHI7}
{$IFEND}

{$IF CompilerVersion >= 17}
  {$DEFINE DELPHI2005}
{$IFEND}

{$IF CompilerVersion >= 18}
  {$DEFINE DELPHI2006}
{$IFEND}

{$IF CompilerVersion >= 18.5}
  {$DEFINE DELPHI2007}
{$IFEND}

{$IF CompilerVersion >= 20}
  {$DEFINE DELPHI2009}
{$IFEND}

{$IF CompilerVersion >= 21}
  {$DEFINE DELPHI2010}
{$IFEND}

{$IF CompilerVersion >= 22}
  {$DEFINE DELPHIXE}
{$IFEND}

{$IF CompilerVersion >= 23}
  {$DEFINE DELPHIXE2}
  {$DEFINE DELPHI_FMX}
{$IFEND}

{$IF CompilerVersion >= 24}
  {$DEFINE DELPHIXE3}
{$IFEND}

{$IF CompilerVersion >= 25}
  {$LEGACYIFEND ON}
  {$DEFINE DELPHIXE4}
{$IFEND}

{$IF CompilerVersion >= 26}
  {$DEFINE DELPHIXE5}
{$IFEND}

{$IF CompilerVersion >= 27}
  {$DEFINE DELPHIXE6}
{$IFEND}

{$IF CompilerVersion >= 28}
  {$DEFINE DELPHIXE7}
{$IFEND}

{$IF CompilerVersion >= 29}
  {$DEFINE DELPHIXE8}
{$IFEND}

{$IF CompilerVersion >= 30}
  {$DEFINE DELPHIDX}
{$IFEND}

{$IF CompilerVersion >= 31}
  {$DEFINE DELPHIDX1}
{$IFEND}

{$IF CompilerVersion >= 32}
  {$DEFINE DELPHIDX2}
{$IFEND}

{$IF CompilerVersion >= 33}
  {$DEFINE DELPHIDX3}
{$IFEND}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

{$IFDEF DELPHI7}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}
