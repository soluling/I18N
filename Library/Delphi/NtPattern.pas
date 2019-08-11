(*
  @abstract Implements a multi pattern enabled Format function @link(TMultiPattern.Format) that supports plurals, genders and selects.

  Most languages have a singular and plural forms. However this is not case
  with every language. Asian languages do not have plural form at all. They only
  have singular form. Most Slavic languages have three forms: singular, plural
  and one between them (dual or paucal). Sometimes 0 is handled as plural,
  sometimes as singular. Some languages have four different forms. Arabic has
  six different forms.

  This is why you can not use the standard Format if you want to create
  grammatically correct dynamic messages that contain counts and genders. Instead you can
  use @link(TMultiPattern.Format) function. It works little bit like GetText's ngettext
  funtion. You pass combined pattern and count, gender or select parameter(s). The functions
  calculates what pattern to use based on the passed parameters and
  returns that pattern.

  In English and most Western languages this is very simple. There are only
  two items in the combined pattern: singular and plural. If count is 1 then
  the singualar pattern is used. If the count is other than 1 then the plural
  pattern is used.

  If you have something like this

  @code(str := Format('%d files', [fileCount]);)

  and you want to localize it convert code to

  @longCode(#
resourcestring
  SFileCountPlural = '{plural, one {%d file} other {%d files}}';
...
  str := TMultiPattern.Format(SFileCountPlural, fileCount);
#)

  As you can see instead of single pattern the markup contains two parts:
  singular and plural. Add a plural form code in the begining of each part.

  Plural form codes are:
  zero   is for nullar.
  one    is for singular.
  two    is for dual.
  few    is for trial, paucal, sexal, minority plural and plural-100
  many   is for large plural
  other  is for plural

  Other part should be on every pattern. If the language requires more forms add them too.
  In addition you can add zero, one or two parts to any string. For example following
  are valid for English:
  "{, plural one {One file} other {%d files}}"
  "{, plural zero;No files} one {One file} other {%d files}}"
  "{, plural zero;No files} one {One file} two {Two files} other {%d files}}"

  If you pattern contains { or } characters you must escape them with \. Also \ must be escaped.
  'Item {example\two} %d car(s)' -> '{cars, plural, one {Item \{example\\two\} %d car} other {Item \{example\\two\} %d cars}}'

  In addition you can use operators: [o]N where o is the operator and N is the operamd (i.e. count).
  Possible operators are:
  =     Used if the count equals the operand
  ~     Used if the count a about the same as operand. The dela is specified by OperatorDelta.
  <     Used if the count is less than the operand
  >     Used if the count is greater than the operand
  <=    Used if the count is less or equal to the operand
  >=    Used if the count is equal or greater than the operand
  a..b  Used if the count is between a and b

  If no operator is given but just number then equal operatori is assumed. "1" is same as "=1".

  For example following
  "{, plural, 0 {No files} =1 {One file} other {%d files}}"

  Most languages also use genders. There might be male, female and neutral genders.

  Gender codes are:
  male     is for male
  female   is for female
  other    is for neutral or other

  Plural and gender engines need to know the language that the application uses.
  Compiled application file does not contain this information. This is why you
  should create a resource string that contains the locale id and set that value
  to @link(NtBase.DefaultLocale). A good place to set @link(NtBase.DefaultLocale)
  is the initialization block of the main unit.

  @longCode(#
unit Unit1;
...
resourcestring
  SNtLocale = 'en';
initialization
  DefaultLocale := SNtLocale;
end.
#)

  Grammatical number rules have been extracted from CLDR to Delphi code that
  exists in @link(NtPluralData).

  If you name the resource string as @bold(SNtLocale) Soluling automatically translates
  it to hold the locale code of the target language. So German column will have "de" and
  French colum will have "fr".

  @italic(See Samples\Delphi\VCL\Patterns) or @italic(Samples\Delphi\FMX\Patterns)
  samples to see how to use the unit.
*)
unit NtPattern;

{$I NtVer.inc}

interface

uses
{$IFDEF AUTOREFCOUNT}
  System.Generics.Collections,
{$ENDIF}
  Classes;

const
  OTHER_C = 'other';

type
  { @abstract Enumeration that specifies the plural form.
    There are six different plural forms from singular to various plurals.
    How many forms a language uses depends on the language. Most languages use only singular and plural. }
  TPlural =
  (
    pfZero,  //< Nullar. Used when count is zero. Few languages support this but you can include a =0 pattern into your pattern string if you want to handle 0 in a different way.
    pfOne,   //< Singular. Used when count is one. Most languages support this.
    pfTwo,   //< Dual. Used when count is two. Few languages support this.
    pfFew,   //< Trial, paucal, sexal, minority plural or plural-100. Used when count is few. Few languages support this. The range depends on the language. Most often this is something between 2 and 4.
    pfMany,  //< Greater paucal. Used when count is many. Few languages support this. The range depends on the language. Most often this is more than 4.
    pfOther  //< Plural or universal. Used when count does not belong to any other group. All languages support this. Most often this is the plural form.
  );

  { @abstract Set that contains plural forms. }
  TPlurals = set of TPlural;

  { @abstract Enumeration that specifies the different gender values.
    There are three different genders: neutral, male and female.
    How many values a language uses depends on the language. }
  TGender =
  (
    geMale,    //< Male.
    geFemale,  //< Female.
    geNeutral  //< Neutral, other or no gender used.
  );

  { @abstract Set that contains gender values. }
  TGenders = set of TGender;

  { @abstract Enumeration that specifies the operator. }
  TOperatorKind =
  (
    okEqual,           //< Equal (e.g. =1).
    okAround,          //< Around (e.g. ~1).
    okLess,            //< Less than (e.g. <1).
    okLessOrEqual,     //< Less or equal than (e.g. <=1).
    okGreater,         //< Greater than (e.g. >1).
    okGreaterOrEqual,  //< Greater or equal than (e.g. >=1).
    okRange            //< Range (e.g 3..5).
  );

  { @abstract Represents the method that will get the plural form matching the given number.
    @param n Absolute value of the source number (integer and decimals) (e.g. 9.870 => 9.87)
    @param i Integer digits of n (e.g. 9.870 => 9)
    @param v Number of visible fraction digits in n, with trailing zeros (e.g. 9.870 => 3)
    @param w Number of visible fraction digits in n, without trailing zeros (e.g. 9.870 => 2)
    @param f Visible fractional digits in n, with trailing zeros (e.g. 9.870 => 870)
    @param t Visible fractional digits in n, without trailing zeros (e.g. 9.870 => 87)
    @return The plural fomr that the passed number uses. }
  TPluralProc = function(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;

  { @abstract Class that contain one pattern. }
  TPattern = class(TObject)
  private
    FValue: String;

  public
{$IFNDEF DELPHI2009}
    function ToString: String; virtual; abstract;
    function Equals(obj: TObject): Boolean; virtual; abstract;
{$ENDIF}

    property Value: String read FValue write FValue;
  end;

  { @abstract Base class for plural patterns. }
  TNumberPattern = class(TPattern)
  end;

  { @abstract Class that contain one plural pattern. }
  TPluralPattern = class(TNumberPattern)
  private
    FPlural: TPlural;

  public
    constructor Create;

    function Equals(obj: TObject): Boolean; override;
    function ToString: String; override;

    property Plural: TPlural read FPlural write FPlural;  //< Plural value of the pattern.
  end;

  { @abstract Class that contain one operator pattern. }
  TOperatorPattern = class(TNumberPattern)
  private
    FKind: TOperatorKind;
    FOperand: Integer;
    FOperand2: Integer;

  public
    constructor Create;

    function Equals(obj: TObject): Boolean; override;
    function ToString: String; override;

    property Kind: TOperatorKind read FKind write FKind;  //< Operator kind of the pattern.
    property Operand: Integer read FOperand write FOperand;  //< Operand value.
    property Operand2: Integer read FOperand2 write FOperand2;  //< Second operand value.
  end;

  { @abstract Class that contain one gender pattern. }
  TGenderPattern = class(TPattern)
  private
    FGender: TGender;

  public
    constructor Create;

    function Equals(obj: TObject): Boolean; override;
    function ToString: String; override;

    property Gender: TGender read FGender write FGender;  //< Gender value of the pattern.
  end;

  { @abstract Class that contain one select pattern. }
  TSelectPattern = class(TPattern)
  private
    FSelect: String;

  public
    constructor Create;

    function Equals(obj: TObject): Boolean; override;
    function ToString: String; override;

    property Select: String read FSelect write FSelect;  //< Select value of the pattern.
  end;

  { @abstract Enumeration that specifies the different parameter types for TMultiPattern.FormatMulti. }
  TFormatParameterKind =
  (
    fpPlural,    //< Plural parameter (e.g. singular or plural)
    fpOperator,  //< Operator parameter (e.g. =1, ~1, >1, <1, >=1, <=1)
    fpGender,    //< Gender parameter (e.g. male or female)
    fpSelect     //< Select parameter. Select among multiple patterns by a name.
  );

  { @abstract Set that contains format parameter types. }
  TFormatParameterKinds = set of TFormatParameterKind;

  { @abstract Record that contains payload for one parameter for TMultiPattern.FormatMulti. }
  TFormatParameter = record
    case Kind: TFormatParameterKind of
    fpPlural, fpOperator: ( Plural: Integer; );
    fpGender: ( Gender: TGender; Value: PChar; );
    fpSelect: ( Select: PChar; );
  end;

  { @abstract Enumeration that specifies the plural usages. }
  TPluralUsage =
  (
    puInteger,  //< Get rules that are used with interger numbers.
    puDecimal   //< Get rules that are used with decimal or floating point numbers.
  );

  { @abstract Set that contains plural usages. }
  TPluralUsages = set of TPluralUsage;

  TFormatPart = class;

  TFormatPartEnumerator = class(TObject)
  private
    FFormatPart: TFormatPart;
    FIndex: Integer;

  public
    constructor Create(formatPart: TFormatPart);

    function GetCurrent: TPattern;
    function MoveNext: Boolean;

    property Current: TPattern read GetCurrent;
  end;

  { @abstract Class that contains one part of a plural and gender enabed format string. }
  TFormatPart = class(TObject)
  private
{$IFDEF AUTOREFCOUNT}
    FItems: TList<TPattern>;
{$ELSE}
    FItems: TList;
{$ENDIF}
    FName: String;
    FParameterType: String;
    FParent: TFormatPart;

    FUsedKind: TFormatParameterKind;
    FUsedPlural: TPlural;
    FUsedOperator: TOperatorKind;
    FUsedOperand: Integer;
    FUsedOperand2: Integer;
    FUsedGender: TGender;
    FUsedSelect: String;

    function GetKind: TFormatParameterKind;
    function GetCount: Integer;
    function GetFirstValue: String;
    function GetOtherValue: String;
    function GetItem(i: Integer): TPattern;
    function GetDefaultValue: String;
    function GetDefaultGender: TGender;
    function GetDefaultSelect: String;
    function GetPluralValue(plural: TPlural): String;
    function GetMatchingValue(value: Integer): String;
    function GetOperatorValue(kind: TOperatorKind; operand, operand2: Integer): String;
    function GetEqualOperatorValue(operand: Integer): String;
    function GetGenderValue(gender: TGender): String;
    function GetSelectValue(const select: String): String;

    procedure SetPluralValue(plural: TPlural; const value: String);
    procedure SetOperatorValue(kind: TOperatorKind; operand, operand2: Integer; const value: String);
    procedure SetEqualOperatorValue(operand: Integer; const value: String);
    procedure SetGenderValue(gender: TGender; const value: String);
    procedure SetSelectValue(const select: String; const value: String);

  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TFormatPartEnumerator;

    procedure Clear;

    procedure Delete(index: Integer);

    function Find(pattern: TPattern): TPattern; overload;
    function FindAny(pattern: TPattern): TPattern;
    function FindOther: TPattern;

    function Find(gender: TGender): TGenderPattern; overload;
    function Exists(gender: TGender): Boolean; overload;
    function Add(const value: String; gender: TGender): TGenderPattern; overload;
    function FindGender: TGenderPattern;

    function Find(const select: String): TSelectPattern; overload;
    function Exists(const select: String): Boolean; overload;
    function Add(const value: String; const select: String): TSelectPattern; overload;
    function Insert(index: Integer; const value: String; const select: String): TSelectPattern; overload;

    function Find(plural: TPlural): TPluralPattern; overload;
    function Exists(plural: TPlural): Boolean; overload;
    function Add(const value: String; plural: TPlural): TPluralPattern; overload;

    function Find(kind: TOperatorKind; operand, operand2: Integer): TOperatorPattern; overload;
    function Exists(kind: TOperatorKind; operand, operand2: Integer): Boolean; overload;
    function Add(const value: String; kind: TOperatorKind; operand: Integer; operand2: Integer = 0): TOperatorPattern; overload;

    function FindOperator(value: Integer): TOperatorPattern;

    function FindMatching(value: Integer): TPluralPattern;
    function ExistsMatching(value: Integer; var plural: TPlural): Boolean;

    { @abstract. Check if the part is a number part such as plural or operator.
      @return True if the part is a number part. }
    function IsNumber: Boolean;

    { @abstract. Check if the part is a gender part.
      @return True if the part is a gender part. }
    function IsGender: Boolean;

    { @abstract. Check if the part is a select part.
      @return True if the part is a select part. }
    function IsSelect: Boolean;

    function GetPluralPattern(plural: TPlural; count: Cardinal): String;
    function GetGenderPattern(gender: TGender): String;

    property Kind: TFormatParameterKind read GetKind;  //< Parameter kind-
    property Count: Integer read GetCount;  //< Pattern count.
    property FirstValue: String read GetFirstValue;  //< Gets the first pattern.
    property OtherValue: String read GetOtherValue;  //<  Gets the pattern for other plural form.
    property Items[i: Integer]: TPattern read GetItem; default;  //< Array of patterns.
    property Name: String read FName write FName;  //< The name of the part.
    property DefaultGender: TGender read GetDefaultGender;  //< The default gender.
    property DefaultSelect: String read GetDefaultSelect;  //< The default select.
    property ParameterType: String read FParameterType write FParameterType;  //< The type of the parameter type.
    property OperatorValues[kind: TOperatorKind; operand, operand2: Integer]: String read GetOperatorValue write SetOperatorValue;  //< Operator patterns.
    property EqualOperatorValues[operand: Integer]: String read GetEqualOperatorValue write SetEqualOperatorValue;  //< Equal operator patterns.
    property PluralValues[plural: TPlural]: String read GetPluralValue write SetPluralValue;  //< Plural patterns.
    property MatchingValues[plural: Integer]: String read GetMatchingValue;  //< Matching patterns.
    property GenderValues[gender: TGender]: String read GetGenderValue write SetGenderValue;  //< Gender patterns.
    property SelectValues[const select: String]: String read GetSelectValue write SetSelectValue;  //< Select patterns.
    property Parent: TFormatPart read FParent write FParent;

    { @abstract. The default pattern.
      If this is a plural part then it is pattern for other. }
    property DefaultValue: String read GetDefaultValue;

    property UsedKind: TFormatParameterKind read FUsedKind;
    property UsedPlural: TPlural read FUsedPlural;
    property UsedOperator: TOperatorKind read FUsedOperator;
    property UsedOperand: Integer read FUsedOperand;
    property UsedOperand2: Integer read FUsedOperand2;
    property UsedGender: TGender read FUsedGender;
    property UsedSelect: String read FUsedSelect;
  end;

  TFormatString = class;

  TFormatStringEnumerator = class(TObject)
  private
    FFormatString: TFormatString;
    FIndex: Integer;

  public
    constructor Create(formatString: TFormatString);

    function GetCurrent: TFormatPart;
    function MoveNext: Boolean;

    property Current: TFormatPart read GetCurrent;
  end;

  { @abstract Enumeration that specifies the placeholder syntax. }
  TPlaceholderKind =
  (
    pkPrintf,       //< %0:d
    pkDoubleBrace,  //< {{0}}
    pkSingleBrace   //< {0}
  );

  { @abstract Enumeration that specifies how braces in ICU message are escaped. }
  TIcuMessageEscape =
  (
    ieDefault,  //< Single backslash is used a escape character. This is \\ \{sample's\} text     -> This is \ {sample's} text
    ieReact,    //< Double backslash is used a escape character. TThis is \\\\ \\{sample's\\} text -> This is \ {sample's} text
    ieOriginal  //< Original ICU specification when single quote escaping is used. This is \ '{sample''s}' text     -> This is \ {sample's} text
  );

  { @abstract Enumeration that specifies the pattern format. }
  TFormatStringSyntax =
  (
    fsIcu,    //< ICU message format
    fsLegacy  //< Soluling's legacy format
  );

  { @abstract Class that contains patterns of plural and/or gender enabled format string. }
  TFormatString = class(TObject)
  private
{$IFDEF AUTOREFCOUNT}
    FItems: TList<TFormatPart>;
{$ELSE}
    FItems: TList;
{$ENDIF}
    FStartPattern: String;
    FPlaceholderKind: TPlaceholderKind;
    FEscape: TIcuMessageEscape;
    FSyntax: TFormatStringSyntax;

    function GetCount: Integer;
    function GetItem(i: Integer): TFormatPart;
    function GetText: String;
    function GetOperatorValue(kind: TOperatorKind; operand, operand2: Integer): String;
    function GetEqualOperatorValue(operand: Integer): String;
    function GetPluralValue(value: TPlural): String;
    function GetMatchingValue(value: Integer): String;
    function GetGenderValue(value: TGender): String;
    function GetSelectValue(const value: String): String;

    function ParseIcuPattern(const pattern: String; var index: Integer): Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function GetEnumerator: TFormatStringEnumerator;

    procedure Clear;

    function Find(pattern: TPattern): TPattern; overload;
    function FindAny(pattern: TPattern): TPattern;

    function Find(gender: TGender; all: Boolean = True): TGenderPattern; overload;
    function Exists(gender: TGender): Boolean; overload;
    function AddValue(gender: TGender; const value: String): TGenderPattern; overload;

    function Find(plural: TPlural; all: Boolean = True): TPluralPattern; overload;
    function Exists(plural: TPlural): Boolean; overload;
    function AddValue(plural: TPlural; const value: String): TPluralPattern; overload;

    function Find(kind: TOperatorKind; operand, operand2: Integer; all: Boolean = True): TOperatorPattern; overload;
    function Exists(kind: TOperatorKind; operand, operand2: Integer): Boolean; overload;

    function FindMatching(operand: Integer; all: Boolean = True): TPluralPattern;
    function ExistsMatching(operand: Integer): Boolean;

    function AddParameter(const name: String = ''): TFormatPart;

    { Parse pattern into parts.
      @param pattern Original multi pattern string.
      @param values  String list to hold parsed patterns. }
    procedure ParsePattern(const pattern: String);

    function ParseLegacy(pattern: String): Boolean;
    function ParseIcu(pattern: String): Boolean;

    class function IsPattern(const pattern: String): Boolean;

    property Count: Integer read GetCount;
    property Items[i: Integer]: TFormatPart read GetItem; default;
    property StartPattern: String read FStartPattern write FStartPattern;
    property Text: String read GetText;
    property OperatorValues[kind: TOperatorKind; operand, operand2: Integer]: String read GetOperatorValue;
    property EqualOperatorValues[value: Integer]: String read GetEqualOperatorValue;
    property PluralValues[value: TPlural]: String read GetPluralValue;
    property MatchingValues[value: Integer]: String read GetMatchingValue;
    property GenderValues[value: TGender]: String read GetGenderValue;
    property SelectValues[const value: String]: String read GetSelectValue;
    property PlaceholderKind: TPlaceholderKind read FPlaceholderKind write FPlaceholderKind;
    property Escape: TIcuMessageEscape read FEscape write FEscape;
    property Syntax: TFormatStringSyntax read FSyntax write FSyntax;
  end;

  { @abstract Class that contains information about plural rules of a single language. }
  TPluralInfo = class
  private
    FId: String;
    FProc: TPluralProc;

    FIntegerCount: Integer;
    FIntegerPlurals: TPlurals;
    FDefaultInteger: TPlural;
    FDecimalCount: Integer;
    FDecimalPlurals: TPlurals;
    FDefaultDecimal: TPlural;
    FExpression: String;

    function GetInteger(i: Integer): TPlural;
    function GetDecimal(i: Integer): TPlural;

    procedure SetIntegerPlurals(value: TPlurals);
    procedure SetDecimalPlurals(value: TPlurals);

  public
    constructor Create;

    { Get a plural info of a language. If direct match is not found try to find a matching and if not found return the default.
      @param id Language or locale to be used. If empty the active locale id is used.
      @return Plural info. }
    class function Get(id: String = ''): TPluralInfo;

    { Get a plural info of a language. If a direct match is not found return null.
      @param id Language or locale to be used.
      @return Plural info. }
    class function Find(const id: String): TPluralInfo;

    property Id: String read FId write FId;
    property Proc: TPluralProc read FProc write FProc;

    property IntegerCount: Integer read FIntegerCount;
    property IntegerPlurals: TPlurals read FIntegerPlurals write SetIntegerPlurals;
    property Integers[i: Integer]: TPlural read GetInteger;
    property DefaultInteger: TPlural read FDefaultInteger;

    property DecimalCount: Integer read FDecimalCount;
    property DecimalPlurals: TPlurals read FDecimalPlurals write SetDecimalPlurals;
    property Decimals[i: Integer]: TPlural read GetDecimal;
    property DefaultDecimal: TPlural read FDefaultDecimal;

    property Expression: String read FExpression write FExpression;
  end;

  { @abstract Static class that contains multi pattern (e.g. plural, gender and select) routines.
    @seealso(NtPattern) }
  TMultiPattern = class
  public
    { Get the plural index procedure.
      @return The pointer to the plural procedure. }
    class function GetProc: TPluralProc; overload;
    class function GetProc(const id: String): TPluralProc; overload;

    { Check if the locale uses single plural form.
      @param locale Language or locale to be used.
      @return True if the locale uses a single plural form. }
    class function IsSingleFormLanguage(const locale: String = ''): Boolean;

    { Check if the locale uses the same plural form for 0 and 1.
      @param locale Language or locale to be used.
      @return True if the locale uses the same plural form for 0 and 1. }
    class function IsZeroLikeOne(const locale: String = ''): Boolean;

    { Get the plural kind the language uses.
      @param count Specifies the count. This is used to calculate the right pattern index.
      @return The standard form that should be used. }
    class function GetPlural(count: Integer): TPlural;

    { Get the plural kind the language uses.
      @param count Specifies the count. This is used to calculate the right pattern index.
      @param format Pattern string that contains patterns for all plural forms used by the language.
      @param plural Pattern string that contains patterns for all plural forms used by the language.
      @param customPlural Pattern string that contains patterns for all plural forms used by the language. }
    class procedure GetMatchingPlural(
      count: Integer;
      const format: String;
      var plural, customPlural: TPlural);

    { Get the plural form label
      @param form The plural form.
      @return The plural form label. }
    class function GetPluralName(plural: TPlural): String;

    { Get the gender label
      @param form The gender.
      @return The gender label. }
    class function GetGenderName(gender: TGender): String;

    { Parses the right pattern from the multi-pattern resource string.
      @return The pattern that is used with passed count on active language. }
    class function GetPattern(
      const patterns: String;
      count: Integer): String; overload;

    class function GetPattern(
      const patterns: String;
      count: Integer;
      var startPattern: String): String; overload;

    class function GetPlurals(usage: TPluralUsage): TPlurals;

    { Parses the right pattern from the multi-pattern resource string.
      @return The pattern that is used with passed gender. }
    class function GetPattern(
      const patterns: String;
      gender: TGender): String; overload;

    class function GetPattern(
      const patterns: String;
      gender: TGender;
      var startPattern: String): String; overload;

    { Parses the right pattern from the multi-pattern resource string.
      @return The pattern that is used with passed select value. }
    class function GetPattern(
      const patterns: String;
      const select: String): String; overload;

    class function GetPattern(
      const patterns: String;
      const select: String;
      var startPattern: String): String; overload;

    { Gets the gender that is used.
      @return The actual gender that was used. }
    class function GetGender(
      const patterns: String;
      gender: TGender): TGender;

    { This is like normal Format but it parses the right part from the multi-pattern.
      @param pattern Multi-pattern string that contains patterns for all plural forms used by the language.
      @param count Specifies the count. This is used to calculate the right pattern index.
      @return The formatted string. }
    class function Format(
      const pattern: String;
      count: Integer): String; overload;

    { This is like normal Format but it parses the right part from the multi-pattern.
      @param pattern     Multi-pattern string that contains patterns for all plural forms used by the language.
      @param count       Specifies the count. This is used to select the right pattern.
      @param args        Arguments like in the standard Format function.
      @return            The formatted string. }
    class function Format(
      const pattern: String;
      count: Integer;
      const args: array of const): String; overload;

    { This is like normal Format but it parses the right part from the multi-pattern.
      @param pattern     Multi-pattern string that contains patterns for all gender forms used by the language.
      @param gender      Specifies the gender. This is used to select the right pattern.
      @param args        Arguments like in the standard Format function.
      @return            The formatted string. }
    class function Format(
      const pattern: String;
      gender: TGender;
      const args: array of const): String; overload;

    { This is like normal Format but it parses the right part from the multi-pattern.
      @param pattern Multi-pattern string that contains patterns for select value.
      @param select Specifies the select value. This is used to select the right pattern.
      @param args Arguments like in the standard Format function.
      @return The formatted string. }
    class function Format(
      const pattern: String;
      const select: String;
      const args: array of const): String; overload;

    { This is like Format but it can have any number of plural enabled parameters.
      @param pattern Multi-pattern string that contains patterns for all plural forms used by the language.
      @param counts Specifies the counts. This contains the count paramaters.
      @return The formatted string. }
    class function Format(
      const pattern: String;
      const counts: array of const): String; overload;

    class procedure GetNumber(
      const pattern: String;
      count: Integer;
      var kind: TFormatParameterKind;
      var plural: TPlural;
      var operatorKind: TOperatorKind;
      var operand, operand2: Integer);

    { This is like Format but it can have any number of plural and/or gender enabled parameters.
      @param pattern Multi-pattern string that contains patterns for all plural forms used by the language.
      @param args Specifies the parameters.
      @return The formatted string. }
    class function FormatMulti(
      const pattern: String;
      const args: array of TFormatParameter): String; overload;

    { Get the plural from from string code.
      @param value Plural form code.
      @return The plural form matching the code. }
    class function StringToPlural(const value: String): TPlural;

    { Get the operator value from string code.
      @param value Operator value code.
      @return The operator value matching the code. }
    class function StringToOperator(
      const value: String;
      out kind: TOperatorKind;
      out operand2: Integer): Integer; overload;

    class function StringToOperator(
      const value: String;
      out kind: TOperatorKind): Integer; overload;

    { Get the gender from string code.
      @param value Gender code.
      @return The gender matching the code. }
    class function StringToGender(const value: String): TGender;

    { Get the plural form from string code.
      @param value Plural form code.
      @param form Plural form.
      @return True of the passed code was a valid. }
    class function TryStringToPlural(const value: String; out plural: TPlural): Boolean;

    { Get the operator value from string code.
      @param value Operator value code.
      @param kind Operator kind.
      @param operand Operand.
      @return True of the passed code was a valid. }
    class function TryStringToOperator(
      value: String;
      out kind: TOperatorKind;
      out operand, operand2: Integer): Boolean;

    { Get the gender from string code.
      @param value Gender code.
      @param form Gender.
      @return True of the passed code was a valid. }
    class function TryStringToGender(const value: String; out gender: TGender): Boolean;

    { Get the plural code from plural form.
      @param value Plural form.
      @return The plural code matching the plural form. }
    class function PluralToString(value: TPlural): String;

    class function OperatorToString(kind: TOperatorKind; operand, operand2: Integer): String;

    { Get the gender code from plural form.
      @param value Gender.
      @return The gender code matching the gender. }
    class function GenderToString(value: TGender): String;

    class function SelectToString(const value: String): String;

    { Check if the plural or operator code is a valid code.
      @param value Plural form or operator code.
      @return True if the passed code was a valid code. }
    class function IsNumber(const value: String): Boolean;

    { Check if the passed code is a valid plural code.
      @param value Plural form code.
      @return True if the passed code was a valid code. }
    class function IsPlural(const value: String): Boolean;

    { Check if the passwed code is a valid oparator code.
      @param value Value code to check.
      @return True if the passed code was a valid code. }
    class function IsOperator(const value: String): Boolean;

    { Check if the passed code is a valid gender code.
      @param value Gender form code.
      @return True if the passed code was a valid code. }
    class function IsGender(const value: String): Boolean;

    class function IsOther(const value: String): Boolean;
    class function IsNeutral(const value: String): Boolean;

    { Check if a language id belongs into a language id array
      @param language Language id to be checked.
      @param languages Language id array.
      @return True if the id is in the array. }
    class function IsLanguageInArray(const language: String; languages: array of String): Boolean;

    { Register a plural function
      @param id Language id.
      @param proc Plural function that is used when the language is active. }
    class procedure Register(const id: String; proc: TPluralProc); overload;

    { Register a required integer and decimal plural forms and plural rule for PO/GetText.
      @param id Language id.
      @param integerPlurals Required plural forms when count is an integer number.
      @param decimalPlurals Required plural forms when count is an decimal or float number.
      @param expression Rule used in .po files. }
    class procedure Register(const id: String; integerPlurals, decimalPlurals: TPlurals; const expression: String); overload;
  end;

var
  OperatorDelta: Integer;
  RaiseExceptionOnInvalidPattern: Boolean;

implementation

uses
  SysUtils, NtBase, NtPluralData;

const
  NEUTRAL_C = 'neutral';
  NEXT_C = 'next';

  PLURAL_DATAS_C: array[TPlural] of string =
  (
    'zero',
    'one',
    'two',
    'few',
    'many',
    OTHER_C
  );

  GENDER_DATAS_C: array[TGender] of string =
  (
    'male',
    'female',
    OTHER_C
  );

  OPERATORS_C: array[TOperatorKind] of String =
  (
    '=',
    '~',
    '<',
    '<=',
    '>',
    '>=',
    '..'
  );

var
  FDatas: TStringList;


// TPluralPattern

constructor TPluralPattern.Create;
begin
  inherited;
  FPlural := pfOther;
end;

function TPluralPattern.Equals(obj: TObject): Boolean;
begin
  Result :=
    (obj is TPluralPattern) and
    (TPluralPattern(obj).FPlural = FPlural)
end;

function TPluralPattern.ToString: String;
begin
  Result := TMultiPattern.PluralToString(FPlural);
end;


// TOperatorPattern

constructor TOperatorPattern.Create;
begin
  inherited;
  FOperand := 0;
end;

function TOperatorPattern.Equals(obj: TObject): Boolean;
begin
  Result :=
    (obj is TOperatorPattern) and
    (TOperatorPattern(obj).FOperand = FOperand) and
    (TOperatorPattern(obj).FKind = FKind)
end;

function TOperatorPattern.ToString: String;
begin
  Result := TMultiPattern.OperatorToString(FKind, FOperand, FOperand2);
end;


// TGenderPattern

constructor TGenderPattern.Create;
begin
  inherited;
  FGender := geNeutral;
end;

function TGenderPattern.Equals(obj: TObject): Boolean;
begin
  Result :=
    (obj is TGenderPattern) and
    (TGenderPattern(obj).FGender = FGender)
end;

function TGenderPattern.ToString: String;
begin
  Result := TMultiPattern.GenderToString(FGender);
end;


// TSelectPattern

constructor TSelectPattern.Create;
begin
  inherited;
  FSelect := '';
end;

function TSelectPattern.Equals(obj: TObject): Boolean;
begin
  Result :=
    (obj is TSelectPattern) and
    (TSelectPattern(obj).FSelect = FSelect)
end;

function TSelectPattern.ToString: String;
begin
  Result := TMultiPattern.SelectToString(FSelect);
end;


// TFormatPartEnumerator

constructor TFormatPartEnumerator.Create(formatPart: TFormatPart);
begin
  inherited Create;
  FFormatPart := formatPart;
  FIndex := -1;
end;

function TFormatPartEnumerator.GetCurrent: TPattern;
begin
  Result := FFormatPart[FIndex];
end;

function TFormatPartEnumerator.MoveNext: Boolean;
begin
  Result := (FFormatPart <> nil) and (FIndex < (FFormatPart.Count - 1));

  if Result then
    Inc(FIndex);
end;


// TFormatPart

constructor TFormatPart.Create;
begin
  inherited;
{$IFDEF AUTOREFCOUNT}
  FItems := TList<TPattern>.Create;
{$ELSE}
  FItems := TList.Create;
{$ENDIF}
end;

destructor TFormatPart.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TFormatPart.GetEnumerator: TFormatPartEnumerator;
begin
  Result := TFormatPartEnumerator.Create(Self);
end;

procedure TFormatPart.Clear;
begin
{$IFDEF AUTOREFCOUNT}
  FItems.Clear;
{$ELSE}
  while FItems.Count > 0 do
  begin
    TObject(FItems[0]).Free;
    FItems.Delete(0);
  end;
{$ENDIF}
end;

function TFormatPart.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFormatPart.GetItem(i: Integer): TPattern;
begin
  Result := FItems[i];
end;

function TFormatPart.GetFirstValue: String;
begin
  if Count > 0 then
    Result := Items[0].Value
  else
    Result := '';
end;

function TFormatPart.GetOtherValue: String;
var
  pattern: TPattern;
begin
  pattern := FindOther;

  if pattern <> nil then
    Result := pattern.Value
  else
    Result := '';
end;

function TFormatPart.GetDefaultValue: String;
begin
  if IsNumber then
    Result := PluralValues[pfOther]
  else
  begin
    Result := GenderValues[geNeutral];

    if Result = '' then
      Result := SelectValues[OTHER_C];

    if Result = '' then
      Result := SelectValues[NEUTRAL_C];

    if Result = '' then
      Result := FirstValue;
  end;
end;

function TFormatPart.GetDefaultGender: TGender;
begin
  // Neutral must be checked first
  if Exists(geNeutral) then
    Result := geNeutral
  else if Exists(geMale) then
    Result := geMale
  else if Exists(geFemale) then
    Result := geFemale
  else
    Result := geNeutral
end;

function TFormatPart.GetDefaultSelect: String;
var
  i: Integer;
  pattern: TPattern;
begin
  // other and neutral must be checked first
  if Exists(OTHER_C) then
    Result := OTHER_C
  else if Exists(NEUTRAL_C) then
    Result := NEUTRAL_C
  else
  begin
    for i := 0 to Count - 1 do
    begin
      pattern := Items[i];

      if pattern is TSelectPattern then
      begin
        Result := TSelectPattern(pattern).Select;
        Exit;
      end;
    end;

    Result := '';
  end;
end;

function TFormatPart.GetGenderValue(gender: TGender): String;
var
  item: TGenderPattern;
begin
  item := Find(gender);

  if item <> nil then
  begin
    Result := item.Value;
    FUsedKind := fpGender;
    FUsedGender := item.Gender;
  end
  else
    Result := '';
end;

procedure TFormatPart.SetGenderValue(gender: TGender; const value: String);
var
  item: TGenderPattern;
begin
  item := Find(gender);

  if item <> nil then
    item.Value := value
  else
    Add(value, gender);
end;

function TFormatPart.GetSelectValue(const select: String): String;
var
  item: TSelectPattern;
begin
  item := Find(select);

  if item <> nil then
  begin
    Result := item.Value;
    FUsedKind := fpSelect;
    FUsedSelect := select;
  end
  else
    Result := '';
end;

procedure TFormatPart.SetSelectValue(const select: String; const value: String);
var
  item: TSelectPattern;
begin
  item := Find(select);

  if item <> nil then
    item.Value := value
  else
    Add(value, select);
end;

function TFormatPart.Find(pattern: TPattern): TPattern;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];

    if pattern.Equals(Result) then
      Exit;
  end;

  Result := nil;
end;

function TFormatPart.FindOther: TPattern;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];

    if (Result is TPluralPattern) and (TPluralPattern(Result).Plural = pfOther) or
      (Result is TGenderPattern) and (TGenderPattern(Result).Gender = geNeutral) then
    begin
      Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.FindAny(pattern: TPattern): TPattern;
var
  i: Integer;
begin
  if pattern is TNumberPattern then
    Result := Find(pfOther)
  else if pattern is TGenderPattern then
    Result := Find(geNeutral)
  else
    Result := nil;

  if Result <> nil then
    Exit;

  for i := 0 to Count - 1 do
  begin
    Result := Items[i];

    if Result.ClassName = pattern.ClassName then
      Exit;
  end;

  if Count > 0 then
    Result := Items[0]
  else
    Result := nil;
end;

function TFormatPart.Find(gender: TGender): TGenderPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TGenderPattern then
    begin
      Result := TGenderPattern(pattern);

      if Result.Gender = gender then
        Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.FindGender: TGenderPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TGenderPattern then
    begin
      Result := TGenderPattern(pattern);
      Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.Find(const select: String): TSelectPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TSelectPattern then
    begin
      Result := TSelectPattern(pattern);

      if Result.Select = select then
        Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.Find(plural: TPlural): TPluralPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TPluralPattern then
    begin
      Result := TPluralPattern(pattern);

      if Result.Plural = plural then
        Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.FindMatching(value: Integer): TPluralPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TPluralPattern then
    begin
      Result := TPluralPattern(pattern);

      if (value = 0) and (Result.Plural = pfZero) or
        (value = 1) and (Result.Plural = pfOne) or
        (value = 2) and (Result.Plural = pfTwo) then
      begin
        Exit;
      end;
    end;
  end;

  Result := nil;
end;

function TFormatPart.FindOperator(value: Integer): TOperatorPattern;
var
  i: Integer;
  pattern: TPattern;
  thisPattern: TOperatorPattern;
begin  //FI:C101
  // Equal: Find first match
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TOperatorPattern then
    begin
      Result := TOperatorPattern(pattern);

      if (Result.Kind = okEqual) and (value = Result.Operand) then
        Exit;
    end;
  end;

  // Around: Find first match
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TOperatorPattern then
    begin
      Result := TOperatorPattern(pattern);

      if (Result.Kind = okAround) and (value >= Result.Operand - OperatorDelta) and (value <= Result.Operand + OperatorDelta) then
        Exit;
    end;
  end;

  // Range: Find first match
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TOperatorPattern then
    begin
      Result := TOperatorPattern(pattern);

      if (Result.Kind = okRange) and (value >= Result.Operand) and (value <= Result.Operand2) then
        Exit;
    end;
  end;

  // <, <=, > or >=: Find match that is closes to the value
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TOperatorPattern then
    begin
      thisPattern := TOperatorPattern(pattern);

      if (
          (thisPattern.Kind = okLessOrEqual) and (value <= thisPattern.Operand) or
          (thisPattern.Kind = okLess) and (value < thisPattern.Operand) or
          (thisPattern.Kind = okGreaterOrEqual) and (value >= thisPattern.Operand) or
          (thisPattern.Kind = okGreater) and (value > thisPattern.Operand)
        ) and ((Result = nil) or (Abs(value - thisPattern.Operand) < Abs(value - Result.Operand))) then
      begin
        Result := thisPattern;
      end
    end;
  end;
end;

function TFormatPart.Find(kind: TOperatorKind; operand, operand2: Integer): TOperatorPattern;
var
  i: Integer;
  pattern: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    pattern := Items[i];

    if pattern is TOperatorPattern then
    begin
      Result := TOperatorPattern(pattern);

      if (Result.Kind = kind) and (Result.Operand = operand) and ((kind <> okRange) or (Result.Operand2 = operand2)) then
        Exit;
    end;
  end;

  Result := nil;
end;

function TFormatPart.Exists(plural: TPlural): Boolean;
begin
  Result := Find(plural) <> nil;
end;

function TFormatPart.Exists(kind: TOperatorKind; operand, operand2: Integer): Boolean;
begin
  Result := Find(kind, operand, operand2) <> nil;
end;

function TFormatPart.Exists(gender: TGender): Boolean;
begin
  Result := Find(gender) <> nil;
end;

function TFormatPart.Exists(const select: String): Boolean;
begin
  Result := Find(select) <> nil;
end;

function TFormatPart.ExistsMatching(value: Integer; var plural: TPlural): Boolean;
var
  pattern: TPluralPattern;
begin
  pattern := FindMatching(value);

  if pattern <> nil then
    plural := pattern.Plural
  else
    plural := pfOther;

  Result := pattern <> nil;
end;

function TFormatPart.GetPluralValue(plural: TPlural): String;
var
  item: TPluralPattern;
begin
  item := Find(plural);

  if item <> nil then
  begin
    Result := item.Value;
    FUsedKind := fpPlural;
  end
  else
    Result := '';
end;

procedure TFormatPart.SetPluralValue(plural: TPlural; const value: String);
var
  item: TPluralPattern;
begin
  item := Find(plural);

  if item <> nil then
    item.Value := value
  else
    Add(value, plural);
end;

function TFormatPart.GetMatchingValue(value: Integer): String;
var
  item: TPluralPattern;
begin
  item := FindMatching(value);

  if item <> nil then
  begin
    Result := item.Value;
    FUsedKind := fpPlural;
  end
  else
    Result := '';
end;

function TFormatPart.GetOperatorValue(kind: TOperatorKind; operand, operand2: Integer): String;
var
  item: TOperatorPattern;
begin
  item := Find(kind, operand, operand2);

  if item <> nil then
    Result := item.Value
  else
    Result := '';
end;

procedure TFormatPart.SetOperatorValue(kind: TOperatorKind; operand, operand2: Integer; const value: String);
var
  item: TOperatorPattern;
begin
  item := Find(kind, operand, operand2);

  if item <> nil then
  begin
    item.Value := value;
    FUsedKind := fpOperator;
    FUsedOperator := kind;
    FUsedOperand := operand;
    FUsedOperand2 := operand2;
  end
  else
    Add(value, okEqual, operand, operand2);
end;

function TFormatPart.GetEqualOperatorValue(operand: Integer): String;
begin
  Result := GetOperatorValue(okEqual, operand, 0);
end;

procedure TFormatPart.SetEqualOperatorValue(operand: Integer; const value: String);
begin
  SetOperatorValue(okEqual, operand, 0, value);
end;

function TFormatPart.GetKind: TFormatParameterKind;
begin
  if IsGender then
    Result := fpGender
  else if IsSelect then
    Result := fpSelect
  else
    Result := fpPlural
end;

function TFormatPart.IsNumber: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TNumberPattern then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TFormatPart.IsGender: Boolean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TGenderPattern then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TFormatPart.IsSelect: Boolean;
var
  i: Integer;
  item: TPattern;
begin
  for i := 0 to Count - 1 do
  begin
    item := Items[i];

    if item is TSelectPattern then
    begin
      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

procedure TFormatPart.Delete(index: Integer);
begin
{$IFNDEF AUTOREFCOUNT}
  Items[index].Free;
{$ENDIF}
  FItems.Delete(index);
end;

function TFormatPart.Add(const value: String; gender: TGender): TGenderPattern;
begin
  Result := TGenderPattern.Create;
  Result.Value := value;
  Result.Gender := gender;
  FItems.Add(Result);
end;

function TFormatPart.Add(const value: String; const select: String): TSelectPattern;
begin
  Result := TSelectPattern.Create;
  Result.Value := value;
  Result.Select := select;
  FItems.Add(Result);
end;

function TFormatPart.Insert(index: Integer; const value: String; const select: String): TSelectPattern;
begin
  Result := TSelectPattern.Create;
  Result.Value := value;
  Result.Select := select;
  FItems.Insert(index, Result);
end;

function TFormatPart.Add(const value: String; plural: TPlural): TPluralPattern;
begin
  Result := TPluralPattern.Create;
  Result.Value := value;
  Result.Plural := plural;
  FItems.Add(Result);
end;

function TFormatPart.Add(const value: String; kind: TOperatorKind; operand: Integer; operand2: Integer): TOperatorPattern;
begin
  Result := TOperatorPattern.Create;
  Result.Value := value;
  Result.Kind := kind;
  Result.Operand := operand;
  Result.Operand2 := operand2;
  FItems.Add(Result);
end;

function TFormatPart.GetPluralPattern(plural: TPlural; count: Cardinal): String;
var
  matchingPlural: TPlural;
  operatorPattern: TOperatorPattern;
begin  //FI:C101
  Result := '';

  if not IsNumber then
  begin
    if RaiseExceptionOnInvalidPattern then
      raise Exception.Create('"Pattern is not a number pattern')
    else
      Exit;
  end;

  operatorPattern := FindOperator(count);

  if operatorPattern <> nil then
  begin
    Result := operatorPattern.Value;

    if Result <> '' then
    begin
      FUsedKind := fpOperator;
      FUsedOperator := operatorPattern.Kind;
      FUsedOperand := operatorPattern.Operand;
      FUsedOperand2 := operatorPattern.Operand2;
      Exit;
    end;
  end;

  FUsedKind := fpPlural;

  if (Result = '') and (ExistsMatching(count, matchingPlural)) then
  begin
    Result := PluralValues[matchingPlural];

    if Result <> '' then
    begin
      FUsedPlural := matchingPlural;
      Exit;
    end;
  end;

  Result := PluralValues[plural];

  if Result <> '' then
  begin
    FUsedPlural := plural;
    Exit;
  end;

  if Result = '' then
  begin
    FUsedPlural := pfOther;
    Result := FirstValue;
  end;
end;

function TFormatPart.GetGenderPattern(gender: TGender): String;
begin
  if not IsGender then
    raise Exception.Create('"Pattern is not a gender pattern');

  Result := GenderValues[gender];

  if Result = '' then
    Result := FirstValue;
end;


// TFormatStringEnumerator

constructor TFormatStringEnumerator.Create(formatString: TFormatString);
begin
  inherited Create;
  FFormatString := formatString;
  FIndex := -1;
end;

function TFormatStringEnumerator.GetCurrent: TFormatPart;
begin
  Result := FFormatString[FIndex];
end;

function TFormatStringEnumerator.MoveNext: Boolean;
begin
  Result := (FFormatString <> nil) and (FIndex < (FFormatString.Count - 1));

  if Result then
    Inc(FIndex);
end;


// TFormatString

constructor TFormatString.Create;
begin
  inherited;
{$IFDEF AUTOREFCOUNT}
  FItems := TList<TFormatPart>.Create;
{$ELSE}
  FItems := TList.Create;
{$ENDIF}
end;

destructor TFormatString.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TFormatString.GetEnumerator: TFormatStringEnumerator;
begin
  Result := TFormatStringEnumerator.Create(Self);
end;

procedure TFormatString.Clear;
begin
  FStartPattern := '';

{$IFDEF AUTOREFCOUNT}
  FItems.Clear;
{$ELSE}
  while FItems.Count > 0 do
  begin
    TObject(FItems[0]).Free;
    FItems.Delete(0);
  end;
{$ENDIF}
end;

function TFormatString.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFormatString.GetItem(i: Integer): TFormatPart;
begin
  Result := FItems[i];
end;

function TFormatString.GetOperatorValue(kind: TOperatorKind; operand, operand2: Integer): String;
begin
  Result := Items[0].OperatorValues[kind, operand, operand2];
end;

function TFormatString.GetEqualOperatorValue(operand: Integer): String;
begin
  Result := Items[0].EqualOperatorValues[operand];
end;

function TFormatString.GetPluralValue(value: TPlural): String;
begin
  Result := Items[0].PluralValues[value];
end;

function TFormatString.GetMatchingValue(value: Integer): String;
begin
  Result := Items[0].MatchingValues[value];
end;

function TFormatString.GetGenderValue(value: TGender): String;
begin
  Result := Items[0].GenderValues[value];
end;

function TFormatString.GetSelectValue(const value: String): String;
begin
  Result := Items[0].SelectValues[value];
end;

function TFormatString.Find(pattern: TPattern): TPattern;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].Find(pattern);

    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;

function TFormatString.FindAny(pattern: TPattern): TPattern;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].FindAny(pattern);

    if Result <> nil then
      Exit;
  end;

  Result := nil;
end;

function TFormatString.Find(plural: TPlural; all: Boolean): TPluralPattern;
var
  i: Integer;
begin
  if all then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i].Find(plural);

      if Result <> nil then
        Exit;
    end;

    Result := nil;
  end
  else
    Result := Items[0].Find(plural);
end;

function TFormatString.Exists(plural: TPlural): Boolean;
begin
  Result := Find(plural) <> nil;
end;

function TFormatString.Find(kind: TOperatorKind; operand, operand2: Integer; all: Boolean): TOperatorPattern;
var
  i: Integer;
begin
  if all then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i].Find(kind, operand, operand2);

      if Result <> nil then
        Exit;
    end;

    Result := nil;
  end
  else
    Result := Items[0].Find(kind, operand, operand2);
end;

function TFormatString.Exists(kind: TOperatorKind; operand, operand2: Integer): Boolean;
begin
  Result := Find(kind, operand, operand2) <> nil;
end;

function TFormatString.FindMatching(operand: Integer; all: Boolean): TPluralPattern;
var
  i: Integer;
begin
  if all then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i].FindMatching(operand);

      if Result <> nil then
        Exit;
    end;

    Result := nil;
  end
  else
    Result := Items[0].FindMatching(operand);
end;

function TFormatString.ExistsMatching(operand: Integer): Boolean;
begin
  Result := FindMatching(operand) <> nil;
end;

function TFormatString.AddValue(plural: TPlural; const value: String): TPluralPattern;
var
  parameter: TFormatPart;
begin
  if FItems.Count > 0 then
    parameter := Items[0]
  else
    parameter := AddParameter;

  Result := parameter.Add(value, plural);
end;

function TFormatString.Find(gender: TGender; all: Boolean): TGenderPattern;
var
  i: Integer;
begin
  if all then
  begin
    for i := 0 to Count - 1 do
    begin
      Result := Items[i].Find(gender);

      if Result <> nil then
        Exit;
    end;

    Result := nil;
  end
  else
    Result := Items[0].Find(gender);
end;

function TFormatString.Exists(gender: TGender): Boolean;
begin
  Result := Find(gender) <> nil;
end;

function TFormatString.AddValue(gender: TGender; const value: String): TGenderPattern;
var
  parameter: TFormatPart;
begin
  if FItems.Count > 0 then
    parameter := Items[0]
  else
    parameter := AddParameter;

  Result := parameter.Add(value, gender);
end;

function TFormatString.AddParameter(const name: String): TFormatPart;
begin
  Result := TFormatPart.Create;
  Result.Name := name;
  FItems.Add(Result);
end;

function TFormatString.GetText: String;

  function DoubleBraceToPrintf(const value: String): String;
  var
    i: Integer;
    bracePlaceholder, printfPlaceholder: String;
  begin
    Result := StartPattern;

    for i := 0 to Count - 1 do
    begin
      bracePlaceholder := Format('{{%d}}', [i]);
      printfPlaceholder := Format('%%%d:s', [i]);
      Result := stringreplace(Result, bracePlaceholder, printfPlaceholder, []);
    end;
  end;

  function SingleBraceToPrintf(const value: String): String;
  var
    i: Integer;
    bracePlaceholder, printfPlaceholder: String;
  begin
    Result := StartPattern;

    for i := 0 to Count - 1 do
    begin
      bracePlaceholder := Format('{%d}', [i]);
      printfPlaceholder := Format('%%%d:s', [i]);
      Result := stringreplace(Result, bracePlaceholder, printfPlaceholder, []);
    end;
  end;

  function Icu: String; //FI:C103
  var
    i, j, p: Integer;
    str, thisStartPattern, kindStr: String;
    parameter: TFormatPart;
    pattern: TPattern;
    patterns: array of String;
    parameters: array of TVarRec;
    placeholder: String;
  begin
    case PlaceholderKind of
      pkDoubleBrace: thisStartPattern := DoubleBraceToPrintf(Self.StartPattern);
      pkSingleBrace: thisStartPattern := SingleBraceToPrintf(Self.StartPattern);
    else
      thisStartPattern := Self.StartPattern;
    end;

    Result := '';
    SetLength(patterns, Count);
    SetLength(parameters, Count);

    for i := 0 to Count - 1 do
    begin
      parameter := Items[i];

      str := '{';

      if parameter.Name <> '' then
        str := str + Format('%s, ', [parameter.Name]);

      case parameter.Kind of
        fpGender: kindStr := 'gender';
        fpSelect: kindStr := 'select';
      else
        kindStr := 'plural';
      end;

      str := str + Format('%s,', [kindStr]); //[parameter.ParameterType]);

      for j := 0 to parameter.Count - 1 do
      begin
        pattern := parameter[j];
        str := str + Format(' %s {%s}', [pattern.ToString, pattern.Value]);
      end;

      str := str + '}';

      if thisStartPattern <> '' then
      begin
        patterns[i] := str;

  {$IFDEF UNICODE}
        parameters[i].VType := vtUnicodeString;
        parameters[i].VUnicodeString := Pointer(patterns[i]);
  {$ELSE}
        parameters[i].VType := vtWideString;
        parameters[i].VWideString := Pointer(patterns[i]);
  {$ENDIF}
      end
      else
        Result := Result + str;
    end;

    if thisStartPattern <> '' then
    begin
      if Pos('{0}', thisStartPattern) > 0 then
      begin
        for i := 0 to Length(patterns) - 1 do
        begin
          placeholder := Format('{%d}', [i]);
          p := Pos(placeholder, thisStartPattern);

          if p > 0 then
          begin
            Delete(thisStartPattern, p, Length(placeholder));
            Insert('%s', thisStartPattern, p);
          end;
        end;
      end;

      Result := SysUtils.Format(thisStartPattern, parameters);
    end;
  end;

  function Legacy: String;

    procedure Add(const value: String);
    var
      i: Integer;
      c: Char;
    begin
      if Result <> '' then
        Result := Result + ';';

      if Pos(';', value) > 0 then
      begin
        for i := 1 to Length(value) do
        begin
          c := value[i];
          Result := Result + c;

          if c = ';' then
            Result := Result + c;
        end;
      end
      else
        Result := Result + value;
    end;

  var
    i, j: Integer;
    parameter: TFormatPart;
    pattern: TPattern;
  begin
    Result := StartPattern;

    for i := 0 to Count - 1 do
    begin
      parameter := Items[i];

      for j := 0 to parameter.Count - 1 do
      begin
        pattern := parameter[j];
        Add(pattern.ToString);
        Add(pattern.Value);
      end;

      if i < Count - 1 then
        Add(NEXT_C);
    end;
  end;

begin
  if Syntax = fsIcu then
    Result := Icu
  else
    Result := Legacy
end;

function IsIcuPattern(const pattern: String): Boolean;
var
  i, level: Integer;
  c: Char;
begin
  Result := (Pos('{', pattern) > 0) and (Pos('}', pattern) > 0);

  if not Result then
    Exit;

  level := 0;
  Result := False;

  for i := 1 to Length(pattern) do
  begin
    c := pattern[i];

    if (c = '{') and ((i = 1) or (pattern[i - 1] <> '\')) then
      Inc(level)
    else if (c = '}') and ((i = 1) or (pattern[i - 1] <> '\')) then
      Dec(level);

    if level >= 2 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function IsLegacyPattern(const pattern: String): Boolean;
begin
  Result := Pos(';', pattern) > 0;
end;

function IsLegacyPatternStrict(const pattern: String): Boolean;

  function Check(codes: array of String): Boolean;
  var
    code: String;
  begin
    // other;...
    // male;...
    for code in codes do
    begin
      if Pos(code + ';', pattern) > 0 then
      begin
        Result := True;
        Exit;
      end;
    end;

    Result := False;
  end;

  function CheckNumber(codes: array of String): Boolean;
  var
    i: Integer;
    code: String;
  begin
    // =1;...
    for code in codes do
    begin
      for i := 0 to 9 do
      begin
        if Pos(code + IntToStr(i) + ';', pattern) > 0 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;

    Result := False;
  end;

begin
  Result := Check(PLURAL_DATAS_C) or Check(GENDER_DATAS_C) or CheckNumber(OPERATORS_C);
end;

class function TFormatString.IsPattern(const pattern: String): Boolean;
var
  str: TFormatString;
begin
  Result := IsIcuPattern(pattern) or IsLegacyPatternStrict(pattern);

  if not Result then
    Exit;

  str := TFormatString.Create;
  try
    try
      str.ParsePattern(pattern);
    except
      Result := False;
    end;
  finally
    str.Free;
  end;
end;

procedure TFormatString.ParsePattern(const pattern: String);
begin
  if not ParseIcu(pattern) then
  begin
    Clear;

    if not ParseLegacy(pattern) then
      raise Exception(Format('"%s" is not a valid pattern string', [pattern]));
  end;
end;

procedure SkipWhiteSpaces(const pattern: String; var index: Integer);
begin
  while (index <= Length(pattern)) and (pattern[index] = ' ') do
    Inc(index);
end;

function GetIcuToken(const pattern: String; var index: Integer): String;
var
  c: Char;
begin
  Result := '';

  while index <= Length(pattern) do
  begin
    c := pattern[index];

    if (c = ' ') or (c = ',') then
    begin
      Inc(index);
      Break;
    end
    else if (c = '{') then
      Break;

    Result := Result + c;
    Inc(index);
  end;

  SkipWhiteSpaces(pattern, index);
end;

function GetIcuPart(const pattern: String; var index: Integer): String;
var
  braces: Integer;
  c: Char;
begin
  Inc(index);
  braces := 1;
  Result := '';

  while (index <= Length(pattern)) and (braces > 0) do
  begin
    c := pattern[index];

    if c = '{' then
      Inc(braces)
    else if c = '}' then
      Dec(braces);

    if braces = 0 then
      Break;

    if c = '\' then
    begin
      Inc(index);
      c := pattern[index];
    end;

    Result := Result + c;
    Inc(index);
  end;

  Inc(index);
  SkipWhiteSpaces(pattern, index);
end;

function TFormatString.ParseIcuPattern(const pattern: String; var index: Integer): Boolean;
var
  name, str: String;
  operand, operand2: Integer;
  operatorKind: TOperatorKind;
  parameter: TFormatPart;
begin
  Result := False;

  // {plural one {%d file} other {%d files}}
  // {plural one {{0} file} other {{0} files}}
  // {file, plural one {{file} file} other {{file} files}}
  if pattern[index] <> '{' then
    Exit;

  parameter := AddParameter();

  Inc(index);
  parameter.Name := GetIcuToken(pattern, index);
  parameter.ParameterType := GetIcuToken(pattern, index);

  while index <= Length(pattern) do
  begin
    if pattern[index] = '}' then
      Break;

    if pattern[index] = '{' then
    begin
      name := parameter.ParameterType;
      parameter.ParameterType := parameter.Name;
      parameter.Name := '';
    end
    else
      name := GetIcuToken(pattern, index);

    str := GetIcuPart(pattern, index);

    if SameText(parameter.ParameterType, 'plural') then
    begin
      if TMultiPattern.IsPlural(name) then
        parameter.Add(str, TMultiPattern.StringToPlural(name))
      else
      begin
        operand := TMultiPattern.StringToOperator(name, operatorKind, operand2);
        parameter.Add(str, operatorKind, operand, operand2);
      end;
    end
    else if SameText(parameter.ParameterType, 'gender') then
      parameter.Add(str, TMultiPattern.StringToGender(name))
    else
      parameter.Add(str, name);
  end;

  Inc(index);
  Result := True;
end;

function TFormatString.ParseIcu(pattern: String): Boolean;
var
  index, placeholderIndex: Integer;
  c: Char;
  str, thisStartPattern, placeholder: String;
begin //FI:C101
  Result := IsIcuPattern(pattern);

  if not Result then
    Exit;

  if Escape = ieReact then
  begin
    str := '';
    index := 1;

    while index <= Length(pattern) do
    begin
      c := pattern[index];

      if c = '\' then
        Inc(index);

      str := str + c;
      Inc(index);
    end;

    pattern := str;
  end;

  // {file, plural one {{0} file} other {{0} files}}
  // I have {file, plural one {{0} file} other {{0} files}.}
  thisStartPattern := '';
  placeholder := '';
  placeholderIndex := 0;
  index := 1;

  while index <= Length(pattern) do
  begin
    c := pattern[index];

    if c = '{' then
    begin
      ParseIcuPattern(pattern, index);

      case PlaceholderKind of
        pkPrintf: placeholder := Format('%%%d:s', [placeholderIndex]);
        pkDoubleBrace: placeholder := Format('{{%d}}', [placeholderIndex]);
        pkSingleBrace: placeholder := Format('{%d}', [placeholderIndex]);
      else
        placeholder := Format('{%d}', [placeholderIndex]);
      end;

      Inc(placeholderIndex);

      if thisStartPattern <> '' then
      begin
        thisStartPattern := thisStartPattern + placeholder;
        placeholder := '';
      end;

      Continue;
    end
    else if c = '\' then
    begin
      Inc(index);
      c := pattern[index];
      thisStartPattern := thisStartPattern + c;
    end
    else
    begin
      if placeholder <> '' then
      begin
        thisStartPattern := thisStartPattern + placeholder;
        placeholder := '';
      end;

      thisStartPattern := thisStartPattern + c;
    end;

    Inc(index);
  end;

  Self.StartPattern := thisStartPattern;
  FSyntax := fsIcu;
  Result := True;
end;

function TFormatString.ParseLegacy(pattern: String): Boolean;  //FI:C103
var
  c: Char;
  str, pluralStr, operatorStr, genderStr, selectStr: String;
  parameter: TFormatPart;
  operatorKind: TOperatorKind;
  operand, operand2: Integer;
begin  //FI:C101
  Result := IsLegacyPattern(pattern);

  if not Result then
    Exit;

  // code1;value1[;code2;value2;][;]...
  pluralStr := '';
  operatorStr := '';
  genderStr := '';
  selectStr := '';
  parameter := AddParameter;

  repeat
    str := '';

    while pattern <> '' do
    begin
      c := pattern[1];

      if c = ';' then
      begin
        Delete(pattern, 1, 1);

        if pattern = '' then
          Break;

        c := pattern[1];

        if c <> ';' then
          Break;
      end;

      str := str + c;
      Delete(pattern, 1, 1);
    end;

    if str = NEXT_C then
    begin
      // New parameter
      parameter := AddParameter;
    end
    else if pluralStr <> '' then
    begin
      parameter.Add(str, TMultiPattern.StringToPlural(pluralStr));
      pluralStr := '';
    end
    else if operatorStr <> '' then
    begin
      operand := TMultiPattern.StringToOperator(operatorStr, operatorKind, operand2);
      parameter.Add(str, operatorKind, operand, operand2);
      operatorStr := '';
    end
    else if genderStr <> '' then
    begin
      parameter.Add(str, TMultiPattern.StringToGender(genderStr));
      genderStr := '';
    end
    else if selectStr <> '' then
    begin
      parameter.Add(str, selectStr);
      selectStr := '';
    end
    else if (selectStr = '') and (Count = 1) and (Pos('%', str) > 0) and (FStartPattern <> '') then
    begin
      selectStr := FStartPattern;
      FStartPattern := '';
      parameter.Add(str, selectStr);
      selectStr := '';
    end
    else if TMultiPattern.IsOther(str) then
    begin
      if parameter.FindGender <> nil then
        genderStr := str
      else
        pluralStr := str
    end
    else if TMultiPattern.IsPlural(str) then
      pluralStr := str
    else if TMultiPattern.IsOperator(str) then
      operatorStr := str
    else if TMultiPattern.IsGender(str) then
      genderStr := str
    else if (FStartPattern <> '') or (parameter.Count > 0) then
      selectStr := str
    else
      FStartPattern := str;
  until pattern = '';

  FSyntax := fsLegacy;
end;


// TMultiPattern

class function TMultiPattern.GetProc: TPluralProc;
begin
  Result := TPluralInfo.Get.Proc;
end;

class function TMultiPattern.GetProc(const id: String): TPluralProc;
begin
  Result := TPluralInfo.Get(id).Proc;
end;

class function TMultiPattern.IsLanguageInArray(const language: String; languages: array of String): Boolean;
var
  i: Integer;
begin
  for i := Low(languages) to High(languages) do
  begin
    Result := languages[i] = language;

    if Result then
      Exit;
  end;

  Result := False;
end;

class function TMultiPattern.GetPlural(count: Integer): TPlural;
var
  proc: TPluralProc;
begin
  proc := TMultiPattern.GetProc();
  Result := proc(count, count, 0, 0, 0, 0);
end;

{
class function TMultiPattern.GetMatchingPlural(count: Integer; const format: String): TPlural;
var
  custom: TPlural;
begin
  GetMatchingPlural(count, format, Result, custom);
end;
}

class procedure TMultiPattern.GetMatchingPlural(
  count: Integer;
  const format: String;
  var plural, customPlural: TPlural);
var
  str: TFormatString;
begin
  str := TFormatString.Create;
  try
    str.ParsePattern(format);
    plural := GetPlural(count);

    if (plural = pfOther) and (count = 0) and str.Exists(pfZero) then
      customPlural := pfZero
    else if (plural = pfOther) and (count = 1) and str.Exists(pfOne) then
      customPlural := pfOne
    else if (plural = pfOther) and (count = 2) and str.Exists(pfTwo) then
      customPlural := pfTwo
    else
      customPlural := plural;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.IsSingleFormLanguage(const locale: String): Boolean;
begin
  Result := @TMultiPattern.GetProc(locale) = @GetSinglePlural;
end;

class function TMultiPattern.IsZeroLikeOne(const locale: String = ''): Boolean;
var
  proc: TPluralProc;
begin
  proc := TMultiPattern.GetProc(locale);
  Result := proc(0, 0, 0, 0, 0, 0) = pfOne;
end;

class function TMultiPattern.GetPattern(const patterns: String; count: Integer): String;
var
  startPattern: String;
begin
  Result := GetPattern(patterns, count, startPattern);
end;

class function TMultiPattern.GetPattern(
  const patterns: String;
  count: Integer;
  var startPattern: String): String;
var
  str: TFormatString;
  plural: TPlural;
  pattern: TOperatorPattern;
begin
  str := TFormatString.Create;
  try
    str.PlaceholderKind := pkPrintf;
    str.ParsePattern(patterns);
    startPattern := str.StartPattern;
    plural := GetPlural(count);

    // Check operator patterns first (=n, >n, etc)
    pattern := str[0].FindOperator(count);

    if pattern <> nil then
    begin
      Result := pattern.Value;
      Exit;
    end;

    // zero, one or two
    Result := str.MatchingValues[count];

    if Result <> '' then
      Exit;

    // plural
    Result := str.PluralValues[plural];

    if Result <> '' then
      Exit;

    Result := str.Items[0].FirstValue;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.GetPlurals(usage: TPluralUsage): TPlurals;
begin
  if usage = puInteger then
    Result := TPluralInfo.Get.IntegerPlurals
  else
    Result := TPluralInfo.Get.DecimalPlurals;
end;

class function TMultiPattern.GetPattern(const patterns: String; gender: TGender): String;
var
  startPattern: String;
begin
  Result := GetPattern(patterns, gender, startPattern);
end;

class function TMultiPattern.GetPattern(
  const patterns: String;
  gender: TGender;
  var startPattern: String): String;
var
  str: TFormatString;
begin
  str := TFormatString.Create;
  try
    str.ParsePattern(patterns);
    startPattern := str.StartPattern;
    Result := str.GenderValues[gender];

    if Result = '' then
      Result := str[0].OtherValue;

    if Result = '' then
      Result := str[0].FirstValue;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.GetPattern(const patterns: String; const select: String): String;
var
  startPattern: String;
begin
  Result := GetPattern(patterns, select, startPattern);
end;

class function TMultiPattern.GetPattern(
  const patterns: String;
  const select: String;
  var startPattern: String): String;
var
  str: TFormatString;
begin
  str := TFormatString.Create;
  try
    str.ParsePattern(patterns);
    startPattern := str.StartPattern;
    Result := str.SelectValues[select];

    if Result = '' then
      Result := str[0].FirstValue;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.GetGender(const patterns: String; gender: TGender): TGender;
var
  i: Integer;
  str: TFormatString;
  pattern: TPattern;
  parameter: TFormatPart;
begin
  str := TFormatString.Create;
  try
    str.ParsePattern(patterns);

    if str.Exists(gender) then
      Result := gender
    else
    begin
      parameter := str[0];

      for i := 0 to parameter.Count - 1 do
      begin
        pattern := parameter[i];

        if pattern is TGenderPattern then
        begin
          Result := TGenderPattern(pattern).Gender;
          Exit;
        end;
      end;

      Result := geNeutral;
    end;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.Format(
  const pattern: String;
  count: Integer;
  const args: array of const): String;
var
  startPattern: String;
begin
  Result := SysUtils.Format(GetPattern(pattern, count, startPattern), args);

  if startPattern <> '' then
    Result := SysUtils.Format(startPattern, [Result]);
end;

class function TMultiPattern.Format(
  const pattern: String;
  gender: TGender;
  const args: array of const): String;
var
  startPattern: String;
begin
  Result := SysUtils.Format(GetPattern(pattern, gender, startPattern), args);

  if startPattern <> '' then
    Result := SysUtils.Format(startPattern, [Result]);
end;

class function TMultiPattern.Format(
  const pattern: String;
  const select: String;
  const args: array of const): String;
var
  startPattern: String;
begin
  Result := SysUtils.Format(GetPattern(pattern, select, startPattern), args);

  if startPattern <> '' then
    Result := SysUtils.Format(startPattern, [Result]);
end;

class function TMultiPattern.Format(
  const pattern: String;
  count: Integer): String;
begin
  Result := TMultiPattern.Format(pattern, count, [count]);
end;

class procedure TMultiPattern.GetNumber(
  const pattern: String;
  count: Integer;
  var kind: TFormatParameterKind;
  var plural: TPlural;
  var operatorKind: TOperatorKind;
  var operand, operand2: Integer);
var
  thisPattern: String;
  str: TFormatString;
  part: TFormatPart;
begin
  str := TFormatString.Create;
  try
    str.ParsePattern(pattern);

    part := str.Items[0];
    plural := GetPlural(count);
    thisPattern := part.GetPluralPattern(plural, count);

    kind := part.UsedKind;
    plural := part.UsedPlural;
    operatorKind := part.UsedOperator;
    operand := part.UsedOperand;
    operand2 := part.UsedOperand2;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.Format( //FI:C103
  const pattern: String;
  const counts: array of const): String;
var
  i, count: Integer;
  thisPattern: String;
  plural: TPlural;
  str: TFormatString;
  startPatternParameters: array of String;
  thisArgs, startPatternArgs: array of TVarRec;
  part: TFormatPart;
begin  //FI:C101
  str := TFormatString.Create;
  try
    str.ParsePattern(pattern);

    count := Length(counts);

    if count <> str.Count then
      raise Exception.CreateFmt('Pattern "%s" does not have enough patterns for %d parameter', [pattern, count]);

    if (str.Syntax = fsIcu) and (str.StartPattern <> '') then
    begin
      // ICU with top level pattern
      SetLength(startPatternArgs, count);
      SetLength(startPatternParameters, count);

      for i := 0 to str.Count - 1 do
      begin
        part := str.Items[i];

        count := counts[i].VInteger;
        plural := GetPlural(count);
        thisPattern := part.GetPluralPattern(plural, count);

        SetLength(thisArgs, 1);
        thisArgs[0] := counts[i];

        startPatternParameters[i] := SysUtils.Format(thisPattern, thisArgs);

  {$IFDEF UNICODE}
        startPatternArgs[i].VType := vtUnicodeString;
        startPatternArgs[i].VUnicodeString := Pointer(startPatternParameters[i]);
  {$ELSE}
        startPatternArgs[i].VType := vtWideString;
        startPatternArgs[i].VWideString := Pointer(startPatternParameters[i]);
  {$ENDIF}
      end;

      Result := SysUtils.Format(str.StartPattern, startPatternArgs);
    end
    else
    begin
      // Legacy or ICU without top level pattern
      Result := '';

      for i := str.Count - 1 downto 0 do
      begin
        part := str.Items[i];

        count := counts[i].VInteger;
        plural := GetPlural(count);
        thisPattern := part.GetPluralPattern(plural, count);

        if i = str.Count - 1 then
        begin
          SetLength(thisArgs, 1);
          thisArgs[0] := counts[i];
        end
        else
        begin
          SetLength(thisArgs, 2);
          thisArgs[0] := counts[i];
  {$IFDEF UNICODE}
          thisArgs[1].VType := vtUnicodeString;
          thisArgs[1].VUnicodeString := Pointer(Result);
  {$ELSE}
          thisArgs[1].VType := vtWideString;
          thisArgs[1].VWideString := Pointer(Result);
  {$ENDIF}
        end;
      end;

      Result := SysUtils.Format(thisPattern, thisArgs);

      if str.StartPattern <> '' then
        Result := SysUtils.Format(str.StartPattern, [Result]);
    end;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.FormatMulti(  //FI:C103
  const pattern: String;
  const args: array of TFormatParameter): String;
var
  i, count: Integer;
  thisPattern, value: String;
  plural: TPlural;
  str: TFormatString;
  arg: TFormatParameter;
  parameters: array of String;
  thisArgs, parameterArgs: array of TVarRec;
begin  //FI:C101
  str := TFormatString.Create;
  try
    str.ParsePattern(pattern);

    count := Length(args);

    if count <> str.Count then
      raise Exception.CreateFmt('Pattern "%s" does not have enough patterns for %d parameter', [pattern, count]);

    if (str.Syntax = fsIcu) and (str.StartPattern <> '') then
    begin
      // ICU with top level pattern
      SetLength(parameters, str.Count);
      SetLength(parameterArgs, str.Count);

      for i := 0 to str.Count - 1 do
      begin
        arg := args[i];

        case arg.Kind of
          fpPlural,
          fpOperator:
          begin
            plural := GetPlural(arg.Plural);
            thisPattern := str.Items[i].GetPluralPattern(plural, arg.Plural);

            SetLength(thisArgs, 1);
            thisArgs[0].VType := vtInteger;
            thisArgs[0].VInteger := arg.Plural;
          end;

          fpGender:
          begin
            thisPattern := str.Items[i].GetGenderPattern(arg.Gender);
            value := arg.Value;

            SetLength(thisArgs, 1);
      {$IFDEF UNICODE}
            thisArgs[0].VType := vtUnicodeString;
            thisArgs[0].VUnicodeString := Pointer(value);
      {$ELSE}
            thisArgs[0].VType := vtWideString;
            thisArgs[0].VWideString := Pointer(value);
      {$ENDIF}
          end;

          fpSelect:
          begin
          end;
        end;

        parameters[i] := SysUtils.Format(thisPattern, thisArgs);

      {$IFDEF UNICODE}
        parameterArgs[i].VType := vtUnicodeString;
        parameterArgs[i].VUnicodeString := Pointer(parameters[i]);
      {$ELSE}
        parameterArgs[i].VType := vtWideString;
        parameterArgs[i].VWideString := Pointer(parameters[i]);
      {$ENDIF}
      end;

      Result := SysUtils.Format(str.StartPattern, parameterArgs);
    end
    else
    begin
      // Legacy or ICU without top level pattern
      Result := '';

      for i := str.Count - 1 downto 0 do
      begin
        arg := args[i];

        case arg.Kind of
          fpPlural,
          fpOperator:
          begin
            plural := GetPlural(arg.Plural);
            thisPattern := str.Items[i].GetPluralPattern(plural, arg.Plural);

            if i = str.Count - 1 then
            begin
              SetLength(thisArgs, 1);
              thisArgs[0].VType := vtInteger;
              thisArgs[0].VInteger := arg.Plural;
            end
            else
            begin
              SetLength(thisArgs, 2);
              thisArgs[0].VType := vtInteger;
              thisArgs[0].VInteger := arg.Plural;

      {$IFDEF UNICODE}
              thisArgs[1].VType := vtUnicodeString;
              thisArgs[1].VUnicodeString := Pointer(Result);
      {$ELSE}
              thisArgs[1].VType := vtWideString;
              thisArgs[1].VWideString := Pointer(Result);
      {$ENDIF}
            end;
          end;

          fpGender:
          begin
            thisPattern := str.Items[i].GetGenderPattern(arg.Gender);
            value := arg.Value;

            if i = str.Count - 1 then
            begin
              SetLength(thisArgs, 1);
      {$IFDEF UNICODE}
              thisArgs[0].VType := vtUnicodeString;
              thisArgs[0].VUnicodeString := Pointer(value);
      {$ELSE}
              thisArgs[0].VType := vtWideString;
              thisArgs[0].VWideString := Pointer(value);
      {$ENDIF}
            end
            else
            begin
              SetLength(thisArgs, 2);
      {$IFDEF UNICODE}
              thisArgs[0].VType := vtUnicodeString;
              thisArgs[0].VUnicodeString := Pointer(value);

              thisArgs[1].VType := vtUnicodeString;
              thisArgs[1].VUnicodeString := Pointer(Result);
      {$ELSE}
              thisArgs[0].VType := vtWideString;
              thisArgs[0].VWideString := Pointer(value);

              thisArgs[1].VType := vtWideString;
              thisArgs[1].VWideString := Pointer(Result);
      {$ENDIF}
            end;
          end;

          fpSelect:
          begin
          end;
        end;

        Result := SysUtils.Format(thisPattern, thisArgs);
      end;

      if str.StartPattern <> '' then
        Result := SysUtils.Format(str.StartPattern, [Result]);
    end;
  finally
    str.Free;
  end;
end;

class function TMultiPattern.IsNumber(const value: String): Boolean;
begin
  Result := IsPlural(value) or IsOperator(value);
end;

class function TMultiPattern.IsPlural(const value: String): Boolean;
var
  plural: TPlural;
begin
  Result := TryStringToPlural(value, plural);
end;

class function TMultiPattern.IsOperator(const value: String): Boolean;
var
  kind: TOperatorKind;
  operand, operand2: Integer;
begin
  Result := TryStringToOperator(value, kind, operand, operand2);
end;

class function TMultiPattern.IsGender(const value: String): Boolean;
var
  gender: TGender;
begin
  Result := TryStringToGender(value, gender);
end;

class function TMultiPattern.IsOther(const value: String): Boolean;
begin
  Result := value = OTHER_C;
end;

class function TMultiPattern.IsNeutral(const value: String): Boolean;
begin
  Result := (value = OTHER_C) or (value = NEUTRAL_C);
end;

class function TMultiPattern.StringToPlural(const value: String): TPlural;
begin
  if not TryStringToPlural(value, Result) then
    raise Exception.CreateFmt('Invalid plural value: %s', [value]);
end;

class function TMultiPattern.StringToOperator(const value: String; out kind: TOperatorKind; out operand2: Integer): Integer;
begin
  if not TryStringToOperator(value, kind, Result, operand2) then
    raise Exception.CreateFmt('Invalid oprator value: %s', [value]);
end;

class function TMultiPattern.StringToOperator(const value: String; out kind: TOperatorKind): Integer;
var
  operand2: Integer;
begin
  if not TryStringToOperator(value, kind, Result, operand2) then
    raise Exception.CreateFmt('Invalid oprator value: %s', [value]);
end;

class function TMultiPattern.StringToGender(const value: String): TGender;
begin
  if not TryStringToGender(value, Result) then
    raise Exception.CreateFmt('Invalid gender value: %s', [value]);
end;

class function TMultiPattern.TryStringToPlural(const value: String; out plural: TPlural): Boolean;
var
  i: TPlural;
begin
  // zero, one, two, few, many, other
  for i := Low(i) to High(i) do
  begin
    Result := SameText(value, PLURAL_DATAS_C[i]);

    if Result then
    begin
      plural := i;
      Exit;
    end
  end;

  Result := False;
end;

class function TMultiPattern.TryStringToOperator(value: String; out kind: TOperatorKind; out operand, operand2: Integer): Boolean;

  function IsNumber(const value: String): Boolean;
  var
    i: Integer;
    c: Char;
  begin
    for i := 1 to Length(value) do
    begin
      c := value[i];

      Result := (c >= '0') and (c <= '9');

      if not Result then
        Exit;
    end;

    Result := value <> '';
  end;

var
  p: Integer;
begin
  p := Pos('..', value);

  if p > 1 then
  begin
    operand := StrToIntDef(Copy(value, 1, p - 1), 0);
    operand2 := StrToIntDef(Copy(value, p + 2, Length(value)), 0);
    kind := okRange;
    Result := True;
  end
  else if (Pos('<=', value) = 1) or (Pos('>=', value) = 1) then
  begin
    // >=0, <=1, <=2, ...
    if Pos('<=', value) = 1 then
      kind := okLessOrEqual
    else if Pos('>=', value) = 1 then
      kind := okGreaterOrEqual;

    Delete(value, 1, 2);
    operand := StrToIntDef(value, 0);
    Result := True;
  end
{$IFDEF UNICODE}
  else if (value <> '') and CharInSet(value[1], ['=', '~', '<', '>']) then
{$ELSE}
  else if (value <> '') and (value[1] in ['=', '~', '<', '>']) then
{$ENDIF}
  begin
    // =0, =1, =2, ...
    case value[1] of
      '=': kind := okEqual;
      '~': kind := okAround;
      '<': kind := okLess;
      '>': kind := okGreater;
    end;

    Delete(value, 1, 1);
    operand := StrToIntDef(value, 0);
    Result := True;
  end
  else if (value <> '') and IsNumber(value) then
  begin
    kind := okEqual;
    operand := StrToIntDef(value, 0);
    Result := True;
  end
  else
    Result := False;
end;

class function TMultiPattern.TryStringToGender(const value: String; out gender: TGender): Boolean;
const
  NEUTRAL_C = 'neutral';
var
  i: TGender;
begin
  // For backward compability
  if SameText(value, NEUTRAL_C) or SameText(value, NEUTRAL_C[1]) then
  begin
    gender := geNeutral;
    Result := True;
    Exit;
  end;

  // male, m, female, f, other, o
  for i := Low(i) to High(i) do
  begin
    Result := SameText(value, GENDER_DATAS_C[i]) or SameText(value, GENDER_DATAS_C[i][1]);

    if Result then
    begin
      gender := i;
      Exit;
    end
  end;

  gender := geNeutral;
  Result := False;
end;

class function TMultiPattern.PluralToString(value: TPlural): String;
begin
  Result := PLURAL_DATAS_C[value]
end;

class function TMultiPattern.OperatorToString(kind: TOperatorKind; operand, operand2: Integer): String;
begin
  if kind = okRange then
    Result := IntToStr(operand) + OPERATORS_C[kind] + IntToStr(operand2)
  else
    Result := OPERATORS_C[kind] + IntToStr(operand)
end;

class function TMultiPattern.GenderToString(value: TGender): String;
begin
  Result := GENDER_DATAS_C[value]
end;

class function TMultiPattern.SelectToString(const value: String): String;
begin
  Result := value
end;

class function TMultiPattern.GetPluralName(plural: TPlural): String;
resourcestring
  SZero = 'Nullar';
  SOne = 'Singular';
  STwo = 'Dual';
  SFew = 'Few';
  SMany = 'Many';
  SPlural = 'Plural';
begin
  case plural of
    pfZero: Result := SZero;
    pfOne: Result := SOne;
    pfTwo: Result := STwo;
    pfFew: Result := SFew;
    pfMany: Result := SMany;
  else
    Result := SPlural;
  end;
end;

class function TMultiPattern.GetGenderName(gender: TGender): String;
resourcestring
  SMale = 'Male';
  SFemale = 'Female';
  SNeutral = 'Neutral';
begin
  case gender of
    geMale: Result := SMale;
    geFemale: Result := SFemale;
  else
    Result := SNeutral;
  end;
end;

function GetDefaultPlural(n: Double; i: Integer; v: Integer; w: Integer; f: Integer; t: Integer): TPlural;  //FI:O804
begin
  if i = 1 then
    Result := pfOne
  else
    Result := pfOther
end;

function CheckInfo(const id: String): TPluralInfo;
var
  index: Integer;
begin
  if FDatas = nil then
  begin
    FDatas := TStringList.Create;
    FDatas.Sorted := True;
    TMultiPattern.Register('', GetDefaultPlural);
    TMultiPattern.Register('', [pfOne, pfOther], [pfOne, pfOther], '(n == 1) ? 0 : 1');
  end;

  index := FDatas.IndexOf(id);

  if index >= 0 then
    Result := FDatas.Objects[index] as TPluralInfo
  else
  begin
    Result := TPluralInfo.Create;
    Result.Id := id;
    FDatas.AddObject(id, Result);
  end;
end;

class procedure TMultiPattern.Register(const id: String; proc: TPluralProc);
begin
  CheckInfo(id).Proc := proc;
end;

class procedure TMultiPattern.Register(const id: String; integerPlurals, decimalPlurals: TPlurals; const expression: String);
var
  data: TPluralInfo;
begin
  data := CheckInfo(id);
  data.IntegerPlurals := integerPlurals;
  data.DecimalPlurals := decimalPlurals;
  data.Expression := expression;
end;


// TPluralInfo

constructor TPluralInfo.Create;
begin
  inherited;
  FDefaultInteger := pfOther;
  FDefaultDecimal := pfOther;
end;

function GetCount(plurals: TPlurals): Integer;
var
  p: TPlural;
begin
  Result := 0;

  for p := Low(p) to High(p) do
    if p in plurals then
      Inc(Result);
end;

function GetPlural(plurals: TPlurals; i: Integer): TPlural;
var
  index: Integer;
begin
  index := -1;

  for Result := Low(Result) to High(Result) do
    if Result in plurals then
    begin
      Inc(index);

      if index = i then
        Exit;
    end;

  Result := pfOther;
end;

function TPluralInfo.GetInteger(i: Integer): TPlural;
begin
  Result := GetPlural(IntegerPlurals, i);
end;

function TPluralInfo.GetDecimal(i: Integer): TPlural;
begin
  Result := GetPlural(DecimalPlurals, i);
end;

procedure TPluralInfo.SetIntegerPlurals(value: TPlurals);
begin
  FIntegerPlurals := value;
  FIntegerCount := GetCount(FIntegerPlurals);
  FDefaultInteger := Integers[IntegerCount - 1];
end;

procedure TPluralInfo.SetDecimalPlurals(value: TPlurals);
begin
  FDecimalPlurals := value;
  FDecimalCount := GetCount(FDecimalPlurals);
  FDefaultDecimal := Decimals[DecimalCount - 1];
end;

class function TPluralInfo.Get(id: String = ''): TPluralInfo;
var
  index: Integer;
  language, country, variant: String;
begin
  if id = '' then
    id := TNtBase.GetActiveLocale;

  index := FDatas.IndexOf(id);

  if index = -1 then
  begin
    TNtBase.ParseLocaleString(id, language, country, variant);
    index := FDatas.IndexOf(language);

    if index = -1 then
      index := FDatas.IndexOf('');

    if index = -1 then
      index := FDatas.IndexOf('en');

    if index = -1 then
      index := 0;
  end;

  Result := FDatas.Objects[index] as TPluralInfo;
end;

class function TPluralInfo.Find(const id: String): TPluralInfo;
var
  index: Integer;
begin
  index := FDatas.IndexOf(id);

  if index >= 0 then
    Result := FDatas.Objects[index] as TPluralInfo
  else
    Result := nil;
end;


initialization
  OperatorDelta := 1;
  RaiseExceptionOnInvalidPattern := False;
finalization
  while FDatas.Count > 0 do
  begin
    FDatas.Objects[0].Free;
    FDatas.Delete(0);
  end;
end.
