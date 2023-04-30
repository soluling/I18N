unit Strings;

interface

resourcestring
  // This multi-plural pattern has two parameters (hours and minutes).
  // It uses standard plural rules of English (1 = singular, all other are plurals) plus special zero case in hours.
  SResultPlural = 'Driving time{plural, zero { } one { %d hour } other { %d hours }}{plural, one {%d minute} other {%d minutes}}.';

  SAbout = 'Driving time calculator';
  SNtLocale = 'en';

implementation

end.
