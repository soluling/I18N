# Internationalization API for .NET, Delphi, Java and TypeScript

This project contains collection of I18N APIs for .NET, Delphi, Java and [TypeScript](https://en.wikipedia.org/wiki/TypeScript). Each API provides additional features to the platforms standard I18N API. For example support for grammatical number, grammatical gender, abbreviated numbers and ordinal numbers. Library also include API to perform runtime language switch in .NET and Delphi applications. Finally the Delphi library contains a proper localization resource for [FireMonkey](https://en.wikipedia.org/wiki/FireMonkey).

## Implementation

Each API is 100% native API. It means that it inly contains code and data in the source code of the platform. This means that .NET API contains only C# code and require no other library or data file. Similarly Delphi API contains only Delphi code. The rules used by the code have been extracted from [CLDR](https://en.wikipedia.org/wiki/Common_Locale_Data_Repository) into source code file that is then a part of the API. You don't need ICU library or CLDR XML files but everything, logic and rules, compiles into the application.

## Multiple patterns

Most resource formats (e.g. .resx, .properties and Delphi resourcestring) only support plain string. Grammatical numbers and genders require structural data where there are several variants for a single string. For example in English "I have 2 cars" would require two variants:

| Grammatical number | .NET            | Delphi and Java |
| :----------------- | --------------- | --------------- |
| singular           | I have {0} car  | I have %d car   |
| plural             | I have {0} cars | I have %d cars  |

Some platforms such as Android have a built-in support for plurals and they also have a special resource format to hold multiple patterns.

This library uses a special syntax to store all related patterns into a single resource string. The syntax is

`form1;pattern1[;form2;pattern2]...[;formN;patternN]`

where

`form` is the code for grammatical number form or for grammatical gender form.


## Abbreviated numbers


## Ordinal numbers


## Runtime language switch
