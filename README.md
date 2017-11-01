# Internationalization API for .NET, Delphi, Java and TypeScript

This project implements collection of I18N APIs for .NET, [Delphi](https://en.wikipedia.org/wiki/Delphi_(programming_language)), Java and [TypeScript](https://en.wikipedia.org/wiki/TypeScript). Each API adds additional features to the start I18N API of the platform. For example support for [grammatical numbers](https://en.wikipedia.org/wiki/Grammatical_number), [grammatical genders](https://en.wikipedia.org/wiki/Grammatical_gender), abbreviated numbers and ordinal numbers. Library also includes APIs to perform runtime language switch in applications. Finally the Delphi library contains a proper localization resource for [FireMonkey](https://en.wikipedia.org/wiki/FireMonkey).

## Implementation

Each API is 100% native API. It means that it contains full source code and requires no data files. This means that .NET API contains only C# code and requires no other library or data files. Similarly Delphi API contains only Delphi code. The rules used by the code have been extracted from [CLDR](https://en.wikipedia.org/wiki/Common_Locale_Data_Repository) into source code file that is part of the API source code. You don't need ICU library or CLDR XML files but everything including logic and rules are compiled into your application.

## Grammatical numbers and genders

Most resource formats (e.g. .resx in .NET, .properties in Java, and resource strings in Delphi) only support plain strings. Grammatical numbers and genders require structural data where there are several language depend variants for a single string. For example in English "I have N cars" would require two variants:

| Grammatical number | .NET            | Delphi and Java |
| :----------------- | --------------- | --------------- |
| singular           | I have {0} car  | I have %d car   |
| plural             | I have {0} cars | I have %d cars  |

Some platforms such as Android have a built-in support for plurals and that is why they also have a special resource format to hold multiple patterns. However .NET, Delphi and Java do not have such a resource format. This library uses a special syntax to store all related patterns into a standard resource string. The syntax is:

`form1;pattern1[;form2;pattern2]...[;formN;patternN]`

where

`form` is the code for grammatical number form or for grammatical gender form.

The following table contains possible forms.

| Form    | Used with                       | Description                              |
| ------- | ------------------------------- | ---------------------------------------- |
| zero    | Grammatical numbers             | Nullar                                   |
| one     | Grammatical numbers             | Singular                                 |
| two     | Grammatical numbers             | Dual                                     |
| few     | Grammatical numbers             | Paucal or similar                        |
| many    | Grammatical numbers             | numbers: Greater paucal or similar.<br />gender: Neutral |
| other   | Grammatical numbers and genders | Plural                                   |
| male    | Grammatical genders             | Male                                     |
| female  | Grammatical genders             | Female                                   |
| neutral | Grammatical genders             | Neutral. Same as other.                  |

In addition of the above forms you can use operator code with grammatical numbers. The operators are:

| Form   | Example | Description         |
| ------ | ------- | ------------------- |
| =N     | =1      | Equal               |
| ~N     | ~12     | Around              |
| &gt;N  | &gt;5   | Greater than        |
| <N     | <10     | Less than           |
| &gt;=N | &gt;=5  | Greater or equal to |
| <=N    | <=10    | Less or equal to    |
| a..b   | 2..6    | Range               |

See samples in `Samples\Delphi\VCL\Patterns`, `Samples\Delphi\FMX\Patterns`, `Samples\WindowsForms\Patterns` and `Samples\WPF\Patterns` directories.

### Multiple placeholders

The API supports multiple plural/gender enabled placeholders. In that case you have split your string into parts each containing one placeholder and then chain the segments into a logical sentence. Use `next` form to start a new part. For example if I want to say "I have `C` cars and `S` skis" you will create the following string.

`one;I have {0} car and {1};other;I have {0} cars and {1};next;one;{0} ski;other;{0} skis`

The above string contains two parts: one for car and another for ski. Each part contains two patterns: singular and plural. {1} placeholder is used to chain the two parts.

See `Samples\Delphi\VCL\Patterns\Multi`, `Samples\Delphi\FMX\Patterns\Multi`, `Samples\WindowsForms\Patterns\Multi` and `Samples\WPF\Patterns\Multi` samples.

## Abbreviated numbers

TBW

## Ordinal numbers

TBW

## Runtime language switch

TBW

## .NET

`Library\NET` contains the .NET API. `Library\NET\Standard` contains a .NET Standard library that contains API for grammatical numbers, grammatical genders, abbreviated numbers and ordinal numbers. Compile it and add that into your solution and finally add the library into the references of your project. Because the library is .NET Standard it works with Windows Forms, WPF, ASP.NET, .NET Core and Xamarin.

## Delphi

`Library\Delphi` contains the Delphi API. Easiest way to include them into your application is to add the path to the search path of your project.

## Java

Java library and samples are under construction. Will be added in Q1 2018.

## TypeScript/JavaScript

TypeScript/JavaScript library and samples are under construction. Will be added in Q1 2018.

## Other programming languages

I would like to see other programming languages such as PHP, Python and Ruby. As I am not that experienced PHP or Python programmer and I have never used Ruby it would be nice if somebody would implement Python library and samples.

## Localization

If you plan to localize your application using multiple patterns strings you better use a localization tool that has support for grammatical numbers and grammatical genders. In Q1 2018 a new localization tool will be released. It has an excellent support for multiple patterns and it supports ASP.NET, .NET, Delphi, Java, Angular and many more.

![NewTool.png](https://github.com/jaska45/I18N/blob/master/NewTool.png)

In addition it supports continuous localization, machine translation, interactive fuzzy matching enabled translation memory, interactive terminology, import/export, build tools and cloud translation.