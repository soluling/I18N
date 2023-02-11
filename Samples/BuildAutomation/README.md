# Soluling's Build Automation Samples

This directory contains Soluling's Build Automation samples. Learn about [build automation](https://www.soluling.com/Help/CommandLineTool.htm).

## Import

This directory contains an INI file and a command line batch file that creates a project for the INI file (`Sport.ini`), then imports Finnish and German translations from a TMX file (`Sport.tmx`) and Japanese translations from an Excel file (`Sport.xlsx`), and finally builds the localized INI files.

## MachineTranslation

This directory contains a Delphi application (`Win32\Debug\Project1.exe`) and a command line batch file that scans an existing Soluling project (`Project1.ntp`) that contains the Delphi application, then uses machine translation to translate the project, and finally builds the localized Delphi applications.

The batch file (Run.bat) uses DeepL engine to translate to German ad French, and Google engine to translate to Finnish. To run the batch file you need to configure the engines. If you configure some other engines but DeepL or Google you need to change the engine parameter in the batch file.

```powershell
SoluMake translate -lang:fr;de -engine:DeepL Work.ntp
```

Let's use Microsoft engine instead of DeepL.

```powershell
SoluMake translate -lang:fr;de -engine:Microsoft Work.ntp
```

Once you run the batch, it translates the project and creates localized Delphi applications: `fi\Project1.exe`, `de\Project1.exe`, and `fr\Project1.exe`.
