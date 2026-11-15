# Soluling's Localization Project

This folder contains Soluling's localization project. `Soluling.ntp` is a Soluling project file for `Soluling.exe` and `SoluMake.exe` applications. If you want to localize Soluling's user interface to a new language, follow these steps.

1. Copy `Soluling.exe` and `vcl370.bpl` files from `C:\Program Files\Soluling` to this folder.
2. Copy Soluling setup file, `SoluSetup.exe`, to this folder.
3. Start Soluling and open `Soluling.ntp`.
4. Click *Home* | *Languages* to show the language dialog.
5. Add the language(s) you want to translate.
6. Start translating. 
7. You can build the language files (`Soluling.??` and `vcl370.??`) any time by performing Soluling's build operation (*Home* | *Build*). 
8. To test localization, copy the new language files to `C:\Program Files\Soluling` and restart Soluling.
9. You can change the active language of Soluling by clicking *Options* | *General* and then selecting from *User interface language* combo box.

## Use language files (resource DLLs) only

Translate Soluling's user interface only with language files, which are resource DLLs. The project already builds them:

- `Soluling.??` contains the translated resources of `Soluling.exe` (and `SoluMake.exe`).
- `vcl370.??` contains the translated resources of `vcl370.bpl`.

The extension is the language code, for example `Soluling.de` or `Soluling.ja`. Copy these files next to `Soluling.exe`. Soluling finds them when it starts.

Do not create a localized `Soluling.exe`:

- Do not change the project's output files to *Localized files*, *Multilingual file*, or *Embedded file*. Keep *Language files*.
- Do not translate or modify `Soluling.exe`, `SoluMake.exe`, or `vcl370.bpl` with Soluling, with a resource editor, or with any other tool.

`Soluling.exe` is digitally signed, and it checks its signature when it starts. Any change to the file, even a single translated string, makes the signature invalid. Soluling then shows *"Soluling.exe has been modified, or its digital signature is not valid"* and does not start. A modified EXE can also fail in other ways, for example when it loses the images of the ribbon.

Language files do not change the signed EXE, so the signature check always passes. Build the language files again when you update Soluling, so that they match the new `Soluling.exe`.

