rem Remove the exiting localized files and the Soluling file, if any
del de\Sport.ini
del fi\Sport.ini
del ja\Sport.ini
del Sport.ntp

rem Let's create a Soluling project for Sport.ini and add Finnish, German, and Japanese
SoluMake add -lang:fi;de;ja Sport.ini Sport.ntp

rem Import Finnish and German from a TMX file.
SoluMake import -lang:fi;de Sport.tmx Sport.ntp

rem Import Japanese from an Excel file
SoluMake import -lang:ja -columns:co;en;ja Sport.xlsx Sport.ntp

rem Create localized files
SoluMake build Sport.ntp