del *.zip
zip -R Python *.md *.ntp *.bat *.sln *.pyproj *.py *.htm* *.po *.mo *.txt -x */env/*
zip -d Python Build.bat