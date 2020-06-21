del Sport.fdb
isql -user SYSDBA -password masterkey -i Sport.sql

cd Field
del Sport.fdb
isql -user SYSDBA -password masterkey -i Sport.sql
cd..

cd Row
del Sport.fdb
del SportNoCase.fdb
del SportAnsi.fdb
isql -user SYSDBA -password masterkey -i Sport.sql
isql -user SYSDBA -password masterkey -i SportNoCase.sql
isql -user SYSDBA -password masterkey -i SportAnsi.sql
cd..

cd RowId
del Sport.fdb
isql -user SYSDBA -password masterkey -i Sport.sql
cd..

cd Table
del Sport.fdb
isql -user SYSDBA -password masterkey -i Sport.sql
cd..