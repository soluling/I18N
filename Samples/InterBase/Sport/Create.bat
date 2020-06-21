rem Rename ibsql to isql before running this script

del Sport.gdb
ibsql -user SYSDBA -password masterkey -i Sport.sql

cd Field
del Sport.gdb
ibsql -user SYSDBA -password masterkey -i Sport.sql
cd..

cd Row
del Sport.gdb
del SportAnsi.gdb
ibsql -user SYSDBA -password masterkey -i Sport.sql
ibsql -user SYSDBA -password masterkey -i SportAnsi.sql
cd..

cd RowId
del Sport.gdb
ibsql -user SYSDBA -password masterkey -i Sport.sql
cd..

cd Table
del Sport.gdb
ibsql -user SYSDBA -password masterkey -i Sport.sql
cd..