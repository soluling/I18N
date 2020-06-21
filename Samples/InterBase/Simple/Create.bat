del Simple.gdb
ibsql -user SYSDBA -password masterkey -i Simple.sql

cd Field
del Simple.gdb
ibsql -user SYSDBA -password masterkey -i Simple.sql
cd..

cd Row
del Simple.gdb
ibsql -user SYSDBA -password masterkey -i Simple.sql
cd..