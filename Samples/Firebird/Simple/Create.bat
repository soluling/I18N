del Simple.fdb
isql -user SYSDBA -password masterkey -i Simple.sql

cd Field
del Simple.fdb
isql -user SYSDBA -password masterkey -i Simple.sql
cd..

cd Row
del Simple.fdb
isql -user SYSDBA -password masterkey -i Simple.sql
cd..
