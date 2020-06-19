javac -classpath ..\..\..\Library\Java -d bin src\Plural\*.java
copy src\Plural\messages.properties bin\Plural

cd bin
jar cvf Plural.jar Plural\*.class soluling\*.class Plural\messages.properties
cd ..

