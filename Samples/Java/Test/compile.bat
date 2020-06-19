javac -d bin src\*.java
copy src\messages.properties bin

cd bin
jar cvf Test.jar *.class Messages.properties
cd ..

