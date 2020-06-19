javac -classpath ..\..\..\Library\Java -d bin src\Driving\*.java
copy src\Driving\messages.properties bin\Driving

cd bin
jar cvf Driving.jar Driving\*.class soluling\*.class Driving\messages.properties Driving\*.png
cd ..
