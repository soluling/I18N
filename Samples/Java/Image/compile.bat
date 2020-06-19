javac -classpath ..\..\..\Library\Java -d bin src\Image\*.java
copy src\Image\messages.properties bin\Image
copy src\Image\flag.png bin\Image

cd bin
jar cvf Image.jar Image\*.class soluling\*.class Image\messages.properties Image\flag.png
cd ..

