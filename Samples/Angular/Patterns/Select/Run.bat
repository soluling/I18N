if "%1"=="" (
  ng serve --aot -o
) else (
  ng serve -c=%1 -o
)