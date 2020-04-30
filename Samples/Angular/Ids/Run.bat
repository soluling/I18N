if "%1"=="" (
  ng serve --aot
) else (
  ng serve -c=%1 -o
)