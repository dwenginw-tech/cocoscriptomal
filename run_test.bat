@echo off
for /f "tokens=*" %%i in ('opam env') do @%%i

if "%1"=="" (
    echo Usage: run_test.bat filename
    echo Example: run_test.bat examples\fizzbuzz.coco
    pause
    exit /b
)
dune exec cocoscript %1
if errorlevel 1 (
    echo Compile failed!
    pause
    exit /b
)
set "name=%~n1"
%name%.exe
