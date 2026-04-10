@echo off
for /f "tokens=*" %%i in ('opam env') do @%%i

echo Building all CocoScript examples...
echo.

for %%f in (examples\*.coco) do (
    echo Compiling %%f...
    dune exec cocoscript %%f
    if errorlevel 1 (
        echo   FAILED: %%f
    ) else (
        echo   OK
    )
    echo.
)

echo Done!
pause
