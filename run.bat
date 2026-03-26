@echo off
opam exec -- dune build
if %errorlevel% neq 0 exit /b %errorlevel%
opam exec -- dune exec cocoscript %1
if %errorlevel% neq 0 exit /b %errorlevel%
%~dpn1.exe
