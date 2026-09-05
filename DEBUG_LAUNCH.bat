@echo off
setlocal
cd /d "%~dp0"
title BOIN-ETC Debug Launcher
set "RSCRIPT=C:\Program Files\R\R-4.5.2\bin\Rscript.exe"
if not exist "%RSCRIPT%" (
  for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do if not defined FOUND set "FOUND=%%I"
  if defined FOUND set "RSCRIPT=%FOUND%"
)
if not exist "%RSCRIPT%" (
 echo Rscript.exe not found.
 pause
 exit /b 1
)
echo Running backend in the foreground so any R error is visible immediately.
echo.
"%RSCRIPT%" --vanilla "%~dp0run_desktop.R"
echo.
echo Backend exited. The error above is the exact reason if startup failed.
pause
