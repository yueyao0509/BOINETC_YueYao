@echo off
setlocal EnableExtensions EnableDelayedExpansion
cd /d "%~dp0"
title BOIN-ETC Operator App Launcher

set "LOG=%~dp0boinetc_startup.log"
set "OUTLOG=%~dp0boinetc_backend_stdout.log"
set "ERRLOG=%~dp0boinetc_backend_stderr.log"
set "PIDFILE=%~dp0boinetc_backend.pid"
>"%LOG%" echo BOIN-ETC startup log - %date% %time%

echo ================================================
echo   BOIN-ETC Operator App - one-click launcher
echo ================================================
echo.
echo [1/4] Locating R...
set "RSCRIPT="

for /f "delims=" %%I in ('where Rscript.exe 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%I"
if not defined RSCRIPT for %%K in (
  "HKCU\Software\R-core\R"
  "HKLM\Software\R-core\R"
  "HKLM\Software\WOW6432Node\R-core\R"
) do (
  for /f "tokens=2,*" %%A in ('reg query %%K /v InstallPath 2^>nul ^| find /i "InstallPath"') do (
    if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
  )
)
if not defined RSCRIPT call :find_r_under "C:\Program Files\R"
if not defined RSCRIPT call :find_r_under "C:\Program Files (x86)\R"
if not defined RSCRIPT call :find_r_under "%LOCALAPPDATA%\Programs\R"
if not defined RSCRIPT call :find_r_under "%LOCALAPPDATA%\R"
if not defined RSCRIPT goto :r_not_found

echo [OK] R detected:
echo      %RSCRIPT%
>>"%LOG%" echo Rscript=%RSCRIPT%

echo.
echo [2/4] Starting R / BOINETC backend...
set "RFILE=%~dp0run_desktop.R"
if not exist "%RFILE%" goto :missing_rfile

powershell -NoProfile -ExecutionPolicy Bypass -Command "$p=(Get-NetTCPConnection -LocalPort 8765 -State Listen -ErrorAction SilentlyContinue ^| Select-Object -First 1 -ExpandProperty OwningProcess); if($p){Stop-Process -Id $p -Force -ErrorAction SilentlyContinue}" >nul 2>&1

del /q "%OUTLOG%" "%ERRLOG%" "%PIDFILE%" 2>nul
rem IMPORTANT: quote the R script argument explicitly. Start-Process otherwise
rem breaks when the app lives under a path containing spaces.
powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$arg='\"' + $env:RFILE + '\"'; $p=Start-Process -FilePath $env:RSCRIPT -ArgumentList @('--vanilla',$arg) -WorkingDirectory '%~dp0' -RedirectStandardOutput $env:OUTLOG -RedirectStandardError $env:ERRLOG -PassThru; Set-Content -Path $env:PIDFILE -Value $p.Id"
if errorlevel 1 goto :backend_launch_failed

echo.
echo [3/4] Waiting for Shiny server on http://127.0.0.1:8765 ...
set /a COUNT=0
:wait_loop
rem First detect a backend crash instead of waiting four minutes with no clue.
if exist "%PIDFILE%" (
  set /p BPID=<"%PIDFILE%"
  powershell -NoProfile -ExecutionPolicy Bypass -Command "if(Get-Process -Id !BPID! -ErrorAction SilentlyContinue){exit 0}else{exit 1}" >nul 2>&1
  if errorlevel 1 goto :server_failed
)

powershell -NoProfile -ExecutionPolicy Bypass -Command "try { $r=Invoke-WebRequest -UseBasicParsing -TimeoutSec 1 http://127.0.0.1:8765; if($r.StatusCode -ge 200){exit 0}else{exit 1} } catch { exit 1 }" >nul 2>&1
if not errorlevel 1 goto :server_ready
set /a COUNT+=1
if !COUNT! GEQ 180 goto :server_failed
if !COUNT! EQU 3 call :show_progress
if !COUNT! EQU 10 echo      Still starting. First launch may install R packages.
if !COUNT! EQU 30 echo      Still working. Backend output is being recorded.
if !COUNT! EQU 60 echo      This is taking longer than usual. Check internet access if packages are being installed.
timeout /t 1 /nobreak >nul
goto :wait_loop

:show_progress
if exist "%OUTLOG%" (
  echo      Backend started. Latest message:
  powershell -NoProfile -Command "Get-Content -Path $env:OUTLOG -Tail 3 -ErrorAction SilentlyContinue" 2>nul
)
exit /b 0

:server_ready
echo [OK] BOIN-ETC backend is ready.
echo.
echo [4/4] Opening the app...
set "EDGE=%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe"
if not exist "%EDGE%" set "EDGE=%ProgramFiles%\Microsoft\Edge\Application\msedge.exe"
if exist "%EDGE%" (
  start "" "%EDGE%" --app="http://127.0.0.1:8765" --start-maximized
) else (
  start "" "http://127.0.0.1:8765"
)
echo.
echo Done. You can close this launcher window.
timeout /t 2 /nobreak >nul
exit /b 0

:r_not_found
echo.
echo [ERROR] Rscript.exe could not be found automatically.
echo Install R or send this screen for troubleshooting.
pause
exit /b 1

:missing_rfile
echo.
echo [ERROR] run_desktop.R is missing from this folder.
pause
exit /b 1

:server_failed
echo.
echo [ERROR] The R backend stopped or the Shiny server did not become ready.
echo.
echo -------- backend stderr --------
if exist "%ERRLOG%" type "%ERRLOG%"
echo.
echo -------- backend stdout --------
if exist "%OUTLOG%" type "%OUTLOG%"
echo.
echo The same logs are saved beside this launcher.
pause
exit /b 1

:backend_launch_failed
echo.
echo [ERROR] Windows could not start the R backend process.
echo R file: %RFILE%
echo Rscript: %RSCRIPT%
pause
exit /b 1

:find_r_under
set "ROOT=%~1"
if not exist "%ROOT%" exit /b 0
for /f "delims=" %%D in ('dir /b /ad /o-n "%ROOT%\R-*" 2^>nul') do (
  if exist "%ROOT%\%%D\bin\Rscript.exe" (
    set "RSCRIPT=%ROOT%\%%D\bin\Rscript.exe"
    exit /b 0
  )
)
exit /b 0
