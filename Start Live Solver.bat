@echo off
REM Starts the OptimalEcospace Live Solver in your web browser.
REM Requires R (https://cloud.r-project.org). Keep this window open while you use the app.
setlocal
cd /d "%~dp0"

set "RSCRIPT="
for /f "delims=" %%R in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%R"
if not defined RSCRIPT (
  for /d %%D in ("%ProgramFiles%\R\R-*") do set "RSCRIPT=%%D\bin\Rscript.exe"
)
if not defined RSCRIPT (
  for /d %%D in ("%LOCALAPPDATA%\Programs\R\R-*") do set "RSCRIPT=%%D\bin\Rscript.exe"
)
if not defined RSCRIPT (
  echo R was not found. Install R from https://cloud.r-project.org and run this file again.
  pause
  exit /b 1
)

echo Using "%RSCRIPT%"
"%RSCRIPT%" "%~dp0R\LiveSolver\launch.R"
if errorlevel 1 pause
