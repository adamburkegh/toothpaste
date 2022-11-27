
echo on

set lg1=%1
set md=%2

echo %DATE% %TIME%

for /F %%i in ("%lg1%") do @set lg=%%~ni
for /F %%i in ("%md%") do @set OUTD=%%~pi
for %%a in ("%~dp0.") do @set TPHOME=%%~dpa

echo "Preparing event log ..."

echo "TPHOME" %TPHOME%
echo "OUTD"   %OUTD%

set VDIR=%TPHOME%tpvenv\scripts
set PYTHON_HOME=%VDIR%

echo %VDIR%\activate.bat
REM %VDIR%\activate.bat

%PYTHON_HOME%\python %TPHOME%scripts\xesdcdt.py --outfile %OUTD%\%lg%.dcdt %lg1%


echo %DATE% %TIME%
echo "Mining ..."

%TPHOME%bin\toothpaste.exe --logformat=dcdt --eventlog %OUTD%\%lg%.dcdt --pnetfile %md% --ptreefile %OUTD%\%lg%.ptree 

echo %DATE% %TIME%
echo "ToothpasteMiner done."

