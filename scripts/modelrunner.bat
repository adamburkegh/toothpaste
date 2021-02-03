
for %%a in ("%~dp0.") do @set TPHOME=%%~dpa

set LDP=ldlib\lp_solve_5.5_java\lib\win64

set JMEM=50g

set JOPTS=-Xmx%JMEM% -Xms%JMEM%
set CONFIG_PARAMS=-Djava.library.path=%LDP% -Dlog4j.configurationFile=config\static\log4j2.xml

for /f %%F in ('dir /b lib\toothpaste*.jar') do (
  set jarfile=%%F
)

set PATH=%PATH%;%TPHOME%\scripts

if not exist var mkdir var


REM Main class is qut.pm.toothpaste.ModelRunner

"%JAVA_HOME%\bin\java" %CONFIG_PARAMS% %JOPTS% -jar lib\%jarfile% > out.log 2>&1


