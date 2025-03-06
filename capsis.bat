@echo off


rem This script launches Capsis under Windows 32 or 64 bits
rem reviewed fc-16.9.2014


rem Check wether the splashscreen should be displayed (yes, except in script mode)
set splashoption=-splash:etc/splash.png
set string0=%* notempty
set string1=%string0:script=%

rem echo.string0:%string0%
rem echo.string1:%string1%

IF NOT "%string0%" == "%string1%" (
  echo.Script: splashscreen was desactivated
  set splashoption=
)


rem fc-19.5.2020 Add commands here to manage useJvmOptions like in capsis.sh
rem fc-19.5.2020 Add commands here to manage useJvmOptions like in capsis.sh
rem fc-19.5.2020 Add commands here to manage useJvmOptions like in capsis.sh


rem Set default memory in Mb (see setmem to change it)
rem Please do not change this default value, capsis may fail to start under Windows fc-31.5.2016
set mem=1024



rem Check if 'memory' file exists (created by setmem), read it (1 single line, e.g. 4096)
if exist memory (
  for /f "tokens=*" %%a in (memory) do (
    set mem=%%a
  )
)


rem Set max memory, adding 'm' for 'Mega bytes'
set memo="%mem%m"


rem If the javalibrarypath file was not already created, create it
if not exist javalibrarypath (
  java -cp ./class;./ext/* jeeb.lib.util.JavaLibraryPathDetector
)


rem Read the javalibrarypath file into a variable
for /f "tokens=*" %%a in (javalibrarypath) do (
  set jlp=%%a
)

rem fc-31.5.2022
rem Check if 'preferredjava' file exists, read it (1 single line)
set javaCommand=java
if exist preferredjava (
    set /p javaCommand=<preferredjava
)
rem echo %javaCommand%

rem Launch Capsis
"%javaCommand%" %splashoption% -Xmx%memo% -cp ./class;./ext/* -Djna.library.path="%jlp%" -Djava.library.path="%jlp%" capsis.app.Starter %*

