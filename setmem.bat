@echo off

rem Max memory available for Capsis: optional.
rem In Mega bytes.
rem Writes the given memory value to the file named 'memory'.
rem The 'memory' file is an optional file.
rem This file will be checked if present when launching Capsis to 
rem override the default memory values in capsis.bat.
rem Will help set java -Xmx parameter.
rem E.g. setmem 2048
rem E.g. setmem

rem Caution: if the given parameter is not a number of Mega bytes, this may prevent Capsis from running
rem In case of trouble, 'setmem' will reset (remove) the memory file

rem Check if a parameter was given
set p1=%1
if not defined p1 (

  rem No parameter: remove the memory file -> default memory
  del memory 1>nul 2>nul
  echo Capsis memory reset to capsis.bat default value

) else (

  rem A parameter was given: write the value (Mbytes) in the file named 'memory'
  echo %1> memory
  echo Capsis will run with %1 mega bytes available
rem  more memory

)




