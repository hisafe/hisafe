ECHO OFF

REM initial dir
SET INIT_DIR=%CD%

REM set version
SET VER=%1
IF "%1"=="" (
     echo "Missing first input argument for model version !"
     exit 1
)

REM set stics dir
SET STICS_DIR=%2
SET DEBUG_DIR=%STICS_DIR%\Debug
IF "%2"=="" (
    SET DEBUG_DIR=Debug
    SET STICS_DIR="."
)

REM Stics dir
echo Stics directory %STICS_DIR%

REM version file creation
echo Informations about Stics model build version > %STICS_DIR%\stics_version.txt
echo Build date: %DATE% (%TIME%) >> %STICS_DIR%\stics_version.txt
echo Version : %VER% >> %STICS_DIR%\stics_version.txt

REM SET TMP_DIR=%STICS_DIR%\src\inputs\tmp
REM if dir name needed
REM IF NOT EXIST %CD%\%TMP_DIR% (
REM  	echo "Invalid directory : %CD%\%TMP_DIR%"
REM        echo "Set src parent directory name as script second input argument"
REM         exit 1
REM )

REM Creating F90 file with version and date
REM if EXIST %TMP_DIR%\call_num_version.f90 (
REM    del /F %TMP_DIR%\call_num_version.f90
REM )

REM type %TMP_DIR%\call_num_version_head.tmp > %TMP_DIR%\call_num_version.f90
REM echo         nomversion='%VER%' >> %TMP_DIR%\call_num_version.f90
REM echo         dateversion='%DATE%' >> %TMP_DIR%\call_num_version.f90
REM type %TMP_DIR%\call_num_version_foot.tmp >> %TMP_DIR%\call_num_version.f90
REM move /Y %TMP_DIR%\call_num_version.f90 %STICS_DIR%\src\inputs


REM moving to Debug folder for compilation
cd %DEBUG_DIR%
IF EXIST SticsV8.dll (
   del /F SticsV8.dll
)

REM make all
mingw32-make all



REM back to initial dir
cd %INIT_DIR%
