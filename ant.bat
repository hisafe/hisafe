@echo off

rem fc-10.2.2014
rem On this machine, ant uses jdk1.6.0 even if java 1.7 is in the system path 
PATH=C:\Program Files\Java\jdk1.8.0_271\bin;%PATH%

REM fc - 3.3.2009 - rely on jdk in the path (trouble when JAVA_HOME contains a jre home)

set JAVA_HOME=""
set spath=%~dp0

cd %spath%
ext/ant/bin/ant.bat %*

