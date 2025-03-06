@echo off

rem A script to launch the SelectJava tool under Windows
rem Inventories the installed java versions on the machine
rem Makes it possible to choose the one to be used in Capsis or AMAPstudio (Xplo, etc...)
rem This script is optional, not needed if the correct Java is in the system PATH
rem Can help in case there are several versions of java installed
rem fc-31.5.2022

java -Dfile.encoding=ISO8859-15 -cp .\bin;.\ext\* jeeb.lib.util.selectjava.SelectJava


