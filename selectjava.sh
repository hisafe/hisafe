# A script to launch the SelectJava tool under Linux and MacOSX
# Inventories the installed java versions on the machine
# Makes it possible to choose the one to be used in AMAPstudio (Xplo, etc...)
# This script is optional, not needed if the correct Java is in the system PATH
# Can help in case there are several versions of java installed
# fc-5.3.2021 (under progress)

java -Dfile.encoding=ISO8859-15 -cp ./bin:./ext/* jeeb.lib.util.selectjava.SelectJava


