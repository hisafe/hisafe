#!/usr/bin/env bash

###----- #!/usr/bin/env bash
###----- #!/bin/bash ---- prev version fc-30.1.2017 Trouble on Mac...


# This script launches Capsis under Linux 32 or 64 bits or MacOSX
# reviewed fc-16.9.2014
# reviewed fc-25.3.2021, added preferredjava

args="$*" #fc-15.5.2020


#---- Check wether the splashscreen should be shown (yes, except in script mode)
splashoption="-splash:./etc/splash.png"
string0="$*"
string1=${string0%script*}

#echo string0: aa $string0 aa
#echo string1: aa $string1 aa

if [ "$string0" != "$string1" ] # removed an extra ';' here fc-27.11.2014
then
  echo Script: splashscreen was desactivated
  splashoption=""
fi


#---- Check wether java options should be considered fc-15.5.2020
ujo=-useJvmOptions
string0="$*"
string1=${string0%$ujo*}
jvmOptions=""

if [ "$string0" != "$string1" ]
then
  #echo -useJvmOptions detected

  # Remove ujo from the command line
  #echo args: $args
  args=$(echo $args | sed 's/'$ujo'//')
  #echo args: $args
  
  # load the file in the variable
  read jvmOptions < "jvmOptions"
  echo "->" jvmOptions: $jvmOptions
  
fi






#---- Set default memory in Mb (see setmem.sh to change it)
mem="1024"


#---- Check if 'memory' file exists (created by setmem.sh), read it (1 single line, e.g. 4096)
if [ -f memory ]  # Check if the file named 'memory' exists
then
  read line < "memory"
  mem=$line
fi


#---- Set max memory, adding 'm' for 'Mega bytes'
memo="$mem"m


#---- If the javalibrarypath file was not already created, create it
if [ ! -f javalibrarypath ]  # if the file named 'javalibrarypath' does not exist
then
  java -cp ./class:./ext/* jeeb.lib.util.JavaLibraryPathDetector
fi


#---- Read the javalibrarypath file into a variable
read line < "javalibrarypath"
jlp=$line

# nb-13.09.2019
# Executables (stored in ./ext/linux64) can potentially need dynamic libraries stored in same directory. Example: genotype_generator needs the 
# dynamic library libgfortran.so.3. The '.' below refers to this directory (current directory).
# Because the LD_LIBRARY_PATH environment variable does not exist mandatory (type printenv command to print the names and values of all defined 
# environment variables), it is needed to create it (as a shell variable) and then exported it as an environment variable.
# Remark: if LD_LIBRARY_PATH does not already exist, $LD_LIBRARY_PATH is a zero-length (null) directory name and LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH 
# gives LD_LIBRARY_PATH=.: where the zero-length string behind ':' means the current directory.
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH


# fc-25.3.2021
# If preferredjava is set, use it preferently
javaCommand="java"
if [ -f preferredjava ]  # if the file named 'preferredjava' exists
then
  read javaCommand < "preferredjava"
  echo Using preferredjava: ${javaCommand}
fi



#---- Launch Capsis
# fc-25.3.2021 added javaCommand for preferredJava if set
${javaCommand} $jvmOptions $splashoption -Xmx${memo} -cp ./class:./ext/*:$jlp -Djna.library.path=$jlp -Djava.library.path=$jlp capsis.app.Starter $args



# fc-28.11.2017 added :$jlp at the end of -cp for Hi-sAFe & Stics
#java $jvmOptions $splashoption -Xmx${memo} -cp ./class:./ext/*:$jlp -Djna.library.path=$jlp -Djava.library.path=$jlp capsis.app.Starter $args
# fc-15.5.2020 added args
#java $splashoption -Xmx${memo} -cp ./class:./ext/*:$jlp -Djna.library.path=$jlp -Djava.library.path=$jlp capsis.app.Starter $*

