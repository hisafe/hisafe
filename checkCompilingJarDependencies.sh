#!/bin/bash

# Author: Nicolas Beudez
# Creation date: April 2017

# Description: for a given .jar file, this script allows to identify among a set of potential .jar dependencies which ones are 
# required and which ones are useless for recompiling this given .jar file. 
#
# BE CAREFUL: This script only identifies required and useless .jar dependencies during recompiling process. It is not made for 
#			  identifying required and useless .jar dependencies during execution process.
#
# Entries: 	- 1st argument: the path (absolute or relative) to the .jar file whose dependencies are checked.
#           - 2nd argument: the path (absolute or relative) to the directory containing potential .jar dependencies. This directory
#			                can be a simple list of .jar dependencies or contain directories with .jar dependencies inside or the both.
#
# Outputs:	- A report displayed in the terminal specifying:
#				- the number and the list of required .jar dependencies;
#				- the number of remaining compiling error(s) with these required .jar dependencies;
#				- the number and the list of useless .jar dependencies.
#			- The "checkCompilingJarDependenciesWorkspace" directory containing:
#				- the "compilationErrors_withAllDependencies_initial.txt" file containing the list of compilation error(s) obtained 
# 				  when recompiling the given .jar file with all potential .jar dependencies. This file can be empty if there is no 
#				  compilation error.
#				- the "compilationErrors_without_nameOfDependency.jar.txt" file containing the list of compilation error(s) obtained 
# 				  when recompiling the given .jar file without the required "nameOfDependency.jar" dependency. This file can be empty 
#				  if there is no compilation error.
#				- the "compilationErrors_withOnlyRequiredDependencies_final.txt" file containing the list of compilation error(s) obtained 
# 				  when recompiling the given .jar file with only required .jar dependencies. This file can be empty if there is no 
#				  compilation error.
#				- the "recompile" directory with its "src" and "class" subdirectories. "src" subdirectory contains the .java source files 
#				  of the .jar file whose dependencies are checked. "class" subdirectory contains generated .class files if compilation 
#				  succeeds (it remains empty if compilation fails).
#
# Run: ./checkCompilingJarDependencies.sh pathToJarFile pathToJarDependenciesDirectory
#      or
#	   bash checkCompilingJarDependencies.sh pathToJarFile pathToJarDependenciesDirectory
#
# BE CARFEFUL: to run this script, do not use "sh checkCompilingJarDependencies.sh pathToJarFile pathToJarDependenciesDirectory" because 
# this script uses some bash mechanisms like [[...]] for example.

#########################################################################
# Section in which the user can choose names for directories and files. #
#########################################################################

# Sets the names of the generated directories and files (not deleted at the end of the script).
# BE CAREFUL: the workspaceDir's name and recompileDir's name should not contain blank character.
workspaceDir="checkCompilingJarDependenciesWorkspace"	# basic directory where the .jar dependencies checking process occurs
recompileDir="recompile"								# directory where to recompile the .jar file whose dependencies are checked
compilationErrorFilePrefix="compilationErrors"			# prefix of files where to store compilation errors: 	
														# 		- compilationErrors_withAllDependencies_initial.txt, 
														# 		- compilationErrors_without_nameOfJarDependency.txt, 
														#		- compilationErrors_withOnlyRequiredDependencies_final.txt

####################################################################################
# Section to be NOT MODIFIED by the user: from this line to the end of the script. #
####################################################################################

# BE CAREFUL: it is important to use the double quotes "..." with variables (use "$var" instead of $var) in order 
# to protect the variables during the interpretation of spaces (case of a path variable containing spaces for example: pathToJarFile or
# pathToJarDependenciesDirectory). 
# Indeed, the interpretation of blanks occurs AFTER the expansion of the variable name.

######################
# Defines functions. #
######################

# BE CAREFUL: theses functions must be defined before being called.

# Note: all variables declared inside a function will be shared with the calling environment. All variables 
# declared "local" will not be shared.

# This function deletes the last character of the path given as parameter (represented by $1 inside 
# the function) if it is "/" (the slash character). The function returns the new path.
# Use of regular expressions with the "sed" command:
#	example: echo "abcd" | sed 's/b/toto/' 
#		This command will substitute ('s' of sed command for 'substitute') the first occurence of "b" string 
#		by the "toto" string. The result is: "atotocd"
#   Regular expressions:
#      . replaces a unique character in a regular expression, except the \n character.
#	   ^ identifies the beginning of a line.
#	   $ identifies the end of a line.
# The s/.$// regular expression means: substitues the last character (.$) by an empty string (there is 
# no character or string between the 2 last slashes '/' characters).
cleanPath() {
	# $1 represents the first parameter given to the function: this is the given path.
	local path="$1"

	# Extracts last character of path. -n to prevent a new line is added at the end of the printed path.
	local lastCharacterOfPath=$(echo -n "$path" | tail -c 1)

	# Deletes last character of path if it is "/". Note: use "=" and not "==" for strings comparison.
	if [ "$lastCharacterOfPath" = "/" ]; then
		path=$(echo "$path" | sed 's/.$//')
	fi

	# Returns the modified path.
	echo "$path"
}

# This function compiles the .java sources files listed in the given file (1st parameter $1), using the given 
# classpath (2nd parameter $2) and writes the generated .class files in the given directory (3rd parameter $3).
# Compilation options:
#	-classpath	sets the classpath
#	-nowarn		disables warning messages (usefull here because we only want to identify errors and moreover, 
#               a warning on a line of .java source file containing the "error" string can be detected as an error 
#               when calculating the number of compilation errors)
#	-encoding	sets the encoding of source files
#	-d			sets the directory where compiled .class files are stored
#	-Xmaxerrs	sets the maximal number of errors to display
compile() {
	local sourcesFile="$1"
	local classpathVariable="$2"
	local destDirectory="$3"

	echo -n "Compiling..." # -n allows to skip printing a new line.
	# Uses the same encoding as the one used for compiling capsis4 and amapstudio sources files (see the build.xml files 
	# of these softwares).
	javac @"$sourcesFile" -classpath "$classpathVariable" -nowarn -encoding ISO-8859-15 -d "$destDirectory" -Xmaxerrs 10000
	echo "Done."
}

# This function calculates the number of compilation errors listed in the given file (parameter $1).
# The "errors" pattern is not used for searching for the errors number because of the case of a single compilation error. 
# Instead the "error" pattern of the last line in given file is searched.
# Note: the case of a warning containing the "error" pattern can not exist (see the -nowarn option in the compile() 
# function above).
calculateNumberOfCompilationErrors() {

	# The "error" pattern of the last line (tail -n 1) in given file is searched.
	# cut command options:
	#	-d	delimiter
	#	-f	part to be displayed
	local errorsNb=$(grep "error" "$1" | tail -n 1 | cut -d " " -f 1)

	# Calculates the size of the value contained in errorsNb:
	#	0 if the above grep | tail | cut command sends nothing (it means no error)
	#	>0 otherwise
	local errorsNbStringSize=${#errorsNb}

	# Sets errorsNb to 0 if errorsNb string is empty.
	if [ $errorsNbStringSize -eq 0 ]; then
		errorsNb=0
	fi

	# Stops program if errorsNb is not a positive integer.
	#
	# If errorsNb is not a positive integer, then a signal is sent to the shell which interprets the script using 
	# the "kill" command. The signal is then intercepted by the "trap" command which will execute the "exit 1" command 
	# (see the "Defines signal interception." section).
	# Note: using "exit 1" in the current function will stop the function but not stop the script.
	#
	# Use of regular expressions to detect if errorsNb is a positive integer:
	# =~ is the regular expression match operator.
	# ^ matches the starting position within the string.
	# [] matches a single character that is contained within the brackets. [0-9] specifies a range which matches any digit.
	# + matches the preceding element one or more times.
	# $ matches the ending position within the string.
	if [[ ! "$errorsNb" =~ ^[0-9]+$ ]]; then

		errorMessage="\nError in calculateNumberOfCompilationErrors() function: the calculated number of compilation errors 
is not a positive integer.
Calculated number of compilation errors: $errorsNb\n"

		# Because the $(...) calling syntax captures the standard output, the standard output (identifiant 1) must be 
		# redirected on the error output (identifiant 2) when printing a message inside a function is needed.
		# Here: errorsNb=$(calculateNumberOfCompilationErrors compilationErrorsFile.txt)
		# >&2 is equivalent to 1>&2 because the part before >& has value 1 by default.
		# The -e option of echo command enables the interpretation of backslash escapes.
		echo -e "$errorMessage" >&2

		# Sends the TERM signal to the $$ pid (process id). TERM is the default signal for kill. $$ is the pid of the shell 
		# that interprets the script.
		kill -TERM $$
	else
		# Returns the number of compilation error(s)	.
		echo "$errorsNb"
	fi
}

################################
# Defines signal interception. #
################################

# Signal interception using the command: trap arg signal_spec
# The arg command is read and executed when shell receives the signal signal_spec. See "help trap".
# Used below in the calculateNumberOfCompilationErrors() function which can send a TERM signal. When the shell 
# receives the TERM signal, the "exit 1" command is executed which terminates the script with the 1 error code.
trap "exit 1" TERM

####################
# 1) Makes checks. #
####################

# Prints a new line.
echo ""

# Checks if the name of the workspace directory does not contain a slash character ("/").
# Use of regular expressions with the [[...]] operator. =~ is the regular expression match operator. 
# Remark: the =~ no longer requires quoting of the pattern within [[...]] since version 3.2 of Bash (here the / character is the pattern).
if [[ "$workspaceDir" =~ "/" ]]; then
	echo "Error: bad name of workspace directory. It should not contain the slash character (\"/\"). Please choose a valid name."
	echo "Name of workspace directory: $workspaceDir"
	echo ""
	exit 1
fi

# Checks if the name of the workspace directory does not contain a blank character (" ").
if [[ "$workspaceDir" =~ " " ]]; then
	echo "Error: bad name of workspace directory. It should not contain the blank character (\" \"). Please choose a valid name."
	echo "Name of workspace directory: $workspaceDir"
	echo ""
	exit 1
fi

# Checks if the name of the recompile directory does not contain a slash character ("/").
if [[ "$recompileDir" =~ "/" ]]; then
	echo "Error: bad name of recompile directory. It should not contain the slash character (\"/\"). Please choose a valid name."
	echo "Name of recompile directory: $recompileDir"
	echo ""
	exit 1
fi

# Checks if the name of the recompile directory does not contain a blank character (" ").
if [[ "$recompileDir" =~ " " ]]; then
	echo "Error: bad name of recompile directory. It should not contain the blank character (\" \"). Please choose a valid name."
	echo "Name of recompile directory: $recompileDir"
	echo ""
	exit 1
fi

# Checks if the name of the compilation errors file's prefix does not contain a slash character ("/").
if [[ "$compilationErrorFilePrefix" =~ "/" ]]; then
	echo "Error: bad name of compilation error file's prefix. It should not contain the slash character (\"/\"). Please choose a valid name."
	echo "Name of compilation error file prefix: $compilationErrorFilePrefix"
	echo ""
	exit 1
fi

# Checks if the script is run with the right number of arguments.
# $# is the number of arguments passed to the script.
if [ $# -ne 2 ]; then
	echo "Error: bad number of arguments."
	echo ""
	echo "Run the script using the following syntax:"
	echo "./$0 pathToJarFile pathToJarDependenciesDirectory" # $0: name of the script
	echo "or (equivalently):"
	echo "bash $0 pathToJarFile pathToJarDependenciesDirectory"
	echo "where: "
	echo "   - pathToJarFile is the path (absolute or relative) to the .jar file whose dependencies are checked;"
	echo "   - pathToJarDependenciesDirectory is the path (absolute or relative) to the directory containing potential .jar dependencies."
	echo ""
	exit 1
fi

# Sets paths to .jar file and .jar dependencies directory.
pathToJarFile="$1"
pathToJarDependenciesDir="$2"

# Checks if file passed as argument to the script exists.
if [ ! -f "$pathToJarFile" ]; then
	echo "Error: $pathToJarFile is not an existing file."
	echo ""
	exit 1
fi

# Extracts file name (with extension) from file path.
jarFileName=$(basename "$pathToJarFile")

# Checks if file passed as argument to the script is a .jar file.
# Extracts extension from file name using the shell parameter expansion.
# The ${var##pattern} command removes from $var the longest part of $pattern that matches the front end of $var.
jarFileExtension="${jarFileName##*.}"
if [ "$jarFileExtension" != "jar" ]; then	
	echo "Error: $pathToJarFile is not a file with a .jar extension."
	echo ""
	exit 1
fi

# Checks if .jar dependencies directory exists.
if [ ! -d "$pathToJarDependenciesDir" ]; then
	echo "Error: $pathToJarDependenciesDir is not en existing directory."
	echo ""
	exit 1
fi

# Deletes the "/" character at the end of the pathToJarDependenciesDir path if it is present. This path is passed as 
# argument to the the cleanPath() function.
pathToJarDependenciesDir=$(cleanPath "$pathToJarDependenciesDir")

#####################################
# 2) Creates directories and files. #
#####################################

# Sets the names of temporary directories (deleted at the end of the script).
unjarDir="unjarFiles"				# directory where the .jar file is unjared
requiredJarDir="requiredJarFiles"	# directory where to store required .jar dependencies
testedJarDir="testedJarFiles"		# directory where to store tested .jar dependencies

# Creates a workspace for checking dependencies. Deletes before every directory and file
# recursively in this workspace if it already exists.
if [ -d "$workspaceDir" ]; then
	rm -r "$workspaceDir"
fi
mkdir "$workspaceDir"

# Creates an empty "unjarDir" directory.
mkdir "$workspaceDir"/"$unjarDir"

# Creates a "recompileDir" directory with empty "src" and "class" subdirectories.
mkdir "$workspaceDir"/"$recompileDir"
mkdir "$workspaceDir"/"$recompileDir"/src
mkdir "$workspaceDir"/"$recompileDir"/class

# Creates an empty "requiredJarDir" directory and fills in initially with the 
# contents of .jar dependencies directory (recursive copy with -r option).
# If the .jar dependencies directory is empty, the cp command will send an error that
# is redirected in /dev/null (using 2> /dev/null).
mkdir "$workspaceDir"/"$requiredJarDir"
cp -r "$pathToJarDependenciesDir"/* "$workspaceDir"/"$requiredJarDir" 2> /dev/null

# Creates an empty "testedJarDir" directory aimed at containing tested .jar dependencies.
mkdir "$workspaceDir"/"$testedJarDir"

# Creates an empty "uselessJarList.txt" file aimed at containing the list of useless .jar dependencies.
touch "$workspaceDir"/uselessJarList.txt

#####################################################################################################
# 3) Deletes the .jar file (the one whose dependencies are checked) from the list of potential .jar #
#	 dependencies if it is present.																    #
#####################################################################################################

# Finds all occurences of .jar file (normally zero or a single).
find "$workspaceDir"/"$requiredJarDir" -type f -name "$jarFileName" > "$workspaceDir"/foundJarFiles.txt

# Deletes all found occurences of .jar file.
numberOfFoundJarFiles=$(cat "$workspaceDir"/foundJarFiles.txt | wc -l) # number of lines in "foundJarFiles.txt"
for i in $(seq 1 $numberOfFoundJarFiles); do
	pathToFoundJarFile=$(sed -n "$i"p "$workspaceDir"/foundJarFiles.txt)
	rm "$pathToFoundJarFile"
done

# Deletes temporary file.
rm "$workspaceDir"/foundJarFiles.txt

################################################################################
# 4) Processes the extraction of the .jar file whose dependencies are checked. #
################################################################################

# The unzip command can be used because a .jar file is a zipped file. 
echo "Extracting $pathToJarFile..."
unzip "$pathToJarFile" -d "$workspaceDir"/"$unjarDir"
echo "Extraction done."

################################################################################################
# 5) Writes in "sources.txt" file the list of all .java files contained in the extracted .jar: #
#    this is the list of sources files to be recompiled.                                       #
################################################################################################

# Copies in the "recompileDir/src" directory all the files contained 
# in the "unjarDir" directory. The arborescence is conserved (-r option).
cp -r "$workspaceDir"/"$unjarDir"/* "$workspaceDir"/"$recompileDir"/src

# Deletes all .class files contained in the "recompileDir/src" directory.
find "$workspaceDir"/"$recompileDir"/src -type f -name "*.class" -delete

# Writes the name of all .java files from "recompileDir/src" directory in a created "sources.txt" file.
find "$workspaceDir"/"$recompileDir"/src -type f -name "*.java" > "$workspaceDir"/sources.txt

############################################################################################################
# 6) Deletes all the .java files inside the .jar dependencies in order to avoid an automatic recompilation #
#    of these .java files causing some troubles during compilation process.                                #
#    Usefull because jeeb-util.jar, jeeb-sketch.jar, jeeb-formats.jar and capsis-kernel.jar                #
#    contain .java files.            																	   #
############################################################################################################

echo ""
echo -n "Deleting existing .java files in .jar dependencies..."

# Writes the list of all .jar dependencies in a temporary "jarDependenciesListTemp.txt" file deleted in step 7.
# This file is temporary because the paths it contains begin with "workspaceDir/requiredJarFiles/". This 
# prefix will be deleted in step 7.
# Note: the "find" command does not send an error if no .jar file is found, so that the error redirection
# in /dev/null (using 2> /dev/null) is not needed.
find "$workspaceDir"/"$requiredJarDir" -type f -name "*.jar" > "$workspaceDir"/jarDependenciesListTemp.txt

# Calculates the number of .jar dependencies. It is the number of lines of "jarDependenciesListTemp.txt".
numberOfJarDependencies=$(cat "$workspaceDir"/jarDependenciesListTemp.txt | wc -l)

# Creates a temporary file aimed at containing results of .java files deleting operation.
touch "$workspaceDir"/deletingSourceFilesResult.txt

# Loop on all .jar dependencies files.
for i in $(seq 1 $numberOfJarDependencies); do
	# Path to .jar file.
	pathToJarDependency=$(sed -n "$i"p "$workspaceDir"/jarDependenciesListTemp.txt)

	# Deletes (-d option) all .java files of pathToJarDependency file. The zip command can be used because 
	# a .jar file is a zipped file. 
	# Information (when deleting) and errors (when no .java file is found) are written in "deletingSourceFilesResult.txt".
	zip -d "$pathToJarDependency" "*.java" > "$workspaceDir"/deletingSourceFilesResult.txt
done
echo "Done."

# Deletes temporary file.
rm "$workspaceDir"/deletingSourceFilesResult.txt

######################################################################################
# 7) Writes the list of all .jar dependencies in "jarDependenciesList.txt".          #
#    In this file the "workspaceDir/requiredJarFiles/" prefix is deleted from paths. #
######################################################################################

# Creates an empty "jarDependenciesList.txt" file.
touch "$workspaceDir"/jarDependenciesList.txt

# Defines prefix to be deleted from .jar paths.
pathPrefix="$workspaceDir/$requiredJarDir/"

# Loop on all .jar dependencies files.
# "jarDependenciesListTemp.txt" has been created at step 6.
# numberOfJarDependencies has been calculated at step 6.
for i in $(seq 1 $numberOfJarDependencies); do

	# Path to .jar file (with prefix to be deleted).
	fullPathToJarDependency=$(sed -n "$i"p "$workspaceDir"/jarDependenciesListTemp.txt)

	# Deletes prefix in the path to .jar file using the shell parameter expansion.
	# The ${var#pattern} command removes from $var the shortest part of $pattern that matches the front end of $var.
	shortPathToJarDependency=${fullPathToJarDependency#"$pathPrefix"}

	# Writes short path to .jar at the end of "jarDependenciesList.txt".
	echo "$shortPathToJarDependency" >> "$workspaceDir"/jarDependenciesList.txt
done

# Deletes temporary file.
rm "$workspaceDir"/jarDependenciesListTemp.txt

###########################################################################
# 8) Creates the classpath variable (for further compilation with javac). #
###########################################################################

echo ""
echo -n "Building classpath variable for compiling with javac..."

# Writes in "listOfDirectories.txt" the list of all subdirectories contained
# in "requiredJarDir" directory (including the "requiredJarDir" directory itself).
find "$workspaceDir"/"$requiredJarDir" -type d > "$workspaceDir"/listOfDirectories.txt

# Creates an empty file aimed at containing the list of subdirectories contained
# in "requiredJarDir" directory with at least one .jar file (including the "requiredJarDir" 
# directory itself if it has at least one .jar file).
touch "$workspaceDir"/listOfDirectoriesWithJarFiles.txt

# Calculates the number of directories listed in "listOfDirectories.txt" file. This number is 
# greater or equal to 1 because "listOfDirectories.txt" contains at least the "requiredJarDir" directory.
numberOfDirectories=$(cat "$workspaceDir"/listOfDirectories.txt | wc -l)

# Loop on all directories listed in "listOfDirectories.txt". numberOfDirectories is greater or equal to 1 (see above).
for i in $(seq 1 $numberOfDirectories); do

	# Extracts the i-th line of "listOfDirectories.txt" file.
	#	-n : avoids printing of processed lines. Only lines specified with p command (p for print) will be printed.
	testedDirectory=$(sed -n "$i"p "$workspaceDir"/listOfDirectories.txt)

	# Calculates the number of .jar files at the root of "testedDirectory".
	# Do not use the following command: find "$testedDirectory" -type f -name "*.jar" 
	# because it will find the number of .jar files in the "testedDirectory" and its subdirectories. 
	# Indeed, "testedDirectory" takes successively the values "dir1", "dir1/dir2", "dir1/dir3" for a directory dir1 
	# containing two subdirectories dir2 and dir3.
	# The errors (when .jar file do not exist in "testedDirectory") are redirected in /dev/null (using 2> /dev/null).
	numberOfJarFiles=$(ls "$testedDirectory"/*.jar 2> /dev/null | wc -l)

	# Appends path of "testedDirectory" to the "listOfDirectoriesWithJarFiles.txt" file.
	if [ $numberOfJarFiles -gt 0 ]; then
		echo "$testedDirectory" >> "$workspaceDir"/listOfDirectoriesWithJarFiles.txt
	fi
done

# Builds classpath variable.
# Be careful: do not use "directoryName/*.jar" but "directoryName/*".
numberOfDirectoriesWithJarFiles=$(cat "$workspaceDir"/listOfDirectoriesWithJarFiles.txt | wc -l)
classpathVariable="" # Initially empty. Stays empty if there is no .jar in dependencies directory.
for i in $(seq 1 $numberOfDirectoriesWithJarFiles); do
	directoryWithJarFiles=$(sed -n "$i"p "$workspaceDir"/listOfDirectoriesWithJarFiles.txt)
	if [ $i -eq 1 ]; then
		classpathVariable="$directoryWithJarFiles/*"
	else
		classpathVariable="$classpathVariable:$directoryWithJarFiles/*"
	fi
done

echo "Done."
echo "Built classpath: $classpathVariable"

# Deletes temporary files.
rm "$workspaceDir"/listOfDirectories.txt
rm "$workspaceDir"/listOfDirectoriesWithJarFiles.txt

################################################################################################################
# 9) Recompilations: identifies from the .jar dependencies which ones are required and which ones are useless. #
################################################################################################################

echo ""
echo "Beginning identification of required and useless .jar dependencies of directory: $pathToJarDependenciesDir"
echo "for recompiling file: $pathToJarFile"
echo "Number of .jar dependencies to be tested: $numberOfJarDependencies"
if [ $numberOfJarDependencies -eq 0 ]; then
	echo "Only one compilation is needed."
fi

# Initial compilation: needs all .jar files.
echo ""
echo "##### Initial compilation #####"
echo "Compilation with all .jar dependencies of $pathToJarDependenciesDir"

# Compiles the .java files of the "recompileDir/src" directory listed in "sources.txt". The compile() function 
# is called with 3 parameters:
# - 1st parameter: the file containing the list of .java sources files;
# - 2nd parameter: the classpath;
# - 3rd parameter: the directory where to store the generated .class files.
# The stderr stream (that is compilation errors) is redirect in a compilation errors file.
# Note: compilation errors file is created even if nothing is written inside (i.e. no compilation error).
compile "$workspaceDir"/sources.txt "$classpathVariable" "$workspaceDir"/"$recompileDir"/class 2> "$workspaceDir"/"$compilationErrorFilePrefix"_initial_withAllDependencies.txt

# Calculates the number of compilation errors: calls the calculateNumberOfCompilationErrors() function
# with the compilation error file as parameter.
# Sets the value of 3 variables:
# 	- errorsNb: the number of compilation errors obtained when removing each .jar file dependency;
# 	- initialErrorsNb: the number of compilation errors obtained during initial compilation (with all .jar dependencies);
#	- minimumErrorsNb: the minimal number of compilation errors obtained during all compilations (including initial 
#     compilation and compilations without each .jar dependency).
errorsNb=$(calculateNumberOfCompilationErrors "$workspaceDir"/"$compilationErrorFilePrefix"_initial_withAllDependencies.txt)

# Sets values of minimum and initial compilation errors numbers.
minimumErrorsNb=$errorsNb
initialErrorsNb=$errorsNb

echo "Number of compilation error(s): $minimumErrorsNb (minimum) / $initialErrorsNb (initial)"

# Loop on all .jar dependencies files: one compilation is made for each removed .jar dependency.
for i in $(seq 1 $numberOfJarDependencies); do

	# Deletes the content of the "recompile/class" directory if it is non-empty. It contains generated .class files 
	# if compilation succeeds or remains empty if compilation fails.
	listOfCompiledFiles=$(ls "$workspaceDir"/"$recompileDir"/class)
	if [ ${#listOfCompiledFiles} -gt 0 ]; then # ${#listOfCompiledFiles} is the size of the listOfCompiledFiles variable
		rm -r "$workspaceDir"/"$recompileDir"/class/*
	fi

	# Extracts the i-th line of "jarDependenciesList.txt". This file contains paths to .jar dependencies relatively to 
	# the "workspaceDir/requiredJarDir/" directory.
	#	-n : avoids printing of processed lines. Only lines specified with p command (p for print) will be printed.
	relativePathToTestedJarFile=$(sed -n "$i"p "$workspaceDir"/jarDependenciesList.txt)

	echo ""
	echo "##### Step: $i/$numberOfJarDependencies #####"
	echo "Compilation without .jar file: $pathToJarDependenciesDir/$relativePathToTestedJarFile"

	# Tested .jar file is copied from "requiredJarDir" directory in "testedJarDir" directory. The --parents option 
	# is used in order to keep arborescence when copying: it is usefull in the case of .jar files from "requiredJar" 
	# having the same name but being in different directories. With this option, no overwrite is possible when 
	# copying from "requiredJarDir" to "testedJarDir".
	# Then tested .jar file is deleted from "requiredJarDir" directory in order to check if it is required or not.
	cd "$workspaceDir"/"$requiredJarDir"
	cp --parents "$relativePathToTestedJarFile" ../"$testedJarDir"
	cd ../..
	rm "$workspaceDir"/"$requiredJarDir"/"$relativePathToTestedJarFile"

	# The relative path of the tested .jar file can contain some slashes ("/") if .jar is in a subdirectory of the 
	# dependencies directory (pathToJarDependenciesDir directory). This situation causes troubles because this 
	# relative path is used below to suffix the name of the corresponding compilation error file. This file is then 
	# read by the calculateNumberOfCompilationErrors function. If the name of this file contains some slashes ("/"), 
	# then the function will interpret this name as the name of a file in a hierarchy of directories (separated by 
	# slashes ("/")) instead of the name of a file with slashes ("/").
	# Therefore, when creating the compilation error file, the relative path of the tested .jar file is modified 
	# using the bash parameter substitution: "/" are replaced by "%".
	# ${var/Pattern/Replacement} --> First match of Pattern, within var replaced with Replacement. If Replacement is omitted, 
	#								 then the first match of Pattern is replaced by nothing, that is, deleted.
	# ${var//Pattern/Replacement} --> Global replacement. All matches of Pattern, within var replaced with Replacement.
	# 								  As above, if Replacement is omitted, then all occurrences of Pattern are replaced by nothing, 
	#								  that is, deleted.
	modifiedPath=${relativePathToTestedJarFile////%}

	# Compiles the .java files of the "recompileDir/src" directory listed in "sources.txt" and writes compilation errors in 
	# a file suffixed by the name of the tested .jar dependency.
	# Note: compilation errors file is created even if nothing is written inside (i.e. no compilation error).
	compile "$workspaceDir"/sources.txt "$classpathVariable" "$workspaceDir"/"$recompileDir"/class 2> "$workspaceDir"/"$compilationErrorFilePrefix"_without_"$modifiedPath".txt

	# Calculates the number of compilation errors. 
	errorsNb=$(calculateNumberOfCompilationErrors "$workspaceDir"/"$compilationErrorFilePrefix"_without_"$modifiedPath".txt)

	echo "Number of compilation error(s): $errorsNb (current) / $minimumErrorsNb (minimum) / $initialErrorsNb (initial)"

	# Identifies if the tested .jar file is required or useless.
	if [ $errorsNb -gt $minimumErrorsNb ]; then
		# Tested .jar file is required.
		echo "--> Status of tested .jar: REQUIRED"

		# Copies tested .jar file in "requiredJarDir" directory.
		cp "$workspaceDir"/"$testedJarDir"/"$relativePathToTestedJarFile" "$workspaceDir"/"$requiredJarDir"/"$relativePathToTestedJarFile"
	else
		# Tested .jar file is useless.
		echo "--> Status of tested .jar: USELESS"

		# The name of the tested .jar file is written in the list of useless .jar files.
		echo "$pathToJarDependenciesDir"/"$relativePathToTestedJarFile" >> "$workspaceDir"/uselessJarList.txt
		
		# Deletes compilation errors file.
		rm "$workspaceDir"/"$compilationErrorFilePrefix"_without_"$modifiedPath".txt
	
		# Updates the minimum number of compilation errors.
		minimumErrorsNb=$errorsNb
	fi

done

# Writes required .jar dependencies in "requiredJarList.txt" file.
touch "$workspaceDir"/requiredJarList.txt
touch "$workspaceDir"/requiredJarListTemp.txt
find "$workspaceDir"/"$requiredJarDir" -type f -name "*.jar" > "$workspaceDir"/requiredJarListTemp.txt
# Calculates the number of required .jar dependencies. It is the number of lines of "requiredJarListTemp.txt"
numberOfRequiredJar=$(cat "$workspaceDir"/requiredJarListTemp.txt | wc -l)
# Loop on all required .jar files
for i in $(seq 1 $numberOfRequiredJar); do
	# Path to .jar (with prefix to be deleted).
	fullPathToJarDependency=$(sed -n "$i"p "$workspaceDir"/requiredJarListTemp.txt)

	# Deletes prefix in the path to .jar file using the shell parameter expansion.
	# The ${var#pattern} command removes from $var the shortest part of $pattern that matches the front end of $var.
	# The prefix to be deleted is: "workspaceDir/requiredJarDir/" (see step 7) above).
	shortPathToJarDependency=${fullPathToJarDependency#"$pathPrefix"}

	# Writes real path to .jar at the end of "requiredJarList.txt" file.
	echo "$pathToJarDependenciesDir"/"$shortPathToJarDependency" >> "$workspaceDir"/requiredJarList.txt
done
# Deletes temporary file.
rm "$workspaceDir"/requiredJarListTemp.txt

# Calculates the numbers of required and useless .jar dependencies.
# Number of required .jar dependencies: numberOfRequiredJar (has already been calculated, see above).
# Number of useless .jar dependencies: see below.
numberOfUselessJar=$(cat "$workspaceDir"/uselessJarList.txt | wc -l)

########################################################
# 10) Final compilation with only required .jar files. #
########################################################

# Final compilation is needed only if "pathToJarDependenciesDir" directory contains 
# at least one .jar file. An initial and unique compilation has already been done in 
# the case of a "pathToJarDependenciesDir" directory containing no .jar file.
if [ $numberOfJarDependencies -gt 0 ]; then
	echo ""
	echo "##### Final compilation #####"
	echo "Compilation with only required .jar files."

	# Compiles the .java files of the "recompileDir/src" directory listed in "sources.txt".
	# Note: compilation errors file is created even if nothing is written inside (i.e. no compilation error).
	compile "$workspaceDir"/sources.txt "$classpathVariable" "$workspaceDir"/"$recompileDir"/class 2> "$workspaceDir"/"$compilationErrorFilePrefix"_final_withOnlyRequiredDependencies.txt

	# Calculates the number of compilation errors.
	errorsNb=$(calculateNumberOfCompilationErrors "$workspaceDir"/"$compilationErrorFilePrefix"_final_withOnlyRequiredDependencies.txt)

	echo "Number of compilation error(s): $errorsNb (current) / $minimumErrorsNb (minimum) / $initialErrorsNb (initial)"
fi

############################
# 11) Prints final report. #
############################

echo ""
echo "##### Final report #####"
echo ""
echo "Number of .jar dependencies of $pathToJarDependenciesDir directory: $((numberOfJarDependencies+numberOfFoundJarFiles))"
if [ $numberOfFoundJarFiles -gt 0 ]; then
	echo "Number of tested .jar dependencies: $numberOfJarDependencies"
	echo "$jarFileName has been deleted from the list of tested .jar dependencies: $jarFileName is present $numberOfFoundJarFiles time(s) in $pathToJarDependenciesDir directory."
fi
echo ""
echo "For RECOMPILING $pathToJarFile :"
echo ""
echo "- Number of REQUIRED .jar dependencies of $pathToJarDependenciesDir directory: $numberOfRequiredJar"
if [ $numberOfRequiredJar -gt 0 ]; then
	echo "  List of REQUIRED .jar:"
	echo ""
	cat "$workspaceDir"/requiredJarList.txt
fi
echo ""
echo "- Number of remaining compiling error(s) with only REQUIRED .jar: $errorsNb"
if [ $errorsNb -gt 0 ]; then
	echo "  Compiling errors are listed in: $workspaceDir/$compilationErrorFilePrefix"_final_withOnlyRequiredDependencies.txt
fi
echo ""
echo "- Number of USELESS .jar dependencies of $pathToJarDependenciesDir directory: $numberOfUselessJar"
if [ $numberOfUselessJar -gt 0 ]; then
	echo "  List of USELESS .jar:"
	echo ""
	cat "$workspaceDir"/uselessJarList.txt
fi
echo ""

################################################
# 12) Deletes temporary files and directories. #
################################################

# Deletes temporary directories and their subdirectories.
rm -r "$workspaceDir"/"$unjarDir"
rm -r "$workspaceDir"/"$requiredJarDir"
rm -r "$workspaceDir"/"$testedJarDir"

# Deletes final compilation errors file only if it is empty.
if [ $errorsNb -eq 0 ]; then	
	rm "$workspaceDir"/"$compilationErrorFilePrefix"_final_withOnlyRequiredDependencies.txt
fi

# Deletes temporary files.
rm "$workspaceDir"/sources.txt
rm "$workspaceDir"/jarDependenciesList.txt
rm "$workspaceDir"/requiredJarList.txt
rm "$workspaceDir"/uselessJarList.txt

# Returns a successful code.
exit 0

