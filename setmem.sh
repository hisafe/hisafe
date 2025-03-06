#! /bin/bash

# Max memory available for Capsis: optional.
# In Mega bytes.
# Writes the given memory value to the file named 'memory'.
# The 'memory' file is an optional file.
# This file will be checked if present when launching Capsis to 
# override the default memory values in capsis.sh.
# Will help set java -Xmx parameter.
# E.g. sh setmem.sh 2048
# E.g. sh setmem.sh

# Caution: if the given parameter is not a number of Mega bytes, this may prevent Capsis from running
# In case of trouble, 'sh setmem.sh' will reset (remove) the memory file

# Check if a parameter was given
if [ -z $1 ]

then
  # No parameter: remove the memory file -> default memory
  rm -f memory  # reset to default values
  echo Capsis memory reset to capsis.sh default value

else
  # A parameter was given: write the value (Mbytes) in the file named 'memory'
  echo $1 > memory
  echo Capsis will run with $1 mega bytes available
  #cat memory
  
fi




