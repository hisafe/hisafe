# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.
# edit.sh $1 $2      // open an editor for user action
# Editor in $1. File $2 may change. A backup is made.

# if no backup, make one
if [ ! -e $2BAK ]; then

	echo Making backup $2BAK
	cp $2 $2BAK

fi

$1 $2 

# Possibly after : restore.sh $2



