# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.

# if no backup, make one
if [ ! -e $1BAK ]; then

	# if directory -> rename it
	if [ -d $1 ]; then
		mv $1 $1BAK
		
	# if file -> make backup
	else
		cp $1 $1BAK
	fi
fi

# if file -> delete file1, without requestiong confirm (backup was made)
if [ ! -d $1 ]; then
	rm -f $1
fi

