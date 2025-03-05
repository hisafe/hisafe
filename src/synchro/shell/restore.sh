# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.

# restore file1, without requestiong confirm for overwrite

# if directory
if [ -d $1BAK ]; then
	mv $1BAK $1

# if file
else 
	cp -f $1BAK $1

	# delete BAK file now unuseful
	rm -f $1BAK

fi

