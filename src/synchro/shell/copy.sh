# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.

# if no backup, make one
if [ ! -e $2BAK ]; then

	echo Making backup $2BAK
	cp $2 $2BAK

fi


# copy file1 on file2, without requestiong confirm for overwrite
cp -rf $1 $2


