# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.
# editdiff.sh $1 $2 $3     // makes diff between $2 and $3 and open an editor for user action
# Editor in $1. File $2 stays unchanged. We work on $3 (overwritten!!! a backup is made)

# if no backup, make one
if [ ! -e $3BAK ]; then

	echo Making backup $3BAK
	cp $3 $3BAK

fi

# Make diff with "synchro style" in $3 (overwritten -> real name -> can be compiled)
diff --old-line-format='d-%L' --new-line-format='d+%L' -abB $2 $3BAK > synchro/tmp/diff.java

# Open editor on result for user action
cp synchro/tmp/diff.java $3
$1 $3 

# Caller can ask for confirmation. yes : do nothing. No : restore.sh $3



