# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.
# browse.sh $1 $2     // open a browser for user notification
# Browser in $1. File $2 stays unchanged.

cp $2 synchro/tmp/file_v1.java

# Open editor on result for user inspection (browse only)
$1 synchro/tmp/file_v1.java  




