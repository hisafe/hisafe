# Synchro - F. de Coligny - april 2003
# This script is called by the synchro application. Please do not alter.
# browsediff.sh $1 $2 $3      // makes diff between $2 and $3 and open a browser for user notification
# Browser in $1. Files $2 and $3 stay unchanged.

# Make diff with "synchro style" in diff.java (temporary file)
diff --old-line-format='d-%L' --new-line-format='d+%L' -abB $2 $3 > synchro/tmp/diff.java

# Open editor on result for user inspection (browse only)
$1 synchro/tmp/diff.java  




