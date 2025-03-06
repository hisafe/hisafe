@rem compilation in capsis for windows

set spath=%~dp0

@rem javac -encoding ISO8859-15 -cp "%spath%.;%spath%\..\class\" -extdirs "%spath%\..\ext" -d "%spath%\..\class\" %1 %2 %3 %4 %5 %6 %7 %8 %9

javac -encoding ISO8859-15 -cp "%spath%.;%spath%..\class" -extdirs "%spath%..\ext" -d "%spath%..\class" %1 %2 %3 %4 %5 %6 %7 %8 %9