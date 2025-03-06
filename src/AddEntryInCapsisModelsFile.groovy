/**
  * This class owns a public (default value in Groovy) run() method that allows to add a module to 
  * the list of modules in the capsis4/etc/capsis.models file. More precisely, the packagename=true 
  * entry is added at the top position of this list, where packagename is the name of the package to 
  * which the module belongs (capsis4/src/packagename directory).
  *
  * Author: Nicolas Beudez
  */
class AddEntryInCapsisModelsFile {

	/**
	  * The path to the capsis.models file.
	  */
	final String CAPSIS_MODELS_FILE_PATH = "etc" + File.separator + "capsis.models"

	/**
	  * The name of the package to which the module to add belongs.
	  */
	String packagename

	/**
	  *	Adds a module to the list of existing modules in the capsis.models file. 
	  */
	public void run() {

		// Checks if the module with name packagename already exists in capsis.models file.
		List<String> linesOfCapsisModelsFile = getLines(CAPSIS_MODELS_FILE_PATH)

		// Adds an entry for the new module if it does not exist yet.
		boolean packageFound = isPackageFound(packagename, linesOfCapsisModelsFile)
		if (!packageFound) {
			println "Writing entry for module ${packagename} in ${CAPSIS_MODELS_FILE_PATH} file"
			String newLine = packagename + '=true'
			addLineAtTopFile(newLine, linesOfCapsisModelsFile, CAPSIS_MODELS_FILE_PATH)
		}
	}

	/**
  	  *	Returns a list containing all the lines of the file given as parameter.
  	  */
	private List<String> getLines(String filePath) {

		File file = new File(filePath)
		List<String> lines = file.readLines()

		return lines 
	}

	/**
	  *	Returns true if the package with name packagename exists in the linesOfFile list, false otherwise.
	  *	This list is supposed to contain:
	  *	- entries corresponding to commentaries lines: they begin with the # sign;
	  *	- entries corresponding to blank lines;
	  *	- entries corresponding to packages lines of the form: packagename=true or packagename=false.  
	  */
	private boolean isPackageFound(String packagename, List<String> linesOfFile) {
	
		boolean packageFound = false
		String packageNameInList
	
		for (int i=0 ; i<linesOfFile.size() ; ++i) {
			if ( (linesOfFile[i].trim().length() > 0) && (linesOfFile[i].charAt(0) != "#") ) {						
				int equalCharacterIndex = linesOfFile[i].indexOf('=')
				if ( equalCharacterIndex != -1 ) {
					packageNameInList = linesOfFile[i].substring(0, equalCharacterIndex).trim()
				}
				if ( packagename.equals(packageNameInList) ) {
					packageFound = true
					break
				}
			}
		}
	
		return packageFound
	}

	/**
	  *	Adds a new line above the first non-commented or empty line of the file given as parameter.
	  *	Lines of file have already been stored in the linesOfFile list.
	  */
	private void addLineAtTopFile(String newLine, ArrayList<String> linesOfFile, String filePath) {

		// Defines an empty list aimed at containing all the lines, including the new line to be added.
		List<String> finalLinesOfFile = new ArrayList<String>()
	
		// Fills in the finalLinesOfFile list. The new line is inserted in the list above the first item 
		// which is different from a commented line (beginning with #) or an empty line.
		boolean newLineAdded = false
		for (int i=0 ; i<linesOfFile.size() ; ++i) {
			if (!newLineAdded) {
				if ( (linesOfFile[i].trim().length() > 0) && (linesOfFile[i].charAt(0) != "#") ) {
					finalLinesOfFile.add(newLine)
					newLineAdded = true
				}
				finalLinesOfFile.add(linesOfFile[i])
			} else {
				finalLinesOfFile.add(linesOfFile[i])
			}
		}
	
		// Replaces the content of the capsis.models file by the one of the finalLinesOfFile list.
		File file = new File(filePath)
		file.text = '' // clears the file
		for (int i=0 ; i<finalLinesOfFile.size() ; ++i) {
			file.append(finalLinesOfFile[i] + '\n')
		}

	}

}
