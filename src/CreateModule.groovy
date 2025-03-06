import java.text.SimpleDateFormat
import java.util.Locale

// Create module script.
// Copy template module to a new directory.

/**
  * Copies a directory. Ignores .svn and .class files.
  */
def copy_dir(directory, srcDir, srcPrefix, destDir, destPrefix) {

  dir = new File(directory + "/" + srcDir)

  dir.eachFileRecurse{ 

    if(it.path.contains(".svn")) { return }
    if(it.path =~ /\.class$/) { return }

    newfile = it.path
    newfile = newfile.replaceFirst(srcDir, destDir)
    newfile = newfile.replaceFirst(srcPrefix, destPrefix)
    
    newfile = new File(newfile)
    
    if (it.isDirectory()) { 
      newfile.mkdirs()
       
    } else {

      new AntBuilder().copy( file: it.canonicalPath, tofile: newfile.canonicalPath )

      updateContents(newfile, srcDir, destDir)
      updateContents(newfile, srcPrefix, destPrefix)
      updateContents(newfile, "__PACKAGENAME__", packagename)
      updateContents(newfile, "__MODULENAME__", modulename)
      updateContents(newfile, "__AUTHOR__", author)
      updateContents(newfile, "__INSTITUTE__", institute)
      updateContents(newfile, "__DATE__", date)
      updateContents(newfile, "__PREFIX__", destPrefix)
      
    }

  }

}

/**
  * Replaces the contents of a file.
  */
def updateContents(file, oldString, newString){
  
  text = file.getText("ISO-8859-1")
  text = text.replaceAll(oldString, newString)
  file.write(text, "ISO-8859-1")

}

/**
  * Checks the script parameters.
  */
def checkParams (args) {
	
	rc = 0
	
	// We need 7 parameters
	if (args.length < 7) {rc = 1}
	
	// Get the main variables
	sourcedir = args[0]
	packagename = args[1]
	prefix = args[2]
	source = args[3]
	sourceprefix = args[4]
	author = args[5]
	institute = args[6]
	
	// Check the main variables
	if (sourcedir.startsWith ("\$")) {
		println "  Error: Missing sourcedir"
		rc = 1
	}
	if (packagename.startsWith ("\$")) {
		println "  Error: Missing packagename"
		rc = 1
	}
	if (prefix.startsWith ("\$")) {
		println "  Error: Missing prefix"
		rc = 1
	}
	if (source.startsWith ("\$")) {
		println "  Error: Missing source"
		rc = 1
	}
	if (sourceprefix.startsWith ("\$")) {
		println "  Error: Missing sourceprefix"
		rc = 1
	}
	if (author.startsWith ("\$")) {
		println "  Error: Missing author"
		rc = 1
	}
	if (institute.startsWith ("\$")) {
		println "  Error: Missing institute"
		rc = 1
	}
	
	if (packagename.toLowerCase() != packagename) {
		println "  Error: packagename must contain only lower case letters"
		rc = 1
	}
	if (prefix.substring(0,1).toUpperCase() != prefix.substring(0,1)) {
		println "  Error: prefix must start with an upper case letter"
		rc = 1
	}
	
	// If trouble, print usage and abort
	if (rc != 0) {
		println "usage: (sh) ant create-module -Dname=packagename -Dprefix=Prefix -Dauthor=Author -Dinstitute=Institute"
		println "  note for author and institute: all '_' chars will be replaced by spaces"
		return rc
	}
	
	author = author.replace('_',' ')
	institute = institute.replace('_',' ')
	
	// packagename = laricio -> modulename = Laricio
	modulename = packagename.substring(0,1).toUpperCase() + packagename.substring (1)
	
	println "  packagename " + packagename
	println "  modulename  " + modulename
	println "  prefix      " + prefix
	println "  author      " + author
	println "  institute   " + institute
	
	return rc  // ok
}


/**
  * Main
  */

println "Creates module..."

// Checks the params, abort if trouble.
r = checkParams (args)
if (r != 0) return r

// Sets the date variable.
//def df = DateFormat.getDateInstance(DateFormat.MEDIUM)
def df = new SimpleDateFormat ("MMMM yyyy", Locale.ENGLISH)
date = df.format(new Date()).toString()

// Adds an entry for the new module in etc/capsis.models file.
def addEntry = new AddEntryInCapsisModelsFile(packagename: packagename)
addEntry.run()

// Deletes src/packagename and data/packagename directories if they already exist.
String srcPackagenameDirectoryPath = "src" + File.separator + packagename
String dataPackagenameDirectoryPath = "data" + File.separator + packagename
File srcPackagenameDirectory = new File(srcPackagenameDirectoryPath)
File dataPackagenameDirectory = new File(dataPackagenameDirectoryPath)

if ( srcPackagenameDirectory.exists() ) {
	println "  Deleting ${srcPackagenameDirectory} directory: it already exists"
	srcPackagenameDirectory.deleteDir()
}
if ( dataPackagenameDirectory.exists() ) {
	println "  Deleting ${dataPackagenameDirectory} directory: it already exists"
	dataPackagenameDirectory.deleteDir()
}

// Creates the new module.
println "  Creating src" + File.separator + packagename + " directory..."
copy_dir(sourcedir, source, sourceprefix, packagename, prefix)
	
// Creates the data/packagename directory.
println "  Creating data" + File.separator + packagename + " directory..."
new File(dataPackagenameDirectoryPath).mkdir()

// Copies files from data/template directory to data/packagename directory.
String dataTemplateDirectoryPath = "data" + File.separator + "template" 
(new AntBuilder()).copy( todir: dataPackagenameDirectoryPath ) {
	fileset(dir: dataTemplateDirectoryPath)
}

// Rename files from data/packagename directory
(new File(dataPackagenameDirectoryPath)).eachFile() {
	String oldFileName = it.getName()
	String newFileName = oldFileName.replace("template", packagename)
	it.renameTo(new File(dataPackagenameDirectoryPath + File.separator + newFileName))
}
