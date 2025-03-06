/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.util;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;

/**	A command in Jib : read from the jib file.
*	Contains a default program name and a list of concerned files.
*
*	@author F. de Coligny - january 2006
*/
public class ReplaceInFiles {

	private String fileName;				// a file or directory name
	private File targetFile;					// the corresponding file (regular file OR directory)
	private String from;						// the String to be replaced
	private String to;							// the String in replacement
	private boolean filterActivated;		// if option -f is used ex: -f.java
	private String filter;						// if -f option is used ex: .java
	private boolean includeSystemFiles;		// ex: -s (files beginning with ".")
	private boolean replaceRequested;		// if option -r is set, indeed do the replacement else count only
	private boolean verbose;				// if option -v is set
	private boolean concise;				// if option -c is set
	private boolean recursive;				// if option -R is set
	private Collection fileNames;
	
	// replacement test: MarieTherese
	
	public ReplaceInFiles (String[] a) throws Exception {
		try {
			for (int i = 0; i < a.length; i++) {
				String p = a[i];
				if (p.startsWith ("-r")) {
					replaceRequested = true;
				} else if (p.startsWith ("-f")) {
					filterActivated = true;
					filter = p.substring (2).trim ();
									//~ System.out.println ("filter=<"+filter+">");	// trace only
				} else if (p.startsWith ("-s")) {
					includeSystemFiles = true;
				} else if (p.startsWith ("-v")) {
					verbose = true;
				} else if (p.startsWith ("-c")) {
					concise = true;
				} else if (p.startsWith ("-R")) {
					recursive = true;
				} else if (from == null) {
					from = p;
					// fc - 6.10.2008
					if (from.startsWith ("_")) {
						from = from.replace ("_", " ");
						from = from.substring (1);
					}
				} else if (to == null) {
					to = p;
					// fc - 6.10.2008
					if (to.startsWith ("_")) {
						to = to.replace ("_", " ");
						to = to.substring (1);
					}
				} else if (fileName == null) {
					fileName = p;
				}
			}
			// something missing (-r is optional)
			if (fileName == null || from == null || to == null) {throw new Exception ("missing parameters, see usage");}
			
		} catch (Exception e) {
			usage ();
			throw e;
		}
	}
	
	private void usage () {
		System.out.println ("usage: replaceinfiles [-rfsv] <from> <to> dir|fileName");
		System.out.println ("  with <from> = the String to be replaced");
		System.out.println ("       <to>   = the String in replacement");
		System.out.println ("       Note: if <from> starts with a \"_\", then all these characters will");
		System.out.println ("             be replaced by spaces, idem for <to> (to change sevaral words)");
		System.out.println ("  options:");
		System.out.println ("      -h  show this usage note");
		System.out.println ("      -r  indeed do the replacement (otherwise, count only)");
		System.out.println ("      -f  use a file filter, ex: -f.java for java files");
		System.out.println ("      -s  include system files (name starting with a \".\")");
		System.out.println ("      -R  traverse recursively sub directories");
		System.out.println ("      -v  verbose, print additional messages");
		System.out.println ("      -c  concise, print messages only for changed files");
		System.out.println ("  Note: use replaceinfiles (windows) or ./replaceinfiles.sh (mac/linux)");
		System.out.println ("  Note: CVS/ directories are ignored");
		System.out.println ("  Note: first evaluate file list by omitting -r, then indeed replace by adding -r");
		System.out.println ("  Ex: replace Point3D by Point3d in all java files under the given dir recursively");
		System.out.println ("      ./replaceinfiles.sh -R -f.java Point3D Point3d ./capsis/util -r");
	}
	
	public void execute () {
		// fileName is either a file or a directory -> get all the file recursively in it
		fileNames = new ArrayList ();
		try {
			targetFile = new File (fileName);
			
			// It is possible to filter the list of returned files
			FilenameFilter someFilter = new FilenameFilter () {
				public boolean accept (File dir, String name) {
					if (!includeSystemFiles && name.startsWith (".")) {return false;}
					if (filterActivated && !name.endsWith (filter)) {return false;}
					return true;
				}
			};
			
			visitAllFiles (targetFile, someFilter);
			
		} catch (Exception e) {
			System.out.println ("File error, should be a file or directory, ignored: "+e+": "+fileName);
		}
		
		replaceInFiles (fileNames, from, to, replaceRequested);
	}
	
	// Process only files under dir - java developers almanac 1.4 - Patrick Chan
	public void visitAllFiles (File dir, FilenameFilter someFilter) {
		if (dir.isDirectory ()) {
			if (!recursive && !dir.equals (targetFile)) {return;}
			if (dir.getName () .equals ("CVS")) {return;}	// fc - 18.9.2008 - ignore CVS/ directories
			String[] children = dir.list ();
			for (int i=0; i<children.length; i++) {
				visitAllFiles (new File (dir, children[i]), someFilter);
			}
		} else {
			try {
				if (someFilter.accept (dir, dir.getName ())) {
						fileNames.add (dir.getCanonicalPath ());}
			} catch (Exception e) {
				System.out.println ("File error, can not get canonical path, ignored: "+e+": "+dir);
			}
		}
	}
	
	// fc - 17.9.2008
	private void replaceInFiles (Collection<String> fileNames, 
			String from, String to, boolean replaceRequested) {
		if (fileNames == null || fileNames.isEmpty ()) {
			System.out.println ("No files to process, aborted");
			usage ();
			return;
		}
		if (from == null || from.length () == 0) {
			System.out.println ("Wrong <from> String, aborted");
			usage ();
			return;
		}
		if (to == null || to.length () == 0) {
			System.out.println ("Wrong <to> String, aborted");
			usage ();
			return;
		}
		
		String filterMsg = "";
		if (filterActivated) {filterMsg = " "+filter;}
		if (replaceRequested) {
			System.out.println ("replacing <"+from+"> by <"+to+"> in "+fileNames.size ()+filterMsg+" file(s)...");
		} else {
			System.out.println ("counting the replacements of <"+from+"> by <"+to+"> in "+fileNames.size ()+filterMsg+" file(s)...");
		}
		
		int count = 0;
		for (Iterator<String> i = fileNames.iterator (); i.hasNext ();) {
			String fileName = i.next ();

			count += (replaceInFile (fileName, from, to, replaceRequested));
		}
		if (replaceRequested) {
			System.out.println (""+count+" replacement(s) were done in "+fileNames.size ()+" file(s)");
		} else {
			System.out.println (""+count+" replacement(s) to be done in "+fileNames.size ()+" file(s), use -r option to indeed replace");
		}
		
	}
	
	// fc - 17.9.2008 - TESTING in static context.....
	/**	Replace in the given file each occurrence of the "from" String by the "to" String.
	*	Return the number of replacements.
	*	Do the replacements only if replaceRequested is true (else, count only). 
	*/
	private int replaceInFile (String fileName, 
			String from, String to, boolean replaceRequested) {
		int numberOfReplace = 0;
				
		//~ if (!Check.isFile (fileName)) {
			//~ System.out.println ("File not found, ignored: "+fileName);
			//~ return 0;
		//~ }
		
		try {
			File fin = new File (fileName);
			BufferedReader in = new BufferedReader (new FileReader (fin));

			//~ File tmp = File.createTempFile (fin.getName (), "replaced");
			Date now = new Date ();
			File tmp = new File (fileName+"-replaced");	// tmp file in the same directory

			BufferedWriter out = new BufferedWriter (new FileWriter (tmp.getCanonicalPath ()));

			String line;
			while ((line = in.readLine ()) != null) {
				
				do {
					int i = line.indexOf (from);
					if (i != -1) {	// found: do the replacement
						String before = line;	// fc - 6.10.2008
						int j = i + from.length ();
						String a = line.substring (0, i);
						a += to;
						a += line.substring (j);
						line = a;
						String after = line;	// fc - 6.10.2008
				if (verbose) {System.out.println (">>before: "+before+ "\n   after: "+after);}
						numberOfReplace++;
					}
				} while (line.indexOf (from) != -1);
				
				out.write (line);
				out.newLine ();
			}
			in.close ();
			out.close ();
			
			// if replace mode, copy the replaced file into the original
			String renameError = "";
			if (numberOfReplace == 0 || !replaceRequested) {
				tmp.delete ();
				
			} else {
				// copy tmp into original file (had problems with File.renameTo ())
				copy (fin, new File (fin.getAbsolutePath ()+"-backup"));
				copy (tmp, fin);
				tmp.delete ();
			}
			
			if (!concise || numberOfReplace != 0) {
				String verboseMsg = "";
				//~ if (verbose) {verboseMsg = " "+renameError+" (tmp="+tmp.getCanonicalPath ()+")";}
				
				System.out.println (fileName+
						(numberOfReplace!=0 ? " #replace="+numberOfReplace : "")
						+verboseMsg);
			}
			
		} catch (IOException e) {
			System.out.println ("File error, ignored: "+e+": "+fileName);
			//e.printStackTrace (System.out);
		}
		
		return numberOfReplace;
	}
	
	// returns true if copy ok
	private boolean copy (File from, File to) {
		try {
			BufferedReader in = new BufferedReader (new FileReader (from));
			BufferedWriter out = new BufferedWriter (new FileWriter (to));
			String line = null;
			while ((line = in.readLine ()) != null) {
				out.write (line);
				out.newLine ();
			}
			in.close ();
			out.close ();
			return true;
		} catch (Exception e) {
			return false;
		}
	}
	
	
	public static void main (String[] a) {
		try {
			ReplaceInFiles rif = new ReplaceInFiles (a);
			rif.execute ();
		} catch (Exception e) {}
	}
	
}