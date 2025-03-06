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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;

import jeeb.lib.util.Log;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.SetMap;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.app.CapsisExtensionManager;
import capsis.kernel.Options;
import capsis.kernel.IdCard;
import capsis.kernel.ModelManager;
import jeeb.lib.util.PathManager;

/**	Build an html documentation for all the available method providers in 
*	capsis.util.methodprovider.
*	@author F. de Coligny - august 2005
*/
public class MethodProviderDoc {

	// A simple file name filter for Method Providers
	static private class MPFilter implements FilenameFilter {
		public boolean accept (File dir, String name) {
			if (name.endsWith (".java")) {return true;}
			return false;
		}
	}
	
	private String outName;
	private BufferedWriter out;
	private Collection modelNames;
	
	private Map<String,TextSearcher> modulesMap;  // module name -> TextSearcher on its MethodProvider
	private Map<String,String> mpFileNames;       // module name -> MethodProvider file name
	
	private Map<String,TextSearcher> extensionsMap;  // extension name -> TextSearcher on its source code
	private Map<String,String> extensionFileNames;   // extension name -> extension file name
	
	private SetMap moduleCrossRefs;
	private SetMap extensionCrossRefs;
	
	private File[] methodProviderFiles;
	private Map sourceDocumenters;  // methodprovider file -> SourceDocumenter on this file
		
	
	/**	Constructor
	*/
	public MethodProviderDoc () throws Exception {
		
		// Build a map for modules : moduleName -> TextSearcher (moduleMethodProvider)
		StatusDispatcher.print (Translator.swap ("MethodProviderDoc.buildingMapsForModulesAndExtensions")+"... ");
		modulesMap = new HashMap ();
		mpFileNames = new HashMap ();
		buildModulesMap ();

		// Build a map for extensions : extensionName -> TextSearcher (extensionSourceCode)
		// for type = "dataextractor", "grouperdisplay", "filter", "intervener"...
		extensionsMap = new HashMap ();
		extensionFileNames = new HashMap ();
		for (String type : CapsisExtensionManager.getInstance ().getExtensionTypes ()) {
			buildExtensionsMap (type);
		}
		
		// Source directory
		// Sort files
		File[] mps = getMethodProviderFiles ();
		TreeSet ts = new TreeSet (Arrays.asList (mps));
		methodProviderFiles = (File[]) ts.toArray (new File[ts.size ()]);	// sorted
		
		// Loop on files to be processed
		sourceDocumenters = new HashMap ();
		moduleCrossRefs = new SetMap ();		// a map of sets
		extensionCrossRefs = new SetMap ();		// a map of sets
		
		ProgressDispatcher.setMinMax (0, methodProviderFiles.length);		
		
		for (int i = 0; i < methodProviderFiles.length; i++) {
			try {
				ProgressDispatcher.setValue (i+1);		
				StatusDispatcher.print (Translator.swap ("MethodProviderDoc.searchingCrossReferencesFor")+" "+methodProviderFiles[i].getName ()+"... ");
				
				getInformation (methodProviderFiles[i]);
				
				// CrossRefs TreeSet contains:
				// 1. methodProviderName -> module names
				// 2. methodProviderName -> extension names
				String mpFileName = methodProviderFiles[i].getName ();
				mpFileName = mpFileName.substring (0, mpFileName.indexOf ("."));	// discard ".java"
				createCrossRefs (moduleCrossRefs, mpFileName, modulesMap);		
				createCrossRefs (extensionCrossRefs, mpFileName, extensionsMap);		
		
			} catch (Exception e) {
				Log.println ("MethodProviderDoc (), process aborted for "+methodProviderFiles[i]);
			}
		}
		ProgressDispatcher.setValue (methodProviderFiles.length);		// last value	
		ProgressDispatcher.stop ();
	
	}

	/**	List of the method providers files
	*/
	public static File[] getMethodProviderFiles () {
		// source directory
		String dirName = PathManager.getDir("src")
				+File.separator
				+"capsis"
				+File.separator
				+"util"
				+File.separator
				+"methodprovider";
		File dir = new File (dirName);
		
		File[] mps = dir.listFiles (new MPFilter ());
		return mps;
	}
	
	
	/**	Build the html doc file
	*/
	public void writeHTMLdoc () throws Exception {
		
		ProgressDispatcher.setMinMax (0, methodProviderFiles.length);		
		
		// output file
		outName = PathManager.getDir("src")
				+File.separator
				+"capsis"
				+File.separator
				+"util"
				+File.separator
				+"methodprovider" 
				+File.separator
				+"methodprovider"
				+"_"+Locale.getDefault ().getLanguage ()
				+".html";
		out = new BufferedWriter (new FileWriter (outName));
		
		writeHeader ();
		
		for (int i = 0; i < methodProviderFiles.length; i++) {
			try {
				ProgressDispatcher.setValue (i+1);	
				StatusDispatcher.print (Translator.swap ("MethodProviderDoc.writingHtmlDocFor")+methodProviderFiles[i].getName ()+"... ");
				
				writeHTMLdoc ((File) methodProviderFiles[i]);
			} catch (Exception e) {
				Log.println ("MethodProviderDoc, process aborted for "+methodProviderFiles[i]);
			}
		}
		
		writeFooter ();
		
		ProgressDispatcher.setValue (methodProviderFiles.length);		// last value	
		ProgressDispatcher.stop ();
		
		out.flush ();
		out.close ();
	}

	
	/**	Build a search map from the idCards : module name -> TextSearcher on its MethodProvider
	*/
	private void buildModulesMap () {
		// get the idCards for all the detected modules
		Collection<String> pkgnames = ModelManager.getInstance ().getPackageNames();
				
		for(String pkgname : pkgnames){
			
			IdCard idCard;
			try {
				idCard = ModelManager.getInstance ().getIdCard(pkgname);
			} catch (Exception e) {
				Log.println(e.toString());
				continue;
			}
			if(idCard == null) continue;
			// module name and MethodProvider source name
			String moduleName = idCard.getModelPackageName ();
			String mpName = PathManager.getDir("src")
					+File.separator
					+moduleName
					+File.separator
					+"model"
					+File.separator
					+idCard.getModelPrefix ()
					+"MethodProvider.java";
			try {
				File mp = new File (mpName);
				TextSearcher ts = new TextSearcher (mp);
				modulesMap.put (moduleName, ts);
				mpFileNames.put (moduleName, mpName);
			} catch (Exception e) {
				Log.println (Log.WARNING, "MethodProviderDoc.buildModulesMap ()", 
						"MethodProvider error for "+moduleName+": "+e.getMessage ());
			}
		}
	}

	
	/**	Build a search map : extension name -> TextSearcher on its source code
	*/
	private void buildExtensionsMap (String extensionType) {	// ex: intervener, dataextractor...
		
		Collection<String> extensionClassNames = 
				CapsisExtensionManager.getInstance ().getExtensionClassNames (extensionType);
		if (extensionClassNames == null) {return;}  // happened
		
		for (String c : extensionClassNames) {
		
			try {
				
				System.out.println("MethodProviderDoc buildExtensionsMap extClassName = "+c);
				
//				File f = new File (getClass().getClassLoader().getResource(c).toURI ());
				
				String eName = PathManager.getDir("src")
						+File.separator
						+c.replace('.', File.separatorChar)
						+".java";
				
				System.out.println("MethodProviderDoc buildExtensionsMap eName = "+eName);
				
				File f = new File (eName);
				
				TextSearcher ts = new TextSearcher (f);
				
				String extensionAbsolutePath = f.getAbsolutePath ();
				
				String n = extensionAbsolutePath;
				n = n.substring (0, n.indexOf (".java"));
				String extensionName = n.substring (n.lastIndexOf (File.separator)+1);
				
				extensionsMap.put (extensionName, ts);
				extensionFileNames.put (extensionName, extensionAbsolutePath);
				Log.println ("extensionName="+extensionName+" ("+extensionAbsolutePath+")");
			} catch (Exception e) {
				Log.println (Log.ERROR, "MethodProviderDoc.buildExtensionsMap ()", "exception :", e);
			}
		}
	}

	
	/**	Get information for the given file : make a SourceDocumenter 
	 *	(className, classComment, classAuthor...)
	 */
	private void getInformation (File file) throws Exception {
				Log.println ("MethodProviderDoc.getInformation () for "+file.getName ()+"... ");
		try {
			SourceDocumenter sc = new SourceDocumenter (file).run ();
			sourceDocumenters.put (file, sc);
		} catch (Exception e) {
			Log.println (Log.ERROR, "MethodProviderDoc.getInformation ()", "exception in file "+file, e);
			throw e;
		}
				Log.println ("done");
	}

	
	/**	CrossRefs TreeSet contains:
	 * 	1. methodProviderName -> module names
	 * 	2. methodProviderName -> extension names
	 */
	private void createCrossRefs (SetMap crossRefs, String methodProviderName, 
			Map<String,TextSearcher> map) throws Exception {
					Log.println ("MethodProviderDoc.createCrossRefs () for "+methodProviderName+"... ");
		
		Iterator<String> i = map.keySet ().iterator ();
		Iterator<TextSearcher> j = map.values ().iterator ();
		
		while (i.hasNext () && j.hasNext ()) {
			String name = i.next ();
			TextSearcher ts = j.next ();
			if (ts.contains (methodProviderName)) {
				crossRefs.addObject (methodProviderName, name);
			}
		}
				Log.println ("done");
	}

	/**	Write html doc for the file
	 */
	private void writeHTMLdoc (File file) throws Exception {
		try {
			SourceDocumenter sc = (SourceDocumenter) sourceDocumenters.get (file);
			writeRow (file.getAbsolutePath (), sc);
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "MethodProviderDoc.writeHTMLdoc (File)", "exception in file "+file, e);
			throw e;
		}
	}


	/**	Page header
	 */
	private void writeHeader () throws Exception {
		out.write ("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		out.newLine ();
		out.write ("<html>");
		out.newLine ();
		out.write ("<head>");
		out.newLine ();
		
				// css link
		
		out.write ("<link href=\"../../../../style.css\" rel=\"stylesheet\" type=\"text/css\">");
		out.newLine ();

		out.write ("<meta http-equiv=\"content-type\" content=\"text/html; charset=ISO-8859-1\">");
		out.newLine ();
		out.write ("<title></title>");
		out.newLine ();
		out.write ("</head>");
		out.newLine ();
		out.write ("<body>");
		out.newLine ();
		
				// title table
		
		out.write ("<table cellpadding=\"0\" cellspacing=\"1\" border=\"0\" style=\"width: 100%;\">");
		out.write ("  <tbody>");
		out.write ("    <tr>");
		out.write ("      <td style=\"vertical-align: top; width: 70%;\">");
		out.write ("      <h1>"+Translator.swap("MethodProviderDoc.pageTitle")+"</h1>");
		out.write ("      </td>");
		out.write ("      <td style=\"vertical-align: middle; text-align: right;\">");
		
		String lang = Locale.getDefault ().getLanguage ();	// fr, en
		if (lang.equals ("fr")) {
			out.write ("      <div align=\"right\"><a href=\"http://capsis.cirad.fr/capsis/\">Aide Capsis<br>");
		} else {
			out.write ("      <div align=\"right\"><a href=\"http://capsis.cirad.fr/capsis/\">Capsis Help<br>");
		}
		
		out.write ("      </a></div>");
		out.write ("      </td>");
		out.write ("    </tr>");
		out.write ("  </tbody>");
		out.write ("</table>		");
		
		
				// explanation paragraph
		
		out.write ("<p>"+Translator.swap("MethodProviderDoc.explanationParagraph")+"</p>");
		out.newLine ();
		out.write ("<p>"+Translator.swap("MethodProviderDoc.whatModulesAndExtensionsMean")+"</p>");
		out.newLine ();
		out.write ("<p>"+Translator.swap("MethodProviderDoc.updateCommentsFromThisPageIfYouAreAuthor")+"</p>");
		out.newLine ();
		out.write ("<p>"+Translator.swap("MethodProviderDoc.whyListIsUpToDate")+" "+new Date ().toLocaleString ()+"</p>");
		out.newLine ();
		
				// begin of main table
		
		out.write ("<table cellpadding=\"1\" cellspacing=\"1\" border=\"0\" style=\"text-align: left; width: 100%;\">");
		out.newLine ();
		out.write ("<tbody>");
		out.newLine ();
		
	}

	/**	One row for each source file
	 */
	private void writeRow (String fileName, SourceDocumenter sc) throws Exception {
		out.write ("<tr>");
		out.newLine ();
		
		// Column 1
		out.write ("<a href=\""+fileName+"\">"+sc.getClassName ()+"</a>");
		out.newLine ();
		
		// Column 2
		out.write ("<table cellpadding=\"1\" cellspacing=\"1\" border=\"0\" style=\"text-align: left; width: 100%;\">");
		out.newLine ();
		out.write ("<tbody>");
		out.newLine ();
		out.write ("<tr>");
		out.newLine ();
			out.write ("<td style=\"vertical-align: top; width: 20px;\">");
			out.write ("</td>");
			out.newLine ();
		out.write ("<td>");
		out.write ("<b>"+sc.getClassComment ()+"</b>");
		out.write ("<br>");
		out.write ("(");
		out.write (""+sc.getClassAuthor ());
		out.write (")<br>");
		
				// modules using this mp
		Collection moduleNames = moduleCrossRefs.getObjects (sc.getClassName ());
		if (moduleNames != null && !moduleNames.isEmpty ()) {
			moduleNames = new TreeSet (moduleNames);	// sort
			out.write (Translator.swap("MethodProviderDoc.modules")+" : ");
			for (Iterator i = moduleNames.iterator (); i.hasNext ();) {
				String name = (String) i.next ();
				
				String mpFileName = mpFileNames.get (name);
				if (mpFileName != null) {
					out.write ("<a href=\""+mpFileName+"\">");	// alink on MP file
				}
				
				out.write (name);	// a module using this mp
				
				if (mpFileName != null) {out.write ("</a>");}
				
				out.write (" ");	// a space between module names
			}	
			
			out.write ("<br>");
			out.newLine ();
		}
		
				// extensions using this mp
		Collection extensionNames = extensionCrossRefs.getObjects (sc.getClassName ());
		if (extensionNames != null && !extensionNames.isEmpty ()) {
			extensionNames = new TreeSet (extensionNames);	// sort
			out.write (Translator.swap("MethodProviderDoc.extensions")+" : ");
			for (Iterator i = extensionNames.iterator (); i.hasNext ();) {
				String name = (String) i.next ();
				
				String extensionName = extensionFileNames.get (name);
				if (extensionName != null) {
					out.write ("<a href=\""+extensionName+"\">");	// alink on extension source code
				}
				
				out.write (name);
				
				if (extensionName != null) {out.write ("</a>");}
				
				out.write (" ");	// a space between extensions names
			}	
			
			out.write ("<br>");
			out.newLine ();
		}
		
		Iterator<String> ms = sc.getMethodNames ().iterator ();
		Iterator<String> cs = sc.getMethodComments ().iterator ();
		while (ms.hasNext () && cs.hasNext ()) {
			String m = ms.next ();
			String c = cs.next ();
			out.write ("<i>"+m+"</i>");
			out.write ("<br>");
			
			if (c == null || c.length () ==0) {continue;}	// no comment for this method
			
			out.write ("<table cellpadding=\"1\" cellspacing=\"1\" border=\"0\" style=\"text-align: left; width: 100%;\">");
			out.newLine ();
			out.write ("<tbody>");
			out.newLine ();
			out.write ("<tr>");
			out.newLine ();
				out.write ("<td style=\"vertical-align: top; width: 20px;\">");
				out.write ("</td>");
				out.newLine ();
				out.write ("<td>");
					
				out.write (c);
					
				out.write ("</td>");
				out.newLine ();
				out.write ("</tr>");
				out.newLine ();
				out.write ("</tbody>");
				out.newLine ();
				out.write ("</table>");
				out.newLine ();
				
		}
		out.write ("</td>");
		out.newLine ();
		
		out.write ("</td>");
		out.newLine ();
		out.write ("</tr>");
		out.newLine ();
		out.write ("</tbody>");
		out.newLine ();
		out.write ("</table>");
		out.newLine ();
		
		out.write ("</tr>");
		out.newLine ();
		
	}


	/**	Page footer
	 */
	private void writeFooter () throws Exception {
		out.write ("</tbody>");
		out.newLine ();
		out.write ("</table>");
		out.newLine ();
		out.write ("</body>");
		out.newLine ();
		out.write ("</html>");
		out.newLine ();
	}

	/**	Return a collection of the names of the modules/extensions... which 
	 *	method provider/source code... contains the given key.
	 */
	//~ private Collection<String> getNamesContaining (
			//~ Map<String,TextSearcher> map, String key) {
				
		//~ Collection names = new TreeSet<String> ();
		
		//~ Iterator<String> i = map.keySet ().iterator ();
		//~ Iterator<TextSearcher> j = map.values ().iterator ();
		
		//~ while (i.hasNext () && j.hasNext ()) {
			//~ String name = i.next ();
			//~ TextSearcher ts = j.next ();
			//~ if (ts.contains (key)) {names.add (name);}
		//~ }
		//~ return names;
	//~ }

	/**	Return the url of the html documentation page which was built.
	*/
	public String getPageUrl () {return outName;}
	
	/**	Tests.
	 */
	public static void main (String[] args) {
		System.out.println ("MethodProviderDoc should be ran from capsis4/src/");
		//Settings.setProperty ("capsis.src", Settings.getProperty ("user.dir"), null);
		
		try {
			
//			Options.getInstance ();
			
			MethodProviderDoc mpd = new MethodProviderDoc ();
			mpd.writeHTMLdoc ();
			
			TextSearcher ts = mpd.modulesMap.get ("mountain");
			System.out.println ("TextSearcher on MethodProvider for mountain="+ts);
			System.out.println ("ts.contains (mountain)="+ts.contains ("mountain"));
			System.out.println ("ts.contains (class)"+ts.contains ("class"));
			System.out.println ("ts.contains (DgProvider)"+ts.contains ("DgProvider"));
			System.out.println ("ts.contains (GProvider)"+ts.contains ("GProvider"));
			System.out.println ("ts.contains (ACGProvider)"+ts.contains ("ACGProvider"));
			
			
			System.out.println ();
			TextSearcher ts2 = mpd.extensionsMap.get ("DEDbhClassN");
			System.out.println ("ts2.contains (DbhProvider)="+ts2.contains ("DbhProvider"));
			System.out.println ("ts2.contains (GProvider)="+ts2.contains ("GProvider"));
			
		} catch (Exception e ){
			System.out.println ("Exception");
			e.printStackTrace (System.out);
		}
	}

}

/*
	private void writeRow (SourceDocumenter sc) throws Exception {
		out.write ("<tr>");
		out.newLine ();
		
		// Column 1
		out.write ("<td style=\"vertical-align: top;\">");
		out.write (sc.getClassName ());
		out.write ("</td>");
		out.newLine ();
		
		// Column 2
		out.write ("<td>");
		out.write ("<b>"+sc.getClassComment ()+"</b>");
		out.write ("<br>");
		out.write ("(");
		out.write (sc.getClassAuthor ());
		out.write (")<br>");
		Iterator<String> ms = sc.getMethodNames ().iterator ();
		Iterator<String> cs = sc.getMethodComments ().iterator ();
		while (ms.hasNext () && cs.hasNext ()) {
			String m = ms.next ();
			String c = cs.next ();
			out.write ("<i>"+m+"</i>");
			out.write ("<br>");
			
			if (c == null || c.length () ==0) {continue;}	// no comment for this method
			
			out.write ("<table cellpadding=\"1\" cellspacing=\"1\" border=\"0\" style=\"text-align: left; width: 100%;\">");
			out.newLine ();
			out.write ("<tbody>");
			out.newLine ();
			out.write ("<tr>");
			out.newLine ();
				out.write ("<td style=\"vertical-align: top; width: 20px;\">");
				out.write ("</td>");
				out.newLine ();
				out.write ("<td>");
					
				out.write (c);
					
				out.write ("</td>");
				out.newLine ();
				out.write ("</tr>");
				out.newLine ();
				out.write ("</tbody>");
				out.newLine ();
				out.write ("</table>");
				out.newLine ();
				
		}
		out.write ("</td>");
		out.newLine ();
		
		
		
		out.write ("</tr>");
		out.newLine ();
		
		
		
	}



*/


