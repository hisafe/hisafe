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

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;

/**	Build an html page containing the tree index for all html pages under the 
*	main capsis4/ directory. A general entry fao all Capsis and modules / extensions
*	help. Can be built in french or in english.
*
*	@author F. de Coligny - january 2004
*/
public class BuildHTMLTree extends JFrame implements ActionListener {
	
	private static final String TAB = "&nbsp&nbsp";	// html tabulation
	
	private class DirectoryFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
		public boolean accept (File f) {
			try {
				f.getCanonicalPath ();
				return (f.isDirectory ());
			} catch (Exception e) {
				return false;
			}
		}
		public String getDescription () {return "Directories only";}
		
	}
	
	private class FileFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
		public boolean accept (File f) {
			try {
				f.getCanonicalPath ();
				return (!f.isDirectory ());
			} catch (Exception e) {
				return false;
			}
		}
		public String getDescription () {return "Files only";}
		
	}
	
	private JTextField browser;
		
	private JTextField dirFile;
	private JButton dirBrowse;
	
	private JTextField pagePrefix;
	
	//~ private JButton watchHTML;
	private JButton execute;
	private JButton close;
		
	private JTextArea log;
	
	private Collection entries;
	private Map maps;		// lang -> entry -> title
	private Map depths;		// entry -> the depth in terms of directories
	private Map dirs;		// entry > the directory where it was found
	private Collection knownLangs;
	
	/**	Constructor
	*/
	public BuildHTMLTree () {
		super ("BuildHTMLTree");
		
		entries = new ArrayList ();
		maps = new HashMap ();
		depths = new HashMap ();
		dirs = new HashMap ();
		
		knownLangs = new ArrayList ();
		knownLangs.add ("fr");
		knownLangs.add ("en");
		
		ColumnPanel main = new ColumnPanel ();
		
		LinePanel l0 = new LinePanel ();
		l0.add (new JWidthLabel ("Base directory : ", 100));
		dirFile = new JTextField (5);
		dirFile.setText (Settings.getProperty ("user.dir", (String)null));	// default value
		l0.add (dirFile);
		dirBrowse = new JButton ("...");
		dirBrowse.addActionListener (this);
		l0.add (dirBrowse);
		main.add (l0);
		
		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel ("Page prefix : ", 100));
		pagePrefix = new JTextField (5);
		pagePrefix.setText ("htmlTree");	// default value
		l1.add (pagePrefix);
		main.add (l1);
		
		log = new JTextArea ();
		
		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel ("Browser : ", 100));
		browser = new JTextField (5);
		browser.setText ("konqueror");	// default value
		l3.add (browser);
		main.add (l3);
		
		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		//~ watchHTML = new JButton ("Watch HTML");
		//~ watchHTML.addActionListener (this);
		//~ l2.add (watchHTML);
		execute = new JButton ("Execute");
		execute.addActionListener (this);
		l2.add (execute);
		close = new JButton ("Close");
		close.addActionListener (this);
		l2.add (close);
		
		// Close the main window exits the application
		setDefaultCloseOperation (DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				System.exit (0);
			}
		});
		
		writeLog ("traverse the directories under the base directory to build an html ");
		writeLog ("tree index for capsis help files");
		writeLog ("create an index file for each language (_fr, _en)");
		writeLog ("select the base directory and the page prefix then press execute");
		writeLog ("current directory is "+Settings.getProperty ("user.dir", (String)null));
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (main, BorderLayout.NORTH);
		getContentPane ().add (new JScrollPane (log), BorderLayout.CENTER);
		getContentPane ().add (l2, BorderLayout.SOUTH);
		
		// Location & size
		int w = 550;
		int h = 400;
		Toolkit toolkit = Toolkit.getDefaultToolkit ();
		int screenWidth = toolkit.getScreenSize ().width;
		int screenHeight = toolkit.getScreenSize ().height;
		setLocation ((screenWidth - w) / 2, (screenHeight - h) / 2);
		
		setSize (w, h);
		setVisible (true);
		
	}
	
	//	Write message to the log
	//
	private void writeLog (String line) {
		log.append (line);
		log.append ("\n");
	}
	
	/**	Actions
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (dirBrowse)) {
			dirBrowseAction ();
		//~ } else if (evt.getSource ().equals (watchHTML)) {
			//~ File aux = new File (dirFile.getText ().trim ());
			//~ konqueror (aux);
		} else if (evt.getSource ().equals (execute)) {
			executeAction ();
		} else if (evt.getSource ().equals (close)) {
			setVisible (false);
			System.exit (0);
		}
	}
	
	//	Base directory browse
	//
	private void dirBrowseAction () {
		// FileName ? -> get it with a JFileChooser
		String name = "";
		
		String path = Settings.getProperty ("base.dir", (String)null);
		if (path == null) {path = Settings.getProperty ("user.dir", (String)null);}
		
		JFileChooser chooser = new JFileChooser (path);
		chooser.setDialogTitle ("Choose Base Directory");
		chooser.setApproveButtonText ("Select");
		chooser.setFileFilter (new DirectoryFilter ());
		
		int returnVal = chooser.showDialog (this, null);	// null : approveButton text was already set
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			name = chooser.getSelectedFile ().toString ();
			dirFile.setText (name);
			Settings.setProperty ("base.dir", name);
		}	// else cancel on file chooser -> do nothing
	}
	
	//	Execute button
	//
	private void executeAction () {
		String directory = dirFile.getText ().trim ();
		String prefix = pagePrefix.getText ().trim ();
		
		if (!new File (directory).exists ()) {
			writeLog ("error: base directory not found");
			return;
		}
		if (prefix.length () == 0) {
			writeLog ("error: page prefix is required");
			return;
		}
		
		writeLog ("traversing directories...");
		File dir = new File (directory);
		visitAllDirsAndFiles (dir, -1);	// first file to be processed is dir, not an html file -> inner files will begin at level 0
		
		// create the index pages, one per lang
		for (Iterator i = maps.keySet ().iterator (); i.hasNext ();) {
			String lang = (String) i.next ();
			writeLog ("creating index page for "+lang+"...");
			createPage (directory, prefix, lang);
		}
		
		//~ if (!konqueror (h)) {return;}
		
	}

	//	Process all files and directories under dir
	//
	private void visitAllDirsAndFiles (File dir, int depth) {
		process (dir, depth);
		
		if (dir.isDirectory ()) {
			File[] children = dir.listFiles (new FileFilter ());	// 1. simple files
			for (int i=0; i<children.length; i++) {
				visitAllDirsAndFiles (children[i], depth + 1);
			}
			children = dir.listFiles (new DirectoryFilter ());	// 2. dirs
			for (int i=0; i<children.length; i++) {
				visitAllDirsAndFiles (children[i], depth + 1);
			}
		}
	}
	
	//	Process one html file
	//
	private void process (File f, int depth) {
		try {
			String fileName = f.getCanonicalPath ();
			if (!fileName.endsWith ("html")) {return;}	// only html files
			
			String lang = getLang (fileName);
			String entry = getEntryName (fileName);
			String directory = getDirectoryName (fileName);
			String title = getTitle (f);
			
	//~ System.out.println (lang+"   "+title+"   "+entry);
			
			if (lang != null && entry != null && title != null) {
				// memorize entries only once, in chronological traversing order
				// for future scaning in same order
				if (!entries.contains (entry)) {entries.add (entry);}
				addEntry (lang, entry, title);	// ex: "fr" -> (".../index" -> "Aide Capsis")
				depths.put (entry, depth);
				dirs.put (entry, directory);
			}
		} catch (Exception e) {
			writeLog ("error: "+e);
			return;
		}			
	}
	
	//	Scans for the first <h1> 1st level title in page
	//	If not found, returns name of the given file
	//
	private String getTitle (File html) {
		String title = html.getName ();	// default
		
		try {
			StringBuffer bin = new StringBuffer ();
			boolean foundBegin = false;
			boolean foundEnd = false;
			BufferedReader in = new BufferedReader (new FileReader (html));
			String line;
			while ((line = in.readLine ()) != null) {
				// End of lines problem, resulting in words fusion
				//~ if (!line.endsWith (">") && !line.endsWith (" ")) {
					//~ bin.append (" ");
				//~ }
				
				if (!foundBegin) {
					int begin = line.indexOf ("<h1");
					if (begin == -1) {begin = line.indexOf ("<H1");}
					if (begin != -1) {
						foundBegin = true;
						line = line.substring (begin);
					}
				}
				
				if (foundBegin) {
					int end = line.indexOf ("</h1>");
					if (end == -1) {end = line.indexOf ("</H1>");}
					if (end != -1) {
						foundEnd = true;
						line = line.substring (0, end);
					}
					
				}
				
				if (foundBegin || foundEnd) {
					bin.append (line);
				}
				
				if (foundEnd) {
					title = bin.toString ();
					title = discardHTMLTags (title);
					
					break;
				}
			}
			in.close ();
			
		} catch (IOException e) {}
		return title;
	}
	
	//	Given a <h1>....</h1> line, extracts the title inside, ignoring
	//	the additionnal html tags
	//
	private String discardHTMLTags (String line) {
		StringBuffer b = new StringBuffer ();
		boolean insideTag = false;
		for (int i = 0; i < line.length (); i++) {
			char c = line.charAt (i);
			if (c == '<') {
				insideTag = true;
				continue;
			}
			if (c == '>') {
				insideTag = false;
				continue;
			}
			if (!insideTag) {
				b.append (c);
			}
		}
		String result = b.toString ();
		if (result.length () == 0) {
			result = line+" error: could not find title";
		}
		return result;
	}
	
	//	Extracts lang name from url 
	//	Ex: .../index_fr.html -> "fr"
	//	if no underscore or no dot, return null
	//	if lang is unknown, return null
	//
	private String getLang (String url) {
		int lastDot = url.lastIndexOf (".");
		int underScore = url.lastIndexOf ("_", lastDot);
		if (underScore == -1 || lastDot == -1) {return null;}
		String candidateLang = url.substring (underScore+1, lastDot);
		if (!knownLangs.contains (candidateLang)) {return null;}
		return candidateLang;	// ex : "fr" or "en"
	}
	
	//	Makes a language independant entry name from an url
	//	Ex: .../index_fr.html -> ".../index"
	//	if no underscore, return null
	//
	private String getEntryName (String url) {
		int lastDot = url.lastIndexOf (".");
		int underScore = url.lastIndexOf ("_", lastDot);
		if (underScore == -1) {return null;}
		return url.substring (0, underScore);	// ex : ../index-fr.html -> .../index
	}
	
	//	Return the directory containing the file
	//	Ex: capsis4/bin/index-fr.html -> capsis4/bin/
	//	If trouble, return the given url without change
	//
	private String getDirectoryName (String url) {
		try {
			File f = new File (url);
			return f.getParent ();	// ex : capsis4/bin/index-fr.html -> capsis4/bin
		} catch (Exception e) {
			return url;
		}
	}
	
	//	
	//
	private void addEntry (String lang, String entry, String title) {
		Map m = (Map) maps.get (lang);	// ex : lang = "en"
		if (m == null) {
			m = new HashMap ();
			maps.put (lang, m);
		}
		m.put (entry, title);		// ex: ".../index" -> "Capsis Help"
		
	}
	
	//	Create the index page for the given language
	//
	private boolean createPage (String directory, String prefix, String lang) {	// ex: lang= "fr" or "en"
		try {
			File htmlFile = new File (directory+File.separator+prefix+"_"+lang+".html");
			int z = directory.lastIndexOf (File.separator)+1;
			String baseDir = directory.substring (z);
System.out.println ("baseDir: "+baseDir);
			
			writeLog ("fileName: "+htmlFile.toString ());
System.out.println ();
System.out.println ("createPage "+htmlFile.toString ());
			// Create temp file.
			//~ String tmpPath = htmlFile.getParent ()+File.separator+"cssLink.tmp";
			//~ File tmp = new File (tmpPath);
			//~ tmp.createNewFile();
			
			// Delete temp file when program exits.
			//~ tmp.deleteOnExit ();			
			
			BufferedWriter out = new BufferedWriter (new FileWriter (htmlFile));
			writeHTMLHeader (out);
			
			Map mainMap = (Map) maps.get (lang);
			StringBuffer line = new StringBuffer ();
			
			writeHTML (out, "<h1>Capsis4 - Index "+lang+"</h1>");
			writeHTML (out, "<ul type=square>");
			int memoDepth = -1;
			String memoDir = "";
			for (Iterator i = entries.iterator (); i.hasNext ();) {
				line.delete (0, line.length ());
				
				String entry = (String) i.next ();
				String mainTitle = (String) mainMap.get (entry);
				if (mainTitle == null) {continue;}	// no title for this entry + lang
				int depth = (Integer) depths.get (entry);
				String dir = (String) dirs.get (entry);
				
				//~ if (memoDepth != -1 && depth != memoDepth) {line.append ("<br>");}
				
				// Levels management
				if (memoDepth != -1) {
					if (depth > memoDepth) {
						for (int k = memoDepth; k < depth; k++) {
								line.append ("<ul type=square>");}
					} else if (depth < memoDepth) {
						for (int k = memoDepth; k > depth; k--) {
								line.append ("</ul>");}
					}
				}
				
				if (!dir.equals (memoDir)) {
					line.append ("<li>");
					String paragraphHeader = dir;
					try {
						paragraphHeader = dir.substring (dir.indexOf (baseDir));
					} catch (Exception e) {}
					line.append (paragraphHeader);	// fc - 2.2.2005
					line.append ("</li>");
				}
				
				// fc - 4.2.2005 - machine independent paths in href (did not work on Mac OS X)
				String relDir = entry.substring (
						entry.indexOf (directory)+directory.length ());
				if (relDir.startsWith (File.separator)) {
					relDir = relDir.substring (1);
				}
				
				line.append ("<a href=\""
						+relDir+"_"+lang+".html"
						+"\">");
				
				//~ line.append ("<a href=\""
						//~ +entry+"_"+lang+".html"
						//~ +"\">");
				
				line.append (mainTitle
						//~ +" ("+depth+")"	// temporary
						+"</a>");
				
				for (Iterator m = maps.keySet ().iterator (); m.hasNext ();) {
					String l = (String) m.next ();
					if (l.equals (lang)) {continue;}	// next map
					Map langMap = (Map) maps.get (l);
					String title = (String) langMap.get (entry);	// is there an entry for the lang l
					if (title != null) {
						line.append (" ("
								+"<a href=\""
								+relDir+"_"+l+".html"
								//~ +entry+"_"+l+".html"
								+"\">"
								+l
								+"</a>"
								+")");
					} else {
						line.append (" (no "+l+" translation)");
					}
					
				}
				
				
				line.append ("<br>");
				
				writeHTML (out, line.toString ());
				memoDepth = depth;
				memoDir = dir;
			}
			
			writeHTML (out, "</ul>");
			writeHTMLFooter (out);
			out.close();
			
			//~ boolean success = tmp.renameTo (htmlFile);
			//~ if (!success) {
				//~ writeLog (""+htmlFile.toString ()+" aborted : could not rename tmp file");
				//~ return false;
			//~ }
			
		} catch (Exception e) {
			writeLog ("write error: "+e);
			return false;
		}
		return true;
		
		
		
	}
	
	private void writeHTMLHeader (BufferedWriter out) throws Exception {
		out.write ("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
		out.newLine ();
		out.write ("<html>");
		out.newLine ();
		out.write ("<head>");
		out.newLine ();
		//~ out.write ("<link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\">");
		//~ out.newLine ();
		out.write ("<title>Html Index</title>");
		out.newLine ();
		out.write ("</head>");
		out.newLine ();
		out.write ("<body>");
		out.newLine ();
	}
	
	private void writeHTML (BufferedWriter out, String text) throws Exception {
		out.write (text);
		out.newLine ();
	}
	
	private void writeHTMLFooter (BufferedWriter out) throws Exception {
		out.write ("</body>");
		out.newLine ();
		out.write ("</html>");
		out.newLine ();
	}
	
	//	Return an HTML tabulation based on &nbsp
	//
	private String getTab () {
		return TAB;
	}
		
	//	Run tidy
	//
	//~ private boolean tidy (File file) {
		//~ try {
			//~ String url = file.toString ();
			//~ String command = "tidy -m "+url;
			//~ writeLog (command);
			
			//~ Runtime.getRuntime ().exec (command);
			//~ return true;
			
		//~ } catch (Exception e) {
			//~ writeLog ("tidy error: "+e);
			//~ return false;
		//~ }
	//~ }
	
	//	Run html browser
	//
	private boolean konqueror (File file) {
		try {
			String url = file.toURL ().toString ();
			String command = browser.getText ().trim ()+" "+url;
			writeLog (command);
			Runtime.getRuntime ().exec (command);
			return true;
			
		} catch (Exception e) {
			writeLog ("browser error: "+e);
			return false;
		}
	}
	
	//	
	//
	private boolean BuildHTMLTree (File htmlFile, File cssFile) {
		
		//~ writeLog ("-> processing: "+htmlFile.toString ()+"...");
		
		//~ String cssLink = getCSSLink (htmlFile, cssFile);
		//~ if (cssLink == null) {return false;}		// crotch
		
		//~ String theLink = "<link href=\"";
		//~ theLink+=cssLink;
		//~ theLink+="\" rel=\"stylesheet\" type=\"text/css\">";
		//~ writeLog ("cssLink "+theLink);

		//~ StringBuffer bin = new StringBuffer ();
		//~ StringBuffer bout = new StringBuffer ();
		
		//~ try {
			//~ BufferedReader in = new BufferedReader (new FileReader (htmlFile));
			//~ String line;
			//~ while ((line = in.readLine ()) != null) {
				//~ bin.append (line);
				
				//~ // End of lines problem, resulting in words fusion
				//~ if (!line.endsWith (">") && !line.endsWith (" ")) {
					//~ bin.append (" ");
				//~ }
			//~ }
			//~ in.close ();
		//~ } catch (IOException e) {}
		
		//~ StringTokenizer st = new StringTokenizer (bin.toString (), "<");	// begin of html tag
		//~ while (st.hasMoreTokens ()) {
			//~ String tag = st.nextToken ();
			
			//~ if (tag.toLowerCase ().startsWith ("head")) {
				//~ bout.append ("<");
				//~ bout.append (tag);
				//~ bout.append (theLink);
				
			//~ } else if (tag.toLowerCase ().startsWith ("link")) {
				//~ // forget it
				
			//~ } else {
				//~ bout.append ("<");
				//~ bout.append (tag);
				
			//~ }
			
		//~ }
		
		//~ boolean success = writeFile (htmlFile, bout);
		//~ return success;
		return true;
	}

	//~ //	Create the css link ex: "../../style.css"
	//~ //
	//~ private String getCSSLink (File htmlFile, File cssFile) {
		
		//~ // Normalize the paths
		//~ try {
			//~ htmlFile = htmlFile.getCanonicalFile();
			//~ cssFile = cssFile.getCanonicalFile();
		//~ } catch (IOException e) {}
		
		//~ String htmlPath = htmlFile.getParent ();
		//~ String cssPath = cssFile.getParent ();
		//~ if (!htmlPath.startsWith (cssPath)) {
			//~ writeLog ("html file must be in a subdir of the dir containing the css file");
			//~ return null;
		//~ }

		//~ String cssFileName = cssFile.getName ();
		//~ String htmlRelativePath = htmlPath.substring (cssPath.length ());
		
		//~ int depth = new StringTokenizer (htmlRelativePath, File.separator).countTokens ();
		
		//~ StringBuffer b = new StringBuffer ();
		//~ String upperDir = ".."+File.separator;
		//~ for (int i = 0; i < depth; i++) {
			//~ b.append (upperDir);
		//~ }
		//~ String relativePrefix = b.toString ();
		
		//~ String cssLink = relativePrefix + cssFileName;
		
		//~ return cssLink;
		
	//~ }

	//	Write modified html to file
	//
	private boolean writeFile (File htmlFile, StringBuffer bout) {
		//~ try {
			//~ writeLog ("write "+htmlFile.toString ());
			//~ // Create temp file.
			//~ String tmpPath = htmlFile.getParent ()+File.separator+"cssLink.tmp";
			//~ File tmp = new File (tmpPath);
			//~ tmp.createNewFile();
			
			//~ // Delete temp file when program exits.
			//~ tmp.deleteOnExit ();			
			
			//~ BufferedWriter out = new BufferedWriter (new FileWriter (tmp));
			
			//~ StringTokenizer st = new StringTokenizer (bout.toString (), "<");	// begin of html tag
			//~ while (st.hasMoreTokens ()) {
				//~ String tag = st.nextToken ();
				//~ out.write ("<");
				//~ out.write (tag);
				//~ out.newLine ();
			//~ }
			
			//~ out.close();
			
			//~ boolean success = tmp.renameTo (htmlFile);
			//~ if (!success) {
				//~ writeLog (""+htmlFile.toString ()+" aborted : could not rename tmp file");
				//~ return false;
			//~ }
			
		//~ } catch (IOException e) {
			//~ writeLog ("write error: "+e);
			//~ return false;
		//~ }
		//~ return true;
		return true;
	}

	public static void main (String[] args) {
		new BuildHTMLTree ();
	}
	
}