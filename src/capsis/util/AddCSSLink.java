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
import java.util.StringTokenizer;

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

/**
 * Checks if link to css is present in html file, if not, add it.
 *
 * @author F. de Coligny - january 2004
 */
public class AddCSSLink extends JFrame implements ActionListener {
	
	private class HTMLFilter extends javax.swing.filechooser.FileFilter {
		public boolean accept (File f) {
			try {
				String fileName = f.getCanonicalPath ();
				return (f.isDirectory () 
						|| fileName.endsWith (".html") 
						|| fileName.endsWith (".html"));
			} catch (Exception e) {
				return false;
			}
		}
		public String getDescription () {return "HTML files";}
		
	}
	
	private class CSSFilter extends javax.swing.filechooser.FileFilter {
		public boolean accept (File f) {
			try {
				String fileName = f.getCanonicalPath ();
				return (f.isDirectory () 
						|| fileName.endsWith (".css"));
			} catch (Exception e) {
				return false;
			}
		}
		public String getDescription () {return "CSS files";}
		
	}
	
	private JTextField browser;
		
	private JTextField hFile;
	private JButton hBrowse;
	
	private JTextField cFile;
	private JButton cBrowse;
	
	private JButton watchHTML;
	private JButton execute;
	private JButton close;
		
	private JTextArea log;
		
	private File h;
	
	/**	Constructor
	*/
	public AddCSSLink () {
		super ("AddCSSLink");
		
		ColumnPanel main = new ColumnPanel ();
		
		LinePanel l0 = new LinePanel ();
		l0.add (new JWidthLabel ("HTML file : ", 70));
		hFile = new JTextField (5);
		l0.add (hFile);
		hBrowse = new JButton ("...");
		hBrowse.addActionListener (this);
		l0.add (hBrowse);
		main.add (l0);
		
		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel ("CSS file : ", 70));
		cFile = new JTextField (5);
		l1.add (cFile);
		cBrowse = new JButton ("...");
		cBrowse.addActionListener (this);
		l1.add (cBrowse);
		main.add (l1);
		
		log = new JTextArea ();
		
		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel ("Browser : ", 70));
		browser = new JTextField (5);
		browser.setText ("konqueror");
		l3.add (browser);
		main.add (l3);
		
		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		watchHTML = new JButton ("Watch HTML");
		watchHTML.addActionListener (this);
		l2.add (watchHTML);
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
		
		writeLog ("ensure there is a reliable link in the HTML file <head> to the given external CSS stylesheet");
		writeLog ("(the CSS must be in a higher & same hierarchy directory than the HTML file)");
		writeLog ("select the HTML file and the CSS file then press execute");
		writeLog ("tidy will be run and the browser will open at the end");
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
		if (evt.getSource ().equals (hBrowse)) {
			hBrowseAction ();
		} else if (evt.getSource ().equals (cBrowse)) {
			cBrowseAction ();
		} else if (evt.getSource ().equals (watchHTML)) {
			File aux = new File (hFile.getText ().trim ());
			konqueror (aux);
		} else if (evt.getSource ().equals (execute)) {
			executeAction ();
		} else if (evt.getSource ().equals (close)) {
			setVisible (false);
			System.exit (0);
		}
	}
	
	//	Html browse
	//
	private void hBrowseAction () {
		// FileName ? -> get it with a JFileChooser
		String name = "";
		
		String path = Settings.getProperty ("html.dir", (String)null);
		if (path == null) {path = Settings.getProperty ("user.dir", (String)null);}
		
		JFileChooser chooser = new JFileChooser (path);
		chooser.setDialogTitle ("Choose HTML file");
		chooser.setApproveButtonText ("Select");
		chooser.setFileFilter (new HTMLFilter ());
		
		int returnVal = chooser.showDialog (this, null);	// null : approveButton text was already set
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			name = chooser.getSelectedFile ().toString ();
			hFile.setText (name);
			Settings.setProperty ("html.dir", name);
		}	// else cancel on file chooser -> do nothing
	}
	
	//	CSS browse
	//
	private void cBrowseAction () {
		// FileName ? -> get it with a JFileChooser
		String name = "";
		
		String path = Settings.getProperty ("css.dir", (String)null);
		if (path == null) {path = Settings.getProperty ("user.dir", (String)null);}
		
		JFileChooser chooser = new JFileChooser (path);
		chooser.setDialogTitle ("Choose CSS file");
		chooser.setApproveButtonText ("Select");
		chooser.setFileFilter (new CSSFilter ());
		
		int returnVal = chooser.showDialog (this, null);	// null : approveButton text was already set
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			name = chooser.getSelectedFile ().toString ();
			cFile.setText (name);
			Settings.setProperty ("css.dir", name);
		}	// else cancel on file chooser -> do nothing
	}
	
	// Execute button
	//
	private void executeAction () {
		h = new File (hFile.getText ().trim ());
		File c = new File (cFile.getText ().trim ());
		
		if (!h.exists ()) {
			writeLog ("wrong HTML file");
			return;
		}
		if (!c.exists ()) {
			writeLog ("wrong CSS file");
			return;
		}
		
		if (!addCSSLink (h, c)) {return;}
		if (!tidy (h)) {return;}
		if (!konqueror (h)) {return;}
	}
	
	//	Run tidy
	//
	private boolean tidy (File file) {
		try {
			String url = file.toString ();
			String command = "tidy -m "+url;
			writeLog (command);
			
			//~ new Launcher (command).execute ();
			Runtime.getRuntime ().exec (command);
			return true;
			
		} catch (Exception e) {
			writeLog ("tidy error: "+e);
			return false;
		}
	}
	
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
	
	//	Insert the link in the <head> tag
	//
	private boolean addCSSLink (File htmlFile, File cssFile) {
		
		writeLog ("-> processing: "+htmlFile.toString ()+"...");
		
		String cssLink = getCSSLink (htmlFile, cssFile);
		if (cssLink == null) {return false;}		// crotch
		
		String theLink = "<link href=\"";
		theLink+=cssLink;
		theLink+="\" rel=\"stylesheet\" type=\"text/css\">";
		writeLog ("cssLink "+theLink);

		StringBuffer bin = new StringBuffer ();
		StringBuffer bout = new StringBuffer ();
		
		try {
			BufferedReader in = new BufferedReader (new FileReader (htmlFile));
			String line;
			while ((line = in.readLine ()) != null) {
				bin.append (line);
				
				// End of lines problem, resulting in words fusion
				if (!line.endsWith (">") && !line.endsWith (" ")) {
					//~ System.out.println ("one space was added at the end of the line below...");
					bin.append (" ");
					//~ line += " ";	// for the trace below
				}
				//~ System.out.println ("#"+line+"#");
			}
			in.close ();
		} catch (IOException e) {}
		
		StringTokenizer st = new StringTokenizer (bin.toString (), "<");	// begin of html tag
		while (st.hasMoreTokens ()) {
			String tag = st.nextToken ();
			
			if (tag.toLowerCase ().startsWith ("head")) {
				bout.append ("<");
				bout.append (tag);
				bout.append (theLink);
				//~ System.out.println ("#"+"<"+tag+theLink+"#");
				
			} else if (tag.toLowerCase ().startsWith ("link")) {
				// forget it
				
			} else {
				bout.append ("<");
				bout.append (tag);
				
				//~ System.out.println ("#"+"<"+tag+"#");
				
			}
			
		}
		
		boolean success = writeFile (htmlFile, bout);
		return success;
	}

	//	Create the css link ex: "../../style.css"
	//
	private String getCSSLink (File htmlFile, File cssFile) {
		
		// Normalize the paths
		try {
			htmlFile = htmlFile.getCanonicalFile();
			cssFile = cssFile.getCanonicalFile();
		} catch (IOException e) {}
		
		//~ writeLog ("htmlFile "+htmlFile.toString ());
		//~ writeLog ("cssFile "+cssFile.toString ());
		
		String htmlPath = htmlFile.getParent ();
		String cssPath = cssFile.getParent ();
		if (!htmlPath.startsWith (cssPath)) {
			writeLog ("html file must be in a subdir of the dir containing the css file");
			return null;
		}
		//~ writeLog ("htmlPath "+htmlPath);
		//~ writeLog ("cssPath "+cssPath);
		
		String cssFileName = cssFile.getName ();
		//~ writeLog ("cssFileName "+cssFileName);
		
		String htmlRelativePath = htmlPath.substring (cssPath.length ());
		//~ writeLog ("htmlRelativePath "+htmlRelativePath);
		
		int depth = new StringTokenizer (htmlRelativePath, File.separator).countTokens ();
		//~ writeLog ("depth "+depth);
		
		StringBuffer b = new StringBuffer ();
		String upperDir = ".."+File.separator;
		for (int i = 0; i < depth; i++) {
			b.append (upperDir);
		}
		String relativePrefix = b.toString ();
		//~ writeLog ("relativePrefix "+relativePrefix);
		
		String cssLink = relativePrefix + cssFileName;
		//~ writeLog ("cssLink "+cssLink);
		
		return cssLink;
		
	}

	//	Write modified html to file
	//
	private boolean writeFile (File htmlFile, StringBuffer bout) {
		try {
			writeLog ("write "+htmlFile.toString ());
			// Create temp file.
			String tmpPath = htmlFile.getParent ()+File.separator+"cssLink.tmp";
			File tmp = new File (tmpPath);
			tmp.createNewFile();
			
			// Delete temp file when program exits.
			//~ tmp.deleteOnExit ();			
			
			BufferedWriter out = new BufferedWriter (new FileWriter (tmp));
			
			StringTokenizer st = new StringTokenizer (bout.toString (), "<");	// begin of html tag
			while (st.hasMoreTokens ()) {
				String tag = st.nextToken ();
				out.write ("<");
				out.write (tag);
				//~ if (!tag.endsWith (" ")) {out.write (" ");}
				out.newLine ();
			}
			
			out.close();
			
			boolean success = tmp.renameTo (htmlFile);
			if (!success) {
				writeLog (""+htmlFile.toString ()+" aborted : could not rename tmp file");
				return false;
			}
			
		} catch (IOException e) {
			writeLog ("write error: "+e);
			return false;
		}
		return true;
		
	}

	public static void main (String[] args) {
		new AddCSSLink ();
	}
	
}