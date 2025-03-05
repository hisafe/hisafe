/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package synchro.kernel;

import java.awt.BorderLayout;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTree;

/**
 * Given a file (ex : "." - current dir), creates a hierarchy of nodes 
 * in order to create a JTree showing the included dirs / subdirs / files.
 * 
 * @author F. de Coligny - april 2003
 */
public class SubFileSystem {

// Memo: diff format for Synchro
// diff --old-line-format='d-%L' --new-line-format='d+%L' -abB MainFrame1.java MainFrame2.java > diff.java ; SciTE diff.java       

	public static final int PASS_1 = 1;
	public static final int PASS_2 = 2;
	
	private int mode;	//PASS_1, PASS_2

	private File root1;
	private String root1Path;	// root1.getPath () + File.separator
	private int root1Length;
	
	private File root2;
	private String root2Path;
	private int root2Length;
	
	private FileComparator fileComparator;
	private Map dirs;	// dir/file name -> dir/file node
	
	private DiffNode rootNode;

	private Collection ignored;
	private Collection mandatory;
	private boolean withDiff;


	public SubFileSystem (File root1, File root2) {
		this.root1 = root1;
		this.root2 = root2;
		
		fileComparator = new FileComparator ();
		ignored = new HashSet ();		// Empty
		mandatory = new HashSet ();
	}
	
	// SubFileSystem can be updated from outside
	//
	public void update () {
		
		// Pass 1
		//
		mode = SubFileSystem.PASS_1;
		dirs = new HashMap ();
		
		root1Path = root1.getPath ()+File.separator;
		root1Length = root1Path.length ();
		
		visitAllDirsAndFiles (null, root1);
		
		// Pass 2
		//
		mode = SubFileSystem.PASS_2;
		
		root2Path = root2.getPath ()+File.separator;
		root2Length = root2Path.length ();
		
		visitAllDirsAndFiles (null, root2);
		
	}
	
	// Process all files and directories under dir
	// Thanks to "The Java Developers Almanac 1.4"
	//
	private void visitAllDirsAndFiles (File parent, File dir) {
		if (mode == SubFileSystem.PASS_1) {
			process_1 (parent, dir);
		} else {
			process_2 (parent, dir);
		}
		
		if (dir.isDirectory ()) {
			File[] children = dir.listFiles ();	// returns null if dir is not a valid dir or if an IOError occurs
			if (children == null) {
				System.out.println ("warning: dir.listFiles () = null on this dir: "+dir);
				return;	// bug correction (H.Rey) - fc - 23.6.2005
			}
			Arrays.sort (children, fileComparator);
			
			for (int i = 0; i < children.length; i++) {
				String childName = children[i].getName ();
				
				// fc - 17.4.2003
				if (!mandatory.isEmpty ()) {
					if (!isMandatory (childName)) {continue;}
				} else {
					if (isIgnored (childName)) {continue;}
				}
				
				visitAllDirsAndFiles (dir, children[i]);
			}
		}
	}

	// First pass : nodes creation
	// Create node and link to its parent
	//
	private void process_1 (File parent, File dir) {
		String dirPath = (dir.getPath ()+File.separator).substring (root1Length);
		String parentPath = null;
		if (parent != null) {
			parentPath = (parent.getPath ()+File.separator).substring (root1Length);
		}
		
		DiffNode node = new DiffNode (dir.getName (), parentPath);
		node.setAllowsChildren (dir.isDirectory ());	// if directory -> may have children
		node.setStatus (DiffNode.ERASED);				// if not found in second pass, will stay erased
		dirs.put (dirPath, node);	// key = path with name inside (name alone is too short, not enough)
		
		//~ ((DiffNode) node.getUserObject ()).setStatus (DiffNode.ERASED);	// if not found in second pass, will stay erased 
		
		if (parent == null) {
			rootNode = node;
		} else {
			DiffNode parentNode = (DiffNode) dirs.get (parentPath);
			parentNode.add (node);
		} 
		
	}
	
	// Second pass : nodes complement (some new nodes may be created)
	//
	private void process_2 (File parent, File dir) {
		String dirPath = (dir.getPath ()+File.separator).substring (root2Length);
		String parentPath = null;
		if (parent != null) {
			parentPath = (parent.getPath ()+File.separator).substring (root2Length);
		}
			
		DiffNode node = (DiffNode) dirs.get (dirPath);
		
		if (node == null) {
			
			node = new DiffNode (dir.getName (), parentPath);
			node.setAllowsChildren (dir.isDirectory ());
			node.setStatus (DiffNode.NEW);	// was not created during first pass
			dirs.put (dirPath, node);	// key = path with name inside (name alone is too short, not enough)
			
			DiffNode parentNode = (DiffNode) dirs.get (parentPath);
			parentNode.add (node);
				
		} else {
			
			File file1 = new File (root1Path + node.getParentPath (), node.getName ());
			File file2 = dir;
			
			if (diff (file1, file2)) {
				node.setStatus (DiffNode.DIFF);
			} else {
				node.setStatus (DiffNode.IDENTITY);
			}
		}
		
	}
	
	private boolean isMandatory (String fileName) {
		for (Iterator i = mandatory.iterator (); i.hasNext ();) {
			String man = (String) i.next ();
			if (fileName.indexOf (man) != -1) {return true;}
		}
		return false;
		
		//~ int i = fileName.lastIndexOf (".");
		//~ if (i < 0) {return true;}		// no extension -> not concerned, return true (directories...)
		
		//~ String ext = fileName.substring (i);
		//~ if (mandatory.contains (ext)) {return true;}
		
		//~ return false;
		
	}

	private boolean isIgnored (String fileName) {
		for (Iterator i = ignored.iterator (); i.hasNext ();) {
			String ign = (String) i.next ();
			if (fileName.indexOf (ign) != -1) {return true;}
		}
		return false;
		
		//~ int i = fileName.lastIndexOf (".");
		//~ if (i < 0) {return false;}		// no extension -> not ignored
		
		//~ String ext = fileName.substring (i);
		//~ if (ignored.contains (ext)) {return true;}
		
		//~ return false;
		
	}

	private boolean diff (File file1, File file2) {
		if (file1.isDirectory () || file2.isDirectory ()) {
			return false;	// no diff on directories
		}
		
		// 1. if size != -> return true
		if (file1.length () != file2.length ()) {
			return true;
		}
		
		if (withDiff) {
			// 2. if size == -> use diff
			int returnCode = Commands.launchAndWait ("diff -q "+file1.getPath ()+" "+file2.getPath ());
			if (returnCode != 0) {return true;}
		}
		
		return false;
	}

	public DiffNode getRootNode () {return rootNode;}
	
	public File getRoot1 () {return root1;}
	public String getRoot1Path () {return root1Path;}
	
	public File getRoot2 () {return root2;}
	public String getRoot2Path () {return root2Path;}
	
	public void setIgnored (Collection set) {ignored = new HashSet (set);}	// fast on contains ()
	public void setMandatory (Collection set) {mandatory = new HashSet (set);}	// ex: {".java" ".html"}
	public void setWithDiff (boolean withDiff) {this.withDiff = withDiff;}
	
	
///////////////////////////////////////////////////////////////////////
////////////////////////////////////////// Test program
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
	public static void main (String[] args) {
		if (args.length != 2) {
			System.out.println ("usage: java SubFileSystem <directory1> <directory2>");
			return;
		}
		try {
			File root1 = new File (args[0]);
			File root2 = new File (args[1]);
System.out.println ("building tree for \""+root1.getPath ()+File.separator+"\"...");
			SubFileSystem sfs = new SubFileSystem (root1, root2);
			Collection mandatory = new ArrayList ();
			mandatory.add (".java");
			//~ mandatory.add (".html");
			sfs.setMandatory (mandatory);
			
System.out.println ("updating tree with \""+root2.getPath ()+File.separator+"\"...");
			sfs.update ();
System.out.println ("done");
			
			JTree tree = new JTree (sfs.getRootNode (), true);
			tree.setCellRenderer (new synchro.gui.DiffTreeCellRenderer ());
			JScrollPane s = new JScrollPane (tree);
			
			JFrame f = new JFrame ("File tree");
			f.getContentPane ().add (s, BorderLayout.CENTER);
			f.setSize (350, 400);
			f.setVisible (true);
			
		} catch (Exception e) {
			System.out.println ("usage: java SubFileSystem <directory1> <directory2>\n"+ e);
			e.printStackTrace (System.out);
		}
		
		
	}

}
