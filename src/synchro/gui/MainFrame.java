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

package synchro.gui;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.RepaintManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import synchro.kernel.Caller;
import synchro.kernel.Commands;
import synchro.kernel.DiffNode;
import synchro.kernel.SubFileSystem;

/**
 * The Synchro application main frame.
 * 
 * @author F. de Coligny - april 2003
 */
public class MainFrame extends JFrame implements Caller, ActionListener, TreeSelectionListener {

	public final static String EDIT = "synchro/shell/edit.sh";
	public final static String BROWSE = "synchro/shell/browse.sh";
	public final static String EDIT_DIFF = "synchro/shell/editdiff.sh";
	public final static String BROWSE_DIFF = "synchro/shell/browsediff.sh";
	public final static String COPY = "synchro/shell/copy.sh";
	public final static String RESTORE = "synchro/shell/restore.sh";
	public final static String DELETE = "synchro/shell/delete.sh";

	private String editor;
	private String browser;

	private File root1;
	private File root2;

	private JTree tree;
	private SubFileSystem sfs;
	
	private JTextField root1Field;
	private JTextField root2Field;
	private JCheckBox withDiff;
	
	private JButton root1Browse;
	private JButton root2Browse;
	
	private JScrollPane scrollPane;
	
	private JRadioButton mandatorySwitch;
	private JRadioButton ignoreSwitch;
	private ButtonGroup mandatoryIgnoreGroup;
	
	private JTextField mandatory;
	private JTextField ignore;
	
	private JButton updateTree;
	private JButton edit;
	
	private JTextField statusBar;
	
	private DiffNode[] currentNodes;
	
	
	
	public String getRoot1Path () {return root1.getPath ()+File.separator;}
	public String getRoot2Path () {return root2.getPath ()+File.separator;}
	
	private boolean popupEnabled;

	
	public MainFrame (File root1, File root2, boolean popupEnabled) {	// root1 /2 may be null
		super ("Synchro");
		this.root1 = root1;
		this.root2 = root2;
		currentNodes = null;
		this.popupEnabled = popupEnabled;
		
		editor = "SciTE";
		browser = "kwrite";
		
		createUI ();
		
		try {
			root1Field.setText (getRoot1Path ());
			root2Field.setText (getRoot2Path ());
		} catch (Exception e) {}
		
		
		// Main frame location & size
		Toolkit toolkit = Toolkit.getDefaultToolkit ();
		int screenWidth = toolkit.getScreenSize ().width;
		int screenHeight = toolkit.getScreenSize ().height;
		int w = 500;
		int h = 600;
		setSize (w, h);
		setLocation ((screenWidth - w) / 2, (screenHeight - h) / 2);
		
		setVisible (true);
	}
	
	
	private void updateTree () {
		printStatus ("Updating tree...");
		
		root1 = null;
		root2 = null;
		try {
			root1 = new File (root1Field.getText ());
			root2 = new File (root2Field.getText ());
		} catch (Exception e) {}
		
		if (root1 == null || root2 == null) {return;}
		
		sfs = new SubFileSystem (root1, root2);
		
		Collection mand = new ArrayList ();
		if (mandatorySwitch.isSelected ()) {
			StringTokenizer st = new StringTokenizer (mandatory.getText (), " ");
			while (st.hasMoreTokens ()) {
				String token = st.nextToken ();
				mand.add (token);
			}
			sfs.setMandatory (mand);
		}
		
		Collection igno = new ArrayList ();
		if (ignoreSwitch.isSelected ()) {
			StringTokenizer st = new StringTokenizer (ignore.getText (), " ");
			while (st.hasMoreTokens ()) {
				String token = st.nextToken ();
				igno.add (token);
			}
			sfs.setIgnored (igno);
		}
		
		sfs.setWithDiff (withDiff.isSelected ());
		
		sfs.update ();
		printStatus ("Building tree...");
		
		tree = new JTree (sfs.getRootNode (), true);
		tree.setCellRenderer (new DiffTreeCellRenderer ());
		tree.addTreeSelectionListener (this);
		
		
		tree.addMouseListener (new MouseAdapter ()  {
			public void mousePressed (MouseEvent e)  {
				if (e.isPopupTrigger ()  &&  e.getClickCount () == 1)  {
					doPopup (e.getX (), e.getY ());
				}
			}
			
			public void mouseReleased(MouseEvent e)  {
				if (e.isPopupTrigger ()  &&  e.getClickCount () == 1)  {
					doPopup (e.getX (), e.getY ());
				}
			}
			
			public void doPopup (int x, int y)  {
				if (!popupEnabled) {return;}
				
				//  Get the tree element under the mouse
				TreePath itemPath = tree.getPathForLocation (x, y);
				if (itemPath == null) {return;}		// right click but not on a node
				
				TreePath[] paths = tree.getSelectionPaths ();	// selection : 0, 1 or n lines
				
				boolean mouseInSelection = false;
				if (paths != null) {
					for (int i = 0; i < paths.length; i++) {
						if (itemPath.equals (paths[i])) {
							mouseInSelection = true;
							break;
						}
					}
				}
				
				if (!mouseInSelection) {	// ex : lines 3 & 4 selected, mouse right click on line 1
					tree.setSelectionPath (itemPath);	// force selection (1 line) for path under mouse
					paths = tree.getSelectionPaths ();	// re-get selection
				} else if (paths.length == 1) {
					// one line selected : ok
				} else {
					// several lines selected : ok
				}
				
				//  Get the desired context menu and show it
				JTreePopup contextMenu = new JTreePopup (MainFrame.this, currentNodes);
				
				contextMenu.show (tree, x, y);
			}
		});
		
		
		// Updating directories status : 
		// Change status if previously IDENTITY but something changed / added / removed below
		//
		DiffNode root = sfs.getRootNode ();
		updateDirectoryStatus (root);
		
		
		printStatus ("done");
		
		scrollPane.getViewport ().setView (tree);
		scrollPane.revalidate ();
	}
	
	
	private int updateDirectoryStatus (DiffNode node) {
		
		if (node.isLeaf ()) {
			return node.getStatus ();	// security, should not be called on leaves
		} else {
			boolean someChangeBelow = false;
			
			Enumeration i = node.children ();
			while (i.hasMoreElements ()) {
				DiffNode child = (DiffNode) i.nextElement ();
				int subStatus = updateDirectoryStatus (child);
				if (subStatus != DiffNode.IDENTITY) {
					someChangeBelow = true;
				}
			}
			if (node.getStatus () == DiffNode.IDENTITY && someChangeBelow) {
				node.setStatus (DiffNode.DIFF);
			}
			return someChangeBelow ? DiffNode.DIFF : DiffNode.IDENTITY;
			
		}
		
	}
	
	
	public void valueChanged (TreeSelectionEvent evt) {
        
		TreePath[] treePaths = tree.getSelectionPaths ();
		if (treePaths == null) {return;}	// may happen
		
		int n = treePaths.length;
		
		currentNodes = new DiffNode[n];
		for (int i = 0; i < n; i++) {
			currentNodes[i] = (DiffNode) treePaths[i].getLastPathComponent ();
		}
		
	}
	
	
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (updateTree)) {
			updateTree ();
		} else if (evt.getSource ().equals (edit)) {
			//~ editDiffAction ();
		} else if (evt.getSource ().equals (root1Browse)) {
			root1BrowseAction ();
		} else if (evt.getSource ().equals (root2Browse)) {
			root2BrowseAction ();
		}
	}
	
	
	public void setAbsoluteRoots (String absolutePath1, String absolutePath2) {
		root1Field.setText (absolutePath1);
		root2Field.setText (absolutePath2);
		updateTree ();
	}
	public void setNewRoot (DiffNode node) {
		String pathRelativeToRoots = node.getParentPath ()+node.getName ();
		setNewRoot (pathRelativeToRoots);
	}
	public void setNewRoot (String pathRelativeToRoots) {
		printStatus ("New root: "+pathRelativeToRoots);
		
		root1Field.setText (getRoot1Path ()+pathRelativeToRoots);
		root2Field.setText (getRoot2Path ()+pathRelativeToRoots);
		updateTree ();
		
	}
	
	
	public void editDiffAction (DiffNode node) {
		String pathRelativeToRoots = node.getParentPath ()+node.getName ();
		
		printStatus ("Edit diff: "+pathRelativeToRoots);
		
		String f1 = getRoot1Path () 
				+ pathRelativeToRoots;
		
		String f2 = getRoot2Path ()
				+ pathRelativeToRoots;
		
		try {
			File file1 = new File (f1);
			File file2 = new File (f2);
			if ((file1 != null && file1.isDirectory ()) 
					|| (file2 != null && file2.isDirectory ())) {
				printStatus ("Can not edit directories");
				return;
			}
		} catch (Exception e) {
			printStatus ("Exception: "+e);
			return;
		}
		
		String cmd = MainFrame.EDIT_DIFF+" "+editor+" "+f1+" "+f2;
		Commands.launchAndForget (cmd, this, node);
		
	}
	
	public void editV2Action (DiffNode node) {
		String pathRelativeToRoots = node.getParentPath ()+node.getName ();
		
		printStatus ("Edit v2: "+pathRelativeToRoots);
		
		String f2 = getRoot2Path ()
				+ pathRelativeToRoots;
		
		String cmd = MainFrame.EDIT+" "+editor+" "+f2;
		Commands.launchAndForget (cmd, this, node);
		
	}
	
	
	
	public void browseDiffAction (DiffNode node) {
		String pathRelativeToRoots = node.getParentPath ()+node.getName ();
		
		printStatus ("Browse diff: "+pathRelativeToRoots);
		
		String f1 = getRoot1Path () 
				+ pathRelativeToRoots;
		
		String f2 = getRoot2Path ()
				+ pathRelativeToRoots;
		
		try {
			File file1 = new File (f1);
			File file2 = new File (f2);
			if ((file1 != null && file1.isDirectory ()) 
					|| (file2 != null && file2.isDirectory ())) {
				printStatus ("Can not browse directories");
				return;
			}
		} catch (Exception e) {
			printStatus ("Exception: "+e);
			return;
		}
		
		String cmd = MainFrame.BROWSE_DIFF+" "+browser+" "+f1+" "+f2;
		Commands.launchAndForget (cmd, this, node);
		
	}
	
	
	
	public void browseV1Action (DiffNode node) {
		String pathRelativeToRoots = node.getParentPath ()+node.getName ();
		
		printStatus ("Browse v1: "+pathRelativeToRoots);
		
		String f1 = getRoot1Path () 
				+ pathRelativeToRoots;
		
		String cmd = MainFrame.BROWSE+" "+browser+" "+f1;
		Commands.launchAndForget (cmd, this, node);
		
	}
	
	
	
	public void upgradeAction (DiffNode[] nodes) {
		
		for (int i = 0; i < nodes.length; i++) {
			String path = nodes[i].getParentPath ()+nodes[i].getName();	// path is relative to roots
			
			String cmd = MainFrame.COPY+" "+getRoot1Path ()+path+" "+getRoot2Path ()+path;
			Commands.launchAndForget (cmd, this, nodes[i]);
		}
		
	}
	
	
	
	
	public void restoreAction (DiffNode[] nodes) {
		
		for (int i = 0; i < nodes.length; i++) {
			String path = getRoot2Path ()+nodes[i].getParentPath ()+nodes[i].getName();	// absolute root2 path
			
			// Can be called on dir/file or dir/fileBAK
			if (path.indexOf ("BAK") >= 0) {
				path = path.substring (0, path.indexOf ("BAK"));
			}
			
			String cmd = MainFrame.RESTORE+" "+path;
System.out.println ("restoring cmd: "+cmd);
			Commands.launchAndForget (cmd, this, nodes[i]);
		}
		
	}
	
	
	public void deleteAction (DiffNode[] nodes) {
			
		for (int i = 0; i < nodes.length; i++) {
			String path = getRoot2Path ()+nodes[i].getParentPath ()+nodes[i].getName();	// absolute root2 path
			
			String cmd = MainFrame.DELETE+" "+path;
			Commands.launchAndForget (cmd, this, nodes[i]);
		}
		
	}
	
	
	private void createUI () {
		
		// Tree panel
		//
		scrollPane = new JScrollPane ();
		
		// Command panel 1
		//
		ColumnPanel cmd1 = new ColumnPanel ();
		
		LinePanel l10 = new LinePanel ();
		l10.add (new JWidthLabel ("Root1 :", 50));
		root1Field = new JTextField (10);
		l10.add (root1Field);
		root1Browse = new JButton ("Browse");
		root1Browse.addActionListener (this);
		l10.add (root1Browse);
		l10.addStrut0 ();
		
		cmd1.add (l10);
		
		LinePanel l11 = new LinePanel ();
		l11.add (new JWidthLabel ("Root2 :", 50));
		root2Field = new JTextField (10);
		l11.add (root2Field);
		root2Browse = new JButton ("Browse");
		root2Browse.addActionListener (this);
		l11.add (root2Browse);
		l11.addStrut0 ();
		
		cmd1.add (l11);
		
		LinePanel l12 = new LinePanel ();
		withDiff = new JCheckBox ("Diff enabled (longer)", false);	// longer
		l12.add (withDiff);
		l12.addStrut0 ();
		
		cmd1.add (l12);
		
		cmd1.addStrut0 ();
		
		
		
		// Command panel 3
		//
		JPanel cmd3 = new JPanel (new GridLayout (9, 1));
		//~ ColumnPanel cmd3 = new ColumnPanel ();
		
		updateTree = new JButton ("Update tree");
		updateTree.addActionListener (this);
		cmd3.add (updateTree);
		
		edit = new JButton ("Edit");
		edit.addActionListener (this);
		//~ cmd3.add (edit);
		
		JPanel aux3 = new JPanel ();
		aux3.add (cmd3, BorderLayout.NORTH);
		
		
		
		// Command panel 5
		//
		ColumnPanel cmd5 = new ColumnPanel ();
		
		LinePanel l99 = new LinePanel ();
		JLabel erased = new JLabel ("Only in root1");
		erased.setForeground (DiffTreeCellRenderer.getErasedColor ());
		erased.setBackground (DiffTreeCellRenderer.getErasedColor2 ());
		erased.setOpaque (true);
		l99.add (erased);
		
		JLabel changed = new JLabel ("Changed in root2");
		changed.setForeground (DiffTreeCellRenderer.getDiffColor ());
		changed.setBackground (DiffTreeCellRenderer.getDiffColor2 ());
		changed.setOpaque (true);
		l99.add (changed);
		
		JLabel newOne = new JLabel ("Only in root2");
		newOne.setForeground (DiffTreeCellRenderer.getNewColor ());
		newOne.setBackground (DiffTreeCellRenderer.getNewColor2 ());
		newOne.setOpaque (true);
		l99.add (newOne);
		
		l99.addStrut0 ();
		
		
		cmd5.add (l99);
		
		
		
		
		LinePanel l1 = new LinePanel ();
		mandatorySwitch = new JRadioButton ("Mandatory :", true);
		l1.add (mandatorySwitch);
		mandatory = new JTextField (10);
		l1.add (mandatory);
		l1.addStrut0 ();
		
		cmd5.add (l1);
		
		LinePanel l2 = new LinePanel ();
		ignoreSwitch = new JRadioButton ("Ignore :", false);
		l2.add (ignoreSwitch);
		ignore = new JTextField (10);
		l2.add (ignore);
		l2.addStrut0 ();
		
		mandatoryIgnoreGroup = new ButtonGroup ();
		mandatoryIgnoreGroup.add (mandatorySwitch);
		mandatoryIgnoreGroup.add (ignoreSwitch);
		
		LinePanel l3 = new LinePanel ();
		statusBar = new JTextField (10);
		statusBar.setEditable (false);
		l3.add (statusBar);
		l3.addStrut0 ();
		
		cmd5.add (l2);
		cmd5.add (l3);
		cmd5.addStrut0 ();
		
		getContentPane ().add (cmd1, BorderLayout.NORTH);
		getContentPane ().add (scrollPane, BorderLayout.CENTER);
		getContentPane ().add (aux3, BorderLayout.EAST);
		getContentPane ().add (cmd5, BorderLayout.SOUTH);
		
		// Close the main window exits the application
		setDefaultCloseOperation (DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				System.exit (0);
			}
		});
		
	}
	
	
	/**
	 * This method is called each time a job in Commands.launchAndForget is over.
	 */
	public void callBack (String command, int returnCode, Object other) {
		
		if (command.startsWith (MainFrame.EDIT_DIFF)) {
			
			DiffNode modifiedNode = (DiffNode) other;
			
			JButton confirmButton = new JButton ("Confirm");
			JButton cancelButton = new JButton ("Restore");
			Vector buttons = new Vector ();
			buttons.add (confirmButton);
			buttons.add (cancelButton);
			
			JButton choice = DMessage.promptUser (this, 
					"Confirmation", 
					"Edition return code was "+returnCode
							+"\nConfirm file modification for\n"+modifiedNode, 
					buttons, 
					cancelButton);
			if (choice.equals (confirmButton)) {
				printStatus ("Edit ok for "+other);
			} else if (choice.equals (cancelButton)) {
				DiffNode[] nodes = {modifiedNode};
				restoreAction (nodes);
			} 
			
		} else if (command.startsWith (MainFrame.RESTORE)) {
			if (returnCode == 0) {
				printStatus ("Restore ok");
				
				DiffNode n = (DiffNode) other;
					
				// Can be called on dir/file or dir/fileBAK
				if (n.getName ().indexOf ("BAK") >= 0) {
					n.setName (n.getName ().substring (0, n.getName ().indexOf ("BAK")));
				}
				n.setStatus (DiffNode.ZERO);
				
				((DefaultTreeModel) tree.getModel ()).nodeChanged (n);
					
			} else {
				printStatus ("Error, restore aborted with code "+ returnCode);
			}
			
		} else if (command.startsWith (MainFrame.COPY)) {
			if (returnCode == 0) {
				printStatus ("Copy ok");
				
				DiffNode n = (DiffNode) other;
				n.setStatus (DiffNode.ZERO);
					
				((DefaultTreeModel) tree.getModel ()).nodeChanged (n);
					
			} else {
				printStatus ("Error, restore aborted with code "+ returnCode);
			}
			
		} else if (command.startsWith (MainFrame.DELETE)) {
			if (returnCode == 0) {
				printStatus ("Delete ok");
				
				DiffNode n = (DiffNode) other;
				n.setName (n.getName ()+"BAK");
				n.setStatus (DiffNode.ZERO);
					
				((DefaultTreeModel) tree.getModel ()).nodeChanged (n);
					
			} else {
				printStatus ("Error, delete aborted with code "+ returnCode);
			}
			
		}
	}
	
	
	private void printStatus (String s) {
		statusBar.setText (s);
		RepaintManager.currentManager (statusBar).paintDirtyRegions ();	// ok!
	}
	
	
	private void root1BrowseAction () {
		JFileChooser chooser = new JFileChooser ();
		chooser.setFileSelectionMode (JFileChooser.DIRECTORIES_ONLY);
		try {
			chooser.setCurrentDirectory (new File (root1Field.getText ().trim ()));
		} catch(Exception e) {
			chooser.setCurrentDirectory (new File (Settings.getProperty ("user.dir", (String)null)));
		}
		
		int returnVal = chooser.showDialog (this, "Select");
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String name = chooser.getSelectedFile ().toString ();
			
			root1Field.setText (name);
		}
	}
	
	
	private void root2BrowseAction () {
		JFileChooser chooser = new JFileChooser ();
		chooser.setFileSelectionMode (JFileChooser.DIRECTORIES_ONLY);
		try {
			chooser.setCurrentDirectory (new File (root2Field.getText ().trim ()));
		} catch(Exception e) {
			chooser.setCurrentDirectory (new File (Settings.getProperty ("user.dir", (String)null)));
		}
		
		int returnVal = chooser.showDialog (this, "Select");
		
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String name = chooser.getSelectedFile ().toString ();
			
			root2Field.setText (name);
		}
	}
	
	
	
	public static void main (String[] args) {
		
		boolean updateNow = false;
		String r1 = null;
		String r2 = null;
		boolean popupEnabled = false;
		
		for (int i = 0; i < args.length; i++) {
			String param = args[i];
			if (param.startsWith ("--update-now")) {
				updateNow = true;
			} else if (param.startsWith ("--popup-enabled")) {
				popupEnabled = true;
			} else if (r1 == null) {
				r1 = param;
			} else {
				r2 = param;
			}
		}
		
		try {
			File root1 = null;
			File root2 = null;
			if (args.length == 2) {
				root1 = new File (r1);
				root2 = new File (r2);
			}
			
			MainFrame f = new MainFrame (root1, root2, popupEnabled);
			if (updateNow) {f.updateTree ();}
			
		} catch (Exception e) {
			System.out.println ("usage: java synchro.gui.MainFrame [options] [<directory1> <directory2>]");
			System.out.println ("options:");
			System.out.println ("  --update-now : updates immediately");
			e.printStackTrace (System.out);
		}
		
		
	}

	
	
}
