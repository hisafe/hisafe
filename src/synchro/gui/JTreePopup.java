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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import synchro.kernel.DiffNode;

/**
 * A popup menu for JTree.
 * 
 * @author F. de Coligny - april 2003
 */
public class JTreePopup extends JPopupMenu implements ActionListener {

	public static final int NEW_ROOT = 1;
	public static final String NEW_ROOT_TEXT = "New root: ";
	
	private DiffNode[] nodes;
	private MainFrame frame;
	
	private JMenuItem newRoot;
	private JMenuItem editDiffNode;
	private JMenuItem editV2Node;
	private JMenuItem browseDiffNode;
	private JMenuItem browseV1Node;
	private JMenuItem restoreFile;
	private JMenuItem deleteFile;
	private JMenuItem upgradeFile;
	
	private Collection menuItems;
	
	private Collection candidates2;
	private Map ancestors;		// parents directories of the current tree root till highest possible reached
	
	/**
	 * Default constructor.
	 */
	public JTreePopup (MainFrame frame, DiffNode[] nodes) {
		this.frame = frame;
		this.nodes = nodes;
		
		menuItems = new ArrayList ();
		
		// potential new roots upper
		//
		Collections potentialUpperRoots = null;
		if (nodes.length == 1) {
			DiffNode n = nodes[0];
			String nodeName = n.getName();
			if (n.isRoot ()) {nodeName = "";}
			
			String r1 = frame.getRoot1Path ()+n.getParentPath ()+nodeName;
			String r2 = frame.getRoot2Path ()+n.getParentPath ()+nodeName;
			
			StringTokenizer st1 = new StringTokenizer (r1, File.separator);
			StringTokenizer st2 = new StringTokenizer (r2, File.separator);
			
			java.util.List l1 = new ArrayList ();
			java.util.List l2 = new ArrayList ();
			
			while (st1.hasMoreTokens ()) {l1.add (st1.nextToken ());}
			while (st2.hasMoreTokens ()) {l2.add (st2.nextToken ());}
			
			Collections.reverse (l1);
			Collections.reverse (l2);
			
			Iterator i1 = l1.iterator ();
			Iterator i2 = l2.iterator ();
			
			Collection candidates1 = new ArrayList ();
			candidates2 = new ArrayList ();		// this is not a local variable (used below)
			
			boolean stop = false;
			while (i1.hasNext () && i2.hasNext () && !stop) {
				String t1 = (String) i1.next ();
				String t2 = (String) i2.next ();
				String key1 = t1+File.separator;
				String key2 = t2+File.separator;
				
				if (t1.equals (t2)) {
					candidates1.add (key1);
					candidates2.add (key2);
				} else {
					candidates1.add (key1);	// higher available root (below which it's identical)
					candidates2.add (key2);
					stop = true;
				}
			}
			
			if (!r1.endsWith (File.separator)) {r1 += File.separator;}
			if (!r2.endsWith (File.separator)) {r2 += File.separator;}
			
			
			ancestors = new HashMap ();
			i1 = candidates1.iterator ();
			i2 = candidates2.iterator ();
			while (i1.hasNext () && i2.hasNext ()) {
				String key1 = (String) i1.next ();
				String key2 = (String) i2.next ();
				
				String absolutePath1 = r1.substring (0, r1.indexOf (key1)+key1.length ());
				String absolutePath2 = r2.substring (0, r2.indexOf (key2)+key2.length ());
				String[] paths = {absolutePath1, absolutePath2};
				ancestors.put (key2, paths);
			}
			
		}
		
		
		
		// Potential menu items creation
		//
		newRoot = new JMenuItem ("-- Change roots --");
		newRoot.setEnabled (false);
		menuItems.add (newRoot);
		
		editDiffNode = new JMenuItem ("Edit diffs (into v2)");
		editDiffNode.setToolTipText ("Opens a diff of the two versions of this file in an editor");
		menuItems.add (editDiffNode);
		
		editV2Node = new JMenuItem ("Edit v2");
		editV2Node.setToolTipText ("Opens file version 2 in an editor");
		menuItems.add (editV2Node);
		
		browseDiffNode = new JMenuItem ("Browse diffs");
		browseDiffNode.setToolTipText ("Shows a diff of the two versions of this file");
		menuItems.add (browseDiffNode);
		
		browseV1Node = new JMenuItem ("Browse v1");
		browseV1Node.setToolTipText ("Browse file version 1");
		menuItems.add (browseV1Node);
		
		upgradeFile = new JMenuItem ("Upgrade file(s) (v1 -> v2)");
		upgradeFile.setToolTipText ("Copies the root1 version(s) into root2 at the same place");
		menuItems.add (upgradeFile);
		
		restoreFile = new JMenuItem ("Restore file(s) (v2)");
		restoreFile.setToolTipText ("Restore BAK file(s) in place of changed / deleted file(s)");
		menuItems.add (restoreFile);
		
		deleteFile = new JMenuItem ("Delete file(s) (v2)");
		deleteFile.setToolTipText ("Backup (*BAK) the file(s) then delete it (them)");
		menuItems.add (deleteFile);
		
		// Nodes vetos evaluation
		//
		int n = nodes.length;
		for (int i = 0; i < n; i++) {
			DiffNode node = nodes[i];
			evaluateVeto (node);				// may remove some menu items from the collection
			if (menuItems.isEmpty ()) {break;}
		}
		
		// Nothing in the popup
		//
		if (menuItems.isEmpty ()) {
			JMenuItem item = new JMenuItem ("No action available");
			item.setEnabled (false);
			add (item);
			return;
		}		// "no popup menu"
		
		// Popup construction
		//
		for (Iterator i = menuItems.iterator (); i.hasNext ();) {
			JMenuItem item = (JMenuItem) i.next ();
			add (item);							// build popup menu with remaining menu items
			item.addActionListener (this);
		}	
		
		// If newRoot still here (not vetoed), add ancestors as potential roots
		//
		if (menuItems.contains (newRoot)) {
			for (Iterator i = candidates2.iterator (); i.hasNext ();) {
				String key = (String) i.next ();
				String[] absolutePaths = (String[]) ancestors.get (key);
				JMenuItem item = new JMenuItem (JTreePopup.NEW_ROOT_TEXT+key);
				item.setToolTipText ("Sets this directory as new root and updates tree");
				item.addActionListener (this);
				add (item);
			}
		}
		
	}
		
	// Evaluate the given node and remove incompatible options in menu items collection
	//
	private void evaluateVeto (DiffNode node) {
		File file1 = null;
		File file2 = null;
		File file = null;
		String nodeName = node.getName();
		if (node.isRoot ()) {nodeName = "";}
		try {
			//~ file1 = new File (frame.getRoot1Path ()+node.getParentPath ()+node.getName());
			//~ file2 = new File (frame.getRoot2Path ()+node.getParentPath ()+node.getName());
			file1 = new File (frame.getRoot1Path ()+node.getParentPath ()+nodeName);
			file2 = new File (frame.getRoot2Path ()+node.getParentPath ()+nodeName);
		} catch (Exception e) {
		}
		
		// file1 or file2 may be null, file is not null
		//
		if (file2 != null) {
			file = file2;
		} else {
			file = file1;
		}
		
		// New root : only available for one SINGLE directory
		//
		if (!file.isDirectory () || nodes.length != 1) {	
			menuItems.remove (newRoot);		
		}
		
		// Edit diff node : available only for one SINGLE changed file
		//
		if (file.isDirectory () || node.getStatus () != DiffNode.DIFF || nodes.length != 1 || node.isRoot ()) {
			menuItems.remove (editDiffNode);	
		}
		
		// Edit v2 node : available only for one SINGLE file
		//
		if (file.isDirectory () || node.getStatus () == DiffNode.ERASED || nodes.length != 1 || node.isRoot ()) {
			menuItems.remove (editV2Node);	
		}
		
		// Browse diff node : available only for one SINGLE changed file
		//
		if (file.isDirectory () || node.getStatus () != DiffNode.DIFF || nodes.length != 1 || node.isRoot ()) {
			menuItems.remove (browseDiffNode);	
		}
		
		// Browse v1 node : available only for one SINGLE file
		//
		if (file.isDirectory () || node.getStatus () == DiffNode.NEW || nodes.length != 1 || node.isRoot ()) {
			menuItems.remove (browseV1Node);	
		}
		
		// Upgrade file : for SEVERAL erased or diff files
		//
		if (file.isDirectory () || (node.getStatus () != DiffNode.ERASED && node.getStatus () != DiffNode.DIFF) 
				|| node.isRoot ()) {
			menuItems.remove (upgradeFile);	
		}
		
		// Restore file : for SEVERAL files or directories with existing BAK files
		File fileBAK = new File (file.getPath ()+"BAK");
		//~ if (file.isDirectory () || node.isRoot () || (!file.getName ().endsWith ("BAK") && !fileBAK.exists ())) {
		if (node.isRoot () || (!file.getName ().endsWith ("BAK") && !fileBAK.exists ())) {
			menuItems.remove (restoreFile);
		}
		
		// Delete file : available for SEVERAL files or directories only if they exist in root2
		//
		//~ if (file.isDirectory () || node.getStatus () == DiffNode.ERASED 
				//~ || node.isRoot () || file.getName ().endsWith ("BAK")) {	
		if (node.getStatus () == DiffNode.ERASED 
				|| node.isRoot () || file.getName ().endsWith ("BAK")) {	
			menuItems.remove (deleteFile);
		}
		
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (editDiffNode)) {
			frame.editDiffAction (nodes[0]);	// single node guarantee (see evaluateVeto ())
			
		} else if (evt.getSource ().equals (editV2Node)) {
			frame.editV2Action (nodes[0]);	// single node guarantee (see evaluateVeto ())
			
		} else if (evt.getSource ().equals (browseDiffNode)) {
			frame.browseDiffAction (nodes[0]);	// single node guarantee (see evaluateVeto ())
			
		} else if (evt.getSource ().equals (browseV1Node)) {
			frame.browseV1Action (nodes[0]);	// single node guarantee (see evaluateVeto ())
			
		} else if (evt.getSource ().equals (upgradeFile)) {
			frame.upgradeAction (nodes);
			
		} else if (evt.getSource ().equals (restoreFile)) {
			frame.restoreAction (nodes);
			
		} else if (evt.getSource ().equals (deleteFile)) {
			frame.deleteAction (nodes);
			
		} else {	// set an ancestor as new root
			JMenuItem item = (JMenuItem) evt.getSource ();
			String text = item.getText ();
			String key = text.substring (text.indexOf (JTreePopup.NEW_ROOT_TEXT)+JTreePopup.NEW_ROOT_TEXT.length ());
			String[] absolutePaths = (String[]) ancestors.get (key);
			frame.setAbsoluteRoots (absolutePaths[0], absolutePaths[1]);
			
		}
	}
	
}