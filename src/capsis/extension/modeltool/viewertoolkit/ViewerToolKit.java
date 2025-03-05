/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.viewertoolkit;

//import nz1.model.*;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;

import jeeb.lib.util.Log;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.OVSelector;
import jeeb.lib.util.OVSelectorSource;
import jeeb.lib.util.Question;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.util.Helper;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.DialogModelTool;
import capsis.gui.GrouperChooser;
import capsis.gui.GrouperChooserListener;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.group.Group;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;

/**
 * A Wood Quality Workshop.
 * 
 * @author D. Pont - December 2005
 */
public class ViewerToolKit extends DialogModelTool implements ActionListener,
		//~ GrouperChooserListener, TableSorterListener, ListSelectionListener, SelectionSource {
		GrouperChooserListener, ListSelectionListener, OVSelectorSource {

	// fc - 17.9.2008 - adapted for OVSelector
	static {
		Translator.addBundle("capsis.extension.modeltool.viewertoolkit.ViewerToolKit");
	}
	
	// nb-07.08.2018
	//static public final String AUTHOR="D. Pont, F. de Coligny";
	//static public final String VERSION="1.1";

	private static final int INITIAL_WIDTH = 700;
	private static final int INITIAL_HEIGHT = 600;
	private static final String[] COLUMN_NAMES = {
			Translator.swap ("ViewerToolKit.id"),
			Translator.swap ("ViewerToolKit.age"),
			Translator.swap ("ViewerToolKit.dbh"),
			Translator.swap ("ViewerToolKit.height"),
			Translator.swap ("ViewerToolKit.number"),
			Translator.swap ("ViewerToolKit.mark"),
			Translator.swap ("ViewerToolKit.x"),
			Translator.swap ("ViewerToolKit.y"),
			Translator.swap ("ViewerToolKit.z"),
			Translator.swap ("ViewerToolKit.species")
			};

		/**	A cell renderer with gray font when disabled.
		*/
		private class StringRenderer extends DefaultTableCellRenderer {
			private Color normalForeground;
			private Color normalBackground;
			public StringRenderer () {super ();}
			public Component getTableCellRendererComponent(
					JTable table, Object value,
					boolean isSelected, boolean hasFocus,
					int row, int column) {
				Component c = super.getTableCellRendererComponent (
						table, value, isSelected, hasFocus, row, column
						);

				if (normalForeground == null || normalBackground == null) {
					normalForeground = c.getForeground ();
					normalBackground = c.getBackground ();
				}

				JLabel l = (JLabel) c;
				l.setOpaque (true);
				if (value instanceof Number) {
					l.setHorizontalAlignment (JLabel.RIGHT);
				} else {
					l.setHorizontalAlignment (JLabel.LEFT);
				}

				if (table.isEnabled ()) {
					if (isSelected) {
						l.setForeground (table.getSelectionForeground ());
						l.setBackground (table.getSelectionBackground ());
					} else {
						l.setForeground (normalForeground);
						l.setBackground (normalBackground);
					}

				} else {
					l.setForeground (Color.GRAY);	// "pending update..."
					l.setBackground (normalBackground);
				}

				//~ setFont (table.getFont ());
				//~ setValue (value);	// see DefaultTableCellRenderer (null is processed)
				return c;
			}
		}

	private Step step;

	private TreeList stand;
	private GrouperChooser grouperChooser;
	private Collection treesInTheTable;
	private JTable table;
	private JScrollPane scrollPane;

	private Collection selectedTrees;



	protected OVSelector ovSelector; 		// fc - 17.9.2008
	protected boolean thisIsAReselection; 	// fc - 17.9.2008
	//~ private JComboBox viewerCombo;
	private JScrollPane viewerScrollPane;

	private JButton close;	// after confirmation
	private JButton help;

	//~ private ObjectViewer currentOV;							// fc - 17.9.2008
	//~ private Collection<SelectionListener> selectionListeners;		// fc - 17.9.2008


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public ViewerToolKit () {		
		super();
	}
	
	@Override
	public void init(GModel m, Step s){

		try {
			step = s;

			stand = (TreeList) step.getScene ();
			treesInTheTable = stand.getTrees ();

			setTitle (getName ()+" - "+step.getCaption ());

			/*nf = NumberFormat.getInstance(Locale.ENGLISH);
			nf.setMinimumFractionDigits(2);
			nf.setMaximumFractionDigits(2);
			nf.setGroupingUsed (false);*/


			createOVSelector (treesInTheTable);
			createUI ();

			setSize (INITIAL_WIDTH, INITIAL_HEIGHT);
			//pack ();	// sets the size
			setVisible (true);
			setModal (false);
			
		} catch (Exception exc) {
			Log.println (Log.ERROR, "ViewerToolKit.c ()", exc.toString (), exc);
		}

	}

	/**	Define what objects are candidate for selection by OVSelector
	*	Can be redefined in subclasses to select other objects
	*/
	protected void createOVSelector (Collection candidateObjects) {
		try {
			candidateObjects = new ArrayList (candidateObjects);
			viewerScrollPane = new JScrollPane ();
			GModel modelForVetoes = null;
			ExtensionManager extMan = CapsisExtensionManager.getInstance ();
			ovSelector = new OVSelector (extMan, this, candidateObjects, viewerScrollPane,
					false, false, modelForVetoes);
		} catch (Exception e) {
			Log.println (Log.ERROR, "TableView.createOVSelector ()",
					"Exception during OVSelector construction, wrote this error and passed", e);
		}
	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene std = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(std instanceof TreeList)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "ViewerToolKit.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("ViewerToolKit.name");
	}

	@Override
	public String getAuthor() {
		return "D. Pont, F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("ViewerToolKit.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	/**	From GrouperChooserListener interface.
	*	The group was changed: reset the treesInTable collection,
	*	re-create the table and replace it in the table scrollpane.
	*/
	public void grouperChanged (String newGrouperName) {
//System.out.println ("grouper changed");
		if (newGrouperName == null || newGrouperName.equals ("")) {
			treesInTheTable = stand.getTrees ();	// Cancel grouper if one is set
		} else {
			GrouperManager gm = GrouperManager.getInstance ();
			Grouper newGrouper = gm.getGrouper (newGrouperName);

			treesInTheTable = newGrouper.apply (stand.getTrees (),
					newGrouperName.toLowerCase ().startsWith ("not "));
		}
//System.out.println ("number of trees:"+treesInTheTable.size());
		scrollPane.getViewport ().setView (makeTable ());

		// fc - 17.9.2008
		cancelSelection ();
	}

	/**	Reset the table according to the treesInTable collection
	*/
	private JTable makeTable () {
		Object[] trees = treesInTheTable.toArray ();

		String[] colNames = COLUMN_NAMES;
		Object[][] mat = new Object[trees.length][COLUMN_NAMES.length];

		StringBuffer buffer = new StringBuffer ();

		String BLANK = "";
		String MARKED = "x";

		for (int i = 0; i < trees.length; i++) {
			/*if (Thread.interrupted ()) {
				System.out.println ("*** thread was interrupted in makeTable");
				return null;
			}*/
			Tree t = (Tree) trees[i];
			mat[i][0] = new Integer (t.getId ());
			mat[i][1] = new Integer (t.getAge ());
			mat[i][2] = new Double (t.getDbh ());
			mat[i][3] = new Double (t.getHeight ());

			try {
				Numberable s = (Numberable) trees[i];
				mat[i][4] = new Double (s.getNumber ());	// fc - 22.8.2006 - Numberable returns double
			} catch (Exception e) {
				mat[i][4] = BLANK;
			}

			if (t.isMarked ()) {
				mat[i][5] = MARKED;
			} else  {
				mat[i][5] = BLANK;
			}

			try {
				Spatialized s = (Spatialized) trees[i];
				mat[i][6] = new Double (s.getX ());
				mat[i][7] = new Double (s.getY ());
				mat[i][8] = new Double (s.getZ ());
			} catch (Exception e) {
				mat[i][6] = BLANK;
				mat[i][7] = BLANK;
				mat[i][8] = BLANK;
			}

			try {
				Speciable s = (Speciable) trees[i];
				buffer.setLength (0);
				buffer.append (s.getSpecies ().getName ());
				buffer.append (" (");
				buffer.append (s.getSpecies ().getValue ());
				buffer.append (")");
				mat[i][9] = buffer.toString ();
			} catch (Exception e) {
				mat[i][9] = BLANK;
			}
		}

		// we use a non editable table model (browse only)
		// we add a sorter connected to the column headers
		DefaultTableModel dataModel = new NonEditableTableModel (mat, colNames);
		table = new JTable (dataModel);
		table.setAutoCreateRowSorter(true);

		// we use our custom string renderer
		StringRenderer stringRenderer = new StringRenderer ();

		table.setRowSelectionAllowed (true);
		//~ table.addMouseListener (this);	// new - fc - 14.2.2002
		table.setAutoResizeMode (JTable.AUTO_RESIZE_OFF);	// fc - 5.10.2002
		table.setDefaultRenderer (Object.class, stringRenderer);
		table.setDefaultRenderer (String.class, stringRenderer);
		table.setDefaultRenderer (Number.class, stringRenderer);
		table.setDefaultRenderer (Double.class, stringRenderer);
		table.setDefaultRenderer (Integer.class, stringRenderer);

		table.setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		table.getSelectionModel ().addListSelectionListener (this);

		// try to restore the previous selection		//
		/*ListSelectionModel m = table.getSelectionModel ();
		int memo = lastSelectedRow;
		lastSelectedRow = -1;
		m.setSelectionInterval (memo, memo);*/

		return table;
	}

		// Load the object viewer which is selected in the combo box
	// Make it show the current selection in table
	//
	//~ private void updateViewer (boolean ovChanged) {
		//~ String name = "";
		//~ String className = "";
		//~ try {
			//~ if (currentOV == null || ovChanged) {
				//~ name = (String) viewerCombo.getSelectedItem ();
				//~ className = (String) viewerName2viewerClassName.get (name);

				//~ Settings.setProperty ("woodqualityworkshop.last.viewer.name", name);

				//~ if (currentOV != null) {	// fc - 8.2.2008
					//~ try {((Disposable) currentOV).dispose ();} catch (Exception e) {}
				//~ }

				//~ // prepare the starter object for the loaded viewer
				//~ // we pass it the selected trees to be viewed
				//~ ExtensionStarter s = new ExtensionStarter ();
				//~ // fc - 8.2.2008
				//~ s.setSelectionSource (this);
				//~ s.setCollection (selectedTrees);
				//~ // fc - 8.2.2008

				//~ currentOV = (ObjectViewer) extMan.loadExtension (className, s);
				//~ if (currentOV == null) {
					//~ // prepare the starter object for the loaded viewer
					//~ // we pass it the first tree in the selection (2nd try)
					//~ s = new ExtensionStarter ();
					//~ // fc - 8.2.2008
					//~ s.setSelectionSource (this);
					//~ Collection tmp = new ArrayList ();
					//~ tmp.add (selectedTrees.iterator ().next ());
					//~ s.setCollection (tmp);
					//~ // fc - 8.2.2008

					//~ currentOV = (ObjectViewer) extMan.loadExtension (className, s);
				//~ }
			//~ } else {
				//~ // Fire a selection event to tell the selection changed -> the objectViewer updates
				//~ boolean selectionActuallyChanged = true;
				//~ fireSelectionEvent (new SelectionEvent (this, selectionActuallyChanged));
			//~ }
			//~ viewerScrollPane.getViewport ().setView (currentOV);

		//~ } catch (Exception e) {
			//~ Log.println (Log.WARNING, "ViewerToolKit.updateViewer ()",
					//~ "exception during object current Object Viewer instanciation");
		//~ }
	//~ }

	/**	Called when selection changed in table
	*/
	public void valueChanged (ListSelectionEvent evt) {
		ListSelectionModel sm = (ListSelectionModel) evt.getSource ();
		// several reasons to abort
		if (sm.isSelectionEmpty ()) {return;}	// we are on the header
		if (table == null) {return;}	// table is null
		// Selection throws 2 events : mouse press and release -> we want only one
		/*int selectedRow = table.getSelectedRow ();
		if (selectedRow == lastSelectedRow) {return;}

		lastSelectedRow = selectedRow;	// ok, there is actually a selection
*/
		// retrieve selected trees in the table
		try {
			int[] indices = table.getSelectedRows ();
			selectedTrees = new ArrayList ();
			for (int i = 0; i < indices.length; i++) {
				int numRow = table.convertRowIndexToModel(indices[i]);
				int treeId = ((Integer) table.getModel ().getValueAt (numRow, 0)).intValue ();
				Tree tree = stand.getTree (treeId);
				selectedTrees.add (tree);
			}
			//~ boolean ovChanged = false;
			//~ updateViewer (ovChanged);	// just update currentOV

			thisIsAReselection = false;
			ovSelector.select (selectedTrees, thisIsAReselection);

		} catch (Exception e) {
//System.out.println ("Error in tree selection");
		}

	}

	/**	From ActionListener interface.
	*	Buttons management.
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			if (Question.ask (MainFrame.getInstance (),
					Translator.swap ("ViewerToolKit.confirm"), Translator.swap ("ViewerToolKit.confirmClose"))) {
				dispose ();
			}

		//~ // update the object viewer
		//~ } else if (evt.getSource ().equals (viewerCombo)) {
			//~ boolean ovChanged = true;
			//~ updateViewer (ovChanged);	// dispose currentOV and load the new one

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**	Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	*/
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("ViewerToolKit.confirm"), Translator.swap ("ViewerToolKit.confirmClose"))) {
			dispose ();
		}
	}

	// When this viewer updates, see if reselection must
	// be triggered towards OVSelector
	// The OVSelector calls this method when its combo box value changes.
	public void reselect () {		// fc - 23.11.2007
		if (ovSelector == null) {
//System.out.println ("*** ViewerToolKit.reselect () : ovSelector null !...");
		} else {
//System.out.println ("*** ViewerToolKit.reselect ()...");
		thisIsAReselection = true;
		ovSelector.select (selectedTrees, thisIsAReselection);
		}
	}

	// If ovDialog is closed by user, remove any selection marks
	// Unused here, we use a JScrollPane (can not be closed) instead of an OVDialog - fc - 17.9.2008
	public void cancelSelection () {
//System.out.println ("VTK.cancelingSelection ...");
//System.out.println ("VTK.checking selection canceling...");
		ovSelector.select (null, false);
//System.out.println ("VTK.selection canceled ...");
	}

	/**	User interface definition
	*/
	private void createUI () {

		// part1 contains the grouperChooser and the table
		JPanel part1 = new JPanel (new BorderLayout ());

		// fc-3.6.2020 Added a null expectedTypes list
		grouperChooser = new GrouperChooser (stand, TreeList.GROUP_ALIVE_TREE, null, 
				"", false, true, false);
		grouperChooser.addGrouperChooserListener (this);
		part1.add (grouperChooser, BorderLayout.NORTH);

		table = makeTable ();
		scrollPane = new JScrollPane (table);
		part1.add (scrollPane, BorderLayout.CENTER);

		//LinePanel l0 = new LinePanel ();
		//part1.addGlue ();

		// part2 contains the OVSelector
		// and the selected object viewer, showing the table selection
		JPanel part2 = new JPanel (new BorderLayout ());

		//~ Collection classNames = extMan.getExtensionClassNames (
				//~ Extension.OBJECT_VIEWER, treesInTheTable);
		//~ try {
			//~ classNames.addAll (extMan.getExtensionClassNames (
					//~ Extension.OBJECT_VIEWER, treesInTheTable));
		//~ } catch (Exception e) {}
		//~ viewerName2viewerClassName = new HashMap ();
		//~ for (Iterator i = classNames.iterator (); i.hasNext ();) {
			//~ String className = (String) i.next ();
			//~ viewerName2viewerClassName.put (extMan.getExtensionName (className), className);
		//~ }
		//~ Collection names = viewerName2viewerClassName.keySet ();
		//~ viewerCombo = new JComboBox (new Vector (new TreeSet (names)));
		//~ viewerCombo.addActionListener (this);
		//viewerCombo.setSelectedItem (...)	// memorized last time in system property
		part2.add (ovSelector, BorderLayout.NORTH);

		// create a scroll pane for the current object viewer
		part2.add (viewerScrollPane, BorderLayout.CENTER);


		// Control panel at the bottom: Close / Help
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (close);
		pControl.add (help);
		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// layout parts
		JSplitPane mainPanel = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT);
		mainPanel.setLeftComponent (part1);
		mainPanel.setRightComponent (part2);
		mainPanel.setOneTouchExpandable (true);
		mainPanel.setDividerLocation (INITIAL_WIDTH/2);

		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

	//~ // fc - 10.12.2007 - SelectionSource
	//~ public void addSelectionListener (SelectionListener l) {
		//~ if (selectionListeners == null) {selectionListeners = new ArrayList<SelectionListener> ();}
		//~ selectionListeners.add (l);
	//~ }
	//~ public void removeSelectionListener (SelectionListener l) {
		//~ if (selectionListeners == null) {return;}
		//~ selectionListeners.remove (l);
	//~ }
	//~ public void fireSelectionEvent (SelectionEvent e) {
		//~ if (selectionListeners == null) {return;}
		//~ for (SelectionListener l : selectionListeners) {
			//~ l.sourceSelectionChanged (e);
		//~ }
	//~ }
	//~ public Collection getSelection () {
		//~ return selectedTrees;
	//~ }
	//~ // fc - 10.12.2007 - SelectionSource


}


