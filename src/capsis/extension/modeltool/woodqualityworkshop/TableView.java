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

package capsis.extension.modeltool.woodqualityworkshop;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.GridLayout;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;

import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Log;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.OVSelector;
import jeeb.lib.util.OVSelectorSource;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.kernel.GModel;
import capsis.util.SelectionEvent;
import capsis.util.SelectionListener;
import capsis.util.SelectionSource;

/**	A Table view: table on the left, object viewer on the right.
*	@author D. Pont, F. de Coligny - december 2005
*/
public class TableView extends JPanel implements 
		ListSelectionListener, OVSelectorSource, SelectionSource {	// fc - 21.10.2008 - restored SelectionSource
//~ public class TableView extends JPanel implements ActionListener,
		//~ TableSorterListener, ListSelectionListener, SelectionSource {
	// fc - 17.9.2008 - adapted for OVSelector framework
			
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

	private boolean constructionCompleted;
	private Collection<SelectionListener> selectionListeners;	// fc - 21.10.2008
		//~ private SelectionListener selectionListener;
	private String callerName;	// the selection listener classname, just to memo the sort colum
	private Map id2items;
	private JTable table;
	private Collection itemsInTheTable;

	private JScrollPane tableScrollPane;
	private ExtensionManager extMan;
	private Map viewerName2viewerClassName;
	
	protected OVSelector ovSelector; 		// fc - 17.9.2008
	protected boolean thisIsAReselection; 	// fc - 17.9.2008
	//~ private JComboBox viewerCombo;			// fc - 17.9.2008
	private JScrollPane viewerScrollPane;		// fc - 17.9.2008
	//~ private ObjectViewer currentOV;			// fc - 17.9.2008

	
	private Collection selectedItems;
	
	
	/**	Constructor
	*	the listener will be called on table selections
	*	items in the table model must be instance of Identfiable (getId ())
	*	the table model will be showed in the table, first column must be item id
	*	the map contains for each item in the table model id -> item
	*/
	public TableView (SelectionListener selectionListener,
			NonEditableTableModel tableModel, Map id2items) {
		super ();
		constructionCompleted = false;	// fc - 21.10.2008
		extMan = CapsisExtensionManager.getInstance ();

		//~ this.selectionListener = selectionListener;	// fc - 17.9.2008
		addSelectionListener (selectionListener);	// fc - 21.10.2008
		
//		callerName = Tools.getClassLittleName (selectionListener.getClass ().getName ());
		callerName = selectionListener.getClass ().getSimpleName ();
		

		itemsInTheTable = id2items.values ();	// fc - 17.9.2008
		createOVSelector (itemsInTheTable);		// fc - 17.9.2008
		
		setTableModel (tableModel, id2items);
		createUI ();
		constructionCompleted = true;	// fc - 21.10.2008
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

	/**	Create the table with nothing in it
	*/
	public void reset () {
		tableScrollPane.getViewport ().setView (new JTable ());
		//~ viewerCombo.removeAllItems ();
		ovSelector.changeCandidateObjects (Collections.EMPTY_LIST);	// fc - 17.9.2008
	}

	/**	Create the table with the table model
	*/
	public void setTableModel (NonEditableTableModel tableModel, Map id2items) {
		// fc - 1.3.2006
		if (tableModel == null || id2items == null) {
			reset ();
			return;
		}
		
		this.id2items = id2items;
		this.itemsInTheTable = id2items.values ();
		
		// we use a non editable table model (browse only)
		// we add a sorter connected to the column headers
		
		this.table = new JTable (tableModel);
		this.table.setAutoCreateRowSorter(true);
		
		
		// we use our custom string renderer
		StringRenderer stringRenderer = new StringRenderer ();
		
		table.setRowSelectionAllowed (true);
		table.setAutoResizeMode (JTable.AUTO_RESIZE_OFF);
		table.setDefaultRenderer (Object.class, stringRenderer);
		table.setDefaultRenderer (String.class, stringRenderer);
		table.setDefaultRenderer (Number.class, stringRenderer);
		table.setDefaultRenderer (Double.class, stringRenderer);
		table.setDefaultRenderer (Integer.class, stringRenderer);
		
		table.setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		table.getSelectionModel ().addListSelectionListener (this);
		
		// If table is already visible, update it
		if (tableScrollPane != null) {
			tableScrollPane.getViewport ().setView (table);
		}
		
		// Possibly the compatible object viewer list changes
		// we may have opened with nothing in the table, nothing in the combo
		
		ovSelector.changeCandidateObjects (itemsInTheTable);	// fc - 17.9.2008
		
		//~ Collection names = refreshComboViewer ();
		
		//~ if (viewerCombo != null) {
			//~ viewerCombo.removeAllItems ();
			//~ for (Iterator i = new TreeSet (names).iterator (); i.hasNext ();) {
				//~ viewerCombo.addItem ((String) i.next ());
			//~ }
		//~ }

	}

	// Load the object viewer which is selected in the combo box
	// Make it show the current selection in table
	//
	//~ private void updateViewer (boolean ovChanged) {
		//~ try {
			//~ if (currentOV == null || ovChanged) {
				//~ String name = (String) viewerCombo.getSelectedItem ();
				//~ String className = (String) viewerName2viewerClassName.get (name);

				//~ if (name == null || className == null) {
					//~ viewerScrollPane.getViewport ().setView (new JPanel ());
					//~ return;
				//~ }	// fc - 1.3.2006
				//~ Settings.setProperty ("woodqualityworkshop.last.viewer.name", name);

				//~ if (currentOV != null) {
					//~ try {((Disposable) currentOV).dispose ();} catch (Exception e) {}
				//~ }
				
				//~ // prepare the starter object for the loaded viewer
				//~ // we pass it the selected trees to be viewed
				//~ ExtensionStarter s = new ExtensionStarter ();
				//~ // fc - 8.2.2008
				//~ s.setSelectionSource (this);	// fc - 8.2.2008
				//~ s.setCollection (selectedItems);
				//~ // fc - 8.2.2008

				//~ currentOV = (ObjectViewer) extMan.loadExtension (className, s);
				//~ if (currentOV == null) {
					//~ // prepare the starter object for the loaded viewer
					//~ // we pass it the first tree in the selection (2nd try)
					//~ s = new ExtensionStarter ();
					//~ // fc - 8.2.2008
					//~ Collection tmp = new ArrayList ();
					//~ tmp.add (selectedItems.iterator ().next ());
					//~ s.setCollection (tmp);
					//~ s.setSelectionSource (this);	// fc - 8.2.2008
					//~ // fc - 8.2.2008

					//~ currentOV = (ObjectViewer) extMan.loadExtension (className, s);
				//~ }
			//~ } else {
				//~ // Fire a selection event to tell the selection changed -> the objectViewer updates
				//~ boolean selectionActuallyChanged = true;
				//~ fireSelectionEvent (new SelectionEvent (this, selectionActuallyChanged));
			//~ }
			//~ currentOV.setPreferredSize (new Dimension (150, 150)); //smaller than needed
			//~ viewerScrollPane.getViewport ().setView (currentOV);

		//~ } catch (Exception e) {
			//~ Log.println (Log.WARNING, "TableView.updateViewer ()",
					//~ "exception during object current Object Viewer instanciation", e);
		//~ }
	//~ }

	/**	Called when selection changed in table
	*/
	public void valueChanged (ListSelectionEvent evt) {
		if (id2items == null || id2items.isEmpty ()) {return;}

		ListSelectionModel sm = (ListSelectionModel) evt.getSource ();

		// several reasons to abort
		if (sm.isSelectionEmpty ()) {return;}	// we are on the header
		if (table == null) {return;}	// table is null

		// retrieve selected items in the table
		try {
			int[] indices = table.getSelectedRows ();
			selectedItems = new ArrayList ();
			for (int i = 0; i < indices.length; i++) {
				int numRow = table.convertRowIndexToModel(indices[i]);
				int itemId = ((Integer) table.getModel ().getValueAt (numRow, 0)).intValue ();
				Identifiable item = (Identifiable) id2items.get  (itemId);
				if (item == null) {continue;}	// may occur if table model is empty (1st time)
				selectedItems.add (item);
			}
			//~ boolean ovChanged = false;	// just update ov
			//~ updateViewer (ovChanged);

			// Fire a selection event to tell the selection changed -> they can updates
			boolean selectionActuallyChanged = true;
			fireSelectionEvent (new SelectionEvent (this, selectionActuallyChanged));	// fc - 8.2.2008
				//~ selectionListener.selectionChanged (this, selectedItems);

			thisIsAReselection = false;
			ovSelector.select (selectedItems, thisIsAReselection);
			
		} catch (Exception e) {
			System.out.println ("Error in selection");
			e.printStackTrace (System.out);
		}
	}

	/**	From ActionListener interface.
	*	Buttons management.
	*/
	//~ public void actionPerformed (ActionEvent evt) {
		//~ if (evt.getSource ().equals (viewerCombo)) {
			//~ boolean ovChanged = true;	// dispose current ov and load new ov
			//~ updateViewer (ovChanged);
		//~ }
	//~ }

	// When this viewer updates, see if reselection must
	// be triggered towards OVSelector
	// The OVSelector calls this method when its combo box value changes.
	public void reselect () {		// fc - 23.11.2007
		if (!constructionCompleted) {return;}
System.out.println ("*** TableView.reselect ()...");
		thisIsAReselection = true;
		ovSelector.select (selectedItems, thisIsAReselection);
	}

	// If ovDialog is closed by user, remove any selection marks
	// Unused here, we use a JScrollPane (can not be closed) instead of an OVDialog - fc - 17.9.2008
	public void cancelSelection () {
		ovSelector.select (null, false);
	}

	/**	User interface definition
	*/
	private void createUI () {

		// part1 contains the table
		JPanel part1 = new JPanel (new BorderLayout ());

		tableScrollPane = new JScrollPane (table);
		part1.add (tableScrollPane, BorderLayout.CENTER);

		// part2 contains the object viewer combo choice
		// and the selected object viewer, showing the table selection
		JPanel part2 = new JPanel (new BorderLayout ());
		//~ Collection names = refreshComboViewer ();
		//~ viewerCombo = new JComboBox (new Vector (new TreeSet (names)));
		//~ viewerCombo.addActionListener (this);
		part2.add (ovSelector, BorderLayout.NORTH);

		// Place the scroll pane for the current object viewer
		part2.add (viewerScrollPane, BorderLayout.CENTER);

		// layout parts
		JSplitPane mainPanel = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT);
		mainPanel.setLeftComponent (part1);
		mainPanel.setRightComponent (part2);
		mainPanel.setOneTouchExpandable (true);
		mainPanel.setResizeWeight (0.6);

		mainPanel.setDividerLocation (500);	// divider location

		setLayout (new GridLayout(1, 1));
		add (mainPanel);
	}

	// When selection changed in the table, refresh the combo items
	// It is possible to begin with an empty table -> nothing compatible
	//
	//~ private Collection refreshComboViewer () {
		//~ Collection classNames = extMan.getExtensionClassNames (
				//~ Extension.OBJECT_VIEWER, itemsInTheTable);
		//~ try {
			//~ Object firstItem = itemsInTheTable.iterator ().next ();
			//~ classNames.addAll (extMan.getExtensionClassNames (
					//~ Extension.OBJECT_VIEWER, firstItem));
		//~ } catch (Exception e) {}
		//~ viewerName2viewerClassName = new HashMap ();
		//~ for (Iterator i = classNames.iterator (); i.hasNext ();) {
			//~ String className = (String) i.next ();
			//~ viewerName2viewerClassName.put (extMan.getExtensionName (className), className);
		//~ }
		//~ Collection names = viewerName2viewerClassName.keySet ();

		//~ return names;

	//~ }

// fc - 21.10.2008 - reactivated this, needed in the wqw, was desactivated a bit too fast for OVSelector framework
	// fc - 8.2.2008 - SelectionSource
	public void addSelectionListener (SelectionListener l) {
		if (selectionListeners == null) {selectionListeners = new ArrayList<SelectionListener> ();}
		selectionListeners.add (l);
	}
	public void removeSelectionListener (SelectionListener l) {
		if (selectionListeners == null) {return;}
		selectionListeners.remove (l);
	}
	public void fireSelectionEvent (SelectionEvent e) {
		if (selectionListeners == null) {return;}
		for (SelectionListener l : selectionListeners) {
			l.sourceSelectionChanged (e);
		}
	}
	public Collection getSelection () {
		return selectedItems;
	}
	// fc - 8.2.2008 - SelectionSource

}


