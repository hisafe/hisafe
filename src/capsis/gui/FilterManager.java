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
package capsis.gui;


import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import capsis.app.CapsisExtensionManager;
import capsis.extensiontype.Filter;
import capsis.extensiontype.GrouperDisplay;
import capsis.kernel.GScene;
import capsis.util.Pilotable;
import capsis.util.SmartListModel;
import capsis.util.Updatable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.group.Filtrer;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * Allows to combine filters on a stand Object.
 * This object must be groupable by the capsis grouping system (see capsis.util.Group).
 * 
 * Classical use : to create groupers (see DGrouperDefiner).
 * A list 1 of filters compatible with the stand is proposed.
 * The user can choose a filter in the list and move it to a "selected filters" 
 * list 2. When a filter is selected in list 2, its configuration panel appears. 
 * The user can change parameters in this panel. 
 * It is possible to add other filters in list 2. They are
 * parametered the same way. Only the last filter in list 2 can be modified.
 * A witness display shows the current selection. It is synchronized 
 * when user hits the "update display" button.
 * 
 * @author F. de Coligny - february 2001 / april 2004 / october 2004
 * @see capsis.extension.intervener.filterthinner.FilterThinner
 */
public class FilterManager extends JPanel implements ListSelectionListener, 
		ActionListener, Updatable {
			
	private final static int LIST_HEIGHT = 6;
	private final static int BUTTON_SIZE = 23;

	private boolean isFilterManagerReady;	// set to true at the end of createUI ().

	protected GScene stand;				// we apply filters on this stand
	protected GroupableType type;		// Group type
	private ExtensionManager extMan;	// a reference to extension manager (filters are extensions)

	private JComboBox displayCombo;
	private JPanel displayPanel;
	private GrouperDisplay display;
	private Map displayName2displayClassName;	// display name -> class name
	private String lastDisplay;

	private Collection filters;		// currently selected filters

	// fc - 22.9.2004
	private Collection concernedIndividuals;	// Filters will be applied on these individuals only (maybe all, maybe a group)

	private Map className_order;	// to compute unique names for redondant filters

	private JList list1;	// available filters
	private JList list2;	// user selected filters
	private SmartListModel list1Model;
	private SmartListModel list2Model;
	private Map list1Map;	// list 1 : filter name (human key) - filter reference
	private Map list2Map;	// list 2 : filter name (human key) - filter (with its current settings)
	private Map map2List;	// opposite of list2Map : managed together
	// (Note : "human key" stands for "human readable label, translated if needed")

	private JButton addButton;
	private JButton removeButton;
	private JButton updateDisplayButton;
	private JTextArea description;

	private int list2Index;		// index in list 2 (selected filters)
	private Filter currentFilter;		// the one which is being configured
	private ConfigurationPanel currentConfigPanel;

	private JPanel validationPanel;

	private boolean forgetSelectionInList2;
	private NumberFormat formater;


	/**	Create a filter manager to filter the given stand.
	 *	Stand must be known by the Group class.
	 *	Type is one of the possible group types in the Group class.
	 */
	// ADDED type - fc - 16.9.2004
	public FilterManager (GScene stand, GroupableType type) {
		this (stand, null, type, new ArrayList ());
	}

	/**	Create a filter manager to filter the given stand with the 
	 *	given filters already active.
	 *	If someIndividuals is not null, filters are based on these individuals, 
	 *	possibly coming from a grouper (see FilterThinner intervener).
	 */
	// ADDED type - fc - 16.9.2004
	public FilterManager (GScene stand, Collection someIndividuals, GroupableType type, Collection filters) {
		super (new BorderLayout ());

		isFilterManagerReady = false;		// fc - 20.4.2004

		formater = NumberFormat.getInstance (Locale.ENGLISH);
		formater.setGroupingUsed (false);
		formater.setMaximumFractionDigits (2);

		this.stand = stand;
		this.type = type;
		this.filters = filters;

		lastDisplay = Settings.getProperty ("extension.last.grouper.display.name", (String)null);
		if (someIndividuals == null) {		// fc - 22.9.2004
			concernedIndividuals = Group.whichCollection (stand, type);
		} else {
			concernedIndividuals = someIndividuals;
		}

		extMan = CapsisExtensionManager.getInstance ();
		currentFilter = null;
		className_order = new Hashtable ();

		prepareList1 ();
		prepareList2 ();

		createUI ();
		changeDisplay ();
		sortAction ();		// limited to list1 !
	}

	/**	Manage button actions
	 */
	public void actionPerformed (ActionEvent evt) {
		// Add button: list1 -> list2
		if (evt.getSource ().equals (addButton)) {
			addAction ();
			// Remove button: from list2
		} else if (evt.getSource ().equals (removeButton)) {
			removeAction ();
			// Change the grouperDisplay
		} else if (evt.getSource ().equals (displayCombo)) {
			changeDisplay ();
			// Update the grouperDisplay
		} else if (evt.getSource ().equals (updateDisplayButton)) {
			update ();
		}
	}

	/**	From ListSelectionListener interface.
	 *	Called when selection changes on list 2 (selected filters).
	 */
	private void updateDescription (String text) {
		description.setText (text);
		description.setToolTipText (text);
		description.setCaretPosition (0);
	}

	/**	From ListSelectionListener interface.
	 *	Called when selection changes in lists (available / selected filters).
	 */
	public void valueChanged (ListSelectionEvent evt) {
		if (evt.getValueIsAdjusting ()) {return;}

		// Selection in list1 (available filters)
		if (evt.getSource ().equals (list1)) {
			if (list1Model.getSize () <= 0) {return;}	// fc - 30.4.2003
			String key = getSelectedKey (list1);	// human readable
			if (key == null) {return;}		// fc - 5.4.2004 - may occur (seen in Log)
			try {
				String className = (String) list1Map.get (key);
				// user information
				// descriptionKey = className (without package i.e;"little name)+".description"
				String descriptionKey = AmapTools.getClassSimpleName (className)+".description";
				updateDescription (Translator.swap (descriptionKey));
			} catch (Exception e) {}	// can happen at first display time
			return;
		}

		// Selection in list2 (selected filters)
		// Case of reselection of old option if its config failed: do nothing
		if (forgetSelectionInList2) {
			forgetSelectionInList2 = false;
			return;
		}

		// Make config for current filter before change
		if (!tryFilterConfig ()) {
			// if config failed, reselect current without redoing config
			forgetSelectionInList2 = true;
			list2.setSelectedIndex (list2Index);	// call valueChanged... forgetSelectionInList2 method body
			return;
		}

		if (list2 == null) {return;}
		if (list2Model.getSize () == 0) {return;}
		try {
			list2Index = list2.getSelectedIndex ();
			String key = getSelectedKey (list2);	// human readable
			if (key == null) {return;}		// fc - 5.4.2004 - may occur (seen in Log)
			Filter f = (Filter) list2Map.get (key);
			// user information
			updateDescription (ExtensionManager.getDescription (f.getClass().getName()));
			setCurrentFilter (f);
		} catch (Exception e) {
			Log.println (Log.ERROR, "FilterManager.valueChanged ()", 
					"Exception while setting new current filter",e);
		}

		// Remove only if last index in list
		if (list2Index >= list2Model.getSize () - 1) {
			removeButton.setEnabled (true);
		} else {
			removeButton.setEnabled (false);
		}
	}



	//	Apply the filters to the stand in up-to-bottom order. 
	//	if endFilter is not null, consider only the filters from the beginning 
	//	to endFilter, the latter excluded. 
	//	fc - 4.10.2004
	private Collection doFilter (Filter endFilter) {	// fc - 20.4.2004
		Collection filters = new ArrayList ();
		Iterator i = list2Model.getContents ().iterator ();
		boolean stop = false;
		while (i.hasNext () && !stop) {
			String filterName = (String) i.next ();
			Filter filter = (Filter) list2Map.get (filterName);
			if (endFilter != null 
					&& filter.equals (endFilter)) {
				stop = true;
			} else {
				filters.add (filter);
			}			
		}

		Grouper gr = new Filtrer ("tmp", type, filters);
		return gr.apply (concernedIndividuals);		// fc - 22.9.2004 - to run FilterThinner on a group
	}



	//	Called on add in list 2 (selected filters).
	//
	private Filter createNewFilter (String className, String humanKey) {
		Filter filter = null;
		try {
			filter = (Filter) extMan.instantiate (className);

			list2Map.put (humanKey, filter);
			map2List.put (filter, humanKey);
		} catch (Exception e) {
			Log.println (Log.ERROR, "FilterManager.createNewFilter ()",
					"Exception caught while attepting to load extension "+className, e);
		}
		return filter;
	}




	//	New current may be null
	//
	private void setCurrentFilter (Filter f) {
		if (!isFilterManagerReady) {return;}	// may be called too early by lists building time selections

		// 1. Try to configure previous current filter
		if (f != null) {tryFilterConfig ();}	// fc - 5.10.2004 - if (f != null)

		// 2. Set f as current
		currentConfigPanel = null;
		currentFilter = f;
		if (currentFilter == null) {	// list 2 is empty -> no config panel

			validationPanel.removeAll ();	
			validationPanel.revalidate ();
			validationPanel.repaint ();
			
			return;
		}

		// fc - 20.4.2004 - stand and candidates are now passed only at configuration time
		//
		// Concerned filter is selected in list 2
		Collection c = doFilter (f);	// stop filtering just before f
		//
		Object[] standAndCandidates = new Object[2];
		standAndCandidates[0] = stand;
		standAndCandidates[1] = c;

		currentConfigPanel = ((Configurable) f).getConfigPanel (standAndCandidates);

		validationPanel.removeAll ();	
		validationPanel.add (new JScrollPane (currentConfigPanel), BorderLayout.CENTER);
		
		// If configPanel is Pilotable, retrieve its pilot (generally a toolbar)
		JComponent pilot = null;
		if (currentConfigPanel instanceof Pilotable) {
			pilot = ((Pilotable) currentConfigPanel).getPilot ();
			if (pilot != null) {
				if (pilot instanceof JToolBar) {((JToolBar) pilot).setFloatable (false);}

				validationPanel.add (pilot, BorderLayout.SOUTH);
				
			} 
		}

		// Disable config panel for config view only
		// if intermediate filter (i.e. not last element in list 2)
		if (list2.getSelectedIndex () < list2Model.getSize () - 1) {
			currentConfigPanel.disablePanel ();
		}

		validationPanel.revalidate ();
		validationPanel.repaint ();
	}

	//	Force configuration for current filter
	//	Return false if config failed (in this case, it showed a message for the user)
	//
	public boolean tryFilterConfig () {
		if (currentFilter == null || currentConfigPanel == null) {return true;}
		if (!currentConfigPanel.checksAreOk ()) {return false;}
		((Configurable) currentFilter).applyConfig (currentConfigPanel);
		return true;
	}

	/**	From Updatable interface.
	 */
	public void update (Object source, Object param) {update ();}	// fc - 23.11.2007 - changed Updatable
	public void update () {
		// If trouble while current filter config, abort
		// (current filter is responsible for user explanation (dialog)
		if (!tryFilterConfig ()) {return;}

		filters = getFilters ();	// all the selected filters
		Grouper grouper = new Filtrer ("", type, filters);

		try {
			display.update (stand, grouper);
		} catch (Exception e) {}
	}

	//	Load the display which is selected in the combo box
	//
	private void changeDisplay () {
		try {
			String name = (String) displayCombo.getSelectedItem ();
			String className = (String) displayName2displayClassName.get (name);

			Settings.setProperty ("extension.last.grouper.display.name", name);

			display = (GrouperDisplay) extMan.instantiate(className);
			display.update(stand, null);

			JPanel p = (JPanel) display;

			displayPanel.removeAll ();
			displayPanel.add (p, BorderLayout.CENTER);
			p.revalidate ();
			p.repaint ();

			update ();
		} catch (Exception e) {
			Log.println (Log.WARNING, "FilterManager.setDisplay ()", 
			"exception during display instanciation");
		}
	}

	//	Initialize the whole UI for FilterManager.
	//
	private void createUI () {
		ColumnPanel masterPanel = new ColumnPanel (Translator.swap ("FilterManager.filters"));
		Border etched = BorderFactory.createEtchedBorder ();

		LinePanel filterPanel = new LinePanel ();

		// 1. List 1 : available filters for the given stand
		list1Model = new SmartListModel ();
		try {
			for (Iterator i = list1Map.keySet ().iterator (); i.hasNext ();) {
				String s = (String) i.next ();
				list1Model.add (s);
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DListSelector.createUI ()",
					"Error while preparing list 1 : "+e.toString (), e);
			return;
		}
		list1 = new JList (list1Model);
		list1.addListSelectionListener (this);
		list1.setSelectedIndex (0);
		list1.setVisibleRowCount (LIST_HEIGHT);
		list1.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		
		list1.addMouseListener (new MouseAdapter () {

			public void mouseClicked (MouseEvent e) {
				if (e.getClickCount () == 2) {
					addAction ();
				}
			}
		});
		
		JScrollPane s1 = new JScrollPane (list1);
		JPanel p1 = new JPanel (new BorderLayout ());
		JLabel l1 = new JLabel (Translator.swap ("FilterManager.availableFilters"));
		int labelHeight = l1.getMaximumSize ().height;
		p1.add (l1, BorderLayout.NORTH);
		p1.add (s1, BorderLayout.CENTER);

		filterPanel.add (p1);

		// 2. Second column : "<" and ">" buttons
		JPanel buttonPanel = new JPanel ();
		buttonPanel.setLayout (new BoxLayout (buttonPanel, BoxLayout.Y_AXIS));

		ImageIcon icon = null;
		// fc - 6.11.2008
		icon = IconLoader.getIcon ("go-next_16.png");

		JLabel empty = new JWidthLabel ("", 1);
		empty.setMaximumSize (new Dimension (1, labelHeight));
		buttonPanel.add (empty);

		addButton = new JButton (icon);
//		Tools.setSizeExactly (addButton, BUTTON_SIZE, BUTTON_SIZE);
		AmapTools.setSizeExactly (addButton);
		addButton.setToolTipText (Translator.swap ("FilterManager.add"));
		if (list1.getModel ().getSize () == 0) {addButton.setEnabled (false);}
		addButton.addActionListener (this);
		buttonPanel.add (addButton);

		icon = IconLoader.getIcon ("go-previous_16.png");
		removeButton = new JButton (icon);
//		Tools.setSizeExactly (removeButton, BUTTON_SIZE, BUTTON_SIZE);
		AmapTools.setSizeExactly (removeButton);
		removeButton.setToolTipText (Translator.swap ("FilterManager.remove"));
		removeButton.addActionListener (this);
		buttonPanel.add (removeButton);
		buttonPanel.add (Box.createVerticalGlue ());

		filterPanel.add (buttonPanel);

		// 3. List 2 : user selected filters : none or given group ones
		list2Model = new SmartListModel ();
		try {
			// Iteration on filters : in the good order
			for (Iterator i = filters.iterator (); i.hasNext ();) {
				Filter f = (Filter) i.next ();
				String s = (String) map2List.get (f);
				list2Model.add (s);
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "FilterManager.createUI ()",
					"Error while preparing list 2 : "+e.toString (), e);
			return;
		}
		list2 = new JList (list2Model);
		list2.addListSelectionListener (this);
		list2.setVisibleRowCount (LIST_HEIGHT);
		list2.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		
		list2.addMouseListener (new MouseAdapter () {

			public void mouseClicked (MouseEvent e) {
				if (e.getClickCount () == 2) {
					removeAction ();
				}
			}
		});

		JScrollPane s2 = new JScrollPane (list2);
		s2.setPreferredSize (s1.getPreferredSize ());

		JPanel p2 = new JPanel (new BorderLayout ());
		JLabel l2 = new JLabel (Translator.swap ("FilterManager.selectedFilters"));
		p2.add (l2, BorderLayout.NORTH);
		p2.add (s2, BorderLayout.CENTER);

		filterPanel.add (p2);
		masterPanel.add (filterPanel);	// fc - 1.10.2004
		masterPanel.addStrut0 ();

		// 4. One button to show/remove display

		// 4.1. One button to update the display
		icon = IconLoader.getIcon ("view-refresh_16.png");
		updateDisplayButton = new JButton (icon);
//		Tools.setSizeExactly (updateDisplayButton, BUTTON_SIZE, BUTTON_SIZE);
		AmapTools.setSizeExactly (updateDisplayButton);
		updateDisplayButton.setToolTipText (Translator.swap ("FilterManager.updateDisplay"));
		if (list1.getModel ().getSize () == 0) {updateDisplayButton.setEnabled (false);}
		updateDisplayButton.addActionListener (this);

		//~ buttonPanel.add (Box.createVerticalGlue ());
		//~ buttonPanel.add (updateDisplayButton);

		// 5. Filters description is shown here for user information
		description = new JTextArea (1, 15);
		description.setEditable (false);
		description.setLineWrap (true);
		description.setWrapStyleWord (true);
		JScrollPane descriptionScrollPane = new JScrollPane (description);



		// Display combo box - fc - 5.10.2004
		//
		LinePanel displayControl = new LinePanel ();
		Object[] typeAndStand = new Object[2];
		typeAndStand[0] = type;
		typeAndStand[1] = stand;

		Collection classNames = extMan.getExtensionClassNames (
				CapsisExtensionManager.GROUPER_DISPLAY, typeAndStand);
		displayName2displayClassName = new HashMap ();
		for (Iterator i = classNames.iterator (); i.hasNext ();) {
			String className = (String) i.next ();
			displayName2displayClassName.put (ExtensionManager.getName (className), className);
		}
		Collection names = displayName2displayClassName.keySet ();
		displayCombo = new JComboBox (new Vector (new TreeSet (names)));
		displayCombo.addActionListener (this);
		if (lastDisplay != null) {
			displayCombo.setSelectedItem (lastDisplay);
		}
		//displayCombo.setSelectedItem (...)	// memorized last time in system property
		displayControl.add (displayCombo);
		displayControl.add (updateDisplayButton);
		displayControl.addStrut0 ();


		// fc - 1.10.2004
		displayPanel = new JPanel (new BorderLayout ());

		JPanel chooser = new JPanel (new BorderLayout ());
		chooser.add (masterPanel, BorderLayout.NORTH);

		JPanel displayStuff = new JPanel (new BorderLayout ());	// fc - 25.4.2007
		Border b1 = BorderFactory.createTitledBorder (etched, 	// fc - 25.4.2007
				Translator.swap ("FilterManager.selectionDisplay"));
		displayStuff.setBorder (b1);

		displayStuff.add (displayControl, BorderLayout.NORTH);	// fc - 25.4.2007
		displayStuff.add (displayPanel, BorderLayout.CENTER);	

		JPanel leftPanel = new JPanel (new BorderLayout ());
		leftPanel.add (chooser, BorderLayout.NORTH);
		leftPanel.add (displayStuff, BorderLayout.CENTER);

		setPreferredSize (new Dimension (700, 500));

		validationPanel = new JPanel (new BorderLayout ()) {
			public boolean isValidateRoot () {return true;}
		};
		Border b2 = BorderFactory.createTitledBorder (etched, 	// fc - 25.4.2007
				Translator.swap ("FilterManager.currentFilter"));
		validationPanel.setBorder (b2);							// fc - 25.4.2007
		JSplitPane mainLine = new JSplitPane (JSplitPane.HORIZONTAL_SPLIT, 
				leftPanel, validationPanel);
		mainLine.resetToPreferredSizes ();
		add (mainLine, BorderLayout.CENTER);

		add (descriptionScrollPane, BorderLayout.SOUTH);

		// If list2 is empty, disable remove button, else select first line
		if (list2Model.getSize () == 0) {
			removeButton.setEnabled (false);
		} else {
			list2.setSelectedIndex (list2Model.getSize ()-1);
		}

		isFilterManagerReady = true;		// fc - 20.4.2004

		// If some filters are passed in constructor (in list 2), set last one as current
		if (!filters.isEmpty ()) {
			for (Iterator i = filters.iterator (); i.hasNext ();) {
				Filter filter = (Filter) i.next ();
				if (!i.hasNext ()) {
					setCurrentFilter (filter);	// last filter in collection is current
					break;
				}
			}
		}

	}

	//--- tool methods ----------------------------------------------------------------
	//--- tool methods ----------------------------------------------------------------
	//--- tool methods ----------------------------------------------------------------
	//--- tool methods ----------------------------------------------------------------
	//--- tool methods ----------------------------------------------------------------

	//	Prepare list 1 : available filters.
	//
	private void prepareList1 () {
		// Retrieve filters matching with the stand (class names)
		// Note: after this call, all filter classes are loaded (Translator usable)
		Collection filtersClassNames = 
			extMan.getExtensionClassNames (CapsisExtensionManager.FILTER, concernedIndividuals);

		// Prepare map list1Map
		list1Map = new Hashtable ();
		for (Iterator i = filtersClassNames.iterator (); i.hasNext ();) {
			String className = (String) i.next ();
			String humanKey = ExtensionManager.getName (className);
			list1Map.put (humanKey, className);
		}
	}

	//	Prepare list 2 : selected filters.
	//	Empty list or current group filters (depending on constructor used).
	//
	private void prepareList2 () {
		list2Map = new Hashtable ();
		map2List = new Hashtable ();

		for (Iterator i = filters.iterator (); i.hasNext ();) {
			Filter filter = (Filter) i.next ();
			String humanKey = getUniqueName (filter.getClass ().getName ());
			list2Map.put (humanKey, filter);
			map2List.put (filter, humanKey);
		}
	}

	//	Return the selected key in list.
	//
	private String getSelectedKey (JList list) {
		try {
			int i = list.getSelectedIndex ();
			String key = (String) list.getModel ().getElementAt (i);
			return key;
		} catch (Exception e) {
			return null;
		}
	}

	//	Build an unique name for the new filter
	//	(maybe several occurences for the same className).
	//
	private String getUniqueName (String className) {
		String suffix = "";
		int order = 0;
		try {
			order = ((Integer) className_order.get (className)).intValue ();
		} catch (Exception e) {}

		order++;
		if (order > 1) {suffix = " ("+order+")";}

		className_order.put (className, new Integer (order));
		return ExtensionManager.getName (className)+suffix;
	}








	//	Click on ">".
	//
	private void addAction () {
		if (list1Model.isEmpty ()) {return;}		// empty list
		if (list1.isSelectionEmpty ()) {return;}	// no selection
		if (!tryFilterConfig ()) {
			return;}			// bad current config

		// Process moves
		String value = (String) list1.getSelectedValue ();	// list1 : single selection only
		String className = (String) list1Map.get (value);
		String humanKey = getUniqueName (className);
		createNewFilter (className, humanKey);

		list2Model.add (humanKey);

		// Deal with selection in list2 after adding
		if (!list2Model.isEmpty ()) {
			list2.setSelectedIndex (list2Model.getSize () - 1);
			list2.ensureIndexIsVisible (list2.getSelectedIndex ());
		}

		//~ removeButton.setEnabled (true);
	}

	//	Click on "<".
	//
	private void removeAction () {
		if (list2Model.isEmpty ()) {return;}		// empty list
		if (list2.isSelectionEmpty ()) {return;}	// no selection
		//if (!tryFilterConfig ()) {return;}					// bad current config does not matter

		// Find selected index in list2
		int iTo = list2.getSelectedIndex ();
		String value = (String) list2.getSelectedValue ();

		currentFilter = null;	// won't be reconfigured in next setCurrentFilter ()

		list2Model.remove (value);					// clean list2Model
		Filter f = (Filter) list2Map.get (value);
		list2Map.remove (value);					// clean list2Map
		map2List.remove (f);						// clean map2List

		// Deal with selections after remove
		if (!list2Model.isEmpty ()) {
			if (iTo < list2Model.getSize ()) {
				list2.setSelectedIndex (iTo);
			} else {
				list2.setSelectedIndex (list2Model.getSize () - 1);
			}
			list2.ensureIndexIsVisible (list2.getSelectedIndex ());
		} else {
			setCurrentFilter (null);	// if empty list, no valueChanged fired -> manual
			removeButton.setEnabled (false);
		}

	}







	//	Sort action is limited to list1. Selected filters must never be sorted !
	//
	private void sortAction () {
		if (!tryFilterConfig ()) {return;}	// current filter config must be ok, else cancel

		java.util.List fromValues = list1Model.getContents ();

		Collections.sort (fromValues);	// Strings -> "natural" order

		list1Model.clear ();
		list1Model.addAll (fromValues);

		if (!list1Model.isEmpty ()) {
			list1.setSelectedIndex (0);
			list1.ensureIndexIsVisible (list1.getSelectedIndex ());
		}
	}

	/**	Return currently selected filters with their current settings.
	 */
	public Collection getFilters () {
		Collection filters = new ArrayList ();
		// Look for selected filters
		Collection list2Values = list2Model.getContents ();
		for (Iterator i = list2Values.iterator (); i.hasNext ();) {
			String humanKey = (String) i.next ();
			Filter f = (Filter) list2Map.get (humanKey);
			filters.add (f);
		}
		return filters;
	}

	/**	Return ids of the filtered objects in a Collection.
	 */
	public Collection getFilteredIds () {
		Collection result = new Vector ();

		Collection c = doFilter (null);	// endFilter = null => all selected filters are considered

		for (Iterator i = c.iterator (); i.hasNext ();) {
			Identifiable ide = (Identifiable) i.next ();
			result.add (new Integer (ide.getId ()));
		}
		return result;
	}

	/**	In case some tool dialog to be disposed.
	 */
	public void dispose () {}

	// fc - 1.3.2005
	public ConfigurationPanel getCurrentConfigPanel () {return currentConfigPanel;}


}
