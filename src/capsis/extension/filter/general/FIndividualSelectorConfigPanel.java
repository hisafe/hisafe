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

package capsis.extension.filter.general;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.util.Controlable;
import capsis.util.NumberStringComparator;
import capsis.util.SmartListModel;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * ConfigurationPanel for Individual Selector.
 * 
 * @author F. de Coligny - may 2002
 */
public class FIndividualSelectorConfigPanel extends ConfigurationPanel 
		implements ActionListener, Controlable, ListDataListener {
	private final static int LIST_HEIGHT = 6;
	private final static int CELL_WIDTH = 60;

	FIndividualSelector filter;		// this filter is under configuration
	
	private Object referent;
	private Collection candidates;

	private String dlgTitle;
	private String dlgText;
	private String empty;
	private Vector fromVector;
	private Vector selectedVector;

	private JTextField fromNumber;
	private JTextField toNumber;
	private JList fromList;
	private JList toList;
	private SmartListModel fromModel;
	private SmartListModel toModel;
	private int fromSize;
	private int toSize;

	private JButton addButton;
	private JButton removeButton;
	private JButton addAllButton;
	private JButton removeAllButton;
	private JButton sortButton;
	private JButton helpButton;


	/**	Constructor
	*/
	public FIndividualSelectorConfigPanel (Configurable configurable) {
		super (configurable);
		
		filter = (FIndividualSelector) configurable;
		
		LinePanel master = new LinePanel ();
		
		referent = filter.referent;
		candidates = filter.candidates;
		
		// Memo: current selected ones are in toIds
		
		// 1. prepare from list
		fromModel = new SmartListModel ();
		
		try {
			for (Iterator i = candidates.iterator (); i.hasNext ();) {
				Identifiable item = (Identifiable) i.next ();
				if (item != null) {
					StringBuffer s = new StringBuffer (""+item.getId ());
					if (item instanceof Numberable) {
						s.append (" (number=");
						s.append (((Numberable) item).getNumber ());
						s.append (")");
					}
					fromModel.add (s.toString ());
				}
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "FIndividualSelectorConfigPanel.c ()", 
					"Exception while preparing from list", e);
			return;
		}
		
		// 2. first column : prepare from list
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("Shared.unSelected")+" : "));
		fromNumber = new JTextField (2);
		fromNumber.setEditable (false);
		l1.add (fromNumber);
		l1.addGlue ();
		
		fromList = new JList (fromModel);
		fromList.setSelectedIndex(0);
		fromList.setVisibleRowCount (LIST_HEIGHT);
		fromList.setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		JScrollPane panFrom = new JScrollPane (fromList);
		JPanel aux1 = new JPanel (new BorderLayout ());
		aux1.add (l1, BorderLayout.SOUTH);
		aux1.add (panFrom, BorderLayout.CENTER);
		master.add (aux1);
		
		// 3. second column : buttons
		JPanel boxButtons = new JPanel ();
		boxButtons.setLayout (new BoxLayout (boxButtons, BoxLayout.Y_AXIS));
		
		ImageIcon icon = null;
		// fc - 6.11.2008
		icon = IconLoader.getIcon ("add-one_16.png");
		addButton = new JButton (icon);
		addButton.addActionListener (this);
		addButton.setToolTipText (Translator.swap ("FIndividualConfigPanel.add"));
		//~ Tools.setSizeExactly (addButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (addButton);
		
		icon = IconLoader.getIcon ("remove-one_16.png");
		removeButton = new JButton (icon);
		removeButton.addActionListener (this);
		removeButton.setToolTipText (Translator.swap ("FIndividualConfigPanel.remove"));
		//~ Tools.setSizeExactly (removeButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (removeButton);
		
		icon = IconLoader.getIcon ("add-all_16.png");
		addAllButton = new JButton (icon);
		addAllButton.addActionListener (this);
		addAllButton.setToolTipText (Translator.swap ("FIndividualConfigPanel.addAll"));
		//~ Tools.setSizeExactly (addAllButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (addAllButton);
		
		icon = IconLoader.getIcon ("remove-all_16.png");
		removeAllButton = new JButton (icon);
		removeAllButton.addActionListener (this);
		removeAllButton.setToolTipText (Translator.swap ("FIndividualConfigPanel.removeAll"));
		//~ Tools.setSizeExactly (removeAllButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (removeAllButton);
		
		icon = IconLoader.getIcon ("go-down_16.png");
		sortButton = new JButton (icon);
		sortButton.addActionListener (this);
		sortButton.setToolTipText (Translator.swap ("FIndividualConfigPanel.sort"));
		//~ Tools.setSizeExactly (sortButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (sortButton);
		
		icon = IconLoader.getIcon ("help_16.png");
		helpButton = new JButton (icon);
		//~ Tools.setSizeExactly (helpButton, BUTTON_SIZE, BUTTON_SIZE);
		Tools.setSizeExactly (helpButton);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);
		
		boxButtons.add (addButton);
		boxButtons.add (removeButton);
		boxButtons.add (addAllButton);
		boxButtons.add (removeAllButton);
		boxButtons.add (sortButton);
		boxButtons.add (helpButton);
		boxButtons.add (Box.createVerticalGlue ());
		master.add (boxButtons);
		
		// 3.1 ensure same width for all buttons.
		//     BoxLayout tries to enlarge all the components to the biggest's size if maximumSize allows it
		Component c[] = boxButtons.getComponents ();
		int wMax = 0;
		for (int i = 0; i < c.length; i++) {
			try {
				if (c[i] instanceof JButton) {
					JButton b = (JButton) c[i];
					if (b.getPreferredSize ().width > wMax) {
						wMax = b.getPreferredSize ().width;
					}
				}
			} catch (Exception e) {
				Log.println (Log.WARNING, "FIndividualSelectorConfigPanel.createUI ()",
						"Trouble while resizing buttons : "+e.toString ());
			}
		}
		for (int i = 0; i < c.length; i++) {
			try {
				if (c[i] instanceof JButton) {
					JButton b = (JButton) c[i];
					b.setMaximumSize (new Dimension (wMax, b.getPreferredSize ().height));
				}
			} catch (Exception e) {}
		}
		
		// 4. third column : prepare to list
		toModel = new SmartListModel ();
		
		try {
			for (Iterator i = filter.toIds.iterator (); i.hasNext ();) {
				Integer id = (Integer) i.next ();
				String s = id.toString ();
				toModel.add (s);
				fromModel.remove (s);
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "FIndividualSelectorConfigPanel.c ()", 
					"Exception while preparing to list", e);
			return;
		}
		
		LinePanel l2 = new LinePanel ();
		l2.add (new JLabel (Translator.swap ("Shared.selected")+" : "));
		toNumber = new JTextField (2);
		toNumber.setEditable (false);
		l2.add (toNumber);
		l2.addGlue ();
		
		toList = new JList (toModel);
		toList.setSelectedIndex(0);
		toList.setVisibleRowCount (LIST_HEIGHT);
		toList.setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		JScrollPane panTo = new JScrollPane (toList);
		panTo.setPreferredSize (panFrom.getPreferredSize ());
		
		panFrom.setPreferredSize (panTo.getPreferredSize ());
		
		JPanel aux2 = new JPanel (new BorderLayout ());
		aux2.add (l2, BorderLayout.SOUTH);
		aux2.add (panTo, BorderLayout.CENTER);
		master.add (aux2);
		master.addStrut0 ();
		
		ColumnPanel col = new ColumnPanel ();
		col.add (master);
		col.addStrut0 ();
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (col, BorderLayout.CENTER);
		//~ getContentPane ().setPreferredSize (new Dimension (300, 300));	// will be put inside a scrollpane
		
		fromModel.addListDataListener (this);
		toModel.addListDataListener (this);
		fromNumber.setText (""+fromModel.getSize ());
		toNumber.setText (""+toModel.getSize ());
		
		sortAction ();
		getContentPane ().revalidate ();
		repaint ();
	}

	//	Manage list data changes
	//
	private void listChanged (ListDataEvent evt) {
		if (evt.getSource ().equals (fromModel)) {
			fromNumber.setText (""+fromModel.getSize ());
		} else if (evt.getSource ().equals (toModel)) {
			toNumber.setText (""+toModel.getSize ());
		}
	}

	/**	ListDataListener
	*/
	public void contentsChanged (ListDataEvent evt) {listChanged (evt);}

	/**	ListDataListener
	*/
	public void intervalAdded (ListDataEvent evt) {listChanged (evt);}

	/**	ListDataListener
	*/
	public void intervalRemoved (ListDataEvent evt) {listChanged (evt);}

	/**	Redirection of events from panel buttons.
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (addButton)) {				// add button
			addAction ();
		} else if (evt.getSource ().equals (removeButton)) {	// remove button
			removeAction ();
		} else if (evt.getSource ().equals (addAllButton)) {	// add all button
			addAllAction ();
		} else if (evt.getSource ().equals (removeAllButton)) {	// remove all button
			removeAllAction ();
		} else if (evt.getSource ().equals (sortButton)) {		// sort button
			sortAction ();
		} else if (evt.getSource ().equals (helpButton)) {		// help button
			Helper.helpFor (this);
		}
	}

	/**	Set the two lists models mute or verbose.
	*/
	private void setModelsMute (boolean b) {
		fromModel.setMute (b);
		toModel.setMute (b);
	}

	/**	Add elements list1 -> list2
	*/
	private void addAction () {
		// find min selected index in fromList
		int[] tab = fromList.getSelectedIndices ();
		int iFrom = tab[0];
		
		// process moves
		int i = 0;
		Object[] values = fromList.getSelectedValues ();
		if (values.length > 1) {setModelsMute (true);}
		for (i = 0; i < values.length; i++) {
			fromModel.remove (values[i]);
			toModel.add (values[i]);
		}
		if (values.length > 1) {setModelsMute (false);}
		
		// deal with selections after moves in both lists
		if (!toModel.isEmpty ()) {
			toList.setSelectedIndex (toModel.getSize () - 1);
			toList.ensureIndexIsVisible (toList.getSelectedIndex ());
		}
		if (!fromModel.isEmpty ()) {
			if (iFrom < fromModel.getSize ()) {
				fromList.setSelectedIndex (iFrom);
			} else {
				fromList.setSelectedIndex (fromModel.getSize () - 1);
			}
			fromList.ensureIndexIsVisible (fromList.getSelectedIndex ());
			
		}
	}

	/**	Remove elements from list2
	*/
	private void removeAction () {
		// find max selected index in toList
		int[] tab = toList.getSelectedIndices ();
		int iTo = tab[0];
		
		int i = 0;
		Object[] values = toList.getSelectedValues ();
		if (values.length > 1) {setModelsMute (true);}
		for (i = 0; i < values.length; i++) {
			toModel.remove (values[i]);
			fromModel.add (values[i]);
		}
		if (values.length > 1) {setModelsMute (false);}
		
		// deal with selections after moves in both lists
		if (!fromModel.isEmpty ()) {
			fromList.setSelectedIndex (fromModel.getSize () - 1);
			fromList.ensureIndexIsVisible (fromList.getSelectedIndex ());
		}
		if (!toModel.isEmpty ()) {
			if (iTo < toModel.getSize ()) {
				toList.setSelectedIndex (iTo);
			} else {
				toList.setSelectedIndex (toModel.getSize () - 1);
			}
			toList.ensureIndexIsVisible (toList.getSelectedIndex ());
		}
	}

	/**	Add all element in list1 to list2	
	*/
	private void addAllAction () {
		int i = 0;
		Collection values = fromModel.getContents ();
		toModel.addAll (values);

		fromModel.clear ();
		if (!toModel.isEmpty ()) {
			toList.setSelectedIndex (toModel.getSize () - 1);
			toList.ensureIndexIsVisible (toList.getSelectedIndex ());
		}
	}

	/**	Remove all elements from list2
	*/
	private void removeAllAction () {
		int i = 0;
		Collection values = toModel.getContents ();
		fromModel.addAll (values);
		
		toModel.clear ();
		if (!fromModel.isEmpty ()) {
			fromList.setSelectedIndex (fromModel.getSize () - 1);
			fromList.ensureIndexIsVisible (fromList.getSelectedIndex ());
		}
	}

	/**	Sort list1 and list2
	*/
	private void sortAction () {
		java.util.List fromValues = fromModel.getContents ();
		java.util.List toValues = toModel.getContents ();
		
		Comparator comparator = new NumberStringComparator ();
		
		Collections.sort (fromValues, comparator);
		Collections.sort (toValues, comparator);
		
		fromModel.clear ();
		fromModel.addAll (fromValues);
		
		toModel.clear ();
		toModel.addAll (toValues);
		
		if (!fromModel.isEmpty ()) {
			fromList.setSelectedIndex (0);
			fromList.ensureIndexIsVisible (fromList.getSelectedIndex ());
		}
		if (!toModel.isEmpty ()) {
			toList.setSelectedIndex (0);
			toList.ensureIndexIsVisible (toList.getSelectedIndex ());
		}
		
	}


	/**	Create and return the list2 ids Collection (Integers)
	*/
	public Collection getToIds () {
		Collection toIds = new HashSet ();
		// prepare vector from list2
		Collection ids = toModel.getContents ();
		for (Iterator i = ids.iterator (); i.hasNext ();) {
			String s = (String) i.next ();
			if (s.indexOf (" ") != -1) {		// fc - 26.3.2004 - "12 - dbh=3.45" -> "12"
				s = s.substring (0, s.indexOf (" "));
			}
			toIds.add (new Integer (s));
		}
		return toIds;
	}


	/**	From Controlable interface.
	*/
	public boolean isControlSuccessful () {return checksAreOk ();}
	
	/**	Test proposed configuration: if trouble, show message, then return false.
	*/
	public boolean checksAreOk () {
		// Check: ask a confirmation in some cases:
		// 1. toList is empty
		if (toModel.isEmpty ()) {
			JOptionPane.showMessageDialog (this, 
				Translator.swap ("FIndividualSelector.targetListIsEmpty"),
				ExtensionManager.getName(filter.getClass().getName()),	// extension name in title bar 
				JOptionPane.WARNING_MESSAGE );
			return false;
		}
		
		// 2. Selection in fromList is multiple
		if (fromList.getSelectedIndices ().length > 1) {
			JOptionPane.showMessageDialog (this, 
				Translator.swap ("FIndividualSelector.selectionInFromListIsMultiple"),
				ExtensionManager.getName(filter.getClass().getName()),	// extension name in title bar 
				JOptionPane.WARNING_MESSAGE );
			return false;
		}
		
		// 3. Selection in toList is multiple
		if (toList.getSelectedIndices ().length > 1) {
			JOptionPane.showMessageDialog (this, 
				Translator.swap ("FIndividualSelector.selectionInToListIsMultiple"),
				ExtensionManager.getName(filter.getClass().getName()),	// extension name in title bar 
				JOptionPane.WARNING_MESSAGE );
			return false;
		}
		
		return true;
	}
	
}


