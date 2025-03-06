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
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.WindowConstants;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.extension.filter.general.FIndividualSelector;
import capsis.kernel.Step;
import capsis.util.Redoable;
import capsis.util.Undoable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.group.Filtrer;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import capsis.util.group.Identifier;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * DGrouperDefiner is a dialog box to build a grouper.
 * 
 * @author F. de Coligny - June 2000 / april 2004
 */
public class DGrouperDefiner extends AmapDialog implements ActionListener, ChangeListener {

	private Step step;				// reference step
	private GroupableType grouperType;		// groupers concern TreeList.GROUP_ALIVE_TREE, Group.CELL (...)
	private Grouper grouper;
	private Grouper newGrouper;
	
	private FilterManager filterManager;
	private String memoName;		// name of grouper being modified (in case of grouper modif)

	private JTabbedPane part1;
	private JPanel card1;
	private String card2Title;
	private JTextField grouperName;
	//private ButtonGroup rdGroup1;			// action  : new grouper / existing grouper (modify)
	private ButtonGroup rdGroup2;		// individual : tree / cell / ...
	private ButtonGroup rdGroup3;		// type             : dynamic / static
	
	//private JRadioButton rdTree;	// radio buttons
	//private JRadioButton rdCell;
	private Map translation2type;	// ex: "poissons" -> "fish"
	private Map type2radioButton;	// ex: "fish" -> matching radio button
	
	private JRadioButton rdStatic;	// static grouper retains only ids
	private JRadioButton rdDynamic;
	
	private JButton ok;				// main controls
	private JButton cancel;
	private JButton help;


	/**	Constructor 1 (new grouper), reference step and grouper type.
	*/
	public DGrouperDefiner (Step step, GroupableType grouperType) {
		this (step, grouperType, null);
	}
	
	/**	Constructor 2 (modify grouper), reference step and grouper.
	*/
	public DGrouperDefiner (Step step, Grouper grouper) {
		this (step, null, grouper);
	}
	
	/**	General constructor
	*/
	public DGrouperDefiner (Step step, GroupableType grouperType, Grouper grouper) {
		super ();
		
		this.step = step;
		this.grouper = grouper;
		
		if (grouper != null) {
			this.grouperType = grouper.getType ();
			this.memoName = grouper.getName ();
		} else if (grouperType != null) {
			this.grouperType = grouperType;
		} else {
			Collection types = Group.getPossibleTypes (step.getScene ());
			if (types != null && !types.isEmpty ()) {	// fc - 2.6.2008
				this.grouperType = (GroupableType) types.iterator ().next ();
			} else {									// fc - 2.6.2008
				this.grouperType = null; // fc-1.7.2019
//				this.grouperType = "";					// fc - 2.6.2008
			}											// fc - 2.6.2008
			
			//~ this.grouperType = TreeList.GROUP_ALIVE_TREE;	// fc - 23.9.2004
		}
		
		createUI ();
		
		//setTitle (step.getCaption ()+" - "+Translator.swap ("DGrouperDefiner"));
		
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				prepareClosing ();
				setValidDialog (false);
			}
		});
		
		
		setModal (true);
		
		// fc-28.6.2019
		activateSizeMemorization(this.getClass ().getName ());
		activateLocationMemorization(this.getClass ().getName ());
		
		pack ();	// uses component's preferredSize
		setVisible (true);
	}

	/**	Called on ctrl-Z. Redefines AmapDialog.ctrlZPressed ()
	*/
	protected void ctrlZPressed () {	// fc - 1.3.2005
		if (filterManager != null) {
			ConfigurationPanel p = filterManager.getCurrentConfigPanel ();
			if (p != null && p.isEnabled () && p instanceof Undoable) {
				Undoable u = (Undoable) p;
				u.undo ();
			}
		}
	}

	/**	Called on ctrl-R. Redefines AmapDialog.ctrlRPressed ()
	*/
	protected void ctrlRPressed () {	// fc - 2.3.2005
		if (filterManager != null) {
			ConfigurationPanel p = filterManager.getCurrentConfigPanel ();
			if (p != null && p.isEnabled () && p instanceof Redoable) {
				Redoable r = (Redoable) p;
				r.redo ();
			}
		}
	}

	//	Close preparation.
	//
	private void prepareClosing () {
		try {
			filterManager.dispose ();
		} catch (Exception e) {}
		Settings.setProperty ("capsis.grouper.definer.last.card", ""+part1.getSelectedIndex ());
	}

	//	Ok button.
	//
	private void okAction () {
		
		// FilterManager can give its parametered filters... (1)
		Collection filters = filterManager.getFilters ();
		
		// Are there filters ?
		if (filters == null || filters.isEmpty ()) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DGrouperDefiner.defineAGrouper"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		// Check filters configuration and abort ok if trouble 
		// (filter is responsible for information dialog about trouble)
		if (!filterManager.tryFilterConfig ()) {return;}
		
		// Do we have a name for grouper ?
		String name = null;
		
		// We consider grouper name in textfield : it may have been changed by user
		if (grouperName == null || Check.isEmpty (grouperName.getText ().trim ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DGrouperDefiner.defineGrouperName"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;		// wrong name was typed in
		} else {
			name = grouperName.getText ().trim ();	// name was typed in
			name = Tools.setFirstLetterUpperCase (name);
		}
		
		// What is grouper type ?
		// (fc - 1.4.2004 - to be enhanced if more types...)
		//
		/*if (isRadioSelected (rdGroup2, rdTree)) {
			grouperType = TreeList.GROUP_ALIVE_TREE;
		} else {
			grouperType = Group.CELL;
		}*/
		// NEW - 6.9.2004
		Iterator ts = type2radioButton.keySet ().iterator ();
		Iterator bs = type2radioButton.values ().iterator ();
		while (ts.hasNext () && bs.hasNext ()) {
			GroupableType t = (GroupableType) ts.next ();
			JRadioButton b = (JRadioButton) bs.next ();
			if (isRadioSelected (rdGroup2, b)) {
				grouperType = t;
				break;
			}
		}
		// NEW - 6.9.2004

//~ System.out.println ("grouperName "+name+" grouperType "+grouperType);
		
		// Is grouper static or dynamic ?
		//
		if (isRadioSelected (rdGroup3, rdStatic)) {
			// (1)...or the corresponding elements ids
			Collection ids = filterManager.getFilteredIds ();
			//~ newGrouper = new StaticGroup (name, grouperType, ids);
			newGrouper = new Identifier (name, grouperType, ids);
		} else {
			//~ newGrouper = new DynamicGroup (name, grouperType, filters);
			newGrouper = new Filtrer (name, grouperType, filters);
		}
		
//~ System.out.println ("newGrouper "+newGrouper);
		
		// If grouper modification, delete old one
		if (grouper != null) {
			GrouperManager.getInstance ().remove (memoName);
		}
		
		GrouperManager.getInstance ().add (newGrouper);
		
		prepareClosing ();
		setValidDialog (true);
	}

	/**	Retrieve events and calls matching methods. 
	*/
	public void actionPerformed (ActionEvent evt) {
		
		// if source is a "group type" radio button
		if (type2radioButton.values ().contains (evt.getSource ())) {
			rdiAction ((JRadioButton) evt.getSource ());
			
		/*if (evt.getSource ().equals (rdTree)) {
			rdTreeAction ();
			
		} else if (evt.getSource ().equals (rdCell)) {
			rdCellAction ();*/
			
		} else if (evt.getSource ().equals (ok)) {
			okAction ();	// if correct, grouper is then available
			
		} else if (evt.getSource ().equals (cancel)) {
			prepareClosing ();
			setValidDialog (false);
		
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	//	Initialize the GUI.
	//
	private void createUI () {
		
		// Create the tabbed pane
		part1 = new JTabbedPane (JTabbedPane.TOP);
		part1.setTabLayoutPolicy (JTabbedPane.SCROLL_TAB_LAYOUT);	// fc - 2.4.2003
		
		part1.addChangeListener (this);
		
		// 1. Grouper card
		//
		card1 = new JPanel (new BorderLayout ());
		String card1Title = Translator.swap ("DGrouperDefiner.grouper");
		
		ColumnPanel content = new ColumnPanel ();
		
		LinePanel l0 = new LinePanel ();
		l0.add (new JWidthLabel (Translator.swap ("DGrouperDefiner.grouperName")+" :", 50));
		grouperName = new JTextField(15);
		if (grouper != null) {
			grouperName.setText (grouper.getName ());
		}
		
		l0.add (grouperName);
		l0.addStrut0 ();
		
		ColumnPanel p1 = new ColumnPanel ();
		p1.setBorder (new TitledBorder (Translator.swap ("DGrouperDefiner.grouperIdentification")));
		p1.add (l0);
		p1.addStrut0 ();
		content.add (p1);
		
		// 1.2. Grouper individual type
		//
		ColumnPanel p2 = new ColumnPanel ();
		p2.setBorder (new TitledBorder (Translator.swap ("DGrouperDefiner.elements")));
		
		translation2type = new HashMap ();
		type2radioButton = new HashMap ();
		
		rdGroup2 = new ButtonGroup ();

		// fc-1.7.2019
		Collection<GroupableType> types = Group.getPossibleTypes (step.getScene ());
		
		for (Iterator<GroupableType> i = types.iterator (); i.hasNext ();) {
			GroupableType type = i.next ();
			
			LinePanel li = new LinePanel ();

			String translation = type.getName (); // fc-1.7.2019
//			String translation = Translator.swap (type);

			JRadioButton rdi = new JRadioButton (translation);
			rdi.addActionListener (this);
			li.add (rdi);
			li.addGlue ();
			
			p2.add (li);
			
			translation2type.put (translation, type);
			type2radioButton.put (type, rdi);
			
			rdGroup2.add (rdi);
			
		}
		
		// Selected type button
		try {
			JRadioButton selectedButton = (JRadioButton) type2radioButton.get (grouperType);
			rdGroup2.setSelected (selectedButton.getModel (), true);
			
		} catch (Exception e) {	// fc - 7.9.2004 - lines upper may clash
			Collection c = type2radioButton.values ();
			if (c != null && !c.isEmpty ()) {
				JRadioButton selectedButton = (JRadioButton) c.iterator ().next ();	// first button
				if (selectedButton != null) {		// fc - 2.6.2008
					rdGroup2.setSelected (selectedButton.getModel (), true);
				}									// fc - 2.6.2008
			}
		}
		
		/*LinePanel l1 = new LinePanel ();
		LinePanel l2 = new LinePanel ();
		rdGroup2 = new ButtonGroup ();
		rdTree = new JRadioButton (Translator.swap (TreeList.GROUP_ALIVE_TREE));
		rdCell = new JRadioButton (Translator.swap (Group.CELL));
		rdCell.setEnabled (false);
		rdTree.addActionListener (this);
		rdCell.addActionListener (this);
		rdGroup2.add (rdTree);
		rdGroup2.add (rdCell);
		if (grouperType == TreeList.GROUP_ALIVE_TREE) {
			rdGroup2.setSelected (rdTree.getModel (), true);
		} else {
			rdGroup2.setSelected (rdCell.getModel (), true);
		} 		
		l1.add (rdTree);
		l1.addGlue ();
		l2.add (rdCell);
		l2.addGlue ();
		
		p2.add (l1);
		p2.add (l2);*/
		
		p2.addStrut0 ();
		content.add (p2);
		
		// 1.3. Grouper type
		//
		LinePanel l3 = new LinePanel ();
		LinePanel l4 = new LinePanel ();
		rdGroup3 = new ButtonGroup ();
		rdStatic = new JRadioButton (Translator.swap ("DGrouperDefiner.static"));
		rdDynamic = new JRadioButton (Translator.swap ("DGrouperDefiner.dynamic"));
		rdGroup3.add (rdStatic);
		rdGroup3.add (rdDynamic);
		
		if (grouper != null) {
			if (grouper instanceof Filtrer) {
				rdGroup3.setSelected (rdDynamic.getModel (), true);
			} else {
				rdGroup3.setSelected (rdStatic.getModel (), true);
			}
		} else {
			rdGroup3.setSelected (rdDynamic.getModel (), true);		// Default
		}
		
		l3.add (rdDynamic);
		l3.addGlue ();
		l4.add (rdStatic);
		l4.addGlue ();
		
		ColumnPanel p3 = new ColumnPanel ();
		p3.setBorder (new TitledBorder (Translator.swap ("DGrouperDefiner.type")));
		p3.add (l3);
		p3.add (l4);
		p3.addStrut0 ();
		content.add (p3);
		
		card1.add (content, BorderLayout.NORTH);
		part1.addTab (card1Title, null, card1, null);
		
		// 2. Group definition 
		Box card2 = Box.createVerticalBox ();
		card2Title = Translator.swap ("DGrouperDefiner.definition");
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.CENTER);
		
		// Create filter manager in second tab according to current options
		createFilterManager ();
		
		// 3. control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		// try to reselect last selected card
		try {
			// fc-4.10.2021 Settings
			part1.setSelectedIndex (new Integer (Settings.getProperty 
					("capsis.grouper.definer.last.card", 0)).intValue ());
//			part1.setSelectedIndex (new Integer (System.getProperty 
//					("capsis.grouper.definer.last.card")).intValue ());
		} catch (Exception e) {}
		
	}

	//	fc - 1.4.2004 - The object being filtered.
	//	Must be known by capsis.util.Group "which" methods.
	//	To be enhanced to filter other individual types.
	//
	//~ private Object getObjectToBeFiltered () {
		
		//~ return (Group.getComposite (step.getStand (), grouperType));
	//~ }

	//	Change the card "definition" according to user choices on first card.
	//	When user chooses an existing grouper, list "selected" of this card must
	//	contain the filters of the given grouper. It must be empty if new grouper
	//	construction.
	//
	private void replaceTab (JTabbedPane pane, String title, Icon icon, 
			Component component, String toolTipText) {
		// Where is the card ?
		int index = pane.indexOfTab (card2Title);
		
		// Add or Remove/Insert card
		if (index == -1) {	// first time, add
			pane.addTab (card2Title, null, component, null);
		} else {			// else, replace
			pane.removeTabAt (index);
			pane.insertTab (card2Title, null, component, null, index);
		}
	}

	//	Default options if new grouper, grouper options if known grouper
	//
	private void synchronizeOptions (Grouper g) {
		// We are going to create a new grouper
		if (g == null) {
			// Default values for options
			//
			// Enable possibility to change grouper type
			rdDynamic.setEnabled (true);
			rdStatic.setEnabled (true);
			
			// Enable possibility to change Grouper individual type
			Collection c = type2radioButton.values ();		// fc - 2.6.2008
			if (c != null & !c.isEmpty ()) {				// fc - 2.6.2008
				JRadioButton first = (JRadioButton) c.iterator ().next ();
				first.setEnabled (true);	// NEW - 6.9.2004
			}												// fc - 2.6.2008
			/*if (step.getStand ().hasPlot ()) {		// if no plot, no cell groups
				rdCell.setEnabled (true);
			} else {
				rdCell.setEnabled (false);
			}*/				
			
		// Known grouper modification
		} else {
			// Update DGrouperDefiner to match with considered grouper		
			
			// NEW - 6.9.2004
			JRadioButton b = (JRadioButton) type2radioButton.get (g.getType ());
			if (b != null) {	// fc - 2.6.2008
				rdGroup2.setSelected (b.getModel (), true);
			}					// fc - 2.6.2008
			// NEW - 6.9.2004
			
			/*if (g.getType ().equals (TreeList.GROUP_ALIVE_TREE)) {
				rdGroup2.setSelected (rdTree.getModel (), true);		// selected element:tree
			} else {
				rdGroup2.setSelected (rdCell.getModel (), true);		// selected element:cell
			}*/
			
			if (g instanceof Filtrer) {
				rdGroup3.setSelected (rdDynamic.getModel (), true);		// selected type:dynamic
			} else {
				rdGroup3.setSelected (rdStatic.getModel (), true);		// selected type:static
			}
			
			// Disable possibility to change element type
			// NEW - 6.9.2004
			Iterator i = type2radioButton.values ().iterator ();
			while (i.hasNext ()) {
				b = (JRadioButton) i.next ();
				b.setEnabled (false);
			}
			// NEW - 6.9.2004
			
			/*rdTree.setEnabled (false);
			rdCell.setEnabled (false);*/
			
			
			// Disable possibility to change grouper type
			rdDynamic.setEnabled (false);
			rdStatic.setEnabled (false);
		}
		repaint ();
	}

	// NEW - 6.9.2004
	private GroupableType getSelectedType () {
		GroupableType type = null;
		Iterator ts = type2radioButton.keySet ().iterator ();
		Iterator bs = type2radioButton.values ().iterator ();
		while (ts.hasNext () && bs.hasNext ()) {
			GroupableType t = (GroupableType) ts.next ();
			JRadioButton b = (JRadioButton) bs.next ();
			if (isRadioSelected (rdGroup2, b)) {
				type = t;
				break;
			}
		}
		return type;
	}

	//	Things to do when type radio button is selected
	//
	// NEW - 6.9.2004
	private void rdiAction (JRadioButton button) {
		grouperType = getSelectedType ();
		createFilterManager ();
	}
	// NEW - 6.9.2004
	
	//	Things to do when Tree radio is selected
	//
	/*private void rdTreeAction () {
		createFilterManager ();
	}*/

	//	Things to do when Cell radio is selected
	//
	/*private void rdCellAction () {
		createFilterManager ();
	}*/

	//	Brand new
	//
	private void createFilterManager () {
		
		// Destroy current filter manager in order to rebuild it
		// with current options
		if (filterManager != null) {filterManager.dispose ();}
		
		// New grouper
		if (grouper == null) {
			filterManager = new FilterManager (step.getScene (), grouperType);	// fc - 17.9.2004
			synchronizeOptions (null);
			
		// Known grouper
		} else {
			// Retrieve grouper in combo box
			String name = grouper.getName ();
			Grouper g = GrouperManager.getInstance ().getGrouper (name);
			
			synchronizeOptions (g);
			
			// Dynamic: create a new FilterManager from the given grouper
			// (we tell him to be visible if its card is selected...)
			//
			if (g instanceof Filtrer) {
				filterManager = new FilterManager (step.getScene (), null, 
						grouperType, ((Filtrer) g).getFilters ());	// fc - 17.9.2004
				
			// Static: no memorized filters: create an FIndividualSelector from grouper ids
			//
			} else {
				
				
				FIndividualSelector f = new FIndividualSelector (((Identifier) g).getIds ());
				
				// 2. filterManager
				Collection c = new Vector ();
				c.add (f);
				filterManager = new FilterManager (step.getScene (), null, 
						grouperType, c);	// fc - 17.9.2004
			}
			
		}
		
		// fc-28.6.2019
		filterManager.setPreferredSize(new Dimension (600, 600));
		
		replaceTab (part1, card2Title, null, filterManager, null);	
		
		updateTitle ();
	}

	/**	Called on card change in the tabbed pane.
	*/
	public void stateChanged (ChangeEvent evt) {
		if (((JTabbedPane) evt.getSource ()).getSelectedComponent ().equals (card1)) {
			if (filterManager != null) {
				// If trouble in current filter config, card change is forbidden
				if (!filterManager.tryFilterConfig ()) {
					((JTabbedPane) evt.getSource ()).setSelectedIndex (1);	// i.e. the 2nd
					return;
				}
			}
		}
		updateTitle ();
	}

	//	Update the dialog title with current grouper name as chosen on card 1.
	//
	private void updateTitle () {
		String suffix = "";
		if (grouper == null) {
			suffix = grouperName.getText ().trim ();
		} else {
			suffix = grouper.getName ();
		}
		String title = step.getCaption ()+" - "+Translator.swap ("DGrouperDefiner");
		if (!suffix.equals ("")) {
			title += " - ["+suffix+"]";
		}
		setTitle (title);
	}

	/**	Good reaction on escape.
	*/
	protected void escapePressed () {
		super.escapePressed ();
		prepareClosing ();
		setValidDialog (false);
	}

	//	Tell if a radio button is selected in a radio grouper.
	//
	private static boolean isRadioSelected (ButtonGroup g, JRadioButton radio) {
		return g.getSelection ().equals (radio.getModel ());
	}

	/**	Retrieve the built grouper from outside after ok was hit.
	*/
	public Grouper getGrouper () {return newGrouper;}

}


