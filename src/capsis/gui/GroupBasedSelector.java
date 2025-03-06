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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.util.Helper;
import capsis.extensiontype.GrouperDisplay;
import capsis.kernel.GScene;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * Dialog box to select individuals of a group.
 * 
 * @author F. de Coligny - april 2007
 */
public class GroupBasedSelector extends AmapDialog implements ActionListener, GrouperChooserListener {
	private GScene stand; // the stand we are working on

	private Collection selectedItems; // result of the process, see accessor at
										// the botom

	private String grouperName; // current group
	private Grouper grouper;

	private JComboBox grouperTypes;
	private GrouperChooser grouperChooser;

	private JTextField numberOfSelectedItems;

	private ExtensionManager extMan;

	private JComboBox displayCombo;
	private JPanel displayPanel;
	private GrouperDisplay display;
	private Map displayName2displayClassName; // display name -> class name
	private String lastDisplay;

	private JButton ok;
	private JButton cancel;
	private JButton helpButton;

	/**
	 * Constructor
	 */
	public GroupBasedSelector(GScene stand) {
		super();

		extMan = CapsisExtensionManager.getInstance();

		this.stand = stand;

		selectedItems = new ArrayList();

		lastDisplay = Settings.getProperty("group.based.selector.last.grouper.display.name", (String) null);

		createUI();
		changeDisplay();
		grouperName = grouperChooser.getGrouperName();
		changeGrouperName(grouperName);

		setTitle(Translator.swap("GroupBasedSelector"));

		setModal(true);

		// fc-13.4.2018 better size
		activateLocationMemorization(this.getClass ().getName ());
		activateSizeMemorization(this.getClass ().getName ());
		pack ();
		setVisible (true);

	}

	/**
	 * Action on Ok button
	 */
	public void okAction() {
		grouperName = grouperChooser.getGrouperName();
		changeGrouperName(grouperName);

		setValidDialog(true);
	}

	/**
	 * GrouperChooserListener
	 */
	public void grouperChanged(String newGrouperName) {

		if (newGrouperName == null || newGrouperName.equals("")) {
			// nothing
		} else {
			GrouperManager gm = GrouperManager.getInstance();
			Grouper newGrouper = gm.getGrouper(newGrouperName);
			changeGrouperName(newGrouperName);
		}
	}

	/**
	 * Redirection of events from panel buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(displayCombo)) {
			changeDisplay();
		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	// Grouper name changed
	//
	private void changeGrouperName(String grouperName) {
		this.grouperName = grouperName;

		if (grouperName != null) {
			GrouperManager gm = GrouperManager.getInstance();
			grouper = gm.getGrouper(grouperName);
			selectedItems = Group.whichCollection(stand, grouper.getType());

			selectedItems = grouper.apply(selectedItems, grouperName.toLowerCase().startsWith("not "));
			numberOfSelectedItems.setText("" + selectedItems.size());

			update();
		}

	}

	/**
	 * Updatable
	 */
	public void update() {
		try {
			display.update(stand, grouper);
		} catch (Exception e) {
		}
	}

	// Load the display which is selected in the displayCombo combo box
	//
	private void changeDisplay() {
		try {
			String name = (String) displayCombo.getSelectedItem();
			String className = (String) displayName2displayClassName.get(name);

			// to remember next time
			Settings.setProperty("group.based.selector.last.grouper.display.name", name);

			display = (GrouperDisplay) extMan.loadInitData(className, new GenericExtensionStarter("scene", stand));
			display.update(stand, null);

			JPanel p = (JPanel) display;

			displayPanel.removeAll();
			displayPanel.add(p, BorderLayout.CENTER);
			p.revalidate();
			p.repaint();

			update();
		} catch (Exception e) {
			Log.println(Log.WARNING, "GroupBasedSelector.changeDisplay ()", "exception during display change");
		}
	}

	/**
	 * Initializes the dialog's GUI.
	 */
	private void createUI() {
		LinePanel master = new LinePanel();

		ColumnPanel groupPanel = new ColumnPanel(Translator.swap("GroupBasedSelector.group"));

		LinePanel l0 = new LinePanel();
		l0.add(new JLabel(Translator.swap("GroupBasedSelector.explanationText")));
		l0.addGlue();
		groupPanel.add(l0);

		GroupableType type0 = null;
		try {
			
			// 2. Grouper type
			Collection<GroupableType> possibleTypes = Group.getPossibleTypes(stand);

			// fc-23.9.2019 there is a grouperType in the grouperChooser, 
			// adding one is not a good idea... REMOVED
//			grouperTypes = new JComboBox(new Vector(possibleTypes));
//			grouperTypes.addActionListener(this);
//			type = (GroupableType) grouperTypes.getSelectedItem();

			// fc -23.9.2019 first possible type
			type0 = possibleTypes.iterator().next ();
			
			// 3. grouperChooser
			LinePanel l10 = new LinePanel();
			
			boolean checked = false;
			boolean not = false;
			boolean showGrouperMode = false;
			
			// fc-3.6.2020 Null below means 'no restriction on grouper types'
			grouperChooser = new GrouperChooser(stand, type0, null, "", not, showGrouperMode, checked);
			grouperChooser.addGrouperChooserListener(this);
			l10.add(grouperChooser);
			
			// fc-23.9.2019 grouperChooser already includes grouperTypes
//			l10.add(grouperTypes);
			
			groupPanel.add(l10);
		} catch (Exception e) {
			// fc-13.4.2018 investigating trouble on Samsara2
			Log.println(Log.WARNING, "GroupBasedSelector.createUI ()",
					"could not createUI, trouble with grouperChooser, stand: " + stand + " type: " + type0, e);
		}

		ColumnPanel selectionPanel = new ColumnPanel(Translator.swap("GroupBasedSelector.selection"));

		JPanel aux = new JPanel(new BorderLayout());
		selectionPanel.add(aux);

		ColumnPanel aux2 = new ColumnPanel();
		aux.add(aux2, BorderLayout.NORTH);

		LinePanel l20 = new LinePanel();
		l20.add(new JLabel(Translator.swap("GroupBasedSelector.numberOfSelectedItems") + " : "));
		numberOfSelectedItems = new JTextField();
		numberOfSelectedItems.setEditable(false);
		l20.add(numberOfSelectedItems);
		l20.addStrut0();
		aux2.add(l20);

		// Display combo box
		//
		LinePanel displayControl = new LinePanel();
		Object[] typeAndStand = new Object[2];
		
		// fc-29.8.2019 type is now a GroupableType instance
		typeAndStand[0] = type0;
//		typeAndStand[0] = (GroupableType) grouperTypes.getSelectedItem();
//		typeAndStand[0] = (String) grouperTypes.getSelectedItem();
		typeAndStand[1] = stand;

		Collection classNames = extMan.getExtensionClassNames(CapsisExtensionManager.GROUPER_DISPLAY, typeAndStand);
		displayName2displayClassName = new HashMap();
		for (Iterator i = classNames.iterator(); i.hasNext();) {
			String className = (String) i.next();
			displayName2displayClassName.put(ExtensionManager.getName(className), className);
		}
		Collection names = displayName2displayClassName.keySet();
		displayCombo = new JComboBox(new Vector(new TreeSet(names)));
		displayCombo.addActionListener(this);
		if (lastDisplay != null) {
			displayCombo.setSelectedItem(lastDisplay);
		}
		displayControl.add(displayCombo);
		displayControl.addStrut0();

		aux2.add(displayControl);

		// fc - 1.10.2004
		displayPanel = new JPanel(new BorderLayout());
		aux.add(displayPanel, BorderLayout.CENTER);

		// Control panel
		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		helpButton = new JButton(Translator.swap("Shared.help"));
		controlPanel.add(ok);
		controlPanel.add(cancel);
		controlPanel.add(helpButton);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		helpButton.addActionListener(this);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(groupPanel, BorderLayout.NORTH);
		getContentPane().add(selectionPanel, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);
	}

	public Collection getSelectedItems() {
		return selectedItems;
	}

}
