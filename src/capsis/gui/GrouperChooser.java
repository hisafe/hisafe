/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.gui;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;

import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Tools;
import capsis.gui.command.BuildGrouper;
import capsis.kernel.GScene;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * An interactive chooser to choose a grouper. This component is to be inserted
 * in other components possibly relying on groups. It can be optionally enabled
 * or disabled by a checkBox. The type of grouper can be changed (trees, cells,
 * fish...). The grouper catalog can be opened to create / modify groupers. Each
 * change in this chooser is fired to the registered listeners for
 * synchronisation.
 * 
 * @author F. de Coligny - October 2002 / April 2004, reviewed in August 2012
 */
public class GrouperChooser extends LinePanel implements ActionListener, ItemListener {

	// fc-3.6.2020 Added expectedTypes

	private GScene scene; // to be filtered by a grouper

	private GroupableType type; // see Group possible types

	// Ignored if null, else restricts typeCombo content
	private List<GroupableType> expectedTypes;

	private String grouperNameAtStartTime; // first selection in grouper combo
	private boolean not; // select the group complementary mode

	private Collection possibleTypes;
	private Collection listeners; // will be told when things change

	private boolean showChooserEnablingCheckBox; // if true, a check box is
													// added to enable /
													// disable the grouper
													// chooser
	private boolean chooserEnabledAtStartTime; // if
												// showChooserEnablingCheckBox,
												// the checkbox may
												// be checked or not

	private JCheckBox chooserEnablingCheckBox; // optional, enables / disables
												// the grouper chooser,
												// see
												// showChooserEnablingCheckBox

	private Map<String, GroupableType> typeLabel_type; // for types translation
	private Map<GroupableType, String> type_typeLabel; // for types translation

	private JComboBox typeCombo; // possible types of groupers (e.g. tree, cell,
									// fish...)
	private JCheckBox ckNot; // NOT: if checked, use complementary grouper
	private JComboBox grouperCombo; // grouper selection
	private JButton grouperCatalog; // opens the grouper catalog for on the fly
									// groupers management

	private boolean tellTheListeners; // to manage finely the events

	// private List<GrouperChooserEnablingListener>
	// grouperChooserEnablingListeners; // fc-4.9.2019

	/**
	 * Default Constructor. Does not show the enabling/disabling checkbox: the
	 * chooser is enabled, it can not be disabled. See constructor 2 for
	 * details.
	 */
	public GrouperChooser(GScene scene, GroupableType type, String grouperNameAtStartTime, boolean not) {
		this(scene, type, null, grouperNameAtStartTime, not, false, true);
	}

	/**
	 * Constructor 2. A checkbox is proposed to enable / disable the grouper
	 * chooser. The initial state of this checkbox can be set. The groupers for
	 * the given scene are proposed. the given type is selected by default in
	 * the type combo. The given grouper name is also selected (if the grouper
	 * name is provided, the provided type is ignored). If 'not' is true, the
	 * not checkbox is selected.
	 */
	public GrouperChooser(GScene scene, GroupableType type, List<GroupableType> expectedTypes,
			String grouperNameAtStartTime, boolean not, boolean showChooserEnablingCheckBox,
			boolean chooserEnabledAtStartTime) {

		// fc-3.6.2020 Added expectedTypes argument. If null no effect, else
		// restricts the list of types in the typeCombo.

		super(0, 0);

		tellTheListeners = false; // will be activated later

		// grouperChooserEnablingListeners = new ArrayList<>(); // fc-4.9.2019

		GrouperManager gm = GrouperManager.getInstance();

		this.scene = scene;
		this.type = type;
		this.expectedTypes = expectedTypes;
		this.grouperNameAtStartTime = gm.removeNot(grouperNameAtStartTime);
		this.not = not;
		this.showChooserEnablingCheckBox = showChooserEnablingCheckBox;
		this.chooserEnabledAtStartTime = showChooserEnablingCheckBox ? chooserEnabledAtStartTime : true;

		initializeTypes();

		createUI();

		dealWithInitialSelection();

	}

	/**
	 * Initialisations for types translations.
	 */
	private void initializeTypes() {
		typeLabel_type = new TreeMap<String, GroupableType>(); // sorted
		type_typeLabel = new HashMap<GroupableType, String>();

		Collection<GroupableType> types = Group.getPossibleTypes(scene);

		// fc-3.6.2020
		if (expectedTypes != null)
			types.retainAll(expectedTypes);

		Iterator i = types.iterator();
		while (i.hasNext()) {
			GroupableType type = (GroupableType) i.next();
			String translation = type.getName();
			// String translation = Translator.swap(type);
			typeLabel_type.put(translation, type); // the translated labels will
													// go to the user combobox
			type_typeLabel.put(type, translation);
		}
	}

	private void dealWithInitialSelection() {

		// fc-18.12.2017
		try {

			// Initial selection
			if (grouperNameAtStartTime != null) { // 1. See if a grouper name
													// was provided (ignore the
													// provided type)
				boolean n = false;
				if (grouperNameAtStartTime.toLowerCase().startsWith("not")) {
					grouperNameAtStartTime = GrouperManager.getInstance().removeNot(grouperNameAtStartTime);
					n = true;
				}

				Grouper g = GrouperManager.getInstance().getGrouper(grouperNameAtStartTime);
				GroupableType ty = g.getType();
				String t = type_typeLabel.get(ty);

				// Type
				DefaultComboBoxModel model = (DefaultComboBoxModel) typeCombo.getModel();
				if (model.getIndexOf(t) > 0) {
					model.setSelectedItem(t);
				}

				// 'Not' and Grouper
				model = (DefaultComboBoxModel) grouperCombo.getModel();
				if (model.getIndexOf(grouperNameAtStartTime) > 0) {
					model.setSelectedItem(grouperNameAtStartTime);
					ckNot.setSelected(n);
				}

			} else { // 2. See if a type name was provided
				if (type != null) {
					String t = type_typeLabel.get(type);

					// fc-15.12.2017 Bug correction (Lemoine, CM + TL)
					if (t == null)
						return;

					DefaultComboBoxModel model = (DefaultComboBoxModel) typeCombo.getModel();
					if (model.getIndexOf(t) > 0) {
						model.setSelectedItem(t);
					}
				}
			}

			tellTheListeners = true;
		} catch (Exception e) {
			// Maybe no groupers, do nothing
		}

	}

	/**
	 * Returns true if NOT is selected (complementary mode).
	 */
	public boolean isGrouperNot() {
		return ckNot.isSelected();
	}

	/**
	 * Returns the grouper chooser type.
	 */
	public GroupableType getType() {
		return type;
	}

	/**
	 * Updates the grouper combo box.
	 */
	private void updateGrouperCombo() {
		// If no groupers found, c is an empty Collection
		Collection c = Group.whichCollection(scene, type);
		TreeSet grouperNames = new TreeSet(GrouperManager.getInstance().getGrouperNames(c)); // TreeSet:
																								// sorted
		grouperCombo.setModel(new DefaultComboBoxModel(new Vector(grouperNames)));

		if (grouperCombo.getItemCount() != 0)
			grouperCombo.setSelectedIndex(0);

	}

	/**
	 * Enables / disables the whole grouper chooser according to the 'enabling'
	 * check box.
	 */
	private void chooserEnablingCheckBoxSynchro() {

		if (chooserEnablingCheckBox != null) { // fc-13.4.2018 it is optional

			// fc-3.6.2020
			boolean singleType = typeCombo.getModel().getSize() == 1;

			typeCombo.setEnabled(chooserEnablingCheckBox.isSelected() && !singleType);

			ckNot.setEnabled(chooserEnablingCheckBox.isSelected());
			grouperCombo.setEnabled(chooserEnablingCheckBox.isSelected());
		}

	}

	/**
	 * Adds a listener to be told when chooserEnablingCheckBox is changed.
	 */
	// public void
	// addGrouperChooserEnablingListener(GrouperChooserEnablingListener l) {
	// grouperChooserEnablingListeners.add(l);
	// }

	/**
	 * Removes a chooser enabling listener.
	 */
	// public void
	// removeGrouperChooserEnablingListener(GrouperChooserEnablingListener l) {
	// grouperChooserEnablingListeners.remove(l);
	// }

	/**
	 * Tell the grouper chooser enabling listener about a change.
	 */
	// private void fireGrouperChooserEnablingEvent() {
	// for (GrouperChooserEnablingListener l : grouperChooserEnablingListeners)
	// l.grouperChooserEnablingChanged(this);
	// }

	/**
	 * Called when an item is selected in a combobox.
	 */
	public void itemStateChanged(ItemEvent evt) {
		// Clicking on a combobox fires two events, we catch only selection
		if (evt.getStateChange() != ItemEvent.SELECTED)
			return;

		if (evt.getSource().equals(typeCombo)) {

			tellTheListeners = false;

			String typeLabel = (String) typeCombo.getSelectedItem();
			this.type = typeLabel_type.get(typeLabel);

			updateGrouperCombo();

			tellTheListeners = true;

			fireGrouperChanged(); // Ok, type change, fire (3)

		} else if (evt.getSource().equals(grouperCombo) && evt.getStateChange() == ItemEvent.SELECTED) {

			fireGrouperChanged(); // Ok, direct grouper selection, fire (1)

		}
	}

	/**
	 * Actions on components.
	 */
	public void actionPerformed(ActionEvent evt) {

		if (evt.getSource().equals(chooserEnablingCheckBox)) {

			chooserEnablingCheckBoxSynchro();

			fireGrouperChanged(); // Ok, enabling/disabling the whole chooser,
									// fire (4)

			// fireGrouperChooserEnablingEvent(); // fc-4.9.2019

		} else if (evt.getSource().equals(ckNot)) {
			fireGrouperChanged(); // Ok, click on not, fire (2)

		} else if (evt.getSource().equals(grouperCatalog)) {

			// If ctrl is hit, open directly the Grouper Definer
			if ((evt.getModifiers() & Tools.getCtrlMask()) != 0) {
				DGrouperDefiner dlg = new DGrouperDefiner(Current.getInstance().getStep(), null, null); // type
																										// ==
																										// null,
																										// default
																										// will
																										// be
																										// used

				// else open the Grouper Catalog
			} else {
				new BuildGrouper(MainFrame.getInstance()).execute();
				updateGrouperCombo();
			}
		}

	}

	/**
	 * Returns true if a grouper is available in this chooser
	 */
	public boolean isGrouperAvailable() {
		// If the optional chooser enabling checkbox is null, it means the
		// chooser is enabled and
		// can not be disabled
		if (chooserEnablingCheckBox != null && !chooserEnablingCheckBox.isSelected())
			return false;
		return grouperCombo != null && grouperCombo.getSelectedItem() != null;
	}

	/**
	 * Returns the final answer (chosen grouper name), or an empty string if no
	 * grouper available.
	 */
	public String getGrouperName() {
		if (!isGrouperAvailable())
			return "";

		String not = ckNot.isSelected() ? "Not " : "";

		return not + (String) grouperCombo.getSelectedItem();
	}

	/**
	 * Fires an event to the listeners if the grouper was changed
	 */
	private void fireGrouperChanged() {

		if (!tellTheListeners || listeners == null)
			return;

		String grouperName = getGrouperName();

		// System.out.println("GrouperChooser changed, grouperName: " +
		// grouperName);

		for (Iterator i = listeners.iterator(); i.hasNext();) {
			GrouperChooserListener l = (GrouperChooserListener) i.next();
			l.grouperChanged(grouperName); // no Event : we simply pass a
											// String: the new grouper
											// name
		}
	}

	/**
	 * Adds a listener
	 */
	public void addGrouperChooserListener(GrouperChooserListener l) {
		if (listeners == null)
			listeners = new ArrayList();
		listeners.add(l);
	}

	/**
	 * Removes a listener
	 */
	public void removeGrouperChooserListener(GrouperChooserListener l) {
		if (listeners == null)
			return;
		listeners.remove(l);
	}

	/**
	 * Creates the gui.
	 */
	private void createUI() {

		// Check if types are available
		possibleTypes = type_typeLabel.keySet();
		if (possibleTypes == null || possibleTypes.isEmpty()) {
			add(new JLabel(Translator.swap("GrouperChooser.noGroupersAvailable")));
			return;
		}

		// Optional chooser enabling check box
		if (showChooserEnablingCheckBox) {
			String label = Translator.swap("GrouperChooser.useAGroup") + ": ";
			chooserEnablingCheckBox = new JCheckBox(label, chooserEnabledAtStartTime);
			chooserEnablingCheckBox.addActionListener(this);
			add(chooserEnablingCheckBox);
		} else {
			add(new JLabel(Translator.swap("GrouperChooser.selectAGroup") + ": "));
		}

		// Type combobox: the translated type labels
		typeCombo = new JComboBox(new Vector(typeLabel_type.keySet())) {
			@Override
			public Dimension getPreferredSize() {
				Dimension d = super.getPreferredSize();
				return new Dimension(100, d.height); // grouper type names can
														// be long, nicer like
														// this
			}

			@Override
			public Dimension getMinimumSize() { // better redimensioning
				return getPreferredSize();
			}
		};

		if (typeCombo.getModel().getSize() == 1)
			typeCombo.setEnabled(false); // fc-3.6.2020

		typeCombo.addItemListener(this);
		LinePanel l2 = new LinePanel(0, 0);
		l2.add(new JLabel(Translator.swap("GrouperChooser.type") + " "));
		l2.add(typeCombo);
		add(l2);

		if (!possibleTypes.contains(type)) {
			String typeLabel = (String) typeCombo.getSelectedItem();
			type = typeLabel_type.get(typeLabel);
		}

		// Complementary option
		ckNot = new JCheckBox(Translator.swap("Shared.NOT"), not);
		ckNot.addActionListener(this);
		LinePanel l3 = new LinePanel(0, 0);
		l3.add(ckNot);

		// Grouper combo box
		grouperCombo = new JComboBox() {
			@Override
			public Dimension getPreferredSize() {
				Dimension d = super.getPreferredSize();
				return new Dimension(120, d.height); // grouper names can be
														// long, nicer like this
			}

			@Override
			public Dimension getMinimumSize() { // better redimensioning
				return getPreferredSize();
			}
		};
		updateGrouperCombo();

		chooserEnablingCheckBoxSynchro(); // enables or not grouperCombo

		grouperCombo.addItemListener(this);
		l3.add(grouperCombo);
		add(l3);

		// Grouper Catalog / Definer (if ctrl-click)
		Icon icon = IconLoader.getIcon("group_16.png");
		grouperCatalog = new JButton(icon);
		grouperCatalog.setToolTipText(Translator.swap("MainFrame.buildGrouperToolTip"));
		grouperCatalog.addActionListener(this);
		Tools.setSizeExactly(grouperCatalog);
		add(grouperCatalog);

		addStrut0();

	}

}
