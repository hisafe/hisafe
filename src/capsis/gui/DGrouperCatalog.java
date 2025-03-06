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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.WindowConstants;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import capsis.commongui.util.Helper;
import capsis.kernel.Step;
import capsis.util.WrapperPanel;
import capsis.util.group.ComplexGrouper;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.Alert;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StringCouple;
import jeeb.lib.util.Translator;

/**
 * DGrouperCatalog is a Dialog box for groupers management.
 * 
 * @author F. de Coligny - october 2002 / april 2004
 */
public class DGrouperCatalog extends AmapDialog implements ActionListener, ListSelectionListener, ItemListener {

	private Step step; // Catalog was opened with this current step

	private String memoGrouper;
	private String memoGrouperType;

	private Map typeLabel_type;
	private GroupableType currentType;

	private JComboBox grouperCombo;
	private JList grouperList;
	private JScrollPane grouperScroll;
	private JTextField fileName;

	private JCheckBox g1Not;
	private JComboBox combo1;
	private JComboBox operator;
	private JCheckBox g2Not;
	private JComboBox combo2;
	private DefaultComboBoxModel combo1Model;
	private DefaultComboBoxModel combo2Model;

	private JButton add;
	private Map operatorMap;

	private GrouperManager gm;

	private JButton close;
	private JButton newGrouper;
	private JButton modifyGrouper;
	private JButton removeGrouper;

	private JButton help;

	/**
	 * Constructor.
	 */
	public DGrouperCatalog(Step step) {
		super();
		this.step = step;

		gm = GrouperManager.getInstance();
		initialize(); // create some Maps...

		operatorMap = new HashMap();
		String andLabel = Translator.swap("DGrouperCatalog.AND") + " ("
				+ Translator.swap("DGrouperCatalog.intersection") + ")";
		String orLabel = Translator.swap("DGrouperCatalog.OR") + " (" + Translator.swap("DGrouperCatalog.union") + ")";
		operatorMap.put(andLabel, Group.AND);
		operatorMap.put(orLabel, Group.OR);

		createUI();

		setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent evt) {
				close();
			}
		});
		
		
		// fc-28.6.2019 size details
		activateLocationMemorization(this.getClass ().getName ());
		activateSizeMemorization(this.getClass ().getName ());

		pack();
		setVisible(true);
	}

	// Initialisations.
	//
	private void initialize() {
		typeLabel_type = new TreeMap();

		// NEW - 6.9.2004
		Collection types = Group.getPossibleTypes(step.getScene());
		Iterator i = types.iterator();
		while (i.hasNext()) {
			GroupableType type = (GroupableType) i.next();
			typeLabel_type.put(type.getName (), type);
//			typeLabel_type.put(Translator.swap(type), type);
		}
		// NEW - 6.9.2004

		// fc - 1.4.2004
		//
		if (typeLabel_type.isEmpty()) {
			Alert.print(Translator.swap("DGrouperCatalog.groupersAreNotAvailableForThisStep"));
			return;
		}

		currentType = (GroupableType) types.iterator().next(); // changed on 6.9.2004
		// currentType = TreeList.GROUP_ALIVE_TREE; // default, till it changes...
	}

	// Ok button.
	//
	private void okAction() {
		// may ask for confirmation
		setValidDialog(true);
	}

	// Type combo changed.
	//
	private void grouperComboAction(String option, String listSelection) { // listSelection
																			// may
																			// be
																			// null
		currentType = (GroupableType) typeLabel_type.get(option);
		grouperCombo.setSelectedItem(option);
		updateGrouperList(listSelection);
	}

	// Update list according to combo current type.
	//
	private void updateGrouperList(String selectedName) {

		// 1. Update grouper list
		Collection individuals = Group.whichCollection(step.getScene(), currentType);
		Collection grouperNames = new TreeSet(GrouperManager.getInstance().getGrouperNames(individuals));
		grouperList = new JList(new Vector(grouperNames));

		grouperList.addListSelectionListener(this);
		grouperList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		grouperList.setFixedCellWidth(300);
		grouperList.setVisibleRowCount(6);

		grouperList.setSelectedValue(selectedName, true); // true : shouldScroll
															// (works not
															// much...)
		// ~ grouperList.setSelectedIndex (grouperList.getSelectedIndex ()); //
		// try to scroll effectively

		grouperScroll.getViewport().setView(grouperList);

		if (grouperList.getModel().getSize() != 0 && grouperList.isSelectionEmpty()) {
			grouperList.setSelectedIndex(0); // select first line
		}

		// trying to scroll to selectedIndex
		try {
			if (grouperList.getSelectedIndex() > grouperList.getVisibleRowCount() - 1) {
				grouperScroll.getViewport()
						.setViewPosition(grouperList.indexToLocation(grouperList.getSelectedIndex()));
			}
		} catch (Exception e) {
		}

		grouperScroll.revalidate();

		// 2. Update combo1 and combo2
		try {
			combo1Model = new DefaultComboBoxModel(new Vector(grouperNames));
			combo1.setModel(combo1Model);
			combo2Model = new DefaultComboBoxModel(new Vector(grouperNames));
			combo2.setModel(combo2Model);
		} catch (Exception e) {
		} // may be called very early
	}

	// New grouper button.
	//
	private void newGrouperAction() {
		DGrouperDefiner dlg = new DGrouperDefiner(step, currentType);
		if (dlg.isValidDialog()) {
			currentType = dlg.getGrouper().getType();
			updateGrouperList(dlg.getGrouper().getName());
		}
		dlg.dispose();
	}

	// Modify grouper button.
	//
	private void modifyGrouperAction() {
		String name = (String) grouperList.getSelectedValue();

		try {
			Grouper grouper = GrouperManager.getInstance().getGrouper(name);

			DGrouperDefiner dlg = new DGrouperDefiner(step, grouper);
			if (dlg.isValidDialog()) {
				currentType = dlg.getGrouper().getType();
				updateGrouperList(dlg.getGrouper().getName());
			}
			dlg.dispose();
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap ("DGrouperCatalog.operationIsNotAvailableForThisGroup"));
		}
	}

	// Remove grouper button.
	//
	private void removeGrouperAction() {
		String name = (String) grouperList.getSelectedValue(); // fc - 2.6.2008
		if (name != null) { // fc - 2.6.2008
			if (Question.ask(MainFrame.getInstance(), Translator.swap("Shared.confirm"),
					Translator.swap("DGrouperCatalog.doYouWantToRemoveThisGrouper"))) {
				// ~ String name = (String) grouperList.getSelectedValue (); //
				// fc - 2.6.2008
				GrouperManager.getInstance().remove(name);
				updateGrouperList(null);
			}
		} // fc - 2.6.2008
	}

	/**
	 * Combo box management.
	 */
	public void itemStateChanged(ItemEvent evt) {
		if (evt.getSource().equals(grouperCombo)) {
			String option = (String) evt.getItem();
			if (!(option.equals(memoGrouperType))) {
				memoGrouperType = option;
				grouperComboAction(option, null);
			}
		}
	}

	// List selection.
	//
	private void selectionAction(String selection) {
		// Complex groupers can not be modified : only removed
		try {
			Grouper g = gm.getGrouper(selection);

			modifyGrouper.setEnabled(!(g instanceof ComplexGrouper));

		} catch (Exception e) {
		} // may be called too early

		// Update grouper file name
		String name = GrouperManager.getInstance().getFileName(selection);
		fileName.setText(name);
	}

	/**
	 * List management.
	 */
	public void valueChanged(ListSelectionEvent evt) {
		if (evt.getSource().equals(grouperList)) {
			JList src = (JList) evt.getSource();
			if (src.getModel().getSize() == 0) {
				return;
			} // maybe list is empty -> no selection possible

			String selection = (String) src.getSelectedValue();

			// For one item selection, two evenements are generated : click and
			// de-click
			// this feature hears only one
			if (!selection.equals(memoGrouper)) {
				memoGrouper = selection;
				selectionAction(selection);
			}
		}
	}

	/**
	 * Called on escape.
	 */
	protected void escapePressed() {
		close();
	}

	// Close catalog.
	//
	private void close() {
		String val = "";
		if (grouperList.getModel().getSize() != 0) {
			val = "" + grouperList.getSelectedValue();
		}
		Settings.setProperty("capsis.group.catalog.last.selection", "(" + grouperCombo.getSelectedItem() + "~" + val
				+ ")");
		setValidDialog(true); // always valid
	}

	// Add button.
	//
	private void addAction() {
		String name1 = (String) combo1.getSelectedItem();
		String op = (String) operator.getSelectedItem();
		String name2 = (String) combo2.getSelectedItem();

		Grouper g1 = gm.getGrouper(name1);
		Grouper g2 = gm.getGrouper(name2);
		byte o = (Byte) operatorMap.get(op);

		ComplexGrouper g = null;
		try {
			g = new ComplexGrouper(g1Not.isSelected(), g1, o, g2Not.isSelected(), g2);
		} catch (Exception e) {
			Log.println(Log.ERROR, "DGrouperCatalog.addAction ()", "could not create ComplexGrouper due to exception",
					e);
			MessageDialog.print(this, Translator.swap("DGrouperCatalog.couldNotCreateComplexGrouperSeeLog"));
			return;
		}

		gm.add(g);
		updateGrouperList(g.getName());
	}

	/**
	 * Buttons management.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(add)) {
			addAction();
		} else if (evt.getSource().equals(close)) {
			close();
		} else if (evt.getSource().equals(newGrouper)) {
			newGrouperAction();
		} else if (evt.getSource().equals(modifyGrouper)) {
			modifyGrouperAction();
		} else if (evt.getSource().equals(removeGrouper)) {
			removeGrouperAction();
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	// Initialize the dialog's graphical user interface.
	//
	private void createUI() {

		// 0. Left column
		JPanel left = new JPanel(new BorderLayout());

		// 1. Group type
		LinePanel l1 = new LinePanel();
		grouperCombo = new JComboBox(new Vector(new TreeSet(typeLabel_type.keySet())));
		grouperCombo.addItemListener(this);
		l1.add(grouperCombo);
		l1.addStrut0();

		// 2. Group list
		LinePanel l2 = new LinePanel();

		grouperList = new JList();
		grouperList.addListSelectionListener(this);
		grouperList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		grouperList.setFixedCellWidth(200);
		grouperList.setVisibleRowCount(8);

		grouperScroll = new JScrollPane(grouperList);
		l2.add(grouperScroll);
		l2.addStrut0();

		// 3. Group file name
		
		ColumnPanel c3 = new ColumnPanel ();
		
		LinePanel l4 = new  LinePanel ();
		MemoPanel memo = new MemoPanel(Translator.swap ("DGrouperCatalog.explanation"));
		memo.setRows(5);
		l4.add(memo);
		l4.addStrut0();
		c3.add(l4);
		
		LinePanel l3 = new LinePanel();
		fileName = new JTextField(5);
		fileName.setEnabled(false);
		l3.add(new JWidthLabel(Translator.swap("DGrouperCatalog.fileName"), 80));
		l3.add(fileName);

		c3.add(l3);
		
		// Restore last selection
		// ~ try {
		// ~ StringCouple sc = new StringCouple (Settings.getProperty
		// ("capsis.group.catalog.last.selection"), "~", true, null);
		// ~ grouperComboAction (sc.a, sc.b);
		// ~ } catch (Exception e) {
		// ~ grouperComboAction ((String) grouperCombo.getSelectedItem (),
		// null);
		// ~ }

		// 4. Complex groupers creation
		ColumnPanel aux = new ColumnPanel();

		ColumnPanel c1 = new ColumnPanel(2, 2); // default is 2, 4 : reduction
												// of vert space
		c1.setBorder(new TitledBorder(Translator.swap("DGrouperCatalog.addition")));

		Collection c = Group.whichCollection(step.getScene(), currentType);
		TreeSet grouperNames = new TreeSet(GrouperManager.getInstance().getGrouperNames(c)); // if
																								// no
																								// groups,
																								// empty
																								// Collection

		LinePanel l21 = new LinePanel();
		g1Not = new JCheckBox(Translator.swap("Shared.NOT"));
		combo1Model = new DefaultComboBoxModel(new Vector(grouperNames));
		combo1 = new JComboBox(combo1Model);
		// ~ combo1.setPreferredSize (new Dimension (250, 50));
		l21.add(g1Not);
		l21.add(combo1);
		l21.addStrut0();
		c1.add(l21);

		LinePanel l22 = new LinePanel();
		operator = new JComboBox(new Vector(operatorMap.keySet()));
		l22.add(operator);
		l22.addStrut0();
		c1.add(l22);

		LinePanel l23 = new LinePanel();
		g2Not = new JCheckBox(Translator.swap("Shared.NOT"));
		combo2Model = new DefaultComboBoxModel(new Vector(grouperNames));
		combo2 = new JComboBox(combo2Model);
		l23.add(g2Not);
		l23.add(combo2);
		l23.addStrut0();
		c1.add(l23);

		LinePanel l11 = new LinePanel();
		add = new JButton(Translator.swap("DGrouperCatalog.add"));
		add.addActionListener(this);
		l11.addGlue();
		l11.add(add);
		l11.addStrut0();

		c1.add(l11);
		c1.addGlue();

		aux.add(c1);
		aux.add(c3);
//		aux.add(l3);
		aux.addGlue();

		left.add(new WrapperPanel(l1, 2, 2), BorderLayout.NORTH);
		left.add(new WrapperPanel(l2, 2, 2), BorderLayout.CENTER);
		left.add(new WrapperPanel(aux, 2, 2), BorderLayout.SOUTH);

		// Restore last selection
		// ~ try {
		// ~ StringCouple sc = new StringCouple (Settings.getProperty
		// ("capsis.group.catalog.last.selection"), "~", true, null);
		// ~ grouperComboAction (sc.a, sc.b);
		// ~ } catch (Exception e) {
		// ~ grouperComboAction ((String) grouperCombo.getSelectedItem (),
		// null);
		// ~ }

		// 3. Right column
		JPanel right = new JPanel(new BorderLayout());

		JPanel up = new JPanel(new GridLayout(5, 1, 4, 4));
		JPanel down = new JPanel(new GridLayout(1, 1, 4, 4));

		// 3.1 Close
		close = new JButton(Translator.swap("Shared.close"));
		close.setToolTipText(Translator.swap("Shared.close"));
		close.addActionListener(this);

		// 3.2 New group
		newGrouper = new JButton(Translator.swap("DGrouperCatalog.newGrouper"));
		newGrouper.setToolTipText(Translator.swap("DGrouperCatalog.newGrouper"));
		newGrouper.addActionListener(this);

		// 3.3 Modify group
		modifyGrouper = new JButton(Translator.swap("DGrouperCatalog.modifyGrouper"));
		modifyGrouper.setToolTipText(Translator.swap("DGrouperCatalog.modifyGrouper"));
		modifyGrouper.addActionListener(this);

		// 3.4 Remove group
		removeGrouper = new JButton(Translator.swap("DGrouperCatalog.removeGrouper"));
		removeGrouper.setToolTipText(Translator.swap("DGrouperCatalog.removeGrouper"));
		removeGrouper.addActionListener(this);

		// 3.6 Help
		help = new JButton(Translator.swap("Shared.help"));
		help.setToolTipText(Translator.swap("Shared.help"));
		help.addActionListener(this);

		up.add(close);
		up.add(newGrouper);
		up.add(modifyGrouper);
		up.add(removeGrouper);

		down.add(help);

		// Restore last selection
		try {
			StringCouple sc = new StringCouple(Settings.getProperty("capsis.group.catalog.last.selection", ""), "~",
					true);
			grouperComboAction(sc.a, sc.b);
		} catch (Exception e) {
			grouperComboAction((String) grouperCombo.getSelectedItem(), null);
		}

		right.add(new WrapperPanel(up, 2, 2), BorderLayout.NORTH);
		right.add(new WrapperPanel(down, 2, 2), BorderLayout.SOUTH);

		// Sets ok as default (see AmapDialog)
		close.setDefaultCapable(true);
		getRootPane().setDefaultButton(close);

		// General layout
		JPanel general = new JPanel(new BorderLayout());
		general.add(new JScrollPane(left), BorderLayout.CENTER);
		general.add(right, BorderLayout.EAST);
		setContentPane(new WrapperPanel(general, 4, 4));

		setTitle(Translator.swap("DGrouperCatalog"));
		setModal(true);
		setResizable(true);
	}

}
