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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.kernel.GScene;
import capsis.util.NumberStringComparator;
import capsis.util.SmartListModel;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;

/**
 * Dialog box to select a sublist of Strings from an original list.
 *
 * @author F. de Coligny - May 2002
 */
public class DListSelector extends AmapDialog implements ActionListener, ListDataListener {

	// fc-17.6.2019 review

	private final static int LIST_HEIGHT = 10;
	private final static int CELL_WIDTH = 60;

	private String dlgTitle;
	private String dlgText;
	private String empty;
	private List candidateItems;
	private List selectedItems;

	private JTextField candidateNumber;
	private JList candidateList;
	private SmartListModel candidateModel;
	private int candidateSize;

	private JTextField selectedNumber;
	private JList selectedList;
	private SmartListModel selectedModel;
	private int selectedSize;

	private JButton addButton;
	private JButton removeButton;
	private JButton addAllButton;
	private JButton removeAllButton;
	private JButton addGroupButton; // fc - 16.4.2007 [for PhD]
	private JButton sortButton;

	private String type;

	private JButton ok;
	private JButton cancel;
	private JButton helpButton;

	private boolean singleSelection;

	/**
	 * Constructor
	 */
	public DListSelector(String title, String text, List candidateItems) {
		this(title, text, candidateItems, false); // default: multiple selection
													// enabled
	}

	/**
	 * Constructor
	 */
	public DListSelector(String title, String text, List candidateItems, boolean singleSelection) {
		// fc-17.6.2019 added selectedItems option in constructors, default:
		// empty
		this(title, text, candidateItems, new ArrayList(), singleSelection);
	}

	/**
	 * Constructor
	 */
	public DListSelector(String title, String text, List candidateItems, List selectedItems, boolean singleSelection) {
		super();
		dlgTitle = title;
		dlgText = text;
		this.candidateItems = new ArrayList(candidateItems);
		this.singleSelection = singleSelection; // fc - 28.9.2006

		this.selectedItems = new ArrayList();

		// fc-17.6.2019 selectedItems initialization
		if (selectedItems != null) {
			this.selectedItems.addAll(selectedItems);

			// Remove selected from candidate if needed
			this.candidateItems.removeAll(this.selectedItems);
		}

		empty = Translator.swap("DListSelector.empty");

		createUI();

		// fc-31.8.2018
		activateSizeMemorization(this.getClass().getName());
		activateLocationMemorization(this.getClass().getName());

		setMinimumSize(new Dimension(300, 300));

		pack();
		show();

	}

	/**
	 * Action on Ok button
	 */
	public void okAction() {
		// Check: ask a confirmation in some cases:
		// 1. selectedList is empty
		if (selectedModel.isEmpty()) {
			JButton yesButton = new JButton(Translator.swap("Shared.yes"));
			JButton noButton = new JButton(Translator.swap("Shared.no"));
			Vector buttons = new Vector();
			buttons.add(yesButton);
			buttons.add(noButton);
			// Prepare vector selected string items
			JButton choice = UserDialog.promptUser(this, Translator.swap("Shared.confirm"),
					Translator.swap("DListSelector.targetListIsEmpty") + "\n" + Translator.swap("Shared.continue")
							+ " ?",
					buttons, noButton);
			if (choice.equals(noButton)) {
				return;
			}
		}

		// 2. selection in candidateList is multiple
		if (candidateList.getSelectedIndices().length > 1) {
			JButton yesButton = new JButton(Translator.swap("Shared.yes"));
			JButton noButton = new JButton(Translator.swap("Shared.no"));
			Vector buttons = new Vector();
			buttons.add(yesButton);
			buttons.add(noButton);
			// Prepare vector selected string items
			JButton choice = UserDialog.promptUser(this, Translator.swap("Shared.confirm"),
					Translator.swap("DListSelector.selectionInFromListIsMultiple") + "\n"
							+ Translator.swap("Shared.continue") + " ?",
					buttons, noButton);
			if (choice.equals(noButton)) {
				return;
			}
		}

		// 3. selection in selectedList is multiple
		if (selectedList.getSelectedIndices().length > 1) {
			JButton yesButton = new JButton(Translator.swap("Shared.yes"));
			JButton noButton = new JButton(Translator.swap("Shared.no"));
			Vector buttons = new Vector();
			buttons.add(yesButton);
			buttons.add(noButton);
			// Prepare vector selected string items
			JButton choice = UserDialog.promptUser(this, Translator.swap("Shared.confirm"),
					Translator.swap("DListSelector.selectionInToListIsMultiple") + "\n"
							+ Translator.swap("Shared.continue") + " ?",
					buttons, noButton);
			if (choice.equals(noButton)) {
				return;
			}
		}

		// Prepare vector of selected items
		selectedItems = new ArrayList(selectedModel.getContents());

		// Back to the caller
		setValidDialog(true);
		// fc setVisible (false);
	}

	// Manage list data changes
	//
	private void listChanged(ListDataEvent evt) {
		if (evt.getSource().equals(candidateModel)) {
			candidateNumber.setText("" + candidateModel.getSize());
		} else if (evt.getSource().equals(selectedModel)) {
			selectedNumber.setText("" + selectedModel.getSize());
		}

		// Enable / disable buttons according to the current situation
		synchro();
	}

	/**
	 * Enables / disables the action buttons (if lists are empty...) Proposed by S.
	 * Aid - fc-15.11.2011
	 */
	private void synchro() {

		addButton.setEnabled(!candidateModel.isEmpty());
		removeButton.setEnabled(!selectedModel.isEmpty());
		addAllButton.setEnabled(!candidateModel.isEmpty());
		removeAllButton.setEnabled(!selectedModel.isEmpty());
		addGroupButton.setEnabled(!candidateModel.isEmpty());

	}

	/**
	 * ListDataListener
	 */
	public void contentsChanged(ListDataEvent evt) {
		listChanged(evt);
	}

	/**
	 * ListDataListener
	 */
	public void intervalAdded(ListDataEvent evt) {
		listChanged(evt);
	}

	/**
	 * ListDataListener
	 */
	public void intervalRemoved(ListDataEvent evt) {
		listChanged(evt);
	}

	/**
	 * Redirection of events from panel buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (selectedModel.contains(empty)) {
			selectedModel.remove(empty);
		}

		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(addButton)) {
			addAction();
		} else if (evt.getSource().equals(removeButton)) {
			removeAction();
		} else if (evt.getSource().equals(addAllButton)) {
			addAllAction();
		} else if (evt.getSource().equals(removeAllButton)) {
			removeAllAction();
		} else if (evt.getSource().equals(sortButton)) {
			sortAction();
		} else if (evt.getSource().equals(addGroupButton)) {
			addGroupAction();
		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Set the two lists models mute or verbose.
	 */
	private void setModelsMute(boolean b) {
		candidateModel.setMute(b);
		selectedModel.setMute(b);
	}

	/**
	 * Add elements list1 -> list2
	 */
	public void addAction() {
		if (singleSelection) {

			Object[] values = selectedList.getSelectedValues();
			if (values.length > 0) { // possibly 0 or 1 value in selectedModel
										// in
										// singleSelection model
				setModelsMute(true);
				selectedModel.remove(values[0]);
				candidateModel.add(values[0]);
				setModelsMute(false);
			}

		} // fc - 28.9.2006

		// Find min selected index in candidateList
		int[] tab = candidateList.getSelectedIndices();
		int iCandidate = tab[0];

		// Process moves
		int i = 0;
		Object[] values = candidateList.getSelectedValues();
		if (values.length > 1) {
			setModelsMute(true);
		}
		for (i = 0; i < values.length; i++) {
			candidateModel.remove(values[i]);
			selectedModel.add(values[i]);
		}
		if (values.length > 1) {
			setModelsMute(false);
		}

		// Deal with selections after moves in both lists
		if (!selectedModel.isEmpty()) {
			selectedList.setSelectedIndex(selectedModel.getSize() - 1);
			selectedList.ensureIndexIsVisible(selectedList.getSelectedIndex());
		}
		if (!candidateModel.isEmpty()) {
			if (iCandidate < candidateModel.getSize()) {
				candidateList.setSelectedIndex(iCandidate);
			} else {
				candidateList.setSelectedIndex(candidateModel.getSize() - 1);
			}
			candidateList.ensureIndexIsVisible(candidateList.getSelectedIndex());

		}
	}

	/**
	 * Remove elements from list2
	 */
	public void removeAction() {
		// Find max selected index in selectedList
		int[] tab = selectedList.getSelectedIndices();
		int iSelected = tab[0];

		int i = 0;
		Object[] values = selectedList.getSelectedValues();
		if (values.length > 1) {
			setModelsMute(true);
		}
		for (i = 0; i < values.length; i++) {
			selectedModel.remove(values[i]);
			candidateModel.add(values[i]);
		}
		if (values.length > 1) {
			setModelsMute(false);
		}

		// Deal with selections after moves in both lists
		if (!candidateModel.isEmpty()) {
			candidateList.setSelectedIndex(candidateModel.getSize() - 1);
			candidateList.ensureIndexIsVisible(candidateList.getSelectedIndex());
		}
		if (!selectedModel.isEmpty()) {
			if (iSelected < selectedModel.getSize()) {
				selectedList.setSelectedIndex(iSelected);
			} else {
				selectedList.setSelectedIndex(selectedModel.getSize() - 1);
			}
			selectedList.ensureIndexIsVisible(selectedList.getSelectedIndex());
		}
	}

	/**
	 * Add all element in list1 to list2
	 */
	public void addAllAction() {
		int i = 0;
		Collection values = candidateModel.getContents();
		selectedModel.addAll(values);

		candidateModel.clear();
		if (!selectedModel.isEmpty()) {
			selectedList.setSelectedIndex(selectedModel.getSize() - 1);
			selectedList.ensureIndexIsVisible(selectedList.getSelectedIndex());
		}
	}

	/**
	 * Remove all elements from list2
	 */
	public void removeAllAction() {
		int i = 0;
		Collection values = selectedModel.getContents();
		candidateModel.addAll(values);

		selectedModel.clear();
		if (!candidateModel.isEmpty()) {
			candidateList.setSelectedIndex(candidateModel.getSize() - 1);
			candidateList.ensureIndexIsVisible(candidateList.getSelectedIndex());
		}
	}

	/**
	 * Add elements of a Group in list2
	 */
	public void addGroupAction() { // fc - 16.4.2007
		GScene stand = Current.getInstance().getStep().getScene();
		GroupBasedSelector dlg = new GroupBasedSelector(stand);
		if (dlg.isValidDialog()) { // valid
			Collection items = dlg.getSelectedItems();
			// System.out.println ("items: "+items );

			boolean shouldScroll = false;
			candidateList.setSelectionInterval(0, 0);

			Collection ids = new ArrayList();

			for (Iterator i = items.iterator(); i.hasNext();) {
				Identifiable identifiable = (Identifiable) i.next();

				String id = "" + identifiable.getId();
				ids.add(id);
			}

			// fc - 17.4.2007 - if some ids were already in selectedModel, do
			// not
			// consider them again
			ids.removeAll(selectedModel.getContents());

			candidateModel.removeAll(ids);
			selectedModel.addAll(ids);

			sortAction();

		} else { // canceled

		}
		dlg.dispose();

	}

	/**
	 * Sort list1 and list2
	 */
	public void sortAction() {

		// fc-17.6.2019 Fixed bugs in sorting when items did not start with
		// numbers

		// System.out.println("DListSelector.sortAction ()...");

		// fc-17.6.2019 reselect better
		// these two objects stay null if no current selection
		Object candidateListSelection = candidateList.getSelectedValue();
		Object selectedListSelection = selectedList.getSelectedValue();

		Comparator comparator = new NumberStringComparator();

		candidateModel.sort(comparator);
		selectedModel.sort(comparator);

		// fc-17.6.2019 reselect better
		boolean shouldScroll = true;

		if (!candidateModel.isEmpty()) {
			if (candidateListSelection != null)
				candidateList.setSelectedValue(candidateListSelection, shouldScroll);
			else
				candidateList.setSelectedIndex(0);

			candidateList.ensureIndexIsVisible(candidateList.getSelectedIndex());
		}
		if (!selectedModel.isEmpty()) {
			if (selectedListSelection != null)
				selectedList.setSelectedValue(selectedListSelection, shouldScroll);
			else
				selectedList.setSelectedIndex(0);

			selectedList.ensureIndexIsVisible(selectedList.getSelectedIndex());
		}

	}

	/**
	 * Initializes the dialog's GUI.
	 */
	private void createUI() {
		// used only if singleSelection
		LinePanel l0 = new LinePanel();
		l0.add(new JLabel(Translator.swap("DListSelector.singleSelection")));
		l0.addStrut0();

		LinePanel master = new LinePanel();

		// 1. prepare candidate list
		candidateModel = new SmartListModel();

		try {
			for (Iterator i = candidateItems.iterator(); i.hasNext();) {
				String id = (String) i.next();
				if (id != null)
					candidateModel.add(id);
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "DListSelector.createUI ()", "Exception while preparing candidate items list", e);
			return;
		}

		// 2. first column: prepare candidate list
		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("Shared.unSelected") + " : "));
		candidateNumber = new JTextField(2);
		candidateNumber.setEditable(false);
		l1.add(candidateNumber);
		l1.addGlue();

		candidateList = new JList(candidateModel);
		candidateList.setSelectedIndex(0);
		candidateList.setVisibleRowCount(LIST_HEIGHT);
		if (singleSelection) {
			candidateList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		} else {
			candidateList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		}

		// fc-23.9.2019
		candidateList.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					addAction();

					// fc-22.12.2020 If single selection expected and double click, select and close
					// the dialog directly
					if (singleSelection)
						okAction();

				}
			}
		});

		JScrollPane panCandidate = new JScrollPane(candidateList);
		JPanel aux1 = new JPanel(new BorderLayout());
		aux1.add(l1, BorderLayout.SOUTH);
		aux1.add(panCandidate, BorderLayout.CENTER);
		master.add(aux1);

		// 3. second column : buttons
		JPanel boxButtons = new JPanel();
		boxButtons.setLayout(new BoxLayout(boxButtons, BoxLayout.Y_AXIS));

		ImageIcon icon = null;
		// fc - 6.11.2008
		icon = IconLoader.getIcon("add-one_16.png");
		addButton = new JButton(icon);
		addButton.addActionListener(this);
		addButton.setToolTipText(Translator.swap("DListSelector.add"));
		Tools.setSizeExactly(addButton, 24, 24);

		icon = IconLoader.getIcon("remove-one_16.png");
		removeButton = new JButton(icon);
		removeButton.addActionListener(this);
		removeButton.setToolTipText(Translator.swap("DListSelector.remove"));
		Tools.setSizeExactly(removeButton, 24, 24);

		icon = IconLoader.getIcon("add-all_16.png");
		addAllButton = new JButton(icon);
		addAllButton.addActionListener(this);
		addAllButton.setToolTipText(Translator.swap("DListSelector.addAll"));
		Tools.setSizeExactly(addAllButton, 24, 24);

		icon = IconLoader.getIcon("remove-all_16.png");
		removeAllButton = new JButton(icon);
		removeAllButton.addActionListener(this);
		removeAllButton.setToolTipText(Translator.swap("DListSelector.removeAll"));
		Tools.setSizeExactly(removeAllButton, 24, 24);

		// fc - 16.4.2007
		icon = IconLoader.getIcon("group_16.png");
		// ~ icon = new IconLoader ("toolbarButtonGraphics/navigation/").getIcon
		// (".gif");
		addGroupButton = new JButton(icon);
		addGroupButton.addActionListener(this);
		addGroupButton.setToolTipText(Translator.swap("DListSelector.addGroup"));
		Tools.setSizeExactly(addGroupButton, 24, 24);

		icon = IconLoader.getIcon("go-down_16.png");
		sortButton = new JButton(icon);
		sortButton.addActionListener(this);
		sortButton.setToolTipText(Translator.swap("DListSelector.sort"));
		Tools.setSizeExactly(sortButton, 24, 24);

		boxButtons.add(addButton);
		boxButtons.add(removeButton);
		if (!singleSelection) { // fc - 28.9.2006
			boxButtons.add(addAllButton);
			boxButtons.add(removeAllButton);
			boxButtons.add(addGroupButton); // fc - 16.4.2007
		}
		boxButtons.add(sortButton);
		boxButtons.add(Box.createVerticalGlue());
		master.add(boxButtons);

		// 3.1 ensure same width for all buttons.
		// BoxLayout tries to enlarge all the components to the biggest's size
		// if maximumSize allows it
		Component c[] = boxButtons.getComponents();
		int wMax = 0;
		for (int i = 0; i < c.length; i++) {
			try {
				if (!(c[i] instanceof JButton)) {
					continue;
				} // glue...
				JButton b = (JButton) c[i];
				if (b.getPreferredSize().width > wMax) {
					wMax = b.getPreferredSize().width;
				}
			} catch (Exception e) {
				Log.println(Log.WARNING, "DListSelector.createUI ()",
						"Trouble while resizing buttons : " + e.toString());
			}
		}
		for (int i = 0; i < c.length; i++) {
			try {
				JButton b = (JButton) c[i];
				b.setMaximumSize(new Dimension(wMax, b.getPreferredSize().height));
			} catch (Exception e) {
			}
		}

		// 4. third column : prepare selected items list
		selectedModel = new SmartListModel();

		// fc-17.6.2019 added this selected items intialization
		try {
			for (Iterator i = selectedItems.iterator(); i.hasNext();) {
				String id = (String) i.next();
				if (id != null)
					selectedModel.add(id);
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "DListSelector.createUI ()", "Exception while preparing selected items list", e);
			return;
		}

		LinePanel l2 = new LinePanel();
		l2.add(new JLabel(Translator.swap("Shared.selected") + " : "));
		selectedNumber = new JTextField(2);
		selectedNumber.setEditable(false);
		l2.add(selectedNumber);
		l2.addGlue();

		selectedList = new JList(selectedModel);
		selectedList.setSelectedIndex(0);
		selectedList.setVisibleRowCount(LIST_HEIGHT);
		selectedList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

		// fc-23.9.2019
		selectedList.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2)
					removeAction();
			}
		});

		JScrollPane panSelected = new JScrollPane(selectedList);
		panSelected.setPreferredSize(panCandidate.getPreferredSize());
		panCandidate.setPreferredSize(panSelected.getPreferredSize());
		JPanel aux2 = new JPanel(new BorderLayout());
		aux2.add(l2, BorderLayout.SOUTH);
		aux2.add(panSelected, BorderLayout.CENTER);
		master.add(aux2);
		master.addStrut0();

		// fc-28.8.2019 if a text was passed to the constructor, add it before
		// the standard explanation message
		String msg = "";
		if (dlgText != null)
			msg += dlgText + "\n";
		msg += Translator.swap("DListSelector.explanation");
		MemoPanel explanation = new MemoPanel(msg);

		ColumnPanel aux = new ColumnPanel();
		aux.add(master);
		JPanel orga = new JPanel(new BorderLayout());
		orga.setLayout(new BorderLayout());
		if (singleSelection) {
			orga.add(l0, BorderLayout.NORTH);
		}

		orga.add(aux, BorderLayout.CENTER);

		// fc-17.6.2019
		orga.add(explanation, BorderLayout.SOUTH);

		orga.setPreferredSize(new Dimension(300, 300));
		sortAction();

		// 5. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		helpButton = new JButton(Translator.swap("Shared.help"));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(helpButton);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		helpButton.addActionListener(this);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(orga, BorderLayout.CENTER);
		getContentPane().add(pControl, BorderLayout.SOUTH);

		candidateModel.addListDataListener(this);
		selectedModel.addListDataListener(this);
		candidateNumber.setText("" + candidateModel.getSize());
		selectedNumber.setText("" + selectedModel.getSize());

		setTitle(dlgTitle);

		setModal(true);

	}

	public boolean getMarkOnly() {
		return false;
	}

	public List getSelectedItems() {
		return selectedItems;
	}

	/**
	 * Returns the first selected item (faster when single selection is true), or
	 * null if no selection.
	 */
	public Object getSelectedItem() {

		// fc-9.12.2020

		if (selectedItems.size() == 0)
			return null;

		return selectedItems.get(0);
	}

}
