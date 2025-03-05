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

package capsis.extension.generictool.assistant;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.DefaultCellEditor;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowFilter;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.TableColumn;
import javax.swing.table.TableRowSorter;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.kernel.ModelManager;

/**
 * Translation Assistant: a tool to edit and update language files.
 * 
 * @author F. de Coligny - S. Dufour
 */
public class TranslationAssistant extends AmapDialog implements ActionListener, CellEditorListener {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	static {
		Translator.addBundle ("capsis.extension.generictool.assistant.TranslationAssistant");
	}

	private TranslationMap swapMap;
	private JScrollPane scrollPane;
	private JTable table;
	private JTextField filterText;
	private TableRowSorter<TranslationMap> sorter;

	private JCheckBox editSystem;
	private JCheckBox selectBundle;
	private boolean useFilename = false;

	private JButton save;
	private JButton cancel;
	private JButton help;
	private JButton add;
	private JButton remove;

	private JButton apply;
	private JComboBox entries;
	private JTextField filterNames;

	private Collection<String> classNames;
	private Collection<String> bundleNames;

	/**
	 * Default constructor.
	 */
	public TranslationAssistant (Window window) {
		super (window);
		init (null);
	}

	/**
	 * Constructor for a particular class
	 */
	public TranslationAssistant (Window window, String name) {
		super (window);
		this.init (name);

	}


	protected void init (String name) {

		setTitle (Translator.swap ("TranslationAssistant.title"));
		setEntries ();

		try {

			setDefaultCloseOperation (JDialog.DO_NOTHING_ON_CLOSE);
			addWindowListener (new WindowAdapter () {
				public void windowClosing (WindowEvent we) {
					escapePressed ();
				}
			});

			// Determine second lang
			Locale lang2 = Locale.getDefault ();
			if (lang2.equals (Locale.US) || lang2.equals (Locale.ENGLISH)) {
				lang2 = Locale.FRENCH;
			}
			swapMap = new TranslationMap (lang2);

			if (name != null) {
				swapMap.loadFromClass (name);
			}

			sorter = new TableRowSorter<TranslationMap> (swapMap);
			createUI ();

			if (name != null) {
				entries.setSelectedItem (name);
			}

			// pack (); // sets the size
			setSize (new Dimension (700, 500));

			System.out.println ("TranslationAssistant getModalityType (): " + getModalityType ());

			setVisible (true);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "TranslationAssistant", exc.toString (), exc);
		}

	}

	/** get the entry data */
	private void setEntries () {

		if (classNames == null) {
			classNames = new TreeSet<String> ();
		}
		getClasseNames (classNames);

		if (bundleNames == null) {
			bundleNames = new TreeSet<String> ();
		}
		getBundles (bundleNames);
	}

	/** Load the selected entry */
	public void loadEntry () {

		String cname = (String) entries.getSelectedItem ();
		if (cname == null || cname.equals ("")) { return; }

		try {
			if (!useFilename) {
				swapMap.loadFromClass (cname);
			} else {
				swapMap.loadFromBundle (cname);
			}

			this.makeTable ();
			swapMap.fireTableDataChanged ();

		} catch (Exception e) {
			JOptionPane.showMessageDialog (this, Translator.swap ("TranslationAssistant.loadError"));
		}

	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (save)) {
			if (table.isEditing ()) {
				table.getCellEditor ().stopCellEditing ();
				// MessageDialog.promptError (Translator.swap
				// ("TranslationAssistant.pleaseFinishTableEdition"));
				// return;
			}
			saveAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);

		} else if (evt.getSource ().equals (apply) || evt.getSource ().equals (entries)) {
			this.loadEntry ();

		} else if (evt.getSource ().equals (editSystem)) {
			swapMap.editStytemBundle = editSystem.isSelected ();

		} else if (evt.getSource ().equals (filterNames)) {
			updateEntries ();

		} else if (evt.getSource ().equals (selectBundle)) {

			if (selectBundle.isSelected () != useFilename) {
				useFilename = selectBundle.isSelected ();
				setEntries ();
			}
			updateEntries ();

		} else if (evt.getSource ().equals (add)) {

			String key = JOptionPane.showInputDialog (Translator.swap ("Shared.EnterKey"));
			if (key != null && key.length () > 0) {
				swapMap.addKey (key);
			}

		} else if (evt.getSource ().equals (remove)) {

			String key = (String) swapMap.getValueAt (table.getSelectedRow (), 0);
			String baseName = (String) swapMap.getValueAt (table.getSelectedRow (), 3);
			swapMap.removeKey (key, baseName);

		}
	}

	/** Update entries */
	public void updateEntries () {

		entries.removeAllItems ();
		String pattern = filterNames.getText ();

		Collection<String> e = classNames;
		if (useFilename) e = bundleNames;

		for (String name : e) {
			if (pattern.equals ("") || name.contains (pattern)) {
				entries.addItem (name);
			}
		}
	}

	public void editingCanceled (ChangeEvent evt) {
		System.out.println ("editingCanceled: source=" + evt.getSource ());
	}

	public void editingStopped (ChangeEvent evt) {
		// int row = table.getSelectedRow ();
		// int col = table.getSelectedColumn ();
	}

	/** Called on Escape and close */
	protected void escapePressed () {
		this.setValidDialog (true);
		this.dispose ();
	}

	/** Save changes in related files and exit */
	private void saveAction () {

		swapMap.validateRowData ();
		Map<String,Set<String>> duplicates = swapMap.getDuplicates ();

		// Test for duplicated entry
		if (duplicates.size () > 0) {

			String message = Translator.swap ("TranslationAssistant.DuplicatesFound") + " :\n\n";
			for (String key : duplicates.keySet ()) {
				Set<String> dup = duplicates.get (key);
				if (dup.size () == 0) {
					continue;
				}

				message += "**" + key + "** : ";
				String line = "";
				for (String f : dup) {
					line += f + ", ";
					if (line.length () > 50) {
						line += "....";
						break;
					}
				}
				message += line + "\n";
			}

			message += "\n" + Translator.swap ("TranslationAssistant.RemoveDuplicates") + "?";
			int ret = JOptionPane.showConfirmDialog (this, message);
			if (ret == JOptionPane.CANCEL_OPTION) { return; }
			if (ret == JOptionPane.OK_OPTION) {
				// Remove duplicates
				swapMap.removeDuplicates ();
			}
		}

		// Save data

		swapMap.saveData ();
		this.setValidDialog (true);
		this.dispose ();
	}

	/** fill in parameter the list of classes */
	protected void getClasseNames (Collection<String> classNames) {

		classNames.clear ();

		// Get class for package capsis
		try {
			classNames.addAll (Tools.getClasses ("capsis"));
		} catch (ClassNotFoundException e) {

		}

		// get class for models
		for (String pkg : ModelManager.getInstance ().getPackageNames ()) {
			try {
				classNames.addAll (Tools.getClasses (pkg));
			} catch (ClassNotFoundException e) {

			}
		}

	}

	/** fill in parameter the list of loaded bundle */
	protected void getBundles (Collection<String> classNames) {

		classNames.clear ();
		classNames.addAll (Translator.getLoadedBundles ());

	}

	/** Initialize the dialog's GUI. */
	private void createUI () {

		table = makeTable ();
		scrollPane = new JScrollPane (table);

		JPanel northPanel = new JPanel (new BorderLayout ());

		// class panel
		JPanel classPanel = new JPanel (new BorderLayout ());
		JWidthLabel classlabel = new JWidthLabel (" " + Translator.swap ("TranslationAssistant.classname") + " ", 100);
		apply = new JButton (Translator.swap ("Shared.apply"));

		Collection<String> cnames = classNames;
		entries = new JComboBox (cnames.toArray ());
		entries.setEditable (true);

		// filter class name
		JPanel filterNamePanel = new JPanel (new BorderLayout ());
		JWidthLabel filterNameLabel = new JWidthLabel (
				" " + Translator.swap ("TranslationAssistant.filterClass") + " ", 100);
		filterNames = new JTextField ();
		filterNames.addActionListener (this);
		selectBundle = new JCheckBox (Translator.swap ("TranslationAssistant.SelectByFile"), useFilename);
		selectBundle.addActionListener (this);
		filterNamePanel.add (filterNames, BorderLayout.CENTER);
		filterNamePanel.add (filterNameLabel, BorderLayout.WEST);
		filterNamePanel.add (selectBundle, BorderLayout.EAST);

		// class name
		classPanel.add (classlabel, BorderLayout.WEST);
		classPanel.add (entries, BorderLayout.CENTER);
		classPanel.add (apply, BorderLayout.EAST);

		apply.addActionListener (this);
		entries.addActionListener (this);

		// Create a separate form for filterText
		JPanel form = new JPanel (new BorderLayout ());
		JWidthLabel l1 = new JWidthLabel (" " + Translator.swap ("TranslationAssistant.filterKey") + " ", 100);
		form.add (l1, BorderLayout.WEST);
		filterText = new JTextField ();

		northPanel.add (form, BorderLayout.SOUTH);
		northPanel.add (classPanel, BorderLayout.CENTER);
		northPanel.add (filterNamePanel, BorderLayout.NORTH);

		// Control panel
		JPanel controlPanel = new JPanel (new FlowLayout (FlowLayout.RIGHT));

		editSystem = new JCheckBox (Translator.swap ("TranslationAssistant.EditSystemBundle"), false);
		save = new JButton (Translator.swap ("Shared.save"));
		ImageIcon icon = IconLoader.getIcon ("save_16.png");
		save.setIcon (icon);

		cancel = new JButton (Translator.swap ("Shared.cancel"));
		icon = IconLoader.getIcon ("cancel_16.png");
		cancel.setIcon (icon);

		help = new JButton (Translator.swap ("Shared.help"));
		icon = IconLoader.getIcon ("help_16.png");
		help.setIcon (icon);

		controlPanel.add (editSystem);
		controlPanel.add (save);
		controlPanel.add (cancel);
		controlPanel.add (help);

		save.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		editSystem.addActionListener (this);

		// add - remove panel
		JPanel addremovePanel = new JPanel (new FlowLayout (FlowLayout.LEFT));
		add = new JButton (Translator.swap ("Shared.add"));
		remove = new JButton (Translator.swap ("Shared.remove"));
		addremovePanel.add (add);
		addremovePanel.add (remove);
		add.addActionListener (this);
		remove.addActionListener (this);

		JPanel southPanel = new JPanel (new BorderLayout ());
		southPanel.add (addremovePanel, BorderLayout.WEST);
		southPanel.add (controlPanel, BorderLayout.EAST);

		// Whenever filterText changes, invoke newFilter.
		filterText.getDocument ().addDocumentListener (new DocumentListener () {
			public void changedUpdate (DocumentEvent e) {
				newFilter ();
			}

			public void insertUpdate (DocumentEvent e) {
				newFilter ();
			}

			public void removeUpdate (DocumentEvent e) {
				newFilter ();
			}
		});
		l1.setLabelFor (filterText);
		form.add (filterText, BorderLayout.CENTER);

		// Add component to main layout
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (scrollPane, BorderLayout.CENTER);
		getContentPane ().add (northPanel, BorderLayout.NORTH);
		getContentPane ().add (southPanel, BorderLayout.SOUTH);

		setTitle (Translator.swap ("TranslationAssistant.title"));

		// Generic tools should not be modal
		// setModal (true);
	}

	private JTable makeTable () {

		// create jtable if necessary
		if (table == null) {
			table = new JTable (swapMap);
			table.setAutoCreateRowSorter (true);
			table.setRowSorter (sorter);
		}

		// Edit source as a combo box
		TableColumn sourceColumn = table.getColumnModel ().getColumn (swapMap.getColumnCount () - 1);
		JComboBox comboBox = new JComboBox ();

		for (String s : swapMap.getBaseNames ()) {
			comboBox.addItem (s);
		}

		sourceColumn.setCellEditor (new DefaultCellEditor (comboBox));

		return table;
	}

	/**
	 * Update the row filter regular expression from the expression in the text
	 * box.
	 */
	private void newFilter () {
		RowFilter<TranslationMap,Object> rf = null;
		// If current expression doesn't parse, don't update.
		try {
			rf = RowFilter.regexFilter (filterText.getText (), 0);
		} catch (java.util.regex.PatternSyntaxException e) {
			return;
		}
		sorter.setRowFilter (rf);
	}

}
