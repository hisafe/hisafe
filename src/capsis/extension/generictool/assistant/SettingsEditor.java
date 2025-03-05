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
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.RowFilter;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableRowSorter;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.OrderedProperties;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * Translation Assistant: a tool to edit and update language files.
 * 
 * @author F. de Coligny - S. Dufour
 */
public class SettingsEditor extends AmapDialog implements ActionListener, CellEditorListener {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	static {
		Translator.addBundle("capsis.extension.generictool.assistant.SettingsEditor");
	}

	private SettingModel model;
	private JScrollPane scrollPane;
	private JTable table;
	private JTextField filterText;
	private TableRowSorter<SettingModel> sorter;

	// private JButton save;
	private JButton close;
	private JButton help;

	/**
	 * Default constructor.
	 */
	public SettingsEditor(Window window) {
		super(window);
		init(null);
	}

	/**
	 * Constructor for a particular class
	 */
	public SettingsEditor(Window window, String name) {
		super(window);
		this.init(name);

	}

	protected void init(String name) {

		try {
			model = new SettingModel();

			// fc-22.7.2019 added .name
			setTitle(Translator.swap("SettingsEditor.name"));

			setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
			addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent we) {
					escapePressed();
				}
			});

			sorter = new TableRowSorter<SettingModel>(model);
			createUI();

			// pack (); // sets the size
			setSize(new Dimension(700, 500));

			setVisible(true);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "SettingsEditor", exc.toString(), exc);
		}

	}

	public void actionPerformed(ActionEvent evt) {

		if (table.isEditing()) {
			table.getCellEditor().stopCellEditing();

		}

		if (evt.getSource().equals(close)) {
			setValidDialog(true);
			this.dispose();

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);

		}
	}

	public void editingCanceled(ChangeEvent evt) {
	}

	public void editingStopped(ChangeEvent evt) {

	}

	/** Called on Escape and close */
	protected void escapePressed() {
		this.setValidDialog(true);
		this.dispose();
	}

	/** Initialize the dialog's GUI. */
	private void createUI() {
		ImageIcon icon;

		table = makeTable();
		scrollPane = new JScrollPane(table);

		JPanel northPanel = new JPanel(new BorderLayout());

		// Create a separate form for filterText
		JPanel form = new JPanel(new BorderLayout());
		JWidthLabel l1 = new JWidthLabel(" " + Translator.swap("SettingsEditor.filterKey") + " ", 100);
		form.add(l1, BorderLayout.WEST);
		filterText = new JTextField();

		northPanel.add(form, BorderLayout.SOUTH);

		// Control panel
		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// save = new JButton (Translator.swap ("Shared.save"));
		// icon = IconLoader.getIcon ("save_16.png");
		// save.setIcon(icon);

		close = new JButton(Translator.swap("Shared.close"));
		icon = IconLoader.getIcon("cancel_16.png");
		close.setIcon(icon);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);

		// controlPanel.add (save);
		controlPanel.add(close);
		controlPanel.add(help);

		// save.addActionListener (this);
		close.addActionListener(this);
		help.addActionListener(this);

		JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.add(controlPanel, BorderLayout.EAST);

		// Whenever filterText changes, invoke newFilter.
		filterText.getDocument().addDocumentListener(new DocumentListener() {
			public void changedUpdate(DocumentEvent e) {
				newFilter();
			}

			public void insertUpdate(DocumentEvent e) {
				newFilter();
			}

			public void removeUpdate(DocumentEvent e) {
				newFilter();
			}
		});
		l1.setLabelFor(filterText);
		form.add(filterText, BorderLayout.CENTER);

		// Add component to main layout
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(scrollPane, BorderLayout.CENTER);
		getContentPane().add(northPanel, BorderLayout.NORTH);
		getContentPane().add(southPanel, BorderLayout.SOUTH);

		// setTitle (Translator.swap ("SettingsEditor.title"));

		// GenericTools should not be modal
		// setModal (true);
	}

	private JTable makeTable() {

		// create jtable if necessary
		if (table == null) {
			table = new JTable(model);
			table.setAutoCreateRowSorter(true);
			table.setRowSorter(sorter);
		}

		return table;
	}

	/**
	 * Update the row filter regular expression from the expression in the text box.
	 */
	private void newFilter() {
		RowFilter<SettingModel, Object> rf = null;
		// If current expression doesn't parse, don't update.
		try {
			// fc-2.2.2024
			rf = RowFilter.regexFilter(filterText.getText().trim(), 0);
//			rf = RowFilter.regexFilter (filterText.getText (), 0);

			// fc-2.2.2024 in the try / catch
			sorter.setRowFilter(rf);

		} catch (java.util.regex.PatternSyntaxException e) {
			return;
		}
//		sorter.setRowFilter (rf);
	}

	static public class SettingModel extends AbstractTableModel {

		protected OrderedProperties map;
		protected List<String> keys;
		static final String[] columnNames = { "key", "value" };

		public SettingModel() {
			map = Settings.getProperties();
			keys = new ArrayList<String>();

			for (Object k : map.keySet()) {
				keys.add((String) k);
			}

		}

		@Override
		public int getColumnCount() {
			return 2;
		}

		@Override
		public int getRowCount() {
			return map.size();
		}

		@Override
		public Object getValueAt(int row, int col) {

			String k = keys.get(row);

			if (col == 0) {
				return k;
			} else {
				return map.get(k);
			}

		}

		public boolean isCellEditable(int row, int col) {
			if (col == 1) {
				return true;
			}
			return false;
		}

		public void setValueAt(Object value, int row, int col) {

			if (col != 1) {
				return;
			}
			String k = keys.get(row);
			map.setProperty(k, (String) value);
		}

		public String getColumnName(int col) {
			return columnNames[col].toString();
		}
	}
}
