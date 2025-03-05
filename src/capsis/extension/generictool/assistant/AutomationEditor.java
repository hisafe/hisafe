/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2009 INRA  Samuel Dufour
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
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.event.CellEditorListener;
import javax.swing.event.ChangeEvent;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableRowSorter;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StringUtil;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.TaskManager;
import jeeb.lib.util.task.TaskManagerView;
import synchro.gui.DMessage;
import capsis.commongui.util.Helper;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.kernel.automation.Automation;
import capsis.kernel.automation.AutomationVariation;
import capsis.kernel.automation.CompletePlan;
import capsis.kernel.automation.OrderedPlan;
import capsis.kernel.automation.RandomPlan;
import capsis.util.AutomationRunner;
import capsis.util.JSmartFileChooser;

/**
 * Edit automation data
 * 
 * @author S. Dufour
 */
public class AutomationEditor extends AmapDialog implements ActionListener,
		CellEditorListener {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	static {
		Translator
				.addBundle("capsis.extension.generictool.assistant.AutomationEditor");
	}

	private AutomationEventModel dataModel;
	private JScrollPane scrollPane;
	private JTable table;
	private TableRowSorter<AutomationEventModel> sorter;

	private JButton save;
	private JButton run;
	private JButton close;
	private JButton help;

	private JComboBox plan;
	static final int defaultNbThread = 2;
	private JSpinner nbThreadSpinner;
	private JComboBox eventCB;

	private Automation automation;
	private String fileName = "";

	private JCheckBox summaryButton;
	private JCheckBox keepProjectButton;

	private DialogWithClose taskViewDialog;
	// private JButton summaryButton;
	// private JTextField summaryTextEditor;

	// private JSpinner nbSimuSpinner;
	private JSpinner repetitionSpinner;

	/**
	 * Constructor for a particular class
	 */
	public AutomationEditor(Window window, String filename) throws Exception {
		super(window);
		this.init(filename, null);

	}

	/**
	 * Constructor for a particular automation
	 */
	public AutomationEditor(Window window, Automation a) throws Exception {
		super(window);
		this.init(null, a);

	}

	/**
	 * Default constructor
	 */
	public AutomationEditor(Window window) {
		super(window);

		setTitle(Translator.swap("AutomationEditor.title"));

		// Retrieve some references
		JFileChooser chooser = new JSmartFileChooser(
				Translator.swap("Automation.import"),
				Translator.swap("Automation.import"),
				Translator.swap("Automation.import"), Settings.getProperty(
						"capsis.automation.path", (String) null), false); // DIRECTORIES_ONLY=false

		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
		// :
		// approveButton
		// text
		// was
		// already
		// set

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty("capsis.automation.path", chooser
					.getSelectedFile().getParent());

			// Load and run automation
			String filename = chooser.getSelectedFile().getAbsolutePath();
			init(filename, null);

		}
	}

	protected void init(String fn, Automation a) {

		fileName = fn;

		try {
			// Load automation
			if (a == null) {
				automation = Automation.buildFromXML(fileName);
			} else {
				automation = a;
			}

			dataModel = new AutomationEventModel(automation.initEvent,
					automation.getVariation());
			sorter = new TableRowSorter<AutomationEventModel>(dataModel);

			setModal(false);
			createUI();

			// pack (); // sets the size
			setSize(new Dimension(700, 500));

			setVisible(true);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "AutomationEditor", exc.toString(), exc);
			MessageDialog.print(this,
					Translator.swap("AutomationEditor.loadError"));
		}

	}

	/** Load the selected entry */
	public void loadEntry() {

		Automation.Event item = (Automation.Event) eventCB.getSelectedItem();
		if (item == null) {
			return;
		}

		try {
			dataModel.initRawData(item);
			this.makeTable();
			dataModel.fireTableDataChanged();

		} catch (Exception e) {
			JOptionPane.showMessageDialog(this,
					Translator.swap("AutomationEditor.loadError"));
		}

	}

	@Override
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(save)) {
			if (table.isEditing()) {
				table.getCellEditor().stopCellEditing();
			}
			saveAction();

		} else if (evt.getSource().equals(close)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(run)) {
			runAction();

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);

		} else if (evt.getSource().equals(eventCB)) {
			loadEntry();
		}
	}

	@Override
	public void editingCanceled(ChangeEvent evt) {
	}

	@Override
	public void editingStopped(ChangeEvent evt) {
	}

	@Override
	protected void escapePressed() {
		this.setValidDialog(true);
		this.dispose();
	}

	/** Save changes in related files and exit */
	private void saveAction() {

		if (table.isEditing()) {
			table.getCellEditor().stopCellEditing();
		}

		if (fileName == null) {
			getExportName();
		}

		try {
			automation.getVariation().plan = (String) plan.getSelectedItem();

			automation.repetition = (Integer) repetitionSpinner.getModel()
					.getValue();
			automation.copytoXML(fileName);
		} catch (Exception e) {
			MessageDialog.print(this, "Save Error", e);
		}

	}

	private void getExportName() {

		boolean trouble = false;
		do {
			JFileChooser chooser = new JSmartFileChooser(
					Translator.swap("automation.export"),
					Translator.swap("Automation.export"),
					Translator.swap("Automation.export"), Settings.getProperty(
							"capsis.automation.path", (String) null), false);

			chooser.setDialogType(JFileChooser.SAVE_DIALOG);
			int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
			// :
			// approveButton
			// text
			// was
			// already
			// set
			fileName = chooser.getSelectedFile().getAbsolutePath();
			Settings.setProperty("capsis.automation.path", chooser
					.getSelectedFile().getParent());

			if (returnVal == JFileChooser.APPROVE_OPTION
					&& chooser.getSelectedFile().exists()) {
				if (!Question
						.ask(MainFrame.getInstance(),
								Translator.swap("Shared.confirm"),
								""
										+ chooser.getSelectedFile().getPath()
										+ "\n"
										+ Translator
												.swap("Shared.fileExistsPleaseConfirmOverwrite"))) {
					trouble = true;
				}
			}

		} while (trouble);
	}

	/** Run Action */
	private void runAction() {

		if (table.isEditing()) {
			table.getCellEditor().stopCellEditing();
		}

		// update data
		int nbthread = (Integer) nbThreadSpinner.getModel().getValue();
		automation.getVariation().plan = (String) plan.getSelectedItem();
		automation.repetition = (Integer) repetitionSpinner.getModel()
				.getValue();

		try {
			AutomationRunner.runInTasks(automation, nbthread,
					summaryButton.isSelected(), keepProjectButton.isSelected());
		} catch (Exception e) {
			Log.println(Log.ERROR, "runAction", "Error", e);
			DMessage.promptError(null, "Cannot run tasks", e);
		}

		TaskManagerView taskView = new TaskManagerView(
				TaskManager.getInstance(), false);

		if (taskViewDialog == null) {
			taskViewDialog = new DialogWithClose(this,
					new JScrollPane(taskView), Translator.swap("Tasks"), false);
		} else {
			taskViewDialog.setVisible(true);
		}

	}

	/** Initialize the dialog's GUI. */
	private void createUI() {

		setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent we) {
				escapePressed();
			}
		});

		table = makeTable();

		scrollPane = new JScrollPane(table);
		int b = 12;
		JPanel northPanel = new JPanel(new BorderLayout());

		// info Panel
		JPanel infoPanel = new JPanel();
		infoPanel.setLayout(new BoxLayout(infoPanel, BoxLayout.PAGE_AXIS));
		JLabel fileNameLabel = new JLabel(
				Translator.swap("AutomationEditor.filename") + " : "
						+ this.fileName);
		JLabel modelNameLabel = new JLabel(
				Translator.swap("AutomationEditor.model") + " : "
						+ automation.model);
		JLabel memoNameLabel = new JLabel(
				Translator.swap("AutomationEditor.memorizer") + " : "
						+ automation.memorizer);
		infoPanel.add(fileNameLabel);
		infoPanel.add(modelNameLabel);
		infoPanel.add(memoNameLabel);
		infoPanel.setBorder(BorderFactory.createEmptyBorder(b, b, b, b));

		// event panel
		JPanel eventPanel = new JPanel(new BorderLayout());
		JWidthLabel eventLabel = new JWidthLabel(" "
				+ Translator.swap("AutomationEditor.event") + " ", 100);

		eventCB = new JComboBox();
		eventCB.addItem(automation.initEvent);
		for (Automation.Event ev : automation.events) {
			eventCB.addItem(ev);
		}
		eventCB.setEditable(true);

		eventPanel.add(eventLabel, BorderLayout.WEST);
		eventPanel.add(eventCB, BorderLayout.CENTER);
		eventPanel.setBorder(BorderFactory.createEmptyBorder(b, b, b, b));

		eventCB.addActionListener(this);

		northPanel.add(infoPanel, BorderLayout.NORTH);
		northPanel.add(eventPanel, BorderLayout.CENTER);

		plan = new JComboBox();

		plan.addItem(OrderedPlan.class.getName());
		plan.addItem(CompletePlan.class.getName());
		plan.addItem(RandomPlan.class.getName());
		plan.setSelectedItem(automation.getVariation().plan);

		// Control panel
		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		JPanel controlPanel2 = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		summaryButton = new JCheckBox(
				Translator.swap("AutomationEditor.summary"));
		summaryButton.setSelected(true);

		keepProjectButton = new JCheckBox(
				Translator.swap("AutomationEditor.keepProject"));
		keepProjectButton.setSelected(true);

		// repeition
		repetitionSpinner = new JSpinner();

		JSpinner.NumberEditor editor2 = new JSpinner.NumberEditor(
				repetitionSpinner);
		editor2.getTextField().setColumns(3);
		repetitionSpinner.setEditor(editor2);

		editor2.getModel().setMinimum(1);
		editor2.getModel().setMaximum(9999);
		editor2.getModel().setStepSize(1);
		if (automation.repetition < 1) {
			automation.repetition = 1;
		}
		editor2.getModel().setValue(automation.repetition);
		editor2.getFormat().applyPattern("####");
		repetitionSpinner.setEditor(editor2);

		// nb thread
		nbThreadSpinner = new JSpinner();
		JSpinner.NumberEditor editor3 = new JSpinner.NumberEditor(
				nbThreadSpinner);
		nbThreadSpinner.setEditor(editor3);

		editor3.getModel().setMinimum(1);
		editor3.getModel().setMaximum(9);
		editor3.getModel().setStepSize(1);
		editor3.getModel().setValue(defaultNbThread);
		editor3.getFormat().applyPattern("##");

		ImageIcon icon;

		// buttons
		save = new JButton(Translator.swap("Shared.save"));
		icon = IconLoader.getIcon("save_16.png");
		save.setIcon(icon);

		run = new JButton(Translator.swap("AutomationEditor.run"));
		icon = IconLoader.getIcon("ok_16.png");
		run.setIcon(icon);

		close = new JButton(Translator.swap("Shared.close"));
		icon = IconLoader.getIcon("cancel_16.png");
		close.setIcon(icon);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);

		controlPanel.add(new JLabel(Translator.swap("Threads") + ":"));
		controlPanel.add(nbThreadSpinner);
		controlPanel.add(plan);
		controlPanel.add(new JLabel(Translator.swap("Repetition") + ":"));
		controlPanel.add(repetitionSpinner);

		controlPanel.add(keepProjectButton);
		controlPanel.add(summaryButton);

		controlPanel2.add(run);
		controlPanel2.add(save);
		controlPanel2.add(close);
		controlPanel2.add(help);

		run.addActionListener(this);
		save.addActionListener(this);
		close.addActionListener(this);
		help.addActionListener(this);

		JPanel southPanel = new JPanel(new BorderLayout());
		southPanel.add(controlPanel, BorderLayout.NORTH);
		southPanel.add(controlPanel2, BorderLayout.SOUTH);

		// southPanel.add(summaryPanel, BorderLayout.NORTH);

		// Add component to main layout

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(scrollPane, BorderLayout.CENTER);
		getContentPane().add(northPanel, BorderLayout.NORTH);
		getContentPane().add(southPanel, BorderLayout.SOUTH);

		setTitle(Translator.swap("AutomationEditor.title"));

	}

	private JTable makeTable() {

		// create jtable if necessary
		if (table == null) {
			table = new JTable(dataModel);
			table.setAutoCreateRowSorter(true);
			table.setRowSorter(sorter);
		}

		return table;
	}

	
	/**
	 * Event table model
	 */
	private static class AutomationEventModel extends AbstractTableModel {

		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		private String[] columnNames;
		private String[][] rawData;
		private Class<?>[] types;
		private Automation.Event event;
		private AutomationVariation variation;

		/** Constructor */
		public AutomationEventModel(Automation.Event ev, AutomationVariation av) {

			String[] cn = { Translator.swap("AutomationEditor.parameterName"),
					Translator.swap("AutomationEditor.defaultValue"),
					Translator.swap("AutomationEditor.variation") };

			columnNames = cn;
			variation = av;
			initRawData(ev);
		}

		/** Build String array for table model */
		protected void initRawData(Automation.Event ev) {

			event = ev;
			int size = getRowCount();
			rawData = new String[size][getColumnCount()];
			types = new Class<?>[size];

			int index = 0;
			for (String key : ev.parameters.keySet()) {
				Object v = ev.parameters.get(key, false);
				String valString = "";
				String varString = "";

				// Try to get type from default value
				if (v != null) {
					types[index] = v.getClass();
					valString = v.toString();

				} else { // search in class

					try {
						Class<?> c = this.getClass().getClassLoader()
								.loadClass(ev.parameters.className);
						Field f = c.getField(key);
						types[index] = f.getType();
						valString = "";

					} catch (Exception e) {
					}
				}

				// Build String for object
				List<Object> l = variation.get(ev.id, key);
				List<String> results = new ArrayList<String>();
				if (l != null) {

					for (Object o : l) {
						String r = "null";
						if (o != null)
							r = o.toString();
						results.add(r);
					}
					varString = StringUtil.join(results, ", ");
				}

				rawData[index][0] = key;
				rawData[index][1] = valString;
				rawData[index][2] = varString;
				index++;
			}

		}

		/** parse a simple string to a list of objects */
		static public List<Object> buildFromString(Class<?> cl, String s) {

			if (cl == null || s.equals("null")) {
				return null;
			}

			// try to parse a range : "[inf:sup:step]"
			if (s.startsWith("[") && s.endsWith("]")) {
				String exp = s.substring(1, s.length() - 1);
				String[] ss = exp.split(":");

				if (ss.length == 3) {
					try {
						BigDecimal inf = new BigDecimal(ss[0].trim());
						BigDecimal sup = new BigDecimal(ss[1].trim());
						BigDecimal step = new BigDecimal(ss[2].trim());
						List<Object> ret = new ArrayList<Object>();

						for (BigDecimal i = inf; i.compareTo(sup) < 0; i = i
								.add(step)) {

							if (i.floatValue() == i.intValue()) {
								ret.add(i.intValue());
							} else {
								ret.add(i.doubleValue());
							}
						}
						return ret;

					} catch (Exception e) {
						Log.println(Log.ERROR, "AutomationEditor",
								"build from String", e);
						e.printStackTrace();
					}
				}

			}

			// try String construcotr
			try {
				Object obj;
				Constructor<?> constructor = cl
						.getConstructor(new Class[] { String.class });
				obj = constructor.newInstance(new Object[] { s });
				List<Object> ret = new ArrayList<Object>();
				ret.add(obj);
				return ret;

			} catch (Exception e) {
				return null;
			}

		}

		/** Table Model functions */
		@Override
		public String getColumnName(int col) {
			return columnNames[col].toString();
		}

		@Override
		public int getRowCount() {
			return event.parameters.size();
		}

		@Override
		public int getColumnCount() {
			return columnNames.length;
		}

		@Override
		public Object getValueAt(int row, int col) {
			return rawData[row][col];
		}

		@Override
		public boolean isCellEditable(int row, int col) {
			if (col == 0) {
				return false;
			}

			Class<?> cl = types[row];

			try {
				return cl != null
						&& cl.getConstructor(new Class[] { String.class }) != null;
			} catch (Exception e) {
				return false;
			}

		}

		/** Parse and Validate value */
		@Override
		public void setValueAt(Object value, int row, int col) {

			String key = rawData[row][0];
			String result = rawData[row][col];

			// value column
			if (col == 1) {

				String s = (String) value;
				s = s.trim();
				List<Object> lo = buildFromString(types[row], s);
				if (lo != null && lo.size() > 0) {
					Object o = lo.get(0); // first object
					result = o.toString();
					event.parameters.put(key, o);
				}

			} else if (col == 2) { // variation column

				// empty entry
				if (value == null || value.equals("")) {

					variation.set(event.id, key, null);
					result = "";

				} else { // list of value

					List<Object> vars = new ArrayList<Object>();
					result = "";
					String[] ss = ((String) value).split(",");

					List<String> results = new ArrayList<String>();

					for (String s : ss) {

						s = s.trim();
						List<Object> lo = buildFromString(types[row], s);

						if (lo != null && lo.size() > 0) {
							for (Object o : lo) {
								String r = o.toString();
								results.add(r);
								vars.add(o);
							}
						} else {
							results.add("null");
							vars.add(null);
						}

					}

					result = StringUtil.join(results, ", ");
					variation.set(event.id, key, vars);

				}
			}

			rawData[row][col] = result;
			fireTableCellUpdated(row, col);

		}

	}
	
}
