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
 * co
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.generictool;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.RowSorter;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.extension.generictool.GraphicalExtensionManagerTablePopup.MenuCommand;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import jeeb.lib.util.Alert;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.LogBrowser;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.CompatibilityManager;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A graphical component to display information about Extensions. Contains a
 * table of compatibility with the available modules.
 * 
 * @author F. de Coligny - August 2001, August 2018
 */
public class GraphicalExtensionManager extends AmapDialog
		implements ItemListener, ListSelectionListener, ActionListener, MouseListener {

	// fc-6.8.2018 Removed dependency to CapsisExtensionManager to prepare
	// moving to jeeb.lib.util and adaptation to AMAPstudio application

	static {
		Translator.addBundle("capsis.extension.generictool.GraphicalExtensionManager");
	}

	/**
	 * A table row sorter with a memory, see resort ().
	 */
	private static class SpecificSorter extends TableRowSorter {
		private List<? extends RowSorter.SortKey> memoSortKeys;

		/** Constructor */
		public SpecificSorter(TableModel m) {
			super(m);
		}

		/** Memorize the sort keys when needed */
		public void toggleSortOrder(int column) {
			super.toggleSortOrder(column);
			memoSortKeys = getSortKeys();

		}

		/** Resort if requested */
		public void resort() {
			if (memoSortKeys != null) {
				setSortKeys(memoSortKeys);
			}
			sort();
		}
	}

	/**
	 * A renderer for the compatibility columns : checkbox / checked or not / "veto"
	 * or not...
	 */
	private static class CompatibilityRenderer extends JPanel implements TableCellRenderer {
		private String compatible = Translator.swap("GraphicalExtensionManager.compatible");
		private String notCompatible = Translator.swap("GraphicalExtensionManager.notCompatible");

		private String empty = "";
		private String emptyFalse = notCompatible;
		private String emptyTrue = compatible;
		private String veto = Translator.swap("GraphicalExtensionManager.veto");
		private String vetoFalse = notCompatible + ", " + Translator.swap("GraphicalExtensionManager.veto");
		private String vetoTrue = compatible + ", " + Translator.swap("GraphicalExtensionManager.veto");
		private String inList = Translator.swap("GraphicalExtensionManager.inList");

		private String inListFalse = notCompatible + ", " + Translator.swap("GraphicalExtensionManager.inList");
		private String inListTrue = compatible + ", " + Translator.swap("GraphicalExtensionManager.inList");

		private DefaultTableCellRenderer originalRenderer;

		public CompatibilityRenderer() {
			originalRenderer = new DefaultTableCellRenderer();
		}

		// Updated [mtv]... - fc - 16.10.2007
		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {

			boolean compatibilityColumn = (value instanceof String) && ((String) value).startsWith("[")
					&& ((String) value).endsWith("]");

			// fc-8.10.2013
			JLabel defaultComponent = (JLabel) originalRenderer.getTableCellRendererComponent(table, value, isSelected,
					hasFocus, row, column);

			if (!compatibilityColumn) {
				return defaultComponent;
			}

			// Compatibility column
			String v = (String) value;

			v = v.substring(1, v.length() - 1);
			if (containsExactly(v, "")) {
				defaultComponent.setText(empty);

			} else if (containsExactly(v, "m")) {
				defaultComponent.setText(emptyFalse);

			} else if (containsExactly(v, "mt")) {
				defaultComponent.setText(emptyTrue);

			} else if (containsExactly(v, "v")) {
				defaultComponent.setText(veto);

			} else if (containsExactly(v, "mv")) {
				defaultComponent.setText(vetoFalse);

			} else if (containsExactly(v, "mtv")) {
				defaultComponent.setText(vetoTrue);

			} else if (containsExactly(v, "l")) {
				defaultComponent.setText(inList);

			} else if (containsExactly(v, "ml")) {
				defaultComponent.setText(inListFalse);

			} else if (containsExactly(v, "mtl")) {
				defaultComponent.setText(inListTrue);

			} else {
				defaultComponent.setText(empty); // security
			}

			return defaultComponent;

		}

		// Returns true if v contains all the characters of chars and no more -
		// fc - 16.10.2007
		private boolean containsExactly(String v, String chars) {
			if (v.length() != chars.length()) {
				return false;
			}
			for (int i = 0; i < chars.length(); i++) {
				if (v.indexOf(chars.charAt(i)) == -1) {
					return false;
				}
			}
			return true;
		}
	}

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	public static final int KEY_COLUMN = 0; // we use a key in it to retrieve
											// rows when sorted differently

	public static final int MIN_COLUMN_NUMBER = 5; // fc - 8.1.2004

	private JCheckBox viewName;
	private JCheckBox viewPackage;
	private JCheckBox viewVersion;
	private JCheckBox viewAuthor;
	private JCheckBox viewCompatibility;

	private String currentExtensionType;
	private JButton close;
	private JButton help;

	// fc-13.12.2017 Translation -> type
	private Map<String, String> typeMap;

	private JComboBox combo;

	private String lastSelectedTypeName = null;
	private int lastSelectedRow = -1;
	private JScrollPane tableScrollPane;
	private JTable table;
	private JButton update; // fc - 1.2.2006

	// private Map<String,String> simple_className; // ex: DETimeH -
	// capsis.extension.dataextractor.DETimeH
	private ArrayList<String> classNameList; // in tableModel order:
												// classNameList.get
												// (tableModelRow) returns the
												// className at the given row

	private ExtensionManager extensionManager;

	private String currentExtensionClassName;
	private JTextField extensionName; // fc - 9.2.2004
	private JButton extensionHelp; // fc - 9.2.2004
	private Icon helpIcon = IconLoader.getIcon("help_16.png");

	private JTextPane propertyPanel;

	private String notFound;

	private CompatibilityRenderer compatibilityRenderer = new CompatibilityRenderer();

	private int memoRow;
	private int memoCol;
	private CompatibilityManager compatMan;

	private SpecificSorter sorter;

	/**
	 * Constructor
	 */
	public GraphicalExtensionManager(Window window, ExtensionManager extensionManager) throws Exception {
		// public GraphicalExtensionManager(Window window) throws Exception {

		// fc-6.8.2018 Added extensionManager parameter just upper

		super(window);

		try {
			sorter = null;

			currentExtensionClassName = ""; // fc - 9.2.2004
			notFound = Translator.swap("GraphicalExtensionManager.notFound");

			// fc-6.8.2018 the extensionManage is now passed as a parameter
			this.extensionManager = extensionManager;
			// extensionManager = CapsisExtensionManager.getInstance();

			compatMan = extensionManager.getCompatibilityManager();

			typeMap = new HashMap<>();

			createUI();

			// fc-22.7.2019 added .name
			setTitle(Translator.swap("GraphicalExtensionManager.name"));

			addWindowListener(new WindowAdapter() {
				public void windowClosing(WindowEvent evt) {
					close();
				}
			});

			setPreferredSize(new Dimension(600, 450));
			activateSizeMemorization(getClass().getName());
			pack(); // sets the size

			setVisible(true);
			setModal(false);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "GraphicalExtensionManager.c ()", exc.toString(), exc);
		}
	}

	/**
	 * A helper to detect the extension anomalies and upgrade the corresponding
	 * extensions. Prints all extension anomalies.
	 */
	// private void printExtensionManagerWarnings() {
	// StringBuffer b = new StringBuffer();
	// for (String line :
	// CapsisExtensionManager.getInstance().getExtensionWarnings()) {
	// b.append(line);
	// b.append('\n');
	// }
	// Log.println("capsis", b.toString());
	// }
	//
	// fc-14.12.2016 see ExtensionManager.getExtensionWarnings(List<String>
	// classNames), called in GraphicalExtensionManager on a selection of
	// extensions + right click

	/**
	 * Return a Map with in used models, i.e. the models currently attached to
	 * Scenarios in the Session.
	 */
	private Map<String, GModel> retrieveInUseModels() {
		Map<String, GModel> modelName_model = new Hashtable<String, GModel>(); // no
																				// duplicates
		try {
			for (Project s : Engine.getInstance().getSession().getProjects()) {

				GModel m = s.getModel();
				modelName_model.put(m.getIdCard().getModelName(), m);
			}
		} catch (Exception e) {
		} // maybe no session at all ;-)
		return modelName_model;
	}

	/**
	 * Create a table with a row for each known extension of the given type.
	 */
	synchronized private void createTableForType(String typeName) {
		if (typeName == null) {
			return;
		}

		currentExtensionType = typeName;

		// fc-13.12.2017 typeName is the translated name in combo, get the
		// matching type

		final String fType = typeMap.get(typeName);

		final Vector<String> columnNames = new Vector<String>();
		final Vector<Vector<String>> rows = new Vector<Vector<String>>();

		SwingWorker3 task = new SwingWorker3() {

			public Object construct() { // Runs in new Thread

				try { // fc-3.9.2018

					Map<String, GModel> inUseModels = retrieveInUseModels();
					Set<String> inUseModelsNames = inUseModels.keySet();

					// simple_className = new HashMap<String,String> ();
					classNameList = new ArrayList<String>();

					// class column is needed
					columnNames.add(Translator.swap("GraphicalExtensionManager.class"));
					if (viewName.isSelected()) {
						columnNames.add(Translator.swap("GraphicalExtensionManager.name2"));
					}
					if (viewPackage.isSelected()) {
						columnNames.add(Translator.swap("GraphicalExtensionManager.package"));
					}
					if (viewVersion.isSelected()) {
						columnNames.add(Translator.swap("GraphicalExtensionManager.version"));
					}
					if (viewAuthor.isSelected()) {
						columnNames.add(Translator.swap("GraphicalExtensionManager.author"));
					}

					if (viewCompatibility.isSelected()) {
						for (Iterator i = inUseModelsNames.iterator(); i.hasNext();) {
							String modelName = (String) i.next(); // "PP3"
							GModel model = (GModel) inUseModels.get(modelName);
							String referentName = model.getClass().getName();
							if (compatMan.usesExtensionList(referentName)) {
								modelName = modelName + " (" + Translator.swap("GraphicalExtensionManager.list") + ")";
							} else if (compatMan.usesExtensionVeto(referentName)) {
								modelName = modelName + " (" + Translator.swap("GraphicalExtensionManager.vetos") + ")";
							}

							columnNames.add(modelName);
						}
					}

					// For each extension

					// System.out.println
					// ("GraphicalExtMan.createTableForType "+fType+"...");
					int count = 0;

					for (String className : extensionManager.getExtensionClassNames(fType)) {

						count++;

						Vector<String> row = new Vector<String>();

						String simpleClassName = AmapTools.getClassSimpleName(className);
						// simple_className.put (simpleClassName, className);
						classNameList.add(className); // replaces
														// simple_className
														// (wrong management of
														// duplicates)

						row.add(simpleClassName); // in first logical position
						if (viewName.isSelected()) {
							row.add(ExtensionManager.getName(className));
						}
						if (viewPackage.isSelected()) {
							row.add(AmapTools.getPackageName(className));
						}

						String version = notFound;
						String author = notFound;

						version = ExtensionManager.getVersion(className);
						author = ExtensionManager.getAuthor(className);

						if (viewVersion.isSelected()) {
							row.add(version);
						}
						if (viewAuthor.isSelected()) {
							row.add(author);
						}

						// fc - 1.12.2003 - compatibility in main table
						if (viewCompatibility.isSelected()) {
							for (Iterator j = inUseModels.values().iterator(); j.hasNext();) {
								GModel model = (GModel) j.next();
								String referentName = model.getClass().getName();

								StringBuffer status = new StringBuffer("[");

								status.append('m');
								if (extensionManager.isCompatible(className, model, false)) {
									status.append('t');
								}

								if (compatMan.isVetoed(referentName, className)
										&& compatMan.usesExtensionVeto(referentName)) {
									status.append('v'); // "Veto"
								} else {
									if (!compatMan.isVetoed(referentName, className)
											&& compatMan.usesExtensionList(referentName)) {
										status.append('l'); // "In list" // fc -
															// 23.10.2007
									}
								}
								status.append(']');
								row.add(status.toString());
							}
						}

						rows.add(row);
					}

					// System.out.println
					// ("...GraphicalExtMan.ExtensionManager.createTableForType
					// (count: "+count+")");

					return null;

				} catch (Exception e) {
					// fc-3.9.2018
					// In case of error somewhere
					return e;

				}
			}

			public void finished() { // Runs in Swing Thread when construct is
										// over

				Object constructResult = get();
				if (constructResult instanceof Exception) {
//					table = null;
					MessageDialog.print(this, Translator.swap("GraphicalExtensionManager.anErrorOccurredCheckLog"),
							(Exception) constructResult);
					return;
				}

				// fc - 22.4.2009 - trying to keep sorting in table when
				// refreshing
				// More to do: see if we can change the rows in the
				// table.getModel ()
				// instead of changing the model
				if (table == null) {
					table = new JTable(rows, columnNames) {

						// nb-27.08.2018
						// private static final long serialVersionUID = 1L;

						public TableCellRenderer getCellRenderer(int row, int column) {
							return compatibilityRenderer;
						}

						public boolean isCellEditable(int row, int col) {
							return false;
						}
					};

					// SORT OREDER...
					// table.setAutoCreateRowSorter (true);
					sorter = new SpecificSorter(table.getModel());
					table.setRowSorter(sorter);

					// fc-8.10.213
					for (int col = 0; col < table.getColumnCount(); col++) {
						sorter.setComparator(col, String.CASE_INSENSITIVE_ORDER);
					}

					table.getSelectionModel().addListSelectionListener(GraphicalExtensionManager.this);
					table.addMouseListener(GraphicalExtensionManager.this); // fc
																			// -
																			// 8.1.2004
																			// -
																			// add/remove
																			// veto
																			// ->
																			// ctx
																			// menu

					lastSelectedRow = -1; // two events are thrown on row
											// selection : press and release
											// mouse button

					// ~ viewport = new JViewport ();
					// ~ viewport.setView (t);
					// ~ tableScrollPane.setViewport (viewport);

					tableScrollPane.getViewport().setView(table);

					// restore view position
					// ~ if (viewPosition != null) {viewport.setViewPosition
					// (viewPosition);}
					// ~ resetViewport = false;

					// ~ setTable (t);
				} else {
					DefaultTableModel m = new DefaultTableModel(rows, columnNames);
					table.setModel(m);
					// Update sorter
					sorter.setModel(m);
					// Restore sort order
					sorter.resort();
				}

				// // Restore sort order
				// sorter.resort ();

				currentExtensionClassName = ""; // fc - 9.2.2004
				extensionHelp.setEnabled(false);
				extensionName.setText(""); // fc - 9.2.2004

				propertyPanel.setText(currentExtensionType + " : " + table.getRowCount()); // fc
																							// -
																							// 27.6.2006
																							// -
																							// number
																							// of
																							// extensions
																							// of
																							// the
																							// current
																							// type

				getContentPane().validate();

			}
		};

		task.start();
	}

	// Memo main table reference
	//
	// ~ synchronized private void setTable (JTable t) {table = t;}

	/** Listen to the extension type combo box. */
	public void itemStateChanged(ItemEvent evt) {
		if (evt.getSource().equals(combo)) {
			Object o = evt.getItem();

			String typeName = (String) o;

			// First time fc-23.11.2011 (there was a bug in some cases)
			if (lastSelectedTypeName == null) {
				lastSelectedTypeName = typeName;
				return;
			}

			if (!(typeName.equals(lastSelectedTypeName))) {
				lastSelectedTypeName = typeName;
				typeChangeAction(typeName);
			}
		}
	}

	/**
	 * Extension type selection in combo box -> sync. main table
	 */
	private void typeChangeAction(String typeName) {
		Settings.setProperty("capsis.graphical.extension.manager.type.name", typeName);

		createTableForType(typeName);
		extensionHelp.setEnabled(false);
	}

	/**
	 * Update table for current type : a general refresh
	 */
	private void update() {

		// Print extension warnings to the log (to help fix the old extensions)
		// printExtensionManagerWarnings ();

		String type = currentExtensionType;
		currentExtensionType = "";

		createTableForType(type); // update gem
	}

	/**
	 * Row selection in main table -> synchronize description field
	 */
	public void valueChanged(ListSelectionEvent evt) {
		updatePropertyPanel();
	}

	private void updatePropertyPanel() {

		int selectedRow = table.getSelectedRow();
		if (selectedRow < 0) {
			return;
		}
		selectedRow = table.convertRowIndexToModel(selectedRow);

		// Selection throws 2 events : mouse press and release -> we want only
		// one
		if (selectedRow != lastSelectedRow) {
			lastSelectedRow = selectedRow;

			try {

				// Retrieve (complete) className of selected row extension
				TableModel model = table.getModel();

				// . String className = (String) simple_className.get
				// (model.getValueAt (selectedRow, 0));
				String className = classNameList.get(selectedRow);

				// fc-6.8.2018
				extensionManager.getPropertyPanel(className, propertyPanel);
				// CapsisExtensionManager.getInstance().getPropertyPanel(className,
				// propertyPanel);

				// Various updates
				String name = ExtensionManager.getName(className);
				extensionName.setText(name);
				currentExtensionClassName = className;

				if (Helper.hasHelpFor(currentExtensionClassName)) {
					extensionHelp.setEnabled(true);
				} else {
					extensionHelp.setEnabled(false);
				}

			} catch (Exception e) {
				// There can be an exception if change in combo and there was a
				// selection
			}

			// // Description sync.
			// try {
			//
			// String name = ExtensionManager.getName (className);
			// String text = name + " : " + ExtensionManager.getDescription
			// (className);
			//
			//
			// //System.out.println
			// ("GraphicalExtMan.valueChanged () name:"+name);
			//
			//
			// // Added this message to detect Paleo extensions
			// try {
			// Class cls = Class.forName (className);
			// if (PaleoExtension.class.isAssignableFrom (cls))
			// text += "\n" + Translator.swap
			// ("GraphicalExtensionManager.oldExtensionArchitecture");
			// } catch (Exception e) {
			// text += "\n" + "Exception: " + e;
			// }
			//
			// description.setText (text); // text zone
			// extensionName.setText (name); // fc - 9.2.2004
			// currentExtensionClassName = className; // fc - 9.2.2004
			// if (Helper.hasHelpFor (currentExtensionClassName)) {
			// extensionHelp.setEnabled (true);
			// } else {
			// extensionHelp.setEnabled (false);
			// }
			//
			// } catch (Throwable t) {
			// description.setText (Translator
			// .swap
			// ("GraphicalExtensionManager.foundInEtcCapsisExtensionsButMissingClass"));
			// // text
			// // zone
			// Log.println (Log.WARNING,
			// "GraphicalExtensionManager.valueChanged ()",
			// "Can not get description for className=" + className
			// + " - maybe lack of extension phantom construtor", t);
			// }

		}
	}

	/**
	 * Called on Escape.
	 */
	protected void escapePressed() {
		close();
	}

	private void close() {

		Settings.setProperty("capsis.graphical.extension.manager.viewName", "" + viewName.isSelected());
		Settings.setProperty("capsis.graphical.extension.manager.viewPackage", "" + viewPackage.isSelected());
		Settings.setProperty("capsis.graphical.extension.manager.viewVersion", "" + viewVersion.isSelected());
		Settings.setProperty("capsis.graphical.extension.manager.viewAuthor", "" + viewAuthor.isSelected());
		Settings.setProperty("capsis.graphical.extension.manager.viewCompatibility",
				"" + viewCompatibility.isSelected());

		dispose();
	}

	/**
	 * Listens to the buttons in control panel.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(close)) {
			close();

		} else if (evt.getSource() instanceof JCheckBox) { // fc - 1.2.2006
			update(); // columns titles changed

		} else if (evt.getSource().equals(update)) { // fc - 1.2.2006
			update();

		} else if (evt.getSource().equals(extensionHelp)) { // fc - 9.2.2004
			Helper.helpFor(currentExtensionClassName);

		} else if (evt.getSource() instanceof JMenuItem) {

			JMenuItem item = (JMenuItem) evt.getSource();

			if (item.getMnemonic() == MenuCommand.EXTENSION_NATIVE.ordinal()) {
				String referentName = item.getActionCommand();

				// fc - 25.8.2008
				compatMan.setNativePolicy(referentName);
				update();
				// Visible lists of extensions may need update
				Current.getInstance().forceUpdate(); // general without cache
														// tools refreshing

			} else if (item.getMnemonic() == MenuCommand.EXTENSION_VETO.ordinal()) {
				String referentName = item.getActionCommand();

				// fc - 25.8.2008
				compatMan.setVetoPolicy(referentName);
				update();
				// Visible lists of extensions may need update
				Current.getInstance().forceUpdate(); // general without cache
														// tools refreshing

			} else if (item.getMnemonic() == MenuCommand.EXTENSION_LIST.ordinal()) {
				String referentName = item.getActionCommand();

				compatMan.setListPolicy(referentName);
				update();
				// Visible lists of extensions may need update
				Current.getInstance().forceUpdate(); // general without cache
														// tools refreshing

			} else if (item.getMnemonic() == MenuCommand.SELECTED_EXTENSIONS_WARNINGS.ordinal()) {
				String referentName = item.getActionCommand();

				List<String> classNames = new ArrayList<String>();
				int[] indices = table.getSelectedRows();

				if (indices.length == 0) {
					MessageDialog.print(this, Translator.swap("GraphicalExtensionManager.pleaseSelectRows"));
					return;
				}

				for (int i = 0; i < indices.length; i++) {
					int numRow = table.convertRowIndexToModel(indices[i]);

					// Build extension class name including package
					// String col0 = (String) table.getModel ().getValueAt
					// (numRow, 0);
					// String extensionClassName = (String) simple_className.get
					// (col0);
					String extensionClassName = classNameList.get(numRow);

					classNames.add(extensionClassName);
				}

				// fc-6.8.2018
				List<String> lines = extensionManager.getExtensionWarnings(classNames);
				// List<String> lines =
				// CapsisExtensionManager.getInstance().getExtensionWarnings(classNames);

				for (String line : lines) {
					Log.println("ExtensionWarnings", line);
				}
				Log.println("ExtensionWarnings", ""); // blank line
				try {
					LogBrowser b = new LogBrowser(this);
					b.selectLog("ExtensionWarnings");
					b.refreshContent();
				} catch (Exception e) {
					Alert.print(
							Translator.swap("GraphicalExtensionManager.seeResultInLogViewer") + " ExtensionWarnings");
				}

			} else if (item.getMnemonic() == MenuCommand.ALL_EXTENSIONS_WARNINGS.ordinal()) {
				String referentName = item.getActionCommand();

				// fc-6.8..2018
				List<String> lines = extensionManager.getExtensionWarnings();
				// List<String> lines =
				// CapsisExtensionManager.getInstance().getExtensionWarnings();

				for (String line : lines) {
					Log.println("ExtensionWarnings", line);
				}
				Log.println("ExtensionWarnings", ""); // blank line
				try {
					LogBrowser b = new LogBrowser(this);
					b.selectLog("ExtensionWarnings");
					b.refreshContent();
				} catch (Exception e) {
					Alert.print(
							Translator.swap("GraphicalExtensionManager.seeResultInLogViewer") + " ExtensionWarnings");
				}

			} else {

				String modelName = (String) table.getColumnName(memoCol);

				if (modelName.lastIndexOf("(") != -1) { // fc - 23.10.2007 -
														// "PP3 (list)" /
														// "PP3 (Vetoes)" ->
														// PP3
					modelName = modelName.substring(0, modelName.lastIndexOf("("));
					modelName = modelName.trim();
				}

				// fc - 15.3.2004 - get model package name in IdCard instead of
				// column title.
				// (problem detected under Safe)

				// fc-18.2.2022 Tracking a bug for Pinus Pinaster and vetoes
				Map<String, GModel> models = null;
				GModel m = null;
				String referentName = null;
				String pkgName = null;
				try {
					models = retrieveInUseModels();
					m = (GModel) models.get(modelName);
					referentName = m.getClass().getName();
					pkgName = m.getIdCard().getModelPackageName();
				} catch (Exception e) {
					Log.println(Log.WARNING, "GraphicalExtensionManager.actionPerformed ()",
							"Could not find modelName: " + modelName + " in the list of in use models: "
									+ AmapTools.toString(models)+", m: "+m+", referentName:"+referentName+", pkgname: "+pkgName,
							e);
				}

				// fc - 7.3.2005 - multiple rows processing
				int[] indices = table.getSelectedRows();
				for (int i = 0; i < indices.length; i++) {
					int numRow = table.convertRowIndexToModel(indices[i]);

					// Build extension class name including package
					// String col0 = (String) table.getModel ().getValueAt
					// (numRow, 0);

					// fc - 26.10.2007
					// String extensionClassName = (String) simple_className.get
					// (col0);
					String extensionClassName = classNameList.get(numRow);

					if (item.getMnemonic() == MenuCommand.ADD_VETO_OPTION.ordinal()) {

						compatMan.addVeto(referentName, extensionClassName);
						StatusDispatcher.print(Translator.swap("GraphicalExtensionManager.addedVetoFor") + " "
								+ referentName + " / " + extensionClassName);
						compatMan.saveExtensionVeto(referentName);

					} else if (item.getMnemonic() == MenuCommand.REMOVE_VETO_OPTION.ordinal()) {

						compatMan.removeVeto(referentName, extensionClassName);
						StatusDispatcher.print(Translator.swap("GraphicalExtensionManager.removedVetoFor") + " "
								+ referentName + " / " + extensionClassName);
						compatMan.saveExtensionVeto(referentName);

					} else if (item.getMnemonic() == MenuCommand.ADD_IN_LIST_OPTION.ordinal()) {

						compatMan.includeInList(referentName, extensionClassName);
						StatusDispatcher.print(Translator.swap("GraphicalExtensionManager.includedInListFor") + " "
								+ referentName + " / " + extensionClassName);
						compatMan.saveExtensionList(referentName);

					} else if (item.getMnemonic() == MenuCommand.REMOVE_FROM_LIST_OPTION.ordinal()) {

						compatMan.excludeFromList(referentName, extensionClassName);
						StatusDispatcher.print(Translator.swap("GraphicalExtensionManager.excludedFromListFor") + " "
								+ referentName + " / " + extensionClassName);
						compatMan.saveExtensionList(referentName);

					}

				} // fc - 7.3.2005
				update();

				// Visible lists of extensions may need update
				Current.getInstance().forceUpdate(); // general without cache
														// tools refreshing
			}

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * From MouseListener interface.
	 */
	public void mouseClicked(MouseEvent mouseEvent) {
	}

	/**
	 * From MouseListener interface. Detection of right click to trigger veto
	 * modifications
	 */
	public void mousePressed(MouseEvent m) {
		memoRow = -1;
		memoCol = -1;
		Object obj = m.getSource();
		if (obj instanceof JTable) {
			JTable table = (JTable) obj;

			if ((m.getModifiers() & InputEvent.BUTTON3_MASK) == InputEvent.BUTTON3_MASK) {
				int x = m.getX();
				int y = m.getY();
				memoRow = table.rowAtPoint(new Point(x, y));
				memoCol = table.columnAtPoint(new Point(x, y));
				// Ensure some selection was made
				if (!table.isRowSelected(memoRow)) {
					table.getSelectionModel().setSelectionInterval(memoRow, memoRow);
				}

				// fc - 7.3.2005
				Object value = table.getValueAt(memoRow, memoCol);

				Collection<MenuCommand> options = new HashSet<MenuCommand>();
				String referentName = null;
				String modelName = null;

				boolean compatibilityColumn = (value instanceof String) && ((String) value).startsWith("[")
						&& ((String) value).endsWith("]");

				if (compatibilityColumn) { // fc - 16.10.2007

					Map inUseModels = retrieveInUseModels();
					String columnTitle = (String) table.getColumnName(memoCol);
					// String referentName = null;
					// String modelName = null;

					// search correct referent name
					for (Iterator i = inUseModels.keySet().iterator(); i.hasNext();) {
						modelName = (String) i.next();
						if (columnTitle.startsWith(modelName)) {
							GModel model = (GModel) inUseModels.get(modelName);
							referentName = model.getClass().getName();
							break;
						}
					}

					// Collection<MenuCommand> options = new
					// HashSet<MenuCommand> ();
					if (referentName != null) {

						String aux = (String) value;
						String v = aux.substring(1, aux.length() - 1); // remove
																		// '['
																		// and
																		// ']'

						if (compatMan.usesExtensionVeto(referentName)) {
							if (v.indexOf("v") != -1) {
								options.add(MenuCommand.REMOVE_VETO_OPTION);
							} else {
								options.add(MenuCommand.ADD_VETO_OPTION);
							}
						} else if (compatMan.usesExtensionList(referentName)) {
							if (v.indexOf("l") != -1) {
								options.add(MenuCommand.REMOVE_FROM_LIST_OPTION);
							} else {
								options.add(MenuCommand.ADD_IN_LIST_OPTION);
							}
						}

					}
					// Always add this entry at the end for compatibility
					// columns
					options.add(MenuCommand.MANAGE_COMPATIBILITY);
				}

				// Always add these entries
				options.add(MenuCommand.SELECTED_EXTENSIONS_WARNINGS);
				options.add(MenuCommand.ALL_EXTENSIONS_WARNINGS);

				JPopupMenu popup = new GraphicalExtensionManagerTablePopup(table, this, options, modelName,
						referentName, extensionManager);
				popup.show(m.getComponent(), m.getX(), m.getY());

			}
		}
	}

	/**
	 * MouseListener interface.
	 */
	public void mouseReleased(MouseEvent mouseEvent) {
	}

	/**
	 * MouseListener interface.
	 */
	public void mouseEntered(MouseEvent mouseEvent) {
	}

	/**
	 * MouseListener interface.
	 */
	public void mouseExited(MouseEvent mouseEvent) {
	}

	/**
	 * Creates the gui.
	 */
	private void createUI() {
		JPanel part1 = new JPanel(new BorderLayout());

		// 1.1. Extension types combo box
		ColumnPanel top = new ColumnPanel();

		Collection<String> types = extensionManager.getExtensionTypes();

		// fc-13.12.2017 Translate types in combo
		for (String type : types) {
			// fc-12.1.2018
			typeMap.put(Translator.swap(type) + " (" + type + ")", type);
			// typeMap.put(Translator.swap (type), type);
		}

		Set<String> sortedTypeNames = new TreeSet<String>(typeMap.keySet()); // fast
																				// :
																				// arround
		// 10 types
		combo = new JComboBox(new Vector(sortedTypeNames));

		try { // try to reproduce last selection
			String lastSelectedTypeName = Settings.getProperty("capsis.graphical.extension.manager.type.name", "");
			if (lastSelectedTypeName != null && lastSelectedTypeName.length() > 0) {
				combo.setSelectedItem(lastSelectedTypeName);
			}
		} catch (Exception e) {
		}
		combo.addItemListener(this);

		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("GraphicalExtensionManager.extensionType") + " :", 150));

		extensionName = new JTextField(5); // fc - 9.2.2004
		extensionName.setEditable(false);

		// fc - 9.2.2004 - adding extension help
		extensionHelp = new JButton(helpIcon);
		AmapTools.setSizeExactly(extensionHelp);
		extensionHelp.setToolTipText(Translator.swap("Shared.help"));
		extensionHelp.addActionListener(this);

		l1.add(combo);
		l1.add(extensionName);
		l1.add(extensionHelp);
		l1.addStrut0();
		top.add(l1);

		// 1.2. checkboxes for columns
		LinePanel l12 = new LinePanel();

		boolean n = Settings.getProperty("capsis.graphical.extension.manager.viewName", false);
		boolean p = Settings.getProperty("capsis.graphical.extension.manager.viewPackage", false);
		boolean v = Settings.getProperty("capsis.graphical.extension.manager.viewVersion", false);
		boolean a = Settings.getProperty("capsis.graphical.extension.manager.viewAuthor", false);
		boolean c = Settings.getProperty("capsis.graphical.extension.manager.viewCompatibility", false);

		// Default situation
		if (!n && !p && !v && !a && !c) {
			n = true;
			v = true;
			a = true;
			c = true;
		}

		viewName = new JCheckBox(Translator.swap("GraphicalExtensionManager.viewName"), n);
		viewPackage = new JCheckBox(Translator.swap("GraphicalExtensionManager.viewPackage"), p);
		viewVersion = new JCheckBox(Translator.swap("GraphicalExtensionManager.viewVersion"), v);
		viewAuthor = new JCheckBox(Translator.swap("GraphicalExtensionManager.viewAuthor"), a);
		viewCompatibility = new JCheckBox(Translator.swap("GraphicalExtensionManager.viewCompatibility"), c);
		viewName.addActionListener(this);
		viewPackage.addActionListener(this);
		viewVersion.addActionListener(this);
		viewAuthor.addActionListener(this);
		viewCompatibility.addActionListener(this);
		l12.add(viewName);
		l12.add(viewPackage);
		l12.add(viewVersion);
		l12.add(viewAuthor);
		l12.add(viewCompatibility);
		top.add(l12);
		top.addStrut0();

		part1.add(top, BorderLayout.NORTH);

		// 2. Main table : extensions matching with selected type in combo box.
		String defaultTypeName = (String) combo.getSelectedItem();

		tableScrollPane = new JScrollPane(new JTable());
		tableScrollPane.setPreferredSize(new Dimension(200, 300));

		createTableForType(defaultTypeName);

		// 3. Description of currently selected extension on main table AND
		// its compatibility table.

		propertyPanel = new JTextPane() {
			@Override
			public Dimension getPreferredScrollableViewportSize() {
				return new Dimension(100, 100);
			}
		};
		propertyPanel.setEditable(false);
		JScrollPane s2 = new JScrollPane(propertyPanel);
		s2.setPreferredSize(new Dimension(200, 100));

		JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tableScrollPane, s2);
		split.setResizeWeight(0.8);
		part1.add(split, BorderLayout.CENTER);

		// 4. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));

		// One button to update the tool (in case some projects were loaded /
		// closed)
		Icon icon = IconLoader.getIcon("view-refresh_16.png");
		update = new JButton(icon);
		AmapTools.setSizeExactly(update);
		update.setToolTipText(Translator.swap("GraphicalExtensionManager.update"));

		close = new JButton(Translator.swap("Shared.close"));
		help = new JButton(Translator.swap("Shared.help"));

		pControl.add(update);
		pControl.add(close);
		pControl.add(help);

		update.addActionListener(this);
		close.addActionListener(this);
		help.addActionListener(this);

		// Sset close as default (see AmapDialog)
		close.setDefaultCapable(true);
		getRootPane().setDefaultButton(close);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(part1, BorderLayout.CENTER);
		aux.add(pControl, BorderLayout.SOUTH);
		setContentPane(aux);
	}

}
