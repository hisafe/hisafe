/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2001-2020 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.extension.standviewer.svcontroller;

import java.awt.BorderLayout;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;

import capsis.defaulttype.ListOfIdentifiable;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Tree;
import capsis.util.ExcelAdapter;
import capsis.util.group.Grouper;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Log;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.NumberRenderer;
import jeeb.lib.util.PlottingPopup;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.StandardTableHeaderPopup;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;

/**
 * A simple table for SVController, shows few main variables in columns for all
 * the selected identifiables (often trees, sometimes Numberable, sometimes
 * Spatialized...) in the scene.
 * 
 * @author F. de Coligny - December 2020
 */
public class LegacyTablePanel extends JPanel implements ListSelectionListener {

	private static final String[] COLUMN_NAMES = { Translator.swap("SVController.id"),
			Translator.swap("SVController.age"), Translator.swap("SVController.dbh"),
			Translator.swap("SVController.height"), Translator.swap("SVController.number"),
			Translator.swap("SVController.sceneStatus"), Translator.swap("SVController.mark"),
			Translator.swap("SVController.x"), Translator.swap("SVController.y"), Translator.swap("SVController.z"),
			Translator.swap("SVController.species"), };

	private static final String BLANK = "";
	private static final String MARKED = "marked";

	private SVController svController;

	private NumberFormat formater;

	private SwingWorker3 task; // only one task at any time

	// A scene containing identifiable objects
	private ListOfIdentifiable scene;

	private Grouper grouper;
	private boolean grouperNot;

	private JTable table;
	private JScrollPane tablePane;

	protected Collection<Integer> memoSelectionIds;
	protected Collection memoSelection;
	protected Collection effectiveSelection;
	protected boolean reportTableSelection;

	// When the table model is updated, this map is also updated to refind an
	// identifiable given its id
	private Map<Integer, Identifiable> id_identifiable;

	private SpecificSorter sorter;
	private NumberRenderer numberRenderer;

	// fc-21.12.2020
	// If numberable individuals, sum of getNumber(), else simply number of
	// individuals
	private double numberOfIndividuals;

	/**
	 * Constructor
	 */
	public LegacyTablePanel(SVController svController) {

		this.svController = svController;

		sorter = null;

		formater = NumberFormat.getInstance(Locale.ENGLISH);
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(3);

		numberRenderer = new NumberRenderer(formater);

		id_identifiable = new HashMap<Integer, Identifiable>();

		memoSelectionIds = new ArrayList<Integer>();
		memoSelection = new ArrayList();

		reportTableSelection = true;

		initTable();

		setLayout(new BorderLayout());
		add(new JScrollPane(table), BorderLayout.CENTER);

	}

	private void initTable() {

		// Data model of the main table: empty at the beginning
		DefaultTableModel dataModel = new NonEditableTableModel(new String[1][10], COLUMN_NAMES);

		// Main table creation
		table = new JTable(dataModel);

		// fc-17.12.2020
		// When activating PlottingPopup.addPlottingFeaturesToTable(table, false),
		// PlottingPopup changes table selection behaviour, maybe
		// table.setCellSelectionEnabled(true) is problematic to reselect a line by
		// selectLines (...)

		// TMP searching why selection can not be restored fc-17.12.2020

//		// fc-15.12.2020 Add the plotting features on Right-click
//		PlottingPopup.addPlottingFeaturesToTable(table, false);
//		// fc-15.12.2020 Add the standard popup menu on the header
//		StandardTableHeaderPopup.addPopup(table);

		// TMP searching why selection can not be restored fc-17.12.2020

		// Numbers rendering
		// table.getColumnModel ().getColumn (0).setCellRenderer
		// (numberRenderer);
		table.setDefaultRenderer(Double.class, numberRenderer);
		table.setDefaultRenderer(Integer.class, numberRenderer);

		// table.setAutoCreateRowSorter(true);
		sorter = new SpecificSorter(table.getModel());
		table.setRowSorter(sorter);

		// Default: sort on column 0
		sorter.toggleSortOrder(0);

		table.setRowSelectionAllowed(true);
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);

		table.getSelectionModel().addListSelectionListener(this);

		// To allow cut & paste with system clipboard (MS-Excel & others...)
		ExcelAdapter myAd = new ExcelAdapter(table);

	}

	public void refresh(ListOfIdentifiable scene, Grouper grouper, boolean grouperNot) {

//		System.out.println("SVController.LegacyTablePanel () > refresh ()...");

		this.scene = scene;
		this.grouper = grouper;
		this.grouperNot = grouperNot;

		// Interrupt previous task if needed
		if (task != null)
			task.interrupt();

		// Disable table during task execution
		if (table != null)
			enableUI(false);

		// Final variables for access in other thread
		final ListOfIdentifiable final_scene = scene;
		final Grouper final_grouper = grouper;
		final boolean final_grouperNot = grouperNot;

		task = new SwingWorker3() {

//			private TableModel tableModel;

			private TableData tableData;
			private boolean interrupted;

			// To restore selection after refresh
			private Collection selectedIdsBeforeRefresh;

			public Object construct() {

				// Runs in a new thread, outside Swing GUI refreshing thread

				interrupted = false;

				// Store selection in order to restore it at the end
				selectedIdsBeforeRefresh = getSelectedIds();

				Collection<Identifiable> candidates = final_scene.getIdentifiables();

				if (final_grouper != null && !final_grouper.equals("")) {
					try {
						candidates = final_grouper.apply(candidates, final_grouperNot);
					} catch (Exception e) {
						Log.println(Log.WARNING, "LegacyTablePanel.refresh ()",
								"Exception while applying grouper " + final_grouper + " on scene " + final_scene, e);
					}
				}

				// makeTableData is the longest method (loop) if interrupted, it
				// returns null
				tableData = makeTableData(candidates);

//				if (tableModel == null) {
//					System.out.println("    SVController.LegacyTablePanel: thread interruption detected in construct");
//					interrupted = true;
//					return null;
//				}

				return tableData;
			}

			public void finished() {

				// Runs in the Swing thread when construct is over

				if (interrupted) {
					System.out.println("    SVController.LegacyTablePanel: thread interruption detected in finished ()");
					enableUI(true);
					return;
				}

				refreshTable(tableData, selectedIdsBeforeRefresh);
//				refreshTable(tableModel, finalMemoSelectionIds);

				enableUI(true);
			}
		};

		task.start();

	}

	/**
	 * Fills the TableData with the given candidate identifiables.
	 */
	private synchronized TableData makeTableData(Collection<Identifiable> candidates) {

		id_identifiable.clear();
		numberOfIndividuals = 0;

		Object[] identifiables = candidates.toArray();

		TableData tableData = new TableData(COLUMN_NAMES, identifiables.length);

//		String[] colNames = COLUMN_NAMES;
//		Object[][] mat = new Object[identifiables.length][11];

		for (int i = 0; i < identifiables.length; i++) {

			if (Thread.interrupted()) {
				System.out.println("*** SVController.LegacyTablePanel: thread was interrupted in makeTableData ()");
				return null;
			}

			Identifiable identifiable = (Identifiable) identifiables[i];

			// Maybe Tree
			Tree tree = null;
			if (identifiable instanceof Tree)
				tree = (Tree) identifiable;

			// Maybe Numberable
			Numberable numberable = null;
			if (identifiable instanceof Numberable)
				numberable = (Numberable) identifiable;

			// Maybe Spatialized
			Spatialized spatialized = null;
			if (identifiable instanceof Spatialized)
				spatialized = (Spatialized) identifiable;

			// Maybe Speciable
			Speciable speciable = null;
			if (identifiable instanceof Speciable)
				speciable = (Speciable) identifiable;

			// fc-21.12.2020
			if (numberable == null)
				numberOfIndividuals++;
			else
				numberOfIndividuals += numberable.getNumber();

			// This map contains id -> identifiable in the table
			id_identifiable.put(identifiable.getId(), identifiable);

			// The id must be in column 0, it is later used for selection interpretation

			Object[][] mat = tableData.mat; // shortcut

			mat[i][0] = new Integer(identifiable.getId());
			mat[i][1] = tree == null ? BLANK : new Integer(tree.getAge());
			mat[i][2] = tree == null ? BLANK : new Double(tree.getDbh());
			mat[i][3] = tree == null ? BLANK : new Double(tree.getHeight());

			mat[i][4] = numberable == null ? BLANK : new Double(numberable.getNumber());

			mat[i][5] = tree == null ? BLANK : tree.getStatusInScene();

			mat[i][5] = tree == null ? BLANK : (tree.isMarked() ? MARKED : BLANK);

			mat[i][7] = spatialized == null ? BLANK : new Double(spatialized.getX());
			mat[i][8] = spatialized == null ? BLANK : new Double(spatialized.getY());
			mat[i][9] = spatialized == null ? BLANK : new Double(spatialized.getZ());

			mat[i][10] = speciable == null ? BLANK
					: speciable.getSpecies().getName() + " (" + speciable.getSpecies().getValue() + ")";

		}

		// fc-17.12.2020 See below
//		DefaultTableModel dataModel = new NonEditableTableModel(mat, colNames);

		// fc-17.12.2020
		// Replace the data in the table's model (do not create a new table model to
		// avoid problems with the sorter and the selection)

//		DefaultTableModel dataModel = (DefaultTableModel) table.getModel();

		return tableData;
	}

	/**
	 * Updates the table with the given table model
	 */
	protected void refreshTable(TableData tableData, Collection selectedIdsBeforeRefresh) {

//		System.out.println("LegacyTablePanel > refreshTable() before dataModel.setDataVector()...");

		DefaultTableModel dataModel = (DefaultTableModel) table.getModel();
		dataModel.setDataVector(tableData.mat, tableData.colNames);

//		System.out.println("LegacyTablePanel > refreshTable() after dataModel.setDataVector()");

		// fc-17.12.2020
		// NOT NEEDED ANY MORE, we replaced the tableModel contents
//		this.table.setModel(tableModel);
//
//		// To allow cut & paste with system clipboard (MS-Excel & others...)
//		ExcelAdapter myAd = new ExcelAdapter(table);

		// fc-21.12.2020 We are in the same tableModel
//		// Update sorter
//		sorter.setModel(tableModel);

		// Restore sort order
		sorter.resort();

		// Restore selection in table (after sorter update)
		selectLines(selectedIdsBeforeRefresh);

//		// fc-16.12.2020
//		revalidate();
//		repaint();

		svController.updateLocalFields();

	}

	/**
	 * Enable / disable the gui during refresh thread work Must be called only from
	 * Swing thread (=> not synchronized)
	 */
	private void enableUI(boolean b) {

		table.setEnabled(b);
//		table.getTableHeader().revalidate();

	}

	/**
	 * Called when selection changed in table
	 */
	public void valueChanged(ListSelectionEvent evt) {

		if (evt.getValueIsAdjusting())
			return;

		if (!reportTableSelection)
			return;

		ListSelectionModel sm = (ListSelectionModel) evt.getSource();

		if (sm.isSelectionEmpty())
			return; // we are on the header

		// Retrieve tree id
		try {

			// LATER...
//			int viewRow = table.getSelectedRow();
//			int selectedRow = table.convertRowIndexToModel(viewRow);
//
//			int pos = descriptionPane.getVerticalScrollBar().getValue();
//
//			Tree tree = tableRow_tree.get(selectedRow);
//
//			descriptionField.setText(tree.toString());
//			descriptionPane.getVerticalScrollBar().setValue(pos);

			// Selection changed, tell the SVController

			// thisIsAReselection: true is we come from reselect, called by ovSelector
			// itself
			boolean thisIsAReselection = false;
			svController.processTableSelection(thisIsAReselection);

		} catch (Exception e) {

			// LATER...
//			descriptionField.setText("");
//			descriptionPane.getVerticalScrollBar().setValue(0);

		}
	}

	/**
	 * Select the lines matching the given ids then tell the SVController the
	 * selection changed.
	 */
	private void selectLines(Collection idsToBeSelected) {

		// fc-17.12.2020 NOT ENOUGH, to be investigated...

//		// PlottingPopup may have changed this, this disturbs the lines selection
//		boolean cellSelectionWasEnabled = table.getCellSelectionEnabled();
//
//		// Disactivate for a while
//		if (cellSelectionWasEnabled) {
//			table.setCellSelectionEnabled(false);
//			table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
//		}

		// 1. Search the rows matching the ids
//		TableModel tableModel = table.getModel();
		idsToBeSelected = new HashSet(idsToBeSelected); // Faster on contains () below

		// Disable table selection listening during selection update
		reportTableSelection = false;

		table.clearSelection();

		for (int modelRow = 0; modelRow < table.getModel().getRowCount(); modelRow++) {

			try {

				Integer id = (Integer) table.getModel().getValueAt(modelRow, 0);

				if (idsToBeSelected.contains(id)) {

					int viewRow = table.convertRowIndexToView(modelRow);

//					table.setRowSelectionInterval(viewRow, viewRow);
//					table.addRowSelectionInterval(viewRow, viewRow);

					table.getSelectionModel().addSelectionInterval(viewRow, viewRow);
				}

			} catch (Exception e) {
				// The sorter may cause trouble
			}
		}

		// fc-17.12.2020 NOT ENOUGH, to be investigated...
//		// Restore if needed
//		if (cellSelectionWasEnabled)
//			table.setCellSelectionEnabled(true);

		reportTableSelection = true;

		///

//		Collection<Integer> rowToBeSelected = new ArrayList<Integer>();
//		for (int i = 0; i < tableModel.getRowCount(); i++) {
//
//			int r = table.convertRowIndexToView(i);
//
//			if (idsToBeSelected.contains(tableModel.getValueAt(r, 0)))
//				rowToBeSelected.add(r);
//		}
//
//		// 2. Select the rows
//
//		System.out.println("LegacyTablePanel > selectLines > rowToBeSelected: " + AmapTools.toString(rowToBeSelected));
//
//		// Disable table selection listening during selection update
//		reportTableSelection = false;
//
//		table.getSelectionModel().clearSelection();
//		for (int i : rowToBeSelected) {
//			table.getSelectionModel().addSelectionInterval(i, i);
//		}
//
//		reportTableSelection = true;

		///

		// 3. Tell SVController the selection changed

		// thisIsAReselection: true in SVController is we come from reselect, called by
		// ovSelector itself
		boolean thisIsAReselection = false;
		svController.processTableSelection(thisIsAReselection);

	}

	/**
	 * Returns the ids (column 0) of all the selected lines, can be used to restore
	 * selection after an update of the table on another step.
	 */
	public Collection getSelectedIds() {

		Collection selectedIds = new ArrayList();

		for (Object o : getSelectedObjects()) {

			selectedIds.add(((Identifiable) o).getId());
		}

		return selectedIds;
	}

	/**
	 * Returns the objects matching the current selection in the table.
	 */
	public Collection getSelectedObjects() {

		Collection selectedObjects = new ArrayList();

		int[] selectedRows = table.getSelectedRows();
		TableModel model = table.getModel();

		for (int i = 0; i < selectedRows.length; i++) {

			int r = table.convertRowIndexToModel(selectedRows[i]);
			int id = ((Integer) model.getValueAt(r, 0)).intValue();

//			memoSelectionIds.add(treeId);

			Identifiable identifiable = id_identifiable.get(id);

			selectedObjects.add(identifiable);

		}

		return selectedObjects;

	}

	public Collection<Identifiable> getIdentifiables() {
		return id_identifiable.values();
	}

	public double getNumberOfIndividuals() {
		return numberOfIndividuals;
	}

	public JTable getTable() {
		return table;
	}

	// -------- Inner classes

	/**
	 * Information for the table, built in a worker thread in makeTableData (), must
	 * be used in Swing thread to update the table, see refreshTable().
	 */
	static private class TableData {

		// fc-21.12.2020

		public String[] colNames;
		public Object[][] mat;

		/**
		 * Constructor
		 */
		public TableData(String[] colNames, int numberOfRowes) {
			this.colNames = colNames;
			mat = new Object[numberOfRowes][colNames.length];
		}

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

}
