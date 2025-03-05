package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;

import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.defaulttype.ListOfIdentifiable;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.ScrollPaneWithRowHeader;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import jeeb.lib.util.csvfileviewer.CsvTable;
import jeeb.lib.util.csvfileviewer.CsvTableModel;
import jeeb.lib.util.inspector2.util.CollectionToTableTransformer;

/**
 * A table showing all the individuals of a scene at a given date with all their
 * numeric properties in columns. Can be used to plot.
 * 
 * @author F. de Coligny - December 2020
 */
public class SVStateTable extends AbstractStandViewer implements ActionListener {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVStateTable");
	}

	private JButton helpButton;

//	private StandardTableHeaderPopup tableHeaderPopup;

	// fc-10.1.2022
	private JTabbedPane tabs;
	
	// fc-10.1.2022
	private Map<String, JScrollPane> scrollPane_map;
//	private JScrollPane scrollPane;

	// fc-10.1.2022 same keys than scrollPane_map
	private Map<String, ScrollPaneWithRowHeader> scrollPaneWithRowHeader_map;
//	private ScrollPaneWithRowHeader scrollPaneWithRowHeader;

//	// fc-4.1.2021 Restore the sorter when updating on a new step
//	private RowSorter rowSorter;

	/**
	 * Constructor
	 */
	public SVStateTable() {
		// fc-10.1.2022
		scrollPane_map = new HashMap<>();
		scrollPaneWithRowHeader_map = new HashMap<>();
//		scrollPaneWithRowHeader = new ScrollPaneWithRowHeader();
	}

	/**
	 * Opens and inits the tool on the given stepButton
	 */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {

		super.init(model, s, but);

		try {
			createUI();
			update();

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVStateTable.c ()", "Error in constructor", e);
			throw e; // propagate
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			GModel model = (GModel) referent;
			GScene rootScene = model.getProject().getRoot().getScene();

			return rootScene instanceof ListOfIdentifiable;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVStateTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVStateTable.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVStateTable.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Update the viewer on a given stepButton
	 */
	public void update(StepButton sb) {
		super.update(sb); // computes boolean sameStep
		update(); // for example
	}

	/**
	 * Update the viewer with the current stepButton
	 */
	public void update() {
		super.update();
		if (sameStep)
			return;

		try {

			GScene scene = step.getScene();
			ListOfIdentifiable listOfIdentifiables = (ListOfIdentifiable) scene;

			Collection<Identifiable> list = listOfIdentifiables.getIdentifiables();

			// fc-15.10.2020
			if (list.isEmpty()) {
				LinePanel l0 = new LinePanel();
				l0.addGlue();
				JLabel l = new JLabel(Translator.swap("SVStateTable.noIndividualsFound"));
				l0.add(l);
				l0.addGlue();
				
				// fc-10.1.2022
				for (JScrollPane scroll : scrollPane_map.values ())
					scroll.getViewport().setView(l0);
				
				return;
			}

			// tableNamePrefix is not used here, will be complemented with a className
			String tableNamePrefix = "";

			// fc-10.1.2022 manage the case of several types in the list (Woudyfor: trees
			// and shrubs)
			// General case: one entry only in tableMap (one single type in list)
			Map<String, CollectionToTableTransformer> tableMap = CollectionToTableTransformer
					.makeOneTablePerTypeInList(tableNamePrefix, list);

			// Update the maps for this step
			for (String key : tableMap.keySet()) {
				
				if (!scrollPane_map.containsKey(key)) {
				
					JPanel aux = new JPanel (new BorderLayout ());
					JScrollPane scroll = new JScrollPane ();
					aux.add(scroll, BorderLayout.CENTER);
					tabs.addTab(key, aux);
					
					// The two maps have the same keys
					scrollPane_map.put (key, scroll);
					scrollPaneWithRowHeader_map.put(key, new ScrollPaneWithRowHeader());
					
				}
			}
			
			// Remove unused entries in tabs
			// Start from the end to avoid index shifts while removing
			for (int i = tabs.getTabCount() - 1; i >= 0; i--) {
				String key = tabs.getTitleAt(i);
				if (!tableMap.keySet().contains(key)) {
					tabs.remove(i);
					scrollPane_map.remove(key);
					scrollPaneWithRowHeader_map.remove(key);
				}
			}

			for (String key : tableMap.keySet()) {
				updateScrollPaneForThisTableName(key, tableMap.get(key));
			}

		} catch (Exception e) {
			// fc-15.10.2020 Added the message in Log
			Log.println(Log.ERROR, "SVStateTable.update ()", "Could not update the table", e);
			MessageDialog.print(this, Translator.swap("SVStateTable.couldNotDisplayTheTableSeeLog"), e);
			return;
		}
	}

	private void updateScrollPaneForThisTableName(String tableName, CollectionToTableTransformer transformer)
			throws Exception {

		// fc-10.1.2022 Updated for several scrollpanes / tables / types in the original
		// identifiable list
		// Turn the list into data compatible with the CsvFileViewer
//		CollectionToTableTransformer transformer = new CollectionToTableTransformer(tableName, list);
//		transformer.execute();

		Object[][] tableContent = transformer.getTable();
		String[] columnNames = transformer.getColumnNames();

		Collection list = transformer.getList(); // fc-10.1.2022

//		// fc-9.12.2020 Add a column 0 with the scene dates
//		table = addAColumn0InTable(table, dateColumn);
//		columnNames = addALeadingStringInArray (columnNames, Translator.swap("SVStateTable.date"));

		CsvTableModel tableModel = new CsvTableModel(tableName, columnNames, tableContent);

		// 26.11.2021 Add the objects matching the lines in the tableModel, enables the
		// table with extra columns feature (in CsvTable contextual menu)
		tableModel.setLineObjects(new ArrayList<Object>(list));

		// fc-3.1.2022 Replaced StandardHeaderPopup by CsvTablePopup
		// We want to manage the tableHeaderPopup from outside
//		boolean withStandardHeaderPopup = false;

		// fc-26.11.2021
		tableModel.restrictToNumericColumns();

		CsvTable csvTable = new CsvTable(tableName, tableModel, Color.BLUE);
//		CsvTable csvTable = new CsvTable(tableName, tableModel, Color.BLUE, withStandardHeaderPopup);

		// fc-26.11.2021 Now set before new CsvTable () to avoid 2 table creations
		// fc-26.11.2021 csvTable.viewNumericColumnsOnly(true);

//		// fc-4.1.2021 Restore row Sorter
//		if (rowSorter == null)
//			rowSorter = csvTable.getTable().getRowSorter();
//		else
//			csvTable.getTable().setRowSorter(rowSorter);

		// fc-3.1.2022 REMOVED, see CsvTablePopup
//		// Manage the tableHeaderPopup from outside
//		if (tableHeaderPopup == null) {
//			// 1st time: create it and keep the ref
//			tableHeaderPopup = StandardTableHeaderPopup.addPopup(csvTable.getTable());
//		} else {
//			// Next times, reassign tableHeaderPopup to new table
//			tableHeaderPopup.changeTable(csvTable.getTable());
//		}

		// fc-3.1.2022 REMOVED, see CsvTablePopup
		// fc-15.12.2020 Add the plotting features on Right-click
//		PlottingPopup tablePopup = PlottingPopup.addPlottingFeaturesToTable(csvTable.getTable(), true);

		// fc-3.1.2022 See CsvTablePopup
		// fc-26.11.2021
//		System.out.println("SVStateTable complementing tablePopup with CsvTable available actions...");
//		csvTable.addAvailableActionsIfAny(tablePopup);

		// fc-4.1.2021 Row header view...
//		scrollpane.getViewport().setView(csvTable.getTable()); // fc-9.12.2020

		JScrollPane scroll = scrollPane_map.get(tableName);
		ScrollPaneWithRowHeader spwrh = scrollPaneWithRowHeader_map.get(tableName);

		try {
			spwrh.updateScrollPane(scroll, csvTable, "Id");// fc-19.12.2023 FIXED
//			spwrh.updateScrollPane(scroll, csvTable.getTable(), "Id");
		} catch (Exception e) {
			// E.g. when updated on a step with no trees, ignore
			System.out.println("SVStateTable, could not update scrollPane for "+tableName);
		}

	}

	/**
	 * Used for the settings buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Create the user interface
	 */
	public void createUI() {

		tabs = new JTabbedPane ();
		
//		scrollPane = new JScrollPane(new JTextArea());

		// Memo
		MemoPanel memo = new MemoPanel(Translator.swap("SVStateTable.explanation"));
		memo.setMinimumSize(new Dimension(150, 20)); // approx one line high

		getContentPane().setLayout(new BorderLayout());

		// fc-27.1.2021 added the splitPane
		JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, tabs, memo);
		split.setResizeWeight(1); // memo keeps a fixed size
		split.setOneTouchExpandable(true);
		getContentPane().add(split, BorderLayout.CENTER);

	}

}
