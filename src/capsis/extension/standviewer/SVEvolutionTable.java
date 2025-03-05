package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.defaulttype.ListOfIdentifiable;
import capsis.extension.AbstractStandViewer;
import capsis.gui.DListSelector;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.util.ScrollPaneWithRowHeader;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
//import jeeb.lib.util.StandardTableHeaderPopup;
import jeeb.lib.util.Translator;
import jeeb.lib.util.csvfileviewer.CsvTable;
import jeeb.lib.util.csvfileviewer.CsvTableModel;
import jeeb.lib.util.inspector2.util.CollectionToTableTransformer;

/**
 * A table showing the variables of an individual with a given id evoluting over
 * time in the simulation. Can be used to plot.
 * 
 * @author F. de Coligny - February 2020
 */
public class SVEvolutionTable extends AbstractStandViewer implements ActionListener {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVEvolutionTable");
	}

	private JTextField id;
	private JButton chooseId;

	private JScrollPane scrollPane;

	private JButton helpButton;

	private int currentId;

//	private StandardTableHeaderPopup popup;

	private ScrollPaneWithRowHeader scrollPaneWithRowHeader;

	/**
	 * Constructor
	 */
	public SVEvolutionTable() {
		scrollPaneWithRowHeader = new ScrollPaneWithRowHeader();
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
			Log.println(Log.ERROR, "SVEvolutionTable.c ()", "Error in constructor", e);
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
			Log.println(Log.ERROR, "SVEvolutionTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVEvolutionTable.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVEvolutionTable.description");
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

			List<Identifiable> list = new ArrayList<>();
			List<Integer> dateColumn = new ArrayList<>();

			Project project = step.getProject();

			// Pass 1: search with currentId
			for (Step stp : project.getStepsFromRoot(step)) {

				ListOfIdentifiable scene = (ListOfIdentifiable) stp.getScene();

				Identifiable idee = scene.getIdentifiable(currentId);
				if (idee != null) { // fc-15.10.2020
					list.add(idee);
					dateColumn.add(stp.getScene().getDate()); // fc-8.12.2020
				}

			}

			// fc-15.10.2020 Pass 2 if list is empty (first opening...), try with defaultId
			if (list.isEmpty()) {

				dateColumn.clear(); // fc-8.12.2020

				int defaultId = searchDefaultId();

				for (Step stp : project.getStepsFromRoot(step)) {

					ListOfIdentifiable scene = (ListOfIdentifiable) stp.getScene();

					Identifiable idee = scene.getIdentifiable(defaultId);
					if (idee != null) { // fc-15.10.2020
						list.add(idee);
						dateColumn.add(stp.getScene().getDate()); // fc-8.12.2020
					}

				}
				if (!list.isEmpty()) {
					// fc-2.4.2021 Was missing in case we use the defaultId
					id.setText("" + defaultId);
					currentId = defaultId; // moved here, if defaulting is confirmed
				}

			}

			// fc-15.10.2020
			if (list.isEmpty()) {
				LinePanel l0 = new LinePanel();
				l0.addGlue();
				JLabel l = new JLabel(Translator.swap("SVEvolutionTable.couldNotFindAnyIndividual"));
				l0.add(l);
				l0.addGlue();
				scrollPane.getViewport().setView(l0);
				return;
			}

			String tableName = "Table name";

//			System.out.println("SVEvolutionTable > update () > list.size (): " + list.size());

			// Turn the list into data compatible with the CsvFileViewer
			CollectionToTableTransformer transformer = new CollectionToTableTransformer(tableName, list);
			transformer.execute();

			Object[][] table = transformer.getTable();
			String[] columnNames = transformer.getColumnNames();

//			System.out.println("SVEvolutionTable > update () > table.length (1): " + table.length);

			// fc-9.12.2020 Add a column 0 with the scene dates
			boolean leading = false; // false means trailing
			table = addAColumn0InTable(table, dateColumn, leading);
			columnNames = addALeadingStringInArray(columnNames, Translator.swap("SVEvolutionTable.date"), leading);

//			System.out.println("SVEvolutionTable > update () > table.length (2): " + table.length);

			CsvTableModel tableModel = new CsvTableModel(tableName, columnNames, table);

			tableModel.restrictToNumericColumns();
			
//			System.out.println("SVEvolutionTable > update () > tableModel.getRowCount(): " + tableModel.getRowCount());

			// fc-3.1.2022 Replaced StandardHeaderPopup by CsvTablePopup
			// We want to manage the popup from outside
//			boolean withStandardHeaderPopup = false;

			CsvTable csvTable = new CsvTable(tableName, tableModel, Color.GREEN);
//			CsvTable csvTable = new CsvTable(tableName, tableModel, Color.GREEN, withStandardHeaderPopup);
//			csvTable.viewNumericColumnsOnly(true);

//			System.out.println("SVEvolutionTable > update () > csvTable.getTable ().getRowCount(): "
//					+ csvTable.getTable().getRowCount());

			// fc-3.1.2022 REMOVED, see CsvTablePopup
//			// Manage the popup from outside
//			if (popup == null) {
//				// 1st time: create it and keep the ref
//				popup = StandardTableHeaderPopup.addPopup(csvTable.getTable());
//			} else {
//				// Next times, reassign popup to new table
//				popup.changeTable(csvTable.getTable());
//			}
			
			// fc-3.1.2022 REMOVED, see CsvTablePopup
			// fc-15.12.2020 Add the plotting features on Right-click
//			PlottingPopup.addPlottingFeaturesToTable(csvTable.getTable(), true);

			// fc-4.1.2021 Row header view... Not yet, see how to 'lightly' remove the date
			// column from the table
//			scrollPane.getViewport().setView(csvTable.getTable()); // fc-9.12.2020

			// fc-27.1.2021 Added scrollPaneWithRowHeader for column date
			scrollPaneWithRowHeader.updateScrollPane(scrollPane, csvTable,
					Translator.swap("SVEvolutionTable.date")); // fc-19.12.2023 FIXED
//			scrollPaneWithRowHeader.updateScrollPane(scrollPane, csvTable.getTable(),
//					Translator.swap("SVEvolutionTable.date"));

		} catch (Exception e) {
			// fc-15.10.2020 Added the message in Log
			Log.println(Log.ERROR, "SVEvolutionTable.update ()", "Could not update the table", e);
			MessageDialog.print(this, Translator.swap("SVEvolutionTable.couldNotDisplayTheTableSeeLog"), e);
			return;
		}
	}

	private int searchDefaultId() {

		Project project = step.getProject();

		// fc-15.10.2020 Pick a default id
		int defaultId = -1;

		for (Step stp : project.getStepsFromRoot(step)) {

			ListOfIdentifiable scene = (ListOfIdentifiable) stp.getScene();

			List<Integer> candidateIds = new ArrayList<>(scene.getIds());
			if (!candidateIds.isEmpty()) {
				defaultId = candidateIds.get(0);
				return defaultId;
			}

		}

		return -1; // not found;
	}

	private void idAction() {

		try {
			currentId = Integer.parseInt(id.getText().trim());
			Settings.setProperty("SVEvolutionTable.current.id", currentId);

			update();

		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("SVEvolutionTable.idMustBeAnInteger"));
			return;
		}

	}

	/**
	 * Make the union of all ids found in all the scenes unders the steps from the
	 * root step of the project.
	 */
	private List<String> searchAllCandidateIds() {

		// fc-9.12.2020

		// We use a set to ignore duplicates
		Set<Integer> ids = new HashSet<>();

		Project project = step.getProject();
		for (Step stp : project.getStepsFromRoot(step)) {

			ListOfIdentifiable scene = (ListOfIdentifiable) stp.getScene();
			ids.addAll(scene.getIds());

		}

		// Sort the integers
		ids = new TreeSet<>(ids);

		// Turn integers into Strings
		List<String> stringIds = new ArrayList<>();
		for (int id : ids) {
			stringIds.add("" + id);
		}

		return stringIds;
	}

	private void chooseIdAction() {

		List<String> candidateIds = searchAllCandidateIds();

		DListSelector dlg = new DListSelector(Translator.swap("SVEvolutionTable.choiceOfTheIndividual"),
				Translator.swap("SVEvolutionTable.selectAnId"), candidateIds, true);

		if (dlg.isValidDialog()) {

			Object selectedItem = dlg.getSelectedItem();

			if (selectedItem == null) {

				// Change nothing

			} else {

				// Update with the selected id
				int selectedId = new Integer((String) selectedItem);
				id.setText("" + selectedId);

				idAction();
			}

		}
		dlg.dispose();

	}

	/**
	 * Used for the settings buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(id)) {
			idAction();
		} else if (evt.getSource().equals(chooseId)) {
			chooseIdAction();
		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Create the user interface
	 */
	public void createUI() {

		ColumnPanel c1 = new ColumnPanel();

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("SVEvolutionTable.currentId") + " : "));
		id = new JTextField();
		id.addActionListener(this);
		id.setText(Settings.getProperty("SVEvolutionTable.current.id", ""));
		l1.add(id);

		chooseId = new JButton(Translator.swap("SVEvolutionTable.chooseId"));
		chooseId.addActionListener(this);
		l1.add(chooseId);

		l1.addGlue();

		c1.add(l1);

		scrollPane = new JScrollPane(new JTextArea());

		// Memo
		MemoPanel memo = new MemoPanel(Translator.swap("SVEvolutionTable.explanation"));
		memo.setMinimumSize(new Dimension(150, 20)); // approx one line high

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(c1, BorderLayout.NORTH);

		// fc-27.1.2021 added the splitPane
		JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scrollPane, memo);
		split.setResizeWeight(1); // memo keeps a fixed size
		split.setOneTouchExpandable(true);
		getContentPane().add(split, BorderLayout.CENTER);

//		getContentPane().add(scrollPane, BorderLayout.CENTER);
//		getContentPane().add(memo, BorderLayout.SOUTH);

	}

	private String[] addALeadingStringInArray(String[] array, String leadingString, boolean leading) {

		// fc-27.1.2021 At the beginning / at the end
		boolean trailing = !leading;

		int size = array.length;

		String[] res = new String[size + 1];

		int targetColIndex = 0;
		if (trailing)
			targetColIndex = size;

		res[targetColIndex] = leadingString;

		for (int i = 0; i < size; i++) {

			int newI = i + 1;
			if (trailing)
				newI = i;

			res[newI] = array[i];
		}

		return res;
	}

	/**
	 * Returns a table similar than the given table, but with one extra leading
	 * column containing the value given in column0. if leadingColumn is true, the
	 * column will be added at the beginning (leading column), else at the end
	 * (trailing column).
	 */
	private Object[][] addAColumn0InTable(Object[][] table, List column0, boolean leadingColumn) {

		// fc-27.1.2020 Added leadingColumn

		if (table.length == 0) {
			System.out.println(
					"SVEvolutionTable addAColumn0InTable () table.length: " + table.length + ", added nothing");
			return table; // do nothing
		}

		if (column0.size() != table.length) {
			System.out.println("SVEvolutionTable addAColumn0InTable () column0.size (): " + column0.size()
					+ " != table.length: " + table.length + ", added nothing");
			return table; // do nothing
		}

		int nLin = table.length;
		int nCol = table[0].length;

		// fc-4.1.2021 Fixed a bug, was adding a blank line at the end
		Object[][] newTable = new Object[nLin][nCol + 1];
//		Object[][] newTable = new Object[nLin + 1][nCol + 1];

		// fc-27.1.2021 leadingColumn / trailingColumn
		boolean trailingColumn = !leadingColumn;

		int targetColIndex = 0;
		if (trailingColumn)
			targetColIndex = nCol;

		for (int i = 0; i < table.length; i++) {

			// Add a value in leading or trailing column on each line
			newTable[i][targetColIndex] = column0.get(i);

			// Copy the rest of the line
			for (int j = 0; j < table[i].length; j++) {

				// fc-27.1.2021 leadingColumn / trailingColumn
				int newJ = j + 1;
				if (trailingColumn)
					newJ = j;

				newTable[i][newJ] = table[i][j];

			}

		}

		return newTable;

	}

}
