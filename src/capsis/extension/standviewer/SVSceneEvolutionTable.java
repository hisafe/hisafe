package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;

import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.util.ScrollPaneWithRowHeader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import jeeb.lib.util.csvfileviewer.CsvTable;
import jeeb.lib.util.csvfileviewer.CsvTableModel;
import jeeb.lib.util.inspector2.util.CollectionToTableTransformer;

/**
 * A table showing all the scenes under the steps of a scenario and the
 * evolution of their numeric fields. The scenario is from the root step to the
 * reference step on which the viewer is synchronized.
 * 
 * @author F. de Coligny - September 2022
 */
public class SVSceneEvolutionTable extends AbstractStandViewer implements ActionListener {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVSceneEvolutionTable");
	}

	private JButton helpButton;

	private JScrollPane scroll;
	private ScrollPaneWithRowHeader scrollPaneWithRowHeader;

	/**
	 * Constructor
	 */
	public SVSceneEvolutionTable() {
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
			Log.println(Log.ERROR, "SVSceneEvolutionTable.c ()", "Error in constructor", e);
			throw e; // propagate
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {

		GModel model = (GModel) referent;
		GScene rootScene = model.getProject().getRoot().getScene();

		return rootScene instanceof GScene;

	}

	@Override
	public String getName() {
		return Translator.swap("SVSceneEvolutionTable.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVSceneEvolutionTable.description");
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

			// Get the list of scenes from root step
			Project p = step.getProject();
			Vector<Step> stepList = p.getStepsFromRoot(step);
			
			List<GScene> sceneList = new ArrayList<> ();
			for (Step st : stepList) {
				sceneList.add(st.getScene());
			}

			// One single type in list (e.g. Samsa2Stand) -> one single entry in tableMap.		
			CollectionToTableTransformer ct = new CollectionToTableTransformer("Scenes", sceneList);
			ct.execute ();
			
			// One single type in list (e.g. Samsa2Stand) -> one single entry in tableMap.
			updateScrollPaneForThisTableName("Scenes", ct);

		} catch (Exception e) {
			// fc-15.10.2020 Added the message in Log
			Log.println(Log.ERROR, "SVSceneEvolutionTable.update ()", "Could not update the table", e);
			MessageDialog.print(this, Translator.swap("SVSceneEvolutionTable.couldNotDisplayTheTableSeeLog"), e);
			return;
		}
	}

	private void updateScrollPaneForThisTableName(String tableName, CollectionToTableTransformer transformer)
			throws Exception {

		Object[][] tableContent = transformer.getTable();
		String[] columnNames = transformer.getColumnNames();

		Collection list = transformer.getList(); // fc-10.1.2022

		CsvTableModel tableModel = new CsvTableModel(tableName, columnNames, tableContent);

		// 26.11.2021 Add the objects matching the lines in the tableModel, enables the
		// table with extra columns feature (in CsvTable contextual menu)
		tableModel.setLineObjects(new ArrayList<Object>(list));

		// fc-3.1.2022 Replaced StandardHeaderPopup by CsvTablePopup
		// We want to manage the tableHeaderPopup from outside
//		boolean withStandardHeaderPopup = false;

		// fc-26.11.2021
		tableModel.restrictToNumericColumns();

		CsvTable csvTable = new CsvTable(tableName, tableModel, Color.MAGENTA);
		
		try {
			scrollPaneWithRowHeader.updateScrollPane(scroll, csvTable, "date");// fc-19.12.2023 FIXED
//			scrollPaneWithRowHeader.updateScrollPane(scroll, csvTable.getTable(), "date");
		} catch (Exception e) {
			// E.g. when updated on a step with no trees, ignore
			System.out.println("SVSceneEvolutionTable, could not update scrollPane for " + tableName);
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

		scroll = new JScrollPane();
		scrollPaneWithRowHeader = new ScrollPaneWithRowHeader ();
		
		// Memo
		MemoPanel memo = new MemoPanel(Translator.swap("SVSceneEvolutionTable.explanation"));
		memo.setMinimumSize(new Dimension(150, 20)); // approx one line high

		getContentPane().setLayout(new BorderLayout());

		// fc-27.1.2021 added the splitPane
		JSplitPane split = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scroll, memo);
		split.setResizeWeight(1); // memo keeps a fixed size
		split.setOneTouchExpandable(true);
		getContentPane().add(split, BorderLayout.CENTER);

	}

}
