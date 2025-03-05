package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.StepButton;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.util.methodprovider.TablesInterface;

/**
 * A viewer for the module which methodprovider implements the TablesInterface.
 * 
 * @author F. de Coligny - March 2012 (based in SVOptim by S. de Miguel)
 */
public class SVTables extends AbstractStandViewer implements ActionListener,
		ChangeListener {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVTables");
	}

	// nb-07.08.2018
	//public static final String NAME = "SVTables";
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String DESCRIPTION = "SVTables.description";

	private JCheckBox perHectare;
//	private JButton configureEconomics;

	private JTabbedPane tabs;
	private int selectedIndex;

	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {

		super.init(model, s, but);

		try {
			getContentPane().setLayout(new BorderLayout());

			LinePanel l1 = new LinePanel();
			perHectare = new JCheckBox(Translator.swap("SVTables.perHectare"));
			perHectare.addActionListener(this);
			l1.add(perHectare);

//			configureEconomics = new JButton(
//					Translator.swap("SVTables.configureEconomics"));
//			configureEconomics.addActionListener(this);
//			l1.add(configureEconomics);

			l1.addStrut0();
			getContentPane().add(l1, BorderLayout.NORTH);

			tabs = new JTabbedPane();
			tabs.addChangeListener(this);
			getContentPane().add(tabs, BorderLayout.CENTER);

			update();

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVTables.c ()", "Error in constructor", e);
			throw e;
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			GModel model = (GModel) referent;
			MethodProvider mp = model.getMethodProvider();
			return mp instanceof TablesInterface;
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "SVTables.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVTables.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVTables.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**
	 * Update the viewer on a given step button
	 */
	public void update(StepButton sb) {
		super.update(sb);
		update();
	}

	/**
	 * Specific update.
	 */
	public void update() {

		super.update();

		try {
			TablesInterface mp = (TablesInterface) model
					.getMethodProvider();

			Map<String, String> map = mp.getTables(step,
					perHectare.isSelected());

			int index = selectedIndex;

			tabs.removeAll();
			for (String tableName : map.keySet()) {
				String table = map.get(tableName);

				JTextArea a = new JTextArea(table);
				a.setTabSize(13); // Size of tabs
				tabs.addTab(tableName, new JScrollPane(a));
			}

			try {
				tabs.setSelectedIndex(index); // try to restore the selected tab
			} catch (Exception e) {
				// No problem
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVTables.update ()", "Error", e);
			error(Translator.swap("SVTables.anErrorOccurredSeeLogForDetails"));
		}

	}

	/**
	 * Prints an error with the given message in the viewer.
	 */
	private void error(String message) {

		getContentPane().add(new JLabel("message"), BorderLayout.SOUTH);

	}

//	/**
//	 * Get the economics evaluator of the nested model and show its
//	 * configuration panel.
//	 */
//	private void configureEconomicsAction() {
//		OptimModel o_model = (OptimModel) model;
//		OptimNestedModel n_model = o_model.getNestedModel();
//
//		try {
//			OptimEconomicEvaluator evaluator = n_model.getEconomicEvaluator();
//			OptimConfigPanel.openInDialog(this,
//					Translator.swap("SVTables.configureEconomics"),
//					evaluator.getConfigPanel());
//
//		} catch (Exception e) {
//			MessageDialog.print(this,
//					"Could not create the configPanel for the Economic Evaluator for "
//							+ n_model, e);
//		}
//
//	}

	@Override
	public void actionPerformed(ActionEvent e) {
//		if (e.getSource().equals(configureEconomics)) {
//			configureEconomicsAction();
//		}
		update();

	}

	@Override
	public void stateChanged(ChangeEvent e) {
		selectedIndex = tabs.getSelectedIndex();

	}

}
