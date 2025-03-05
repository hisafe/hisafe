/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2001-2003 Francois de Coligny
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.ListOfIdentifiable;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.AbstractStandViewer;
import capsis.gui.GrouperChooser;
import capsis.gui.GrouperChooserListener;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.JWidthTextField;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.OVSelector;
import jeeb.lib.util.OVSelectorSource;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * SVController is a tool for TreeList detailed analysis.
 * 
 * @author F. de Coligny - November 2003
 */
public class SVController extends AbstractStandViewer implements ActionListener, GrouperChooserListener, /*
																											 * ListSelectionListener,
																											 */
		OVSelectorSource {

	static {
		Translator.addBundle("capsis.extension.standviewer.SVController");
	}

	private static final String BLANK = "";

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	// nb-07.08.2018
	// static public String NAME = "SVController";
	// static public String DESCRIPTION = "SVController.description";
	// static public String AUTHOR = "F. de Coligny";
	// static public String VERSION = "1.2";

	private NumberFormat formater;

//	private static final String BLANK = "";
//	private static final String MARKED = "marked";

	private TreeCollection currentStand;

	private SwingWorker3 task; // only one task at any time

	private GrouperChooser grouperChooser;
	private Grouper currentGrouper;
	private boolean currentGrouperNot;

	private OVSelector ovSelector;
//	protected boolean thisIsAReselection;

	private JTextField nField;
	private JTextField nLinField;

//	private JTable table;

	private JPanel tablePane;

	private JTextArea descriptionField;
	private JScrollPane descriptionPane;
	private JButton helpButton;

//	protected Collection<Integer> memoSelectionIds;
//	protected Collection memoSelection;
//	protected Collection effectiveSelection;
//	protected boolean listenTableSelection;

	private boolean constructionTime;

//	private Map<Integer, Tree> tableRow_tree;

//	private SpecificSorter sorter;
//	private NumberRenderer numberRenderer;

	// fc-16.12.2020 Introduced LegacyTablePanel
	private LegacyTablePanel tablePanel;

	/**
	 * Default constructor
	 */
	public SVController() {
	}

	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);

//		sorter = null;

		formater = NumberFormat.getInstance(Locale.ENGLISH);
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(3);

//		numberRenderer = new NumberRenderer(formater);

		try {
			constructionTime = true;

//			tableRow_tree = new HashMap<Integer, Tree>();

			createOVSelector(step.getScene());

			currentStand = null;
			currentGrouper = null;
			currentGrouperNot = false;

//			memoSelectionIds = new ArrayList<Integer>();
//			memoSelection = new ArrayList();

			tablePanel = new LegacyTablePanel(this);

			createUI();

			constructionTime = false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVController.init ()", "Error in init ()", e);
			throw e; // propagate
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			Step step = (Step) ((GModel) referent).getProject().getRoot();

			// fc-16.12.2020 Extend compatibility to ListOfIdentifiable objects ?

			if (!(step.getScene() instanceof TreeCollection))
				return false;
			
			// fc-29.1.2020 Sill work to do for this (TreeCollection / ListOfIdentifiable)
			if (!(step.getScene() instanceof ListOfIdentifiable))
				return false;
		

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVController.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("SVController.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVController.description");
	}

	@Override
	public String getVersion() {
		return "2.0";
	}

	/**
	 * Update tool on new step button (StandViewer superclass)
	 */
	public void update(StepButton sb) {
		super.update(sb); // Updates this.step

		update ();
		
//		// New scene, same grouper
//		tablePanel.refresh((ListOfIdentifiable) step.getScene(), currentGrouper, currentGrouperNot);
//
////		refresh((TreeCollection) step.getScene(), currentGrouper, currentGrouperNot);
//
//		updateLocalFields();
		
	}

	/**
	 * Update the viewer
	 */
	public void update() {
		super.update();

		// same stand, same grouper
		tablePanel.refresh((ListOfIdentifiable) step.getScene(), currentGrouper, currentGrouperNot);

//		refresh((TreeCollection) step.getScene(), currentGrouper, currentGrouperNot);
		
		updateLocalFields();

	}

	public void updateLocalFields () {
		nField.setText (formater.format(tablePanel.getNumberOfIndividuals()));
		nLinField.setText(formater.format(tablePanel.getTable().getRowCount()));
	}
	
	// For thread access
	public synchronized void setCurrentStand(TreeCollection stand) {
		currentStand = stand;
	}

	public synchronized void setCurrentGrouper(Grouper grouper) {
		currentGrouper = grouper;
	}

	public synchronized void setCurrentGrouperNot(boolean not) {
		currentGrouperNot = not;
	}

//	/**
//	 * Enable / disable the gui during refresh thread work Must be called only from
//	 * Swing thread (=> not synchronized)
//	 */
//	private void enableUI(boolean b) {
//		table.setEnabled(b);
//		table.getTableHeader().revalidate();
//		helpButton.setEnabled(b);
//	}

	/**
	 * Used for the settings buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Grouper changed in grouper chooser.
	 */
	public void grouperChanged(String grouperName) {

		if (grouperName == null || grouperName.equals(BLANK)) {
			
			currentGrouper = null;
			currentGrouperNot = false;
			
		} else {
			
			GrouperManager gm = GrouperManager.getInstance();
			Grouper newGrouper = gm.getGrouper(grouperName);
			
			currentGrouper = newGrouper;
			currentGrouperNot = grouperName.toLowerCase().startsWith("not ");
			
		}
		
		update ();
		
//		if (grouperName == null || grouperName.equals(BLANK)) {
//
//			// Cancel grouper if one is set
//			tablePanel.refresh((ListOfIdentifiable) step.getScene(), null, false);
//
////			refresh((TreeCollection) step.getScene(), null, false);
//
//		} else {
//
//			// Set new grouper
//			tablePanel.refresh((ListOfIdentifiable) step.getScene(), newGrouper,
//					grouperName.toLowerCase().startsWith("not "));
//
////			refresh((TreeCollection) step.getScene(), newGrouper, grouperName.toLowerCase().startsWith("not "));
//
//		}
//		
//		updateLocalFields();

	}

	/**
	 * Called by OVSelector when it is enabled or when a new object viewer is
	 * selected in its combo box.
	 */
	public void reselect() {

//		System.out.println("SVController > reselect ()... ");

		// When creating ovSelector and its combo box, an itemStateChangedEvent
		// is thrown and we arrive here... Ignore
		if (constructionTime)
			return; // Ignored at construction time

		// thisIsAReselection: true is we come from reselect, called by ovSelector
		// itself
		boolean thisIsAReselection = true;
		processTableSelection(thisIsAReselection);

	}

	/**
	 * Send the current selection to the OVSelector. thisIsAReselection: true is we
	 * come from reselect, called by ovSelector itself
	 */
	protected void processTableSelection(boolean thisIsAReselection) {

//		System.out.println("SVController > processTableSelection ()... ");

		// If we arrive here during construction time... Ignore
		if (constructionTime)
			return;

		Collection selectedObjects = tablePanel.getSelectedObjects();

		ovSelector.select(selectedObjects, thisIsAReselection);

		// effectiveSelection = ovSelector.select(memoSelection, thisIsAReselection);

//		thisIsAReselection = false;
		
		// fc-22.12.2020 Update description field
		descriptionField.setText("");
		if (selectedObjects != null && selectedObjects.size () == 1) {
			
			Object o = selectedObjects.iterator().next ();
			descriptionField.setText(o.toString ());
			
		}
		

	}

	/**
	 * Called by OVSelector when the ovDialog is closed by user, remove any
	 * selection marks if any.
	 */
	public void cancelSelection() {

		// fc-12.9.2008
		// we can ignore selection canceling in the table, does not matter here
		// and makes it possible to reselect the same selection by clicking 2
		// times
		// on the enable button in OVSelector

		// ~ select (null, false);

	}

//	/**
//	 * Update the memoSelection collection from the table selection
//	 */
//	private void updateMemoSelection() {
//		try {
//			memoSelectionIds = new ArrayList<Integer>();
//			memoSelection = new ArrayList();
//			int[] selectedRows = table.getSelectedRows();
//
//			TableModel model = table.getModel();
//			for (int i = 0; i < selectedRows.length; i++) {
//				int r = table.convertRowIndexToModel(selectedRows[i]);
//
//				int treeId = ((Integer) model.getValueAt(r, 0)).intValue();
//				memoSelectionIds.add(treeId);
//
//				Tree tree = tableRow_tree.get(r);
//				memoSelection.add(tree);
//				// ~ memoSelection.add (currentStand.getTree (treeId));
//
//			}
//		} catch (Exception e) {
//		} // can happen if called early
//
//	}

	/**
	 * Define what objects are candidate for selection by OVSelector Can be
	 * redefined in subclasses to select other objects
	 */
	protected void createOVSelector(GScene stand) {

		try {

			ExtensionManager extMan = CapsisExtensionManager.getInstance();

			// Send objects to the ovSelector so it can find compatible object viewers.
			// Default: trees if any
			Collection candidateObjects = new ArrayList(((TreeCollection) stand).getTrees());

			JScrollPane targetScrollPane = null; // not used here
			GModel modelForVetoes = null; // not used here

			ovSelector = new OVSelector(extMan, this, candidateObjects, targetScrollPane, true, false, modelForVetoes);

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVController.createOVSelector ()",
					"Exception during OVSelector construction, wrote this error and passed", e);
		}

	}

	/**
	 * Build main user interface. CurrentStand is still null here.
	 */
	private void createUI() {

//		System.out.println("SVController.createUI ()...");

		ColumnPanel header = new ColumnPanel();

		// N + GrouperChooser
		LinePanel l1 = new LinePanel();
		// fc-15.12.2020 changed size from 5 to 7 in JWidthTextField
		nField = new JWidthTextField(7, 20, true, true); // min and max size
		// fixed
		nField.setEditable(false);
		l1.add(new JWidthLabel(Translator.swap("SVController.numberOfTrees") + " :", 50));
		l1.add(nField);

		// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
		grouperChooser = new GrouperChooser(step.getScene(), TreeList.GROUP_ALIVE_TREE,
				Arrays.asList(TreeList.GROUP_ALIVE_TREE), BLANK, false, true, false);

		grouperChooser.addGrouperChooserListener(this);
		l1.add(grouperChooser);
		l1.addStrut0();
		header.add(l1);

		// nLin + ObjectViewers chooser + help button
		LinePanel l2 = new LinePanel();
		// fc-15.12.2020 changed size from 5 to 7 in JWidthTextField
		nLinField = new JWidthTextField(7, 20, true, true); // min and max size
		// fixed
		nLinField.setEditable(false);
		l2.add(new JWidthLabel(Translator.swap("SVController.numberOfLines") + " :", 50));
		l2.add(nLinField);
		l2.add(ovSelector);

		// Help button
		ImageIcon icon = IconLoader.getIcon("help_16.png");
		helpButton = new JButton(icon);
		Tools.setSizeExactly(helpButton);
		helpButton.setToolTipText(Translator.swap("Shared.help"));
		helpButton.addActionListener(this);
		l2.add(helpButton);
		l2.addStrut0();

		header.add(l2);
		header.addStrut0();

//		// Data model of the main table: empty at the beginning
//		DefaultTableModel dataModel = new NonEditableTableModel(new String[1][10], COLUMN_NAMES);
//
//		// Main table creation
//		table = new JTable(dataModel);
//
//		// fc-15.12.2020 Add the plotting features on Right-click
//		PlottingPopup.addPlottingFeaturesToTable(table, false);		
//		// fc-15.12.2020 Add the standard popup menu on the header
//		StandardTableHeaderPopup.addPopup(table);
//
//		// Numbers rendering
//		// table.getColumnModel ().getColumn (0).setCellRenderer
//		// (numberRenderer);
//		table.setDefaultRenderer(Double.class, numberRenderer);
//		table.setDefaultRenderer(Integer.class, numberRenderer);
//
//		// table.setAutoCreateRowSorter(true);
//		sorter = new SpecificSorter(table.getModel());
//		table.setRowSorter(sorter);
//
//		// Default: sort on column 0
//		sorter.toggleSortOrder(0);
//
//		table.setRowSelectionAllowed(true);
//		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
//
//		table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
//		table.getSelectionModel().addListSelectionListener(this);
//		// To allow cut & paste with system clipboard (MS-Excel & others...)
//		ExcelAdapter myAd = new ExcelAdapter(table);

		tablePane = tablePanel;

		// Tree description
		descriptionField = new JTextArea(3, 5); // 4 rows approximatively
		descriptionField.setEditable(false);
		descriptionField.setLineWrap(true);
		descriptionPane = new JScrollPane(descriptionField);

		setLayout(new BorderLayout());
		add(header, BorderLayout.NORTH);
		add(tablePane, BorderLayout.CENTER);
		add(descriptionPane, BorderLayout.SOUTH);
	}

	/**
	 * Dispose the used resources (dialog box).
	 */
	public void dispose() {
		super.dispose();
		ovSelector.dispose();
	}

}
