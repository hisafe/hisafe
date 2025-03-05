/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.woodqualityworkshop;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.TitledBorder;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.gui.GrouperChooser;
import capsis.gui.GrouperChooserListener;
import capsis.kernel.Step;
import capsis.util.SelectionEvent;
import capsis.util.SelectionListener;
import capsis.util.SelectionSource;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Command;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.table.NonEditableTableModel;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;
import repicea.simulation.treelogger.TreeLogger;
import repicea.simulation.treelogger.TreeLoggerParameters;

/**
 * A Tree tab with a table view inside. The TableView contains thess trees in
 * the stand, restricted to the current group if required. When trees are
 * selected in the TableView component, we are told and memo them
 * (selectedTrees) for the next Log job.
 * 
 * @author D. Pont, F. de Coligny - december 2005
 */
public class TreeTab extends JPanel
		implements ActionListener, GrouperChooserListener, SelectionListener, Disposable, Command {

	private static final String[] COLUMN_NAMES = { Translator.swap("TreeTab.id"), Translator.swap("TreeTab.age"),
			Translator.swap("TreeTab.dbh"), Translator.swap("TreeTab.height"), Translator.swap("TreeTab.number"),
			Translator.swap("TreeTab.mark"), Translator.swap("TreeTab.x"), Translator.swap("TreeTab.y"),
			Translator.swap("TreeTab.z"), Translator.swap("TreeTab.species") };

	private WoodQualityWorkshop wqw;
	private Step step;
	private TreeLoggerManager logJobManager; // listened by LogTab
	private TreeList stand;
	private GrouperChooser grouperChooser;
	private Map id2items; // refreshed in makeTableModel
	private TableView tableView;

	private Collection candidateTrees; // according to the current group
	private Collection selectedTrees; // selected by user in the TableView

	private JButton launch;
	// F.Mothe 04.04.2006 Tempo, modified 29.01.2008
	private JCheckBox addCutTrees;
	private JCheckBox addDeadTrees;
	private LoggerChooserPanel treeLoggerChooserPanel;

	private SelectionSource source; // fc - 8.2.2008

	private Map<String, TreeLoggerParameters> treeLoggerParametersMap;

	/**
	 * Constructor. We may launch jobs in logJobManager
	 */
	protected TreeTab(WoodQualityWorkshop wqw, Step step, TreeLoggerManager logJobManager) {
		this.wqw = wqw;
		this.step = step;
		this.logJobManager = logJobManager;
		this.treeLoggerParametersMap = new HashMap<String, TreeLoggerParameters>();

		stand = (TreeList) step.getScene();

		addCutTrees = new JCheckBox(Translator.swap("TreeTab.addCutTrees"));
		addDeadTrees = new JCheckBox(Translator.swap("TreeTab.addDeadTrees"));

		setCandidateTrees();

		createUI();

	}

	/**
	 * From GrouperChooserListener interface. The group was changed: reset the
	 * treesInTable collection, re-create the table and replace it in the table
	 * scrollpane.
	 */
	public void grouperChanged(String newGrouperName) {
		System.out.println("grouper changed");
		if (newGrouperName == null || newGrouperName.equals("")) {
			candidateTrees = stand.getTrees(); // Cancel grouper if one is set
		} else {
			GrouperManager gm = GrouperManager.getInstance();
			Grouper newGrouper = gm.getGrouper(newGrouperName);

			candidateTrees = newGrouper.apply(stand.getTrees(), newGrouperName.toLowerCase().startsWith("not "));
		}
		System.out.println("number of trees:" + candidateTrees.size());

		// Update the table in table view
		tableView.setTableModel(makeTableModel(), id2items);
	}

	/**
	 * Reset the table model according to the candidateTrees collection
	 */
	private NonEditableTableModel makeTableModel() {
		id2items = new HashMap();
		Object[] trees = candidateTrees.toArray();

		String[] colNames = COLUMN_NAMES;
		Object[][] mat = new Object[trees.length][COLUMN_NAMES.length];

		StringBuffer buffer = new StringBuffer();

		String BLANK = "";
		String MARKED = "x";

		for (int i = 0; i < trees.length; i++) {
			/*
			 * if (Thread.interrupted ()) { System.out.println
			 * ("*** thread was interrupted in makeTable"); return null; }
			 */
			Tree t = (Tree) trees[i];

			// this map is up to date with the table model under construction
			id2items.put(t.getId(), t);

			mat[i][0] = new Integer(t.getId());
			mat[i][1] = new Integer(t.getAge());
			mat[i][2] = new Double(t.getDbh());
			mat[i][3] = new Double(t.getHeight());

			try {
				Numberable s = (Numberable) trees[i];
				mat[i][4] = new Double(s.getNumber()); // fc - 22.8.2006 -
														// Numberable returns
														// double
			} catch (Exception e) {
				mat[i][4] = BLANK;
			}

			if (t.isMarked()) {
				mat[i][5] = MARKED;
			} else {
				mat[i][5] = BLANK;
			}

			try {
				Spatialized s = (Spatialized) trees[i];
				mat[i][6] = new Double(s.getX());
				mat[i][7] = new Double(s.getY());
				mat[i][8] = new Double(s.getZ());
			} catch (Exception e) {
				mat[i][6] = BLANK;
				mat[i][7] = BLANK;
				mat[i][8] = BLANK;
			}

			try {
				Speciable s = (Speciable) trees[i];
				buffer.setLength(0);
				buffer.append(s.getSpecies().getName());
				buffer.append(" (");
				buffer.append(s.getSpecies().getValue());
				buffer.append(")");
				mat[i][9] = buffer.toString();
			} catch (Exception e) {
				mat[i][9] = BLANK;
			}
		}

		// we use a non editable table model (browse only)
		// we add a sorter connected to the column headers
		NonEditableTableModel dataModel = new NonEditableTableModel(mat, colNames);

		return dataModel;
	}

	/**
	 * This method implements the different tree logger in a generic way.
	 */
	private void treeLogAction() {

		// fc - 1.3.2006 - tree selection required
		if (selectedTrees == null || selectedTrees.isEmpty()) {
			MessageDialog.print(this, Translator.swap("TreeTab.treeSelectionIsNeeded"));
			return;
		}

		if (selectedTrees.size() > 1000) {
			JButton ok = new JButton(Translator.swap("Shared.ok"));
			JButton cancel = new JButton(Translator.swap("Shared.cancel"));
			Vector<JButton> proposedButtons = new Vector<JButton>();
			proposedButtons.add(cancel);
			proposedButtons.add(ok);

			JButton answer = new JButton();

			answer = UserDialog.promptUser(this, Translator.swap("TreeTab.nbTreesWarningTitle"),
					Translator.swap("TreeTab.nbTreesWarningText"), proposedButtons, ok);
			if (answer == cancel)
				return;
		}

		TreeLogger treeLogger = null;
		TreeLoggerJob job = null;

		try {
			treeLogger = treeLoggerChooserPanel.getTreeLogger();
			treeLogger.setSaveMemoryEnabled(false); // in order to produce
													// GPiece whenever its
													// necessary
			wqw.print("Initialising " + treeLoggerChooserPanel.getSelectedLoggerName() + "...");
			treeLogger.init(selectedTrees);

			String treeLoggerClass = treeLogger.getClass().getName();
			TreeLoggerParameters param;
			if (!treeLoggerParametersMap.containsKey(treeLoggerClass)) {
				param = treeLogger.createDefaultTreeLoggerParameters();
				treeLoggerParametersMap.put(treeLoggerClass, param);
			} else {
				param = treeLoggerParametersMap.get(treeLoggerClass);
			}
			treeLogger.setTreeLoggerParameters(param);
			treeLogger.getTreeLoggerParameters().showUI(wqw);
			if (treeLogger.getTreeLoggerParameters().isParameterDialogCanceled()) {
				wqw.print(Translator.swap("TreeTab.MsgAbort"));
				return;
			}

			// message in Log
			String treeIds = "";
			for (Iterator i = selectedTrees.iterator(); i.hasNext();) {

				// F.Mothe 16.08.2006 (we may have 70000 trees and more !) :
				if (treeIds.length() > 100) {
					treeIds += "...";
					break;
				}

				treeIds += ((Tree) i.next()).getId();
				treeIds += " ";
			}

			treeLogger.setProgressBarEnabled(true, wqw); // enables the progress
															// bar
			job = new TreeLoggerJob(treeLogger);
			// F.Mothe 20.02.2006 : added nb of trees info
			wqw.print("Job " + job.getId() + " starting for " + selectedTrees.size() + " trees: " + treeIds + "...");

			logJobManager.start(job);

			// F.Mothe 20.02.2006 :
			// fix for removing the error message when dialog is aborted
			// } catch (GeoLog.NormalException e) {
			// } catch (Exception e) {
			//
			// // Normal exception (e.g. : dialog aborted) :
			// wqw.print (e.getMessage ());

		} catch (Exception e) {
			String loggerName = treeLogger.getClass().getName();
			
			String jobId = "";
			if (job != null)
				jobId = "Job " + job.getId()+", ";
			Log.println(Log.ERROR, "TreeTab.TreeLogAction ()",
					jobId + "error, could not run the " + loggerName + " tree logger", e);
			
//			try {
//				Log.println(Log.ERROR, "TreeTab.TreeLogAction ()",
//						"Job " + job.getId() + " error, could not run the " + loggerName + " tree logger", e);
//			} catch (Exception f) {
//				Log.println(Log.ERROR, "TreeTab.TreeLogAction ()",
//						"error, could not run " + loggerName + " tree logger", e); // <-
//																					// e!
//																					// (not
//																					// f)
//			}
			
			wqw.print("Error, could not run " + loggerName + " tree logger, Exception: "+e.getMessage());
			MessageDialog.print(this, Translator.swap("TreeTab.errorPleaseSeeLog"), e);
		}
	}

	// F.Mothe 04.04.2006 Tempo
	private void addCutTreesAction() {
		// TODO : cancel (or manage) groups when switching addCutTrees
		this.setCandidateTrees();
		// Update the table in table view
		tableView.setTableModel(makeTableModel(), id2items);
	}

	/**
	 * This private method set the collection of candidate trees from the last
	 * step to the root step.
	 */
	private void setCandidateTrees() {
		// TODO : manage numberable trees like Pp3Tree
		Step lastStep = stand.getStep();
		boolean withCuts = addCutTrees.isSelected();
		boolean withDeads = addDeadTrees.isSelected();

		Collection<Tree> treesFromRoot = new ArrayList<Tree>();
		Vector<Step> steps = lastStep.getProject().getStepsFromRoot(lastStep);
		treesFromRoot.addAll(((TreeList) lastStep.getScene()).getTrees()); // put
																			// alive
																			// collection
																			// into
																			// the
																			// output
																			// collection

		for (Step step : steps) {
			if (withCuts) {
				treesFromRoot.addAll(((TreeList) step.getScene()).getTrees("cut"));
			}
			if (withDeads) {
				treesFromRoot.addAll(((TreeList) step.getScene()).getTrees("dead"));
			}
		}

		candidateTrees = treesFromRoot;

		if (withCuts || withDeads) {
			String info = (withCuts && withDeads) ? "cut and dead" : withCuts ? "cut" : "dead";
			System.out.println("Tree collection now including " + info + " trees. Size = " + candidateTrees.size());
		} else {
			System.out.println("Tree collection now restricted to living trees. Size = " + candidateTrees.size());
		}

	}

	// When a job is terminated, it calls this method
	public int execute() {
		// could update job list...
		StatusDispatcher.print("Job finished");
		return 0;
	}

	/**
	 * From ActionListener interface. Buttons management.
	 */
	public void actionPerformed(ActionEvent evt) {
		// F.Mothe 04.04.2006 Tempo
		if (evt.getSource().equals(addCutTrees) || evt.getSource().equals(addDeadTrees)) {
			addCutTreesAction();
		} else if (evt.getSource().equals(launch)) {
			treeLogAction();
		}
	}

	/**
	 * Disposable
	 */
	public void dispose() {
		System.out.println("TreeTab.dispose ()...");
		try {
			source.removeSelectionListener(this);
		} catch (Exception e) {
		} // does not matter very much
	}

	/**
	 * SelectionListener interface Triggered by TableView when user selects in
	 * the table
	 */
	public void sourceSelectionChanged(SelectionEvent e) { // fc - 8.2.2008
		// ~ params were (Object source, Collection newSelection)
		// ~ selectedTrees = newSelection;
		source = e.getSource();
		selectedTrees = source.getSelection(); // fc - 8.2.2008
	}

	/**
	 * User interface definition
	 */
	private void createUI() {

		// 1. up contains the grouperChooser and table view
		JPanel up = new JPanel(new BorderLayout());
		up.setBorder(new TitledBorder(Translator.swap("TreeTab.treesOfTheStep") + " " + step.getCaption()));

		// 1.1 grouper chooser
		
		// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
		grouperChooser = new GrouperChooser(stand, TreeList.GROUP_ALIVE_TREE, Arrays.asList(TreeList.GROUP_ALIVE_TREE),
				"", false, true, false);
		grouperChooser.addGrouperChooserListener(this);
		up.add(grouperChooser, BorderLayout.NORTH);

		// 1.2 table view
		ColumnPanel aux = new ColumnPanel();
		tableView = new TableView(this, makeTableModel(), id2items);
		aux.add(tableView);
		aux.addStrut0();
		up.add(aux, BorderLayout.CENTER);

		// 2. middle contains the action buttons (Log...)
		ColumnPanel middle = new ColumnPanel();
		middle.setBorder(new TitledBorder(Translator.swap("TreeTab.launchJobsForSelection")));

		// 2.1 Action buttons: Launch logging...
		JPanel l3 = new JPanel(new GridLayout ()); // fc-11.12.2023 better layout

		// Label
		JLabel loggerChooseLabel = new JLabel(Translator.swap("TreeTab.availableLoggers")+" : ");
		l3.add(loggerChooseLabel);

		// Logger chooser
		treeLoggerChooserPanel = new LoggerChooserPanel(step.getProject().getModel());
		l3.add(treeLoggerChooserPanel);


		// Include cut / thinned trees
		// F.Mothe 04.04.2006 Tempo
		LinePanel l4 = new LinePanel (); // fc-11.12.2023 better layout
		
		l4.add(new JLabel(Translator.swap("TreeTab.addTrees")));

		addCutTrees.addActionListener(this);
		l4.add(addCutTrees);
		addDeadTrees.addActionListener(this);
		l4.add(addDeadTrees);

		l3.add(l4);

		// Launch button
		ImageIcon icon = IconLoader.getIcon("ok_16.png");
		launch = new JButton(Translator.swap("TreeTab.launch"), icon);
		launch.addActionListener(this);

		l3.add(launch);
		
		middle.add(l3); // fc-11.12.2023 better layout
		
		// General layout
		setLayout(new BorderLayout());
		add(up, BorderLayout.CENTER);
		add(middle, BorderLayout.SOUTH);
	}

}
