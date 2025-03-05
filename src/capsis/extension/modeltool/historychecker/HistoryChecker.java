/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2011  Francois de Coligny
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

package capsis.extension.modeltool.historychecker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import jeeb.lib.util.Log;
import jeeb.lib.util.Node;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.CellTree;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.extension.DialogModelTool;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**
 * A tool to run technical checks on a Capsis simulation history to detect bad
 * reference problems. Adapted for modules with a scene extending TreeList.
 * 
 * @author F. de Coligny - December 2011
 */
public class HistoryChecker extends DialogModelTool implements ActionListener {

	static {
		Translator.addBundle("capsis.extension.modeltool.historychecker.HistoryChecker");
	}

	// nb-06.08.2018
	//public static final String NAME = "HistoryChecker";
	//static public final String VERSION = "1.0";
	//static public final String AUTHOR = "F. de Coligny";
	//public static final String DESCRIPTION = "HistoryChecker.description";

	private Step step;
	private GModel model;
	private Project project;

	private int errorCount;
	private int stepCount;
	private JTextArea out;
	private JButton close;
	private JButton help;

	/**
	 * Default constructor.
	 */
	public HistoryChecker() {
		super();
	}

	@Override
	public void init(GModel model, Step step) {

		try {
			this.step = step;
			this.model = model;

			setTitle(getName () + " - " + step.getCaption());

			createUI();

			check();

			print("Checked " + stepCount + " steps");
			if (errorCount == 0)
				print("\nCHECK SUCCESSFUL");
			else
				print("\nCHECK FAILED, " + errorCount + " errors found, see higher");

			setSize(new Dimension(600, 400));
			// pack (); // sets the size
			setModal(false);
			setVisible(true);

		} catch (Exception e) {
			Log.println(Log.ERROR, "HistoryChecker.c ()", "Trouble during construction", e);
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene std = ((Step) m.getProject().getRoot()).getScene();
			if (!(std instanceof TreeList)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "HistoryChecker.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("HistoryChecker.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("HistoryChecker.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	private void check() {

		try {

			print("Launching History check on step " + step.getCaption() + "...");

			project = step.getProject();
			if (project == null)
				report("Project = null");

			// Check all the steps in the project
			Iterator<Node> i = project.preorderIterator();
			while (i.hasNext()) {
				Step s = (Step) i.next();
				checkStep(s);
			}

		} catch (Exception e) {
			report("An exception stopped the checking: " + e.toString());
		}

	}

	private void checkStep(Step s) {
		stepCount++;
		print("Checking step " + s + "...");
		if (s == null) {
			report("Step is null");
			return;
		}

		String stepCaption = "Step (" + s + "): ";

		if (s.isRoot())
			print(stepCaption + "Root step");
		if (s.isLeaf())
			print(stepCaption + "Leaf step");

		if (s.getProject() == null)
			report(stepCaption + "Step has a null project reference");
		if (!s.getProject().equals(project))
			report(stepCaption + "Step has a wrong project reference");

		checkTreeList(s);

	}

	private void checkTreeList(Step s) {
		GScene scene = s.getScene();
		if (scene == null) {
			report("Scene is null");
			return;
		}

		String sceneCaption = "Scene (" + scene + "): ";

		if (!(scene instanceof TreeList))
			report(sceneCaption + "Scene is not a TreeList (HistoryChecker is only for TreeLists)");
		TreeList treeList = (TreeList) scene;
		if (treeList.getStep() == null)
			report(sceneCaption + "Step is null");
		if (!treeList.getStep().equals(s))
			report(sceneCaption + "Scene has a wrong step reference");

		for (Tree t : treeList.getTrees()) {
			if (t.getScene() == null)
				report(sceneCaption + "Tree " + t.getId() + ": scene is null");
			if (!t.getScene().equals(scene))
				report(sceneCaption + "Tree " + t.getId() + " has a wrong scene reference");
			if (t instanceof CellTree) {
				TreeListCell c = ((CellTree) t).getCell();
				if (c == null) {
					print("Warning: " + sceneCaption + "Tree " + t.getId() + " has a null cell reference");
					continue;
				}
				if (!c.getTrees().contains(t))
					report(sceneCaption + "Tree " + t.getId() + "\'s cell (cell " + c.getId()
							+ " does not contain this tree");
			}
		}

		checkPlot(scene);

	}

	private void checkPlot(GScene scene) {
		Plot plot = scene.getPlot();
		if (plot == null)
			return; // Plot is optional, not an error

		String plotCaption = "Plot (" + plot + "): ";

		if (plot.getScene() == null)
			report(plotCaption + "Scene is null");
		if (!plot.getScene().equals(scene))
			report(plotCaption + "Plot has a wrong scene reference");

		if (plot instanceof PlotOfCells) {
			
			PlotOfCells plotc = scene.getPlot(); // fc-26.10.2017
			
			Collection<Cell> cells = plotc.getCells();
			for (Cell c : cells) {
				if (c.getPlot() == null)
					report(plotCaption + "Cell " + c.getId() + ": plot is null");
				if (!c.getPlot().equals(plot))
					report(plotCaption + "Cell " + c.getId() + " has a wrong plot reference");

				if (c instanceof TreeListCell)
					checkTreeListCell((TreeListCell) c);

			}
		}

	}

	private void checkTreeListCell(TreeListCell c) {

		String cellCaption = "Cell (" + c + "): ";

		Collection<CellTree> trees = c.getTrees();
		for (CellTree t : trees) {
			if (t == null)
				report(cellCaption + "Cell " + c.getId() + " contains a null tree reference");
			if (!t.getCell().equals(c))
				report(cellCaption + "Tree " + t.getId() + " has a wrong cell reference");

		}

	}

	private void print(String message) {
		out.append(message + "\n");
	}

	private void report(String message) {
		errorCount++;
		print("Error " + errorCount + ": " + message);
	}

	@Override
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(close)) {
			// setVisible (false);
			dispose();

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}

	}

	/**
	 * User interface definition
	 */
	private void createUI() {

		out = new JTextArea();
		out.setLineWrap(false);
		out.setWrapStyleWord(false);
		JScrollPane scroll = new JScrollPane(out);

		// Control panel at the bottom: Close / Help
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		close = new JButton(Translator.swap("Shared.close"));
		close.addActionListener(this);
		help = new JButton(Translator.swap("Shared.help"));
		help.addActionListener(this);
		pControl.add(close);
		pControl.add(help);
		// set close as default (see AmapDialog)
		close.setDefaultCapable(true);
		getRootPane().setDefaultButton(close);

		getContentPane().add(scroll, BorderLayout.CENTER);
		getContentPane().add(pControl, BorderLayout.SOUTH);
	}

}
