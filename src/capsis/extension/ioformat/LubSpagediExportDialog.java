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
package capsis.extension.ioformat;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import capsis.commongui.util.Helper;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.gui.GrouperChooser;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.genetics.GeneticTree;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;

/**
 * Configuration dialog for LubSpagediExport.
 * 
 * @author F. de Coligny - December 2003
 */
public class LubSpagediExportDialog extends AmapDialog implements ActionListener {
	public final static int BUTTON_SIZE = 23;
	private Step step;
	private TreeList stand;

	private JPanel groupPanel;
	private GrouperChooser grouperChooser;

	private JRadioButton nuclear;
	private JRadioButton cytoP;
	private JRadioButton cytoM;
	private ButtonGroup radioGroup;

	private Collection selectedTrees; // with genotype individual != null

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	/**
	 * Dialog construction.
	 */
	public LubSpagediExportDialog(GScene stand) {
		setTitle(Translator.swap("LubSpagediExport"));
		this.stand = (TreeList) stand;

		// fc - 6.10.2003 - matchWith () complement : more tests now, cancel if
		// trouble
		//
		if (!extensiveMatchWith()) {
			JOptionPane.showMessageDialog(this,
					Translator.swap(
							"LubSpagediExportDialog.noGeneticTreesWithNotNullIndividualGenotypesWereFoundInStand"),
					Translator.swap("Shared.error"), JOptionPane.WARNING_MESSAGE);
			setValidDialog(false); // cancel
			return;
		}

		createUI(stand);
		pack();
		setModal(true);
		show();
	}

	/**
	 * Checks if stand contains GeneticTrees with not null Individual Genotypes
	 * fc - 6.10.2003
	 */
	public boolean extensiveMatchWith() {
		// cast to GTCStand is guaranteed by GenePopExport.matchWith ()
		for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			if (t instanceof GeneticTree) {
				Genotype g = ((GeneticTree) t).getGenotype();
				if (g != null && g instanceof IndividualGenotype) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Return selectedTrees.
	 */
	public Collection getSelectedTrees() {
		return selectedTrees;
	}

	/**
	 * Return true if nuclear button is selected.
	 */
	public boolean isNuclearSelected() {
		return nuclear.isSelected();
	}

	/**
	 * Return true if cytoM button is selected.
	 */
	public boolean isCytoMSelected() {
		return cytoM.isSelected();
	}

	/**
	 * Return true if cytoP button is selected.
	 */
	public boolean isCytoPSelected() {
		return cytoP.isSelected();
	}

	/**
	 * Checks before leaving on Ok.
	 */
	public void okAction() {
		selectedTrees = new ArrayList();

		try {

			// 1. If user gave no group, consider all indiv genotyped trees !=
			// null
			//
			if (!grouperChooser.isGrouperAvailable()) {
				for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {
					Tree t = (Tree) i.next();
					if (t instanceof GeneticTree) {
						GeneticTree gt = (GeneticTree) t;
						if (gt.getGenotype() != null && gt.getGenotype() instanceof IndividualGenotype) {
							selectedTrees.add(gt);
						}
					}
				}

				// 2. If user gave one group, consider all indiv genotyped trees
				// != null in the group
				//
			} else {
				GrouperManager gm = GrouperManager.getInstance();
				Grouper grouper = gm.getGrouper(grouperChooser.getGrouperName());

				Collection allTrees = stand.getTrees();
				Collection trees = grouper.apply(allTrees);

				// ~ Filtrable f = stand;
				// ~ try {
				// ~ f = group.apply (stand); // a group is also a Filter
				// ~ } catch (Exception e) {
				// ~ Log.println (Log.ERROR, "LubSpagediExportDialog.okAction
				// ()",
				// ~ "Exception while applying group "+groupChooser.getGroupName
				// ()
				// ~ +" on filtrable. ",e);
				// ~ MessageDialog.promptWarning (
				// ~ Translator.swap
				// ("LubSpagediExportDialog.troubleWithThisGroup"), e);
				// ~ return;
				// ~ } // if trouble, f is unchanged
				// ~ GTCStand std = (GTCStand) f;

				// ~ for (Iterator i = std.getTrees ().iterator (); i.hasNext
				// ();) {
				for (Iterator i = trees.iterator(); i.hasNext();) { // fc -
																	// 9.4.2004
					Tree t = (Tree) i.next();
					if (t instanceof GeneticTree) { // maybe some trees are not
													// GeneticTrees
						GeneticTree gt = (GeneticTree) t;
						if (gt.getGenotype() != null && gt.getGenotype() instanceof IndividualGenotype) {
							selectedTrees.add(gt);
						}
					}
				}
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "LubSpagediExportDialog.okAction ()", "Exception in method ", e);
			MessageDialog.print(this, Translator.swap("LubSpagediExportDialog.exceptionDuringTreesSelectionSeeLog"));
			return;
		}

		if (selectedTrees == null || selectedTrees.isEmpty()) {
			MessageDialog.print(this, Translator.swap("LubSpagediExportDialog.abortedBecauseNoTreesWereSelected"));
			return;
		}

		// Check request coherence
		//
		GeneticTree firstTree = (GeneticTree) selectedTrees.iterator().next();
		IndividualGenotype g = (IndividualGenotype) firstTree.getGenotype();
		short[][] nuclearDNA = g.getNuclearDNA();
		short[] mCytoplasmicDNA = g.getMCytoplasmicDNA();
		short[] pCytoplasmicDNA = g.getPCytoplasmicDNA();

		if (isNuclearSelected() && (nuclearDNA == null || nuclearDNA.length <= 0)) {
			MessageDialog.print(this, Translator.swap("LubSpagediExportDialog.firstSelectedTreeContainsNoNuclearDNA"));
			return;
		}

		if (isCytoMSelected() && (mCytoplasmicDNA == null || mCytoplasmicDNA.length <= 0)) {
			MessageDialog.print(this,
					Translator.swap("LubSpagediExportDialog.firstSelectedTreeContainsNoMCytoplasmicDNA"));
			return;
		}

		if (isCytoPSelected() && (pCytoplasmicDNA == null || pCytoplasmicDNA.length <= 0)) {
			MessageDialog.print(this,
					Translator.swap("LubSpagediExportDialog.firstSelectedTreeContainsNoPCytoplasmicDNA"));
			return;
		}

		// ~ System.out.println ("Trees "+selectedTrees.size ());
		// ~ for (Iterator i = selectedTrees.iterator (); i.hasNext ();) {
		// ~ GeneticTree t = (GeneticTree) i.next ();
		// ~ System.out.println (""+t);
		// ~ }

		setValidDialog(true);
	}

	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Called on escape : ask for confirmation.
	 */
	protected void escapePressed() {
		if (Question.ask(MainFrame.getInstance(), Translator.swap("LubSpagediExportDialog.confirm"),
				Translator.swap("LubSpagediExportDialog.confirmClose"))) {
			dispose();
		}
	}

	/**
	 * Initializes the GUI.
	 */
	private void createUI(GScene stand) {

		ColumnPanel mainPanel = new ColumnPanel();

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("LubSpagediExportDialog.chooseAGroupWithGenotypedTrees")));
		l1.addGlue();

		LinePanel l2 = new LinePanel();
		l2.add(new JLabel(Translator.swap("LubSpagediExportDialog.leaveUnselectedToconsiderAllTrees")));
		l2.addGlue();

		LinePanel l3 = new LinePanel();
		boolean selected = false;
		String lastGroupName = "";
		
		// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
		grouperChooser = new GrouperChooser(stand, TreeList.GROUP_ALIVE_TREE, Arrays.asList(TreeList.GROUP_ALIVE_TREE),
				lastGroupName, false, true, selected);
		
		l3.add(grouperChooser);
		l3.addStrut0();

		LinePanel l4 = new LinePanel();
		l4.add(new JLabel(Translator.swap("LubSpagediExportDialog.whichADNToExport")));
		l4.addGlue();

		LinePanel l5 = new LinePanel();
		nuclear = new JRadioButton(Translator.swap("LubSpagediExportDialog.nuclear"));
		nuclear.setSelected(true);
		l5.add(nuclear);
		l5.addGlue();

		LinePanel l6 = new LinePanel();
		cytoP = new JRadioButton(Translator.swap("LubSpagediExportDialog.cytoP"));
		l6.add(cytoP);
		l6.addGlue();

		LinePanel l7 = new LinePanel();
		cytoM = new JRadioButton(Translator.swap("LubSpagediExportDialog.cytoM"));
		l7.add(cytoM);
		l7.addGlue();

		radioGroup = new ButtonGroup();
		radioGroup.add(nuclear);
		radioGroup.add(cytoP);
		radioGroup.add(cytoM);

		mainPanel.add(l1);
		mainPanel.add(l2);
		mainPanel.add(l3);
		mainPanel.add(l4);
		mainPanel.add(l5);
		mainPanel.add(l6);
		mainPanel.add(l7);

		mainPanel.addGlue();

		getContentPane().add(mainPanel, BorderLayout.NORTH);

		// 2. Control panel (ok cancel help);
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		help = new JButton(Translator.swap("Shared.help"));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(help);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable(true);
		getRootPane().setDefaultButton(ok);

		getContentPane().add(pControl, BorderLayout.SOUTH);
	}
}
