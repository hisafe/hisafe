/* 
* The Genetics library for Capsis4
* 
* Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
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
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
package capsis.extension.ioformat;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.commongui.util.Helper;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.gui.GrouperChooser;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.genetics.Genotypable;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;

/**
 * A dialog to parametrize GenePopExport IOFormat.
 * 
 * @author I. Seynave - October 2002, F. de Coligny - november 2004
 */
public class GenePopExportDialog extends AmapDialog implements ActionListener, ItemListener {

	private Step step;
	private GScene stand;

	private boolean foundAtLeastOneGenotypable;
	private boolean foundNuclearDNA;
	private boolean foundMCytoplasmicDNA;
	private boolean foundPCytoplasmicDNA;

	private String groupName;
	private boolean dendroFilter;
	private int dendroClassWith;
	private String dendroCriterium;
	private boolean geneFilter;
	private String geneCriterium;
	private boolean exportNuclearDNA;
	private boolean exportMCytoplasmicDNA;
	private boolean exportPCytoplasmicDNA;

	public Map<String, String> dendroFilterMap;
	public Map<String, String> geneFilterMap;

	// ~ public Map population;
	// ~ public Map dna;

	private String lastSelectedOption;
	private String selectedGroupName;
	private String lastGroupName; // first selection in combo

	public final static int BUTTON_SIZE = 23;
	private JPanel groupPanel;
	private GrouperChooser grouperChooser;

	private ColumnPanel dendroPanel;
	private Vector dendro = new Vector();
	private JCheckBox checkDendro;
	private JComboBox dendroList;
	private JTextField dendroClassWidth;
	private String lastDendro;

	private ColumnPanel genePanel;
	private Vector gene = new Vector();
	private JCheckBox checkGene;
	private JComboBox geneList;
	// ~ private JTextField geneClassWidth;
	private String lastGene;

	private ColumnPanel dnaPanel;
	private JCheckBox checkN;
	private JCheckBox checkM;
	private JCheckBox checkP;

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	/**
	 * Constructor
	 */
	public GenePopExportDialog(GScene stand) {
		super();
		this.stand = stand;

		// fc - 6.10.2003 - matchWith () complement : more tests now, cancel if
		// trouble
		//
		if (!extensiveMatchWith()) {
			JOptionPane.showMessageDialog(this,
					Translator.swap("GenePopExportDialog.noGeneticTreesWithNotNullIndividualGenotypesWereFoundInStand"),
					Translator.swap("Shared.error"), JOptionPane.WARNING_MESSAGE);
			setValidDialog(false); // cancel
			return;
		}

		dendroFilterMap = new HashMap();
		dendroFilterMap.put(Translator.swap("GenePopExportDialog.age"), "age");
		dendroFilterMap.put(Translator.swap("GenePopExportDialog.dbh"), "dbh");
		dendroFilterMap.put(Translator.swap("GenePopExportDialog.height"), "height");

		geneFilterMap = new HashMap();
		geneFilterMap.put(Translator.swap("GenePopExportDialog.mId"), "mId");
		geneFilterMap.put(Translator.swap("GenePopExportDialog.pId"), "pId");
		geneFilterMap.put(Translator.swap("GenePopExportDialog.creationDate"), "creationDate");

		inspectGenotypables(); // check what genotypables we can find...

		createUI(stand);
		// location is set by AmapDialog
		pack();
		setModal(true);
		show();
	}

	// Checks if stand contains GeneticTrees with not null Individual Genotypes
	// fc - 6.10.2003
	//
	private boolean extensiveMatchWith() {
		// cast to GTCStand is guaranteed by GenePopExport.matchWith ()
		for (Iterator i = ((TreeList) stand).getTrees().iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			if (t instanceof Genotypable) {
				Genotype g = ((Genotypable) t).getGenotype();
				if (g != null && g instanceof IndividualGenotype) {
					return true;
				}
			}
		}
		return false;
	}

	// Checks before leaving on Ok.
	//
	public void okAction() {
		groupName = (grouperChooser.isGrouperAvailable()) ? grouperChooser.getGrouperName() : null;

		dendroFilter = checkDendro.isSelected();
		dendroClassWith = -1;
		dendroCriterium = "-1";
		if (dendroFilter) {
			if (!Check.isInt(dendroClassWidth.getText().trim())) {
				MessageDialog.print(this, Translator.swap("GenePopExportDialog.dendrometricParameterDefined"));
				return;
			}
			dendroClassWith = Check.intValue(dendroClassWidth.getText().trim());
			if (dendroClassWith <= 0) {
				MessageDialog.print(this, Translator.swap("GenePopExportDialog.dendrometricParameterDefined"));
				return;
			}

			dendroCriterium = (String) dendroList.getSelectedItem();
			dendroCriterium = dendroFilterMap.get(dendroCriterium); // translated
																	// text ->
																	// key
			if (dendroCriterium == null) {
				MessageDialog.print(this, Translator.swap("GenePopExportDialog.dendrometricParameterDefined"));
				return;
			}
		}

		geneFilter = checkGene.isSelected();
		geneCriterium = "-1";
		if (geneFilter) {
			geneCriterium = (String) geneList.getSelectedItem();
			geneCriterium = geneFilterMap.get(geneCriterium); // translated text
																// -> key
			if (geneCriterium == null) {
				MessageDialog.print(this, Translator.swap("GenePopExportDialog.geneticParameterDefined"));
				return;
			}
		}

		exportNuclearDNA = checkN.isSelected();
		exportMCytoplasmicDNA = checkM.isSelected();
		exportPCytoplasmicDNA = checkP.isSelected();

		if (!exportNuclearDNA && !exportMCytoplasmicDNA && !exportPCytoplasmicDNA) {
			MessageDialog.print(this, Translator.swap("GenePopExportDialog.oneDNAMustBeSelected"));
			return;
		}

		setValidDialog(true);
	}

	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(checkDendro)) {
			synchroGui();
		} else if (evt.getSource().equals(checkGene)) {
			synchroGui();
		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Called when an item is selected in species.
	 */
	public void itemStateChanged(ItemEvent evt) {
	}

	/**
	 * Called on escape
	 */
	protected void escapePressed() {
		if (Question.ask(MainFrame.getInstance(), Translator.swap("GenePopExportDialog.confirm"),
				Translator.swap("GenePopExportDialog.confirmClose"))) {
			dispose();
		}
	}

	/**
	 * GUI synchronisation
	 */
	private void synchroGui() { // fc - 16.5.2006
		dendroList.setEnabled(checkDendro.isSelected());
		dendroClassWidth.setEnabled(checkDendro.isSelected());

		geneList.setEnabled(checkGene.isSelected());

	}

	/**
	 * Inspect genotypables
	 */
	private void inspectGenotypables() {
		for (Iterator i = ((TreeList) stand).getTrees().iterator(); i.hasNext();) {
			Object o = i.next();
			if (o instanceof Genotypable) {
				foundAtLeastOneGenotypable = true;
				Genotypable gee = (Genotypable) o;

				IndividualGenotype g = (IndividualGenotype) gee.getGenotype();
				if (g.getNuclearDNA().length != 0) {
					foundNuclearDNA = true;
				}
				if (g.getMCytoplasmicDNA().length != 0) {
					foundMCytoplasmicDNA = true;
				}
				if (g.getPCytoplasmicDNA().length != 0) {
					foundPCytoplasmicDNA = true;
				}

				break;
			}
		}
	}

	/**
	 * Initialize the GUI.
	 */
	private void createUI(GScene stand) {
		ColumnPanel mainPanel = new ColumnPanel();
		Border etched = BorderFactory.createEtchedBorder();

		ColumnPanel groupPanel = new ColumnPanel();
		Border b1 = BorderFactory.createTitledBorder(etched, Translator.swap("GenePopExportDialog.group"));
		groupPanel.setBorder(b1);

		// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
		grouperChooser = new GrouperChooser(stand, TreeList.GROUP_ALIVE_TREE, Arrays.asList(TreeList.GROUP_ALIVE_TREE),
				lastGroupName, false, true, false);

		LinePanel l1 = new LinePanel();
		l1.add(grouperChooser);
		groupPanel.add(l1);
		mainPanel.add(groupPanel);
		mainPanel.addGlue();

		ColumnPanel dendroPanel = new ColumnPanel();
		Border b3 = BorderFactory.createTitledBorder(etched, Translator.swap("GenePopExportDialog.dendro"));
		dendroPanel.setBorder(b3);
		LinePanel l3 = new LinePanel();
		dendro.add(Translator.swap("GenePopExportDialog.age"));
		dendro.add(Translator.swap("GenePopExportDialog.dbh"));
		dendro.add(Translator.swap("GenePopExportDialog.height"));
		checkDendro = new JCheckBox(Translator.swap("GenePopExportDialog.dendroCritere"));
		checkDendro.addActionListener(this);
		dendroList = new JComboBox(dendro);
		dendroList.addItemListener(this);
		dendroList.setSelectedItem(lastDendro);
		dendroList.addActionListener(this);
		dendroClassWidth = new JTextField(10);
		l3.add(checkDendro);
		// ~ l3.add (new JWidthLabel (Translator.swap
		// ("GenePopExportDialog.dendroCritere"), 40));
		l3.add(dendroList);
		l3.add(dendroClassWidth);
		dendroPanel.add(l3);
		mainPanel.add(dendroPanel);

		ColumnPanel genePanel = new ColumnPanel();
		Border b4 = BorderFactory.createTitledBorder(etched, Translator.swap("GenePopExportDialog.gene"));
		genePanel.setBorder(b4);
		LinePanel l4 = new LinePanel();
		gene.add(Translator.swap("GenePopExportDialog.mId"));
		gene.add(Translator.swap("GenePopExportDialog.pId"));
		gene.add(Translator.swap("GenePopExportDialog.creationDate"));
		checkGene = new JCheckBox(Translator.swap("GenePopExportDialog.genoCritere"));
		checkGene.addActionListener(this);
		geneList = new JComboBox(gene);
		geneList.addItemListener(this);
		geneList.setSelectedItem(lastGene);
		geneList.addActionListener(this);
		l4.add(checkGene);
		// ~ l4.add (new JWidthLabel (Translator.swap
		// ("GenePopExportDialog.genoCritere"), 40));
		l4.add(geneList);
		genePanel.add(l4);
		mainPanel.add(genePanel);

		ColumnPanel dnaPanel = new ColumnPanel();
		Border b5 = BorderFactory.createTitledBorder(etched, Translator.swap("GenePopExportDialog.dna"));
		dnaPanel.setBorder(b5);
		ColumnPanel l5 = new ColumnPanel();

		checkN = new JCheckBox(Translator.swap("GenePopExportDialog.n"));
		checkN.setEnabled(foundNuclearDNA); // fc - 16.5.2006
		checkN.addActionListener(this);

		checkM = new JCheckBox(Translator.swap("GenePopExportDialog.m"));
		checkM.setEnabled(foundMCytoplasmicDNA); // fc - 16.5.2006
		checkM.addActionListener(this);

		checkP = new JCheckBox(Translator.swap("GenePopExportDialog.p"));
		checkP.setEnabled(foundPCytoplasmicDNA); // fc - 16.5.2006
		checkP.addActionListener(this);

		// ~ l5.add (new JWidthLabel (Translator.swap ("GenePopExportDialog.n"),
		// 40));
		l5.add(checkN);
		// ~ l5.add (new JWidthLabel (Translator.swap ("GenePopExportDialog.m"),
		// 40));
		l5.add(checkM);
		// ~ l5.add (new JWidthLabel (Translator.swap ("GenePopExportDialog.p"),
		// 40));
		l5.add(checkP);
		LinePanel aux = new LinePanel();
		aux.add(l5);
		dnaPanel.add(aux);
		mainPanel.add(dnaPanel);
		mainPanel.addGlue();

		getContentPane().add(mainPanel, BorderLayout.NORTH);

		synchroGui();

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
		setTitle(Translator.swap("GenePopExport")); // fc - 9.11.2004
	}

	// fc - 16.5.2006 - accessors for population & dna maps creation, see
	// GenePopExport
	public String getGroupName() {
		return groupName;
	}

	public boolean isDendroFilter() {
		return dendroFilter;
	}

	public int getDendroClassWith() {
		return dendroClassWith;
	}

	public String getDendroCriterium() {
		return dendroCriterium;
	}

	public boolean isGeneFilter() {
		return geneFilter;
	}

	public String getGeneCriterium() {
		return geneCriterium;
	}

	public boolean isExportNuclearDNA() {
		return exportNuclearDNA;
	}

	public boolean isExportMCytoplasmicDNA() {
		return exportMCytoplasmicDNA;
	}

	public boolean isExportPCytoplasmicDNA() {
		return exportPCytoplasmicDNA;
	}
	// fc - 16.5.2006 - accessors for population & dna maps creation, see
	// GenePopExport

	/**
	 * Accessor on population map
	 */
	// ~ public Map getPopulation () {return population;} // fc - 9.11.2004

	/**
	 * Accessor on dna map
	 */
	// ~ public Map getDna () {return dna;} // fc - 9.11.2004

}

/*
 * // Checks before leaving on Ok. // public void okAction () {
 * 
 * // 1. If user gave no group, // population <- the trees with genotype:
 * Individual and != null // if (!grouperChooser.isGrouperSelected ()) {
 * population = new Hashtable (); Set set = new HashSet (); for (Iterator i =
 * ((GTCStand) stand).getTrees ().iterator (); i.hasNext ();) { GTree t =
 * (GTree) i.next (); if (t instanceof Genotypable) { // fc - 6.10.2003 - maybe
 * some trees are not GeneticTrees Genotypable tree = (Genotypable) t; Genotype
 * g = tree.getGenotype (); if (g != null && g instanceof IndividualGenotype)
 * {set.add (tree);} } } population.put ("allTreeWithGenotype", set);
 * 
 * // 2. If user gave one group, // population <- the trees in the group with
 * genotype: Individual and != null // } else { Map temporaire = new Hashtable
 * ();
 * 
 * GrouperManager gm = GrouperManager.getInstance (); Grouper grouper =
 * gm.getGrouper (grouperChooser.getGrouperName ()); // return null if not found
 * 
 * Collection allTrees = ((TreeCollection) stand).getTrees (); Collection trees
 * = grouper.apply (allTrees); // fc - 9.4.2004
 * 
 * Set allTreesInGroup= new HashSet (); for (Iterator i = trees.iterator ();
 * i.hasNext ();) { GTree t = (GTree) i.next (); if (t instanceof Genotypable) {
 * // fc - 6.10.2003 - maybe some trees are not GeneticTrees Genotypable tr =
 * (Genotypable) t; if (tr.getGenotype () != null && tr.getGenotype ()
 * instanceof IndividualGenotype) { allTreesInGroup.add (tr); } } } String
 * grouperName = "Group"+grouperChooser.getGrouperName (); temporaire.put
 * (grouperName, allTreesInGroup); population = temporaire; }
 * 
 * // From this point, we have only Genotypable GTrees in the population (see
 * upper) - fc
 * 
 * // 3. If all trees in the group have not same species, a message is writen in
 * the log // Set name = population.keySet (); String s = (String) name.iterator
 * ().next ();
 * 
 * Set trees = (Set) population.get (s);
 * 
 * // fc - 9.11.2004 if (trees.isEmpty ()) { MessageDialog.promptError
 * (Translator.swap ("GenePopExportDialog.noTreesSelected")); return; }
 * 
 * // Species of first tree is "none" or something else - fc Genotypable tr =
 * (Genotypable) trees.iterator ().next ();
 * 
 * //~ String speciesName = "none"; // fc - 6.10.2003 - maybe tree is not
 * instanceof SpeciesDefined //~ if (tr instanceof SpeciesDefined) { //~
 * SpeciesDefined sd = (SpeciesDefined) tr; //~ QualitativeProperty esp =
 * sd.getSpecies (); //~ speciesName = esp.getName (); //~ }
 * 
 * String speciesName = tr.getGenoSpecies ().toString ();
 * 
 * for (Iterator i = trees.iterator (); i.hasNext ();) { // Species of current
 * tree is "none" or something else - fc tr = (Genotypable) i.next (); // cast
 * ok: case 1. and 2. upper ensure each tree in set is a Genetic tree - fc
 * 
 * //~ String speciesOfTree = "none"; // fc - 6.10.2003 - maybe tree is not
 * instanceof SpeciesDefined //~ if (tr instanceof SpeciesDefined) { //~
 * SpeciesDefined sd = (SpeciesDefined) tr; //~ QualitativeProperty esp =
 * sd.getSpecies (); //~ speciesOfTree = esp.getName (); //~ }
 * 
 * String speciesOfTree = tr.getGenoSpecies ().toString ();
 * 
 * // if species of current tree is different from species of first tree,
 * trouble - fc if (!speciesName.equals (speciesOfTree)) { Log.println
 * (Log.ERROR, "GenePopExportDialog.okAction ()",
 * "all trees must have same species"); JOptionPane.showMessageDialog (this,
 * Translator.swap ("GenePopExportDialog.allTreesMustHaveSameSpecies"),
 * Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; }
 * }
 * 
 * // 4. If requested, build populations from dendrometric parameter // double
 * maxDendro = 0; double minDendro = 100000; int nbClasse = 0; int
 * minFirstClasse = 0; if (checkDendro.isSelected ()) { try { dendroPas = (new
 * Integer (dendroClassWidth.getText ().trim ())).intValue (); } catch
 * (Exception e) { Log.println (Log.ERROR, "GenePopExportDialog.okAction ()",
 * "dendrometric parameters must be defined"); JOptionPane.showMessageDialog
 * (this, Translator.swap ("GenePopExportDialog.dendrometricParameterDefined"),
 * Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; }
 * dendroCritere = (String) dendroList.getSelectedItem (); if (dendroCritere ==
 * null) { Log.println (Log.ERROR, "GenePopExportDialog.okAction ()",
 * "genetics parameters must be defined"); JOptionPane.showMessageDialog (this,
 * Translator.swap ("GenePopExportDialog.geneticParameterDefined"),
 * Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; }
 * for (Iterator i = trees.iterator (); i.hasNext ();) { GTree tree = (GTree)
 * i.next (); // fc - 6.10.2003 - GeneticTree -> GTree double dendro =
 * getCritereDendro (tree, dendroCritere); maxDendro = Math.max (maxDendro,
 * dendro); minDendro = Math.min (minDendro, dendro); } nbClasse = ((int)
 * ((maxDendro/dendroPas)+1) - (int) (minDendro/dendroPas)); minFirstClasse =
 * (int) ((int) (minDendro/dendroPas)) * dendroPas; //~ } // fc - 6.10.2003
 * (useless) //~ if (checkDendro.isSelected ()) { // fc - 6.10.2003 (useless)
 * Map temporaire = new Hashtable (); for (Iterator i = (population.keySet
 * ()).iterator (); i.hasNext ();) { String nameKey = (String) i.next (); Set
 * value = (Set) population.get (nameKey); for (Iterator j = value.iterator ();
 * j.hasNext ();) { GTree tree = (GTree) j.next (); // fc - 6.10.2003 -
 * GeneticTree -> GTree double dendro = getCritereDendro (tree, dendroCritere);
 * for (int k = 0; k<nbClasse; k++) { if (dendro<(minFirstClasse
 * +(k+1)*dendroPas) && dendro>(minFirstClasse +k*dendroPas)) { if (!
 * temporaire.keySet ().contains
 * (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas))) { String
 * newNameKey = nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas);
 * Set temp1 = new HashSet (); temporaire.put
 * (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas), temp1); } Set
 * temp2 = (Set) temporaire.get
 * (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas)); temp2.add
 * (tree); } } } } population = temporaire; // fc - 6.10.2003 - one line: dendro
 * class - trees in the class }
 * 
 * // 5. If requested, build populations from genotypic parameter // if
 * (checkGene.isSelected ()) { geneCritere = (String) geneList.getSelectedItem
 * (); if (geneCritere == null) { Log.println (Log.ERROR,
 * "GenePopExportDialog.okAction ()", "genetics parameters must be defined");
 * JOptionPane.showMessageDialog (this, Translator.swap
 * ("GenePopExportDialog.geneticParameterDefined"), Translator.swap
 * ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; } Map temporaire =
 * new Hashtable (); for (Iterator i = (population.keySet ()).iterator ();
 * i.hasNext ();) { String nameKey = (String) i.next (); Set value = (Set)
 * population.get (nameKey); for (Iterator j = value.iterator (); j.hasNext ();)
 * { //~ GeneticTree tree = (GeneticTree) j.next (); // cast ok, only
 * GeneticTrees in population (see upper) - fc Genotypable tree = (Genotypable)
 * j.next (); // fc - 15.5.2006 // cast ok, only Genotypable GTrees in
 * population (see upper) - fc //~ double gene = getCritereGene (tree,
 * geneCritere); int gene = getCritereGene (tree, geneCritere); // fc -
 * 7.10.2003 if (! temporaire.keySet ().contains
 * (nameKey+"_"+geneCritere+"="+gene)) { String newNameKey =
 * nameKey+"_"+geneCritere+"="+gene; Set temp1 = new HashSet (); temporaire.put
 * (nameKey+"_"+geneCritere+"="+gene, temp1); } Set temp2 = (Set) temporaire.get
 * (nameKey+"_"+geneCritere+"="+gene); temp2.add (tree); } } population =
 * temporaire; }
 * 
 * // If requested, check if every tree in population has NuclearDNA - fc // dna
 * = new Hashtable (); String test = "0"; if (checkN.isSelected ()) { test =
 * "1"; Set ss = (Set) population.get ((population.keySet ()).iterator ().next
 * ()); Genotypable tt = (Genotypable) ss.iterator ().next ();
 * IndividualGenotype gg = (IndividualGenotype) tt.getGenotype (); if
 * ((gg.getNuclearDNA ()).length == 0) { Log.println (Log.ERROR,
 * "GenePopExportDialog.okAction ()", "no genes on mCytoplasmicDNA");
 * JOptionPane.showMessageDialog (this, Translator.swap
 * ("GenePopExportDialog.nogenesonNuclearDNA"), Translator.swap
 * ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; } } dna.put
 * ("nuclearDNA", test);
 * 
 * // If requested, check if every tree in population has MCytoplasmicDNA - fc
 * // test = "0"; if (checkM.isSelected ()) { test = "1"; Set ss = (Set)
 * population.get ((population.keySet ()).iterator ().next ()); Genotypable tt =
 * (Genotypable) ss.iterator ().next (); IndividualGenotype gg =
 * (IndividualGenotype) tt.getGenotype (); if ((gg.getMCytoplasmicDNA ()).length
 * == 0) { Log.println (Log.ERROR, "GenePopExportDialog.okAction ()",
 * "no genes on mCytoplasmicDNA"); JOptionPane.showMessageDialog (this,
 * Translator.swap ("GenePopExportDialog.nogenesonmCytoplasmicDNA"),
 * Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; }
 * } dna.put ("mCytoplasmicDNA", test);
 * 
 * // If requested, check if every tree in population has PCytoplasmicDNA - fc
 * // test = "0"; if (checkP.isSelected ()) { test = "1"; Set ss = (Set)
 * population.get ((population.keySet ()).iterator ().next ()); Genotypable tt =
 * (Genotypable) ss.iterator ().next (); IndividualGenotype gg =
 * (IndividualGenotype) tt.getGenotype (); if ((gg.getPCytoplasmicDNA ()).length
 * == 0) { Log.println (Log.ERROR, "GenePopExportDialog.okAction ()",
 * "no genes on mCytoplasmicDNA"); JOptionPane.showMessageDialog (this,
 * Translator.swap ("GenePopExportDialog.nogenesonpCytoplasmicDNA"),
 * Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; }
 * } dna.put ("pCytoplasmicDNA", test);
 * 
 * // If nothing requested, trouble - fc // if (!checkN.isSelected () &&
 * !checkM.isSelected () && !checkP.isSelected ()) { Log.println (Log.ERROR,
 * "GenePopExportDialog.okAction ()", "give genotype to export");
 * JOptionPane.showMessageDialog (this, Translator.swap
 * ("GenePopExportDialog.oneDNAMustBeSelected"), Translator.swap
 * ("Shared.warning"), JOptionPane.WARNING_MESSAGE ); return; } //~ settings =
 * new GenePopExportSettings (population, dna); setValidDialog (true); }
 */
