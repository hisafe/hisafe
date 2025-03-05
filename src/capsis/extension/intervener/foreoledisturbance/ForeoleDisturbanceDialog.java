/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.extension.intervener.foreoledisturbance;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.SwingConstants;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;
import jeeb.lib.util.task.StatusBar;
import capsis.commongui.util.Helper;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.kernel.Step;
import capsis.lib.biomechanics.MecaConstraints;
import capsis.lib.biomechanics.MecaDConstraints;
import capsis.lib.biomechanics.MecaDHelp;
import capsis.lib.biomechanics.MecaDInspector;
import capsis.lib.biomechanics.MecaDSettings;
import capsis.lib.biomechanics.MecaDViewer2D;
import capsis.lib.biomechanics.MecaDamageGraph;
import capsis.lib.biomechanics.MecaDamageHisto;
import capsis.lib.biomechanics.MecaProcess;
import capsis.lib.biomechanics.MecaSettings;
import capsis.lib.biomechanics.MecaTreeInfo;

/**
 * Foreole - Model FOREOLE: model tool to analyse wind damages on trees at stand level.
 *
 * @author Ph. Ancelin - october 2001
 */
public class ForeoleDisturbanceDialog extends AmapDialog implements ActionListener {
	
	private ForeoleDisturbance intervener;
	
	private Collection treeIdsToCut;	// Cut (or mark) these trees (contains Integers)

	private JTabbedPane masterPane;
	private JCheckBox grouped;
	private JComboBox groupName;
	private JButton newGroup;
	private JButton setMecaSettings;
	private JButton createStructure;

	private JButton setConstraints;
	private JButton mecaComputation;
	private JButton export;

	private JButton searchCriticalWS;

	private JButton launchInspector;
	private JButton launchPercentage;
	private JButton launchViewer2D;
	private JButton launchViewer3D;
	private JButton launchDamageGraph;
	private JButton launchDamageHisto;
	private JComboBox graphListX;
	private JComboBox graphListY;
	private JComboBox histoList;
	private String graphTypeX;
	private String graphTypeY;
	private String histoType;

	private JScrollPane scroll;
	private JTextArea display;
	private StatusBar bar;

	private MecaSettings settings;
	private MecaConstraints constraints;
	private MecaProcess mecaProcess;

	private MecaDInspector showInspector;
	private MecaDViewer2D viewer2D;

	private JButton valid;
	private JButton cancel;
	private JButton help;

	private Step step;
	private String percentage;

	static {
		Translator.addBundle("capsis.extension.intervener.foreoledisturbance.ForeoleDisturbance");
	}


	/**
	 * Default constructor.
	 */
	public ForeoleDisturbanceDialog () {}

	/**
	 * Official constructor redefinition : chaining with superclass official constructor.
	 */
	public ForeoleDisturbanceDialog (ForeoleDisturbance intervener, Step s) {
		super ();
		this.intervener = intervener;
		
		try {
			treeIdsToCut = new ArrayList ();

			step = s;
			TreeCollection tc = (TreeCollection) (step.getScene ());
			if (tc.getTrees ().isEmpty ()) {
				JOptionPane.showMessageDialog (this, "Actual TreeCollection is Empty!\nNo compatibility with Foreole Extension!",
						"Error in ForeoleDisturbanceDialog constructor", JOptionPane.ERROR_MESSAGE );
				return;
			} else {
				Tree tree = tc.getTrees ().iterator ().next ();
				if (!(tree instanceof MecaTreeInfo)) {
					JOptionPane.showMessageDialog (this, "FirstTree is not a MecaTreeInfo!\nNo compatibility with Foreole Extension!",
							"Error in ForeoleDisturbanceDialog constructor", JOptionPane.ERROR_MESSAGE );
					return;
				}
			}

			settings = new MecaSettings ();
			constraints = new MecaConstraints ();

			// fc-31.8.2018
			setTitle (intervener.getName()+" - "+step.getCaption ());
//	 		setTitle (Translator.swap ("ForeoleDisturbance")+" - "+step.getCaption ());

			int nbTrees = tc.getTrees ().size ();
			boolean searchCWS;
			if (nbTrees > 9) {
				searchCWS = false;
			} else {
				searchCWS = true;
			}

			createUI (searchCWS);
			createStructureAction ();

			showInspector = null;
			viewer2D =null;
			// Close the main window exits the application - fc - 3.2.2003
			setDefaultCloseOperation (DO_NOTHING_ON_CLOSE);
			addWindowListener (new WindowAdapter () {
				public void windowClosing (WindowEvent evt) {
					escapePressed ();
				}
			});

			
			setModal (true);
			pack ();	// sets the size
			show ();

		} catch (Exception exc) {
			Log.println (Log.ERROR, "ForeoleDisturbanceDialog ():", exc.toString (), exc);
		}
	}

	public Collection getTreeIdsToCut () {return treeIdsToCut;}

	/**
	 * Sets the constraints parameters to apply on trees.
	 */
	private void setMecaSettingsAction () {
		MecaSettings settingsCopy = (MecaSettings) settings.clone ();
		AmapDialog dlg = new MecaDSettings (this, settingsCopy);
		if (dlg.isValidDialog ()) {
			settings = settingsCopy;
		}
		dlg.dispose ();
	}

	/**
	 * Uses biomechanics library to describe current trees with growth units
	 * according to growth data of previous steps from the root step.
	 */
	private void createStructureAction () {
		// Write a message in the display of dialog box to wait for the creation of biomechanical structure
		bar.print(Translator.swap ("ForeoleDisturbance.barWaitCreation"));
		// Create the collection of considered tree ids : those of the reference step
		Collection treeIds = new Vector ();
		TreeCollection tc = (TreeCollection) step.getScene ();
		for (Iterator i = tc.getTrees ().iterator (); i.hasNext ();) {
			Tree tree = (Tree) i.next ();
			Integer id = new Integer (tree.getId ());
			treeIds.add (id);
		}

		mecaProcess = new MecaProcess ();
		boolean processIsOk = mecaProcess.createMecaTrees (treeIds, settings, step, bar);

		if (processIsOk) {
			// Write a message in the display of dialog box to confirm the creation of biomechanical structure
			display.append (Translator.swap ("ForeoleDisturbance.displayCreationOK"));
			bar.print(Translator.swap ("ForeoleDisturbance.barCreationOK"));
			//masterPane.setEnabledAt (1, true);
			//masterPane.setEnabledAt (2, true);
			display.append (Translator.swap ("ForeoleDisturbance.displaySecondInfo"));

			mecaProcess.setConstraints (constraints);
			mecaProcess.setWindSpeeds ();
		} else {
			display.append (Translator.swap ("ForeoleDisturbance.displayCreationERROR"));
			bar.print(Translator.swap ("ForeoleDisturbance.barCreationERROR"));
		}
	}

	/**
	 * Uses biomechanics library to describe current trees with growth units
	 * according to growth data of previous steps from the root step.
	 */
	private void searchCriticalWSAction () {

		double Uhm = 6;
		for (int run=1; run<=600; run++) {
			Collection treeIds = new Vector ();
			TreeCollection tc = (TreeCollection) step.getScene ();
			for (Iterator i = tc.getTrees ().iterator (); i.hasNext ();) {
				Tree tree = (Tree) i.next ();
				Integer id = new Integer (tree.getId ());
				treeIds.add (id);
			}
			mecaProcess = new MecaProcess ();
			boolean processIsOk = mecaProcess.createMecaTrees (treeIds, settings, step, bar);
			if (!processIsOk) {
				bar.print(Translator.swap ("ForeoleDisturbance.barCreationERROR"));
				return;
			}

			constraints.windSpeedEdgeAtH = Uhm;
			constraints.windAt10m = false;
			mecaProcess.setConstraints (constraints);
			mecaProcess.setWindSpeeds ();

			processIsOk = mecaProcess.applyConstraints (constraints, bar);
			if (!processIsOk) {
				bar.print(Translator.swap ("ForeoleDisturbance.barComputationERROR"));
				return;
			}

			processIsOk = mecaProcess.damageAnalysis (bar, true);
			if (!processIsOk) {
				bar.print(Translator.swap ("ForeoleDisturbance.barAnalysisERROR"));
				return;
			}

			Uhm += 0.1;
		}

		new MecaDamageGraph (this, mecaProcess, "Dbh (cm)", "Height (m)");
	}

	/**
	 * Sets the constraints parameters to apply on trees.
	 */
	private void setConstraintsAction () {
		MecaConstraints constraintsCopy = (MecaConstraints) constraints.clone ();
		AmapDialog dlg = new MecaDConstraints (this, constraintsCopy, mecaProcess.getMeanHeight (), mecaProcess.getDominantHeight ());
		if (dlg.isValidDialog ()) {
			constraints = constraintsCopy;
		}
		dlg.dispose ();
		mecaProcess.setConstraints (constraints);
		mecaProcess.setWindSpeeds ();
	}

	/**
	 * Uses biomechanics library to apply specified constraints on trees
	 * and compute their biomechanical behaviour. Then analyses damages on trees.
	 */
	private void computeStructureAction () {
		// Write a message in the display of dialog box to wait for the computation of biomechanical structure
		bar.print(Translator.swap ("ForeoleDisturbance.barWaitComputation"));

		boolean processIsOk = mecaProcess.applyConstraints (constraints, bar);

		if (processIsOk) {
			// Write a message in the display of dialog box to confirm the computation of biomechanical structure
			display.append (Translator.swap ("ForeoleDisturbance.displayComputationOK"));
			bar.print(Translator.swap ("ForeoleDisturbance.barComputationOK"));
		} else {
			display.append (Translator.swap ("ForeoleDisturbance.displayComputationERROR"));
			bar.print(Translator.swap ("ForeoleDisturbance.barComputationERROR"));
			return;
		}

		// damages analysis...
		bar.print(Translator.swap ("ForeoleDisturbance.barWaitAnalysis"));
		processIsOk = mecaProcess.damageAnalysis (bar, false);

		if (processIsOk) {
			display.append (Translator.swap ("ForeoleDisturbance.displayAnalysisOK") + "\n\n");
			bar.print(Translator.swap ("ForeoleDisturbance.barAnalysisOK"));
		} else {
			display.append (Translator.swap ("ForeoleDisturbance.displayAnalysisERROR"));
			bar.print(Translator.swap ("ForeoleDisturbance.barAnalysisERROR"));
		}

		treeIdsToCut = (ArrayList) (mecaProcess.getTreeIdsToCut ());

		createPercentage ();
		printLogPercentage ();
	}

	// Write structure in Log.
	//
	private void exportStructureAction () {
		boolean processIsOk;
		// Export trees in log file.
		bar.print(Translator.swap ("ForeoleDisturbance.barWaitExport"));

		//processIsOk = mecaProcess.exportTrees (bar);
		processIsOk = mecaProcess.exportAttributes (bar);

		if (processIsOk) {
			display.append (Translator.swap ("ForeoleDisturbance.displayExportOK") + "\n\n");
			bar.print(Translator.swap ("ForeoleDisturbance.barExportOK"));
		} else {
			display.append (Translator.swap ("ForeoleDisturbance.displayExportERROR"));
			bar.print(Translator.swap ("ForeoleDisturbance.barExportERROR"));
		}
	}

	// Show an inspector on mecaProcess.
	//
	private void createInspector () {
		showInspector = new MecaDInspector (this, mecaProcess);
	}

	// Create percentages.
	//
	private void createPercentage () {
		percentage = "";
		int nT = mecaProcess.getTreeIds ().size ();
		int nC = mecaProcess.getNbTreesWindThrow ();
		int nV = mecaProcess.getNbTreesStemBreakage ();
		double vT = mecaProcess.getStandVolume ();
		double vC = mecaProcess.getWindThrowVolume ();
		double vV = mecaProcess.getStemBreakageVolume ();

		NumberFormat nf2 = NumberFormat.getInstance ();
		nf2.setMinimumFractionDigits (2);
		nf2.setMaximumFractionDigits (2);
		nf2.setGroupingUsed (false);

		percentage += "\n  Number of trees =\t" + nT + "\n";
		percentage += "  Number of windThrows =\t" + nC + "\n";
		percentage += "  Number of stemBreakages =\t" + nV + "\n";
		percentage += "  Percentage of damages =\t" + nf2.format (100d*(nC+nV)/nT) + "\n";
		percentage += "     with % of windThrows =\t" + nf2.format (100d*nC/nT) + "\n";
		percentage += "     and % of stemBreakages =\t" + nf2.format (100d*nV/nT) + "\n";
		percentage += "\n  ----------------------------------------------\n  (volume in m3)\n";
		percentage += "  Volume of trees =\t" + nf2.format (vT) + "\n";
		percentage += "  Volume of windThrows =\t" + nf2.format (vC) + "\n";
		percentage += "  Volume of stemBreakages =\t" + nf2.format (vV) + "\n";
		percentage += "  Percentage of damages =\t" + nf2.format (100*(vC+vV)/vT) + "\n";
		percentage += "     with % of windThrows =\t" + nf2.format (100*vC/vT) + "\n";
		percentage += "     and % of stemBreakages =\t" + nf2.format (100*vV/vT) + "\n";
	}


	public String getPercentage () {return percentage;}

	// Show percentages.
	//
	private void showPercentage () {
		MecaDHelp helpDialog = new MecaDHelp (this, "Damages Percentages", getPercentage (), 300, 400);
	}

	// Print the percentage of damage in the log file.
	//
	private void printLogPercentage () {
		String expstr = "\n\n<<*****************************************************************************>>\n";
		expstr += "\nExport des Pourcentages de D�g�ts pour " + mecaProcess.getStep ().getCaption ();
		double h, hb;
		if (mecaProcess.getConstraints ().standHeight.equals ("mean")) {
			h = mecaProcess.getMeanHeight ();
			hb = mecaProcess.getMeanCrownBaseHeight ();
		} else {
			h = mecaProcess.getDominantHeight ();
			hb = mecaProcess.getDominantCrownBaseHeight ();
		}

		String windLevel;
		if (mecaProcess.getConstraints ().windAt10m) {
			windLevel = "At 10 m above the ground";
		} else {
			windLevel = "At h (m) above the ground";
		}

		expstr += "\nCalculs faits pour location =\t" + mecaProcess.getConstraints ().location;
		expstr += "\nHauteurs (m) prises en compte : \th =\t" + h + "\thb =\t" + hb;
		expstr += "\nWind Level at stand edge =\t" + windLevel;
		expstr += "\nWindSpeedEdgeAt10m (m/s) =\t" + mecaProcess.getWindSpeedEdgeAt10m ();
		expstr += "\nWindSpeedEdgeAtH (m/s) =\t" + mecaProcess.getWindSpeedEdgeAtH ();
		expstr += "\nWindSpeedStandAtH (m/s) =\t" + mecaProcess.getWindSpeedStandAtH ();
		expstr += "\nWindSpeedStandAtHb (m/s) =\t" + mecaProcess.getWindSpeedStandAtHb ();
		expstr += "\n\n" + getPercentage ();
		expstr += "\n\n<<*****************************************************************************>>\n\n";
		Log.println (expstr);
	}

	// Create a viewer2D.
	//
	private void createViewer2D () {
		viewer2D = new MecaDViewer2D (this, mecaProcess, step);
	}

	// Create a viewer3D.
	//
	private void createViewer3D () {

	}

	// Create a damage graph.
	//
	private void createMecaDamageGraph () {
		new MecaDamageGraph (this, mecaProcess, graphTypeX, graphTypeY);
	}

	// Create a damage histo.
	//
	private void createMecaDamageHisto () {
		new MecaDamageHisto (this, mecaProcess, histoType);
	}

	// Groups management.
	//
	private void setEnabledGroupOptions (boolean b) {
		groupName.setEnabled (b);
		newGroup.setEnabled (b);
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (valid)) {
			if (treeIdsToCut.isEmpty ()) {
				setValidDialog (false);
			} else {
				setValidDialog (true);
			}
			/*if (Question.isTrue (Translator.swap ("ForeoleDisturbance.confirm"),
					Translator.swap ("ForeoleDisturbance.confirmValid"))) {
			}*/
		} else if (evt.getSource ().equals (cancel)) {
				setValidDialog (false);
			/*if (Question.isTrue (Translator.swap ("ForeoleDisturbance.confirm"),
					Translator.swap ("ForeoleDisturbance.confirmCancel"))) {
			}*/
		} else if (evt.getSource ().equals (grouped)) {
			if (grouped.isSelected ()) {
				setEnabledGroupOptions (true);
			} else {
				setEnabledGroupOptions (false);
			}
		} else if (evt.getSource ().equals (setMecaSettings)) {
			setMecaSettingsAction ();
		} else if (evt.getSource ().equals (createStructure)) {
			createStructureAction ();
		} else if (evt.getSource ().equals (searchCriticalWS)) {
			searchCriticalWSAction ();
		} else if (evt.getSource ().equals (setConstraints)) {
			setConstraintsAction ();
		} else if (evt.getSource ().equals (mecaComputation)) {
			computeStructureAction ();
		} else if (evt.getSource ().equals (export)) {
			exportStructureAction ();
		} else if (evt.getSource ().equals (launchInspector)) {
			createInspector ();
		} else if (evt.getSource ().equals (launchPercentage)) {
			showPercentage ();
		} else if (evt.getSource ().equals (launchViewer2D)) {
			createViewer2D ();
		} else if (evt.getSource ().equals (launchViewer3D)) {
			createViewer3D ();
		} else if (evt.getSource ().equals (launchDamageGraph)) {
			if (graphTypeX.equals (graphTypeY)) {
				JOptionPane.showMessageDialog (this, Translator.swap ("ForeoleDisturbance.identicalGraphXY"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			} else {
				createMecaDamageGraph ();
			}
		} else if (evt.getSource ().equals (launchDamageHisto)) {
			createMecaDamageHisto ();
		} else if (evt.getSource ().equals (graphListX)) {
			graphTypeX = (String) graphListX.getSelectedItem ();
		} else if (evt.getSource ().equals (graphListY)) {
			graphTypeY = (String) graphListY.getSelectedItem ();
		} else if (evt.getSource ().equals (histoList)) {
			histoType = (String) histoList.getSelectedItem ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);	// fc - 3.2.2003
		}
	}

	/**
	 * Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	 */
	public void escapePressed () {
		//if (Question.isTrue (Translator.swap ("ForeoleDisturbance.confirm"),
		//		Translator.swap ("ForeoleDisturbance.confirmCancel"))) {
			setValidDialog (false);
		//}
	}

	public Step getStep () {return step;}

	/**
	 * User interface definition.
	 */
	private void createUI (boolean searchCWS) {
		JPanel mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());

		masterPane = new JTabbedPane ();

		// 1. First tab : construction of biomechanical structures
		JPanel part1 = new JPanel ();
		part1.setLayout (new BoxLayout (part1, BoxLayout.Y_AXIS));


		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l2.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.mecaSettings")+" :", 290));
		setMecaSettings = new JButton (Translator.swap ("ForeoleDisturbance.setMecaSettings"));
		setMecaSettings.addActionListener (this);
		l2.add (setMecaSettings);
		part1.add (l2);

		JPanel l3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l3.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.constructStructure")+" :", 290));
		createStructure = new JButton (Translator.swap ("ForeoleDisturbance.createStructure"));
		createStructure.addActionListener (this);
		l3.add (createStructure);
		part1.add (l3);

		if (searchCWS) {
			JPanel l1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			l1.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.searchCriticalWS")+" :", 290));
			searchCriticalWS = new JButton (Translator.swap ("ForeoleDisturbance.search"));
			searchCriticalWS.addActionListener (this);
			l1.add (searchCriticalWS);
			part1.add (l1);
		}

		masterPane.add (Translator.swap ("ForeoleDisturbance.construction"), new NorthPanel (part1));

		// 2. Second tab : computation of biomechanical structures
		JPanel part2 = new JPanel ();
		part2.setLayout (new BoxLayout (part2, BoxLayout.Y_AXIS));

		JPanel l4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l4.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.initializeConstraints")+" :", 320));
		setConstraints = new JButton (Translator.swap ("ForeoleDisturbance.setConstraints"));
		setConstraints.addActionListener (this);
		l4.add (setConstraints);
		part2.add (l4);

		JPanel l5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l5.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.launchmecaComputation")+" :", 320));
		mecaComputation = new JButton (Translator.swap ("ForeoleDisturbance.mecaComputation"));
		mecaComputation.addActionListener (this);
		l5.add (mecaComputation);
		part2.add (l5);

		JPanel l11 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l11.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.exportStructures")+" :", 320));
		export = new JButton ("Export");
		export.addActionListener (this);
		l11.add (export);
		part2.add (l11);

		masterPane.add (Translator.swap ("ForeoleDisturbance.computation"), new NorthPanel (part2));
		//masterPane.setEnabledAt (1, false);

		// 3. Third tab : Viewers ans inspector results commands
		JPanel part3 = new JPanel ();
		part3.setLayout (new BoxLayout (part3, BoxLayout.Y_AXIS));

		JPanel l6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l6.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.inspectorStructureData")+" :", 120));
		launchInspector = new JButton (Translator.swap ("ForeoleDisturbance.launchInspector"));
		launchInspector.addActionListener (this);
		l6.add (launchInspector);
		l6.add (new JWidthLabel ("", 50));
		launchPercentage = new JButton (Translator.swap ("ForeoleDisturbance.launchPercentage"));
		launchPercentage.addActionListener (this);
		l6.add (launchPercentage);
		part3.add (l6);

		JPanel l7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l7.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.viewer2DTreeBaseTop")+" :", 120));
		launchViewer2D = new JButton (Translator.swap ("ForeoleDisturbance.launchViewer2D"));
		launchViewer2D.addActionListener (this);
		l7.add (launchViewer2D);
		part3.add (l7);

		JPanel l8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l8.add (new JWidthLabel (Translator.swap ("ForeoleDisturbance.damageViewers")+" :", 150));
		launchDamageGraph = new JButton (Translator.swap ("ForeoleDisturbance.launchDamageGraph"));
		launchDamageGraph.addActionListener (this);
		l8.add (launchDamageGraph);
		l8.add (new JWidthLabel ("", 70));
		launchDamageHisto = new JButton (Translator.swap ("ForeoleDisturbance.launchDamageHisto"));
		launchDamageHisto.addActionListener (this);
		l8.add (launchDamageHisto);
		part3.add (l8);

		JPanel l9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		JWidthLabel lab = new JWidthLabel (Translator.swap ("ForeoleDisturbance.graphAbscissa")+" :   ", 150);
		lab.setHorizontalAlignment (SwingConstants.RIGHT);
		l9.add (lab);
		String [] listG = { "Dbh (cm)", "Height (m)", "CrownLength (m)",
							"Slenderness", "CrownRatio (%)", "CrDens (kg/m3)", "CArea / SArea",
							"SxxMax (MPa)", "BBM (kN.m)", "P_SB", "P_WT",
							"HsmRatio (%)", "Dbh.CL (cm x m)", "H� (m x m)" };
		graphListX = new JComboBox (listG);
		graphListX.setSelectedIndex(0);
		graphTypeX = (String) graphListX.getSelectedItem ();
		graphListX.addActionListener (this);
		l9.add (graphListX);

		l9.add (new JWidthLabel ("", 65));

		String [] listH = { "N = f(Slenderness)", "N = f(CrownRatio)", "N = f(Dbh)", "N = f(Height)",
							"N = f(CrDensity)", "N = f(CArea/SArea)", "N = f(SL/CR)", "N = f(HsmRatio)" };
		histoList = new JComboBox (listH);
		histoList.setSelectedIndex(0);
		histoType = (String) histoList.getSelectedItem ();
		histoList.addActionListener (this);
		l9.add (histoList);

		JPanel l10 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		lab = new JWidthLabel (Translator.swap ("ForeoleDisturbance.graphOrdinate")+" :   ", 150);
		lab.setHorizontalAlignment (SwingConstants.RIGHT);
		l10.add (lab);
		graphListY = new JComboBox (listG);
		graphListY.setSelectedIndex(1);
		graphTypeY = (String) graphListY.getSelectedItem ();
		graphListY.addActionListener (this);
		l10.add (graphListY);

		part3.add (l9);
		part3.add (l10);

		masterPane.add (Translator.swap ("ForeoleDisturbance.resultsCommands"), new NorthPanel (part3));
		//masterPane.setEnabledAt (2, false);

		masterPane.setSelectedIndex(1);
		mainPanel.add (masterPane, BorderLayout.NORTH);

		// 4. Display to output informations
		//~ JPanel part4 = new JPanel ();	// fc - 31.1.2003
		//~ part4.setLayout (new BoxLayout (part4, BoxLayout.Y_AXIS));
		display = new JTextArea (Translator.swap ("ForeoleDisturbance.displayFirstInfo"));
		scroll = new JScrollPane (display);
		//~ part4.add (scroll);
		mainPanel.add (scroll, BorderLayout.CENTER);

		// 5. Status bar to show tasks evolution
		JPanel part5 = new JPanel ();
		part5.setLayout (new BoxLayout (part5, BoxLayout.Y_AXIS));
		bar = new StatusBar ();
		bar.print(Translator.swap ("Shared.ready"));
		part5.add (bar);
		mainPanel.add (part5, BorderLayout.SOUTH);

		// 5. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		valid = new JButton (Translator.swap ("ForeoleDisturbance.valid"));
		valid.addActionListener (this);
		cancel = new JButton (Translator.swap ("ForeoleDisturbance.cancel"));
		cancel.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (valid);
		pControl.add (cancel);
		pControl.add (help);


		// layout parts
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}

