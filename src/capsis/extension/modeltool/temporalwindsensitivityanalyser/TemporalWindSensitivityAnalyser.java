/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.extension.modeltool.temporalwindsensitivityanalyser;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.StatusBar;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.DialogModelTool;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.biomechanics.MecaConstraints;
import capsis.lib.biomechanics.MecaDConstraintsSensi;
import capsis.lib.biomechanics.MecaDSettings;
import capsis.lib.biomechanics.MecaProcess;
import capsis.lib.biomechanics.MecaSettings;
import capsis.lib.biomechanics.MecaTreeInfo;

/**
 * TemporalWindSensitivityAnalyser - CWIND Model: model tool to analyse wind damages on trees at stand level.
 *
 * @author Ph. Ancelin - October 2001
 */
public class TemporalWindSensitivityAnalyser extends DialogModelTool implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003

	static {
		Translator.addBundle("capsis.extension.modeltool.temporalwindsensitivityanalyser.TemporalWindSensitivityAnalyser");
	}

	// nb-07.08.2018
	//static public final String AUTHOR="Ph. Ancelin";
	//static public final String VERSION="1.0";

	private JButton setMecaSettings;
	private JButton setConstraints;
	private JButton launchAnalysis;
	private JButton viewResult;

	private StatusBar bar;

	private MecaSettings settings;
	private MecaConstraints constraints;
	private MecaProcess mecaProcess;

	private JButton close;
	private JButton help;

	private Step step;
	private Step currentStep;
	private GModel model;

	//private String percentage;
	private int nbSteps;
	private boolean [] computed;
	private int [] dates;
	private double [][] percenNV;
	private double percenNT;
	private double percenVT;

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public TemporalWindSensitivityAnalyser () {		
		super();
	}
	
	@Override
	public void init(GModel m, Step s){

		try {
			step = s;
			model = step.getProject ().getModel ();

			settings = new MecaSettings ();
			constraints = new MecaConstraints ();
			//mecaProcess = new MecaProcess ();

			setTitle (getName ()+" - "+step.getCaption ());

			createUI ();


			// Close the main window exits the application - fc - 3.2.2003
			setDefaultCloseOperation (DO_NOTHING_ON_CLOSE);
			addWindowListener (new WindowAdapter () {
				public void windowClosing (WindowEvent evt) {
					escapePressed ();
				}
			});

			pack ();	// sets the size
			setVisible (true);
			setModal (false);
			
		} catch (Exception exc) {
			Log.println (Log.ERROR, "TemporalWindSensitivityAnalyser.c ()", exc.toString (), exc);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;

			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}

			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees ().isEmpty ()) {
				Log.println ("\n  TemporalWindSensitivityAnalyser.matchWith () : Initial TreeCollection is Empty! Potential compatibility!\n");
			} else {
				Tree tree = tc.getTrees ().iterator ().next ();
				if (!(tree instanceof MecaTreeInfo)) {return false;}
				// the first tree of collection must be an instance of MecaTreeInfo
				// but it can be a Maid or Madd Tree...
				// if Maid Tree : simulated spatialization
				// if Madd Tree : spatialization using the x, y, z properties of tree.
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "TemporalWindSensitivityAnalyser.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("TemporalWindSensitivityAnalyser.name");
	}

	@Override
	public String getAuthor() {
		return "Ph. Ancelin";
	}

	@Override
	public String getDescription() {
		return Translator.swap("TemporalWindSensitivityAnalyser.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	public int getNbSteps () {return nbSteps;}
	public boolean [] getComputed () {return computed;}
	public int [] getDates () {return dates;}
	public double [][] getPercenNV () {return percenNV;}
	public MecaProcess getMecaProcess () {return mecaProcess;}


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
	 * Sets the constraints parameters to apply on trees.
	 */
	private void setConstraintsAction () {
		MecaConstraints constraintsCopy = (MecaConstraints) constraints.clone ();
		AmapDialog dlg = new MecaDConstraintsSensi (this, constraintsCopy);
		if (dlg.isValidDialog ()) {
			constraints = constraintsCopy;
		}
		dlg.dispose ();
		//mecaProcess.setConstraints (constraints);
		//mecaProcess.setWindSpeeds ();
	}

	/**
	 * Launch the analyser on temporal wind sensitivity
	 */
	private void launchAnalysisAction () {
		Vector steps = step.getProject ().getStepsFromRoot (step);
		int size = steps.size ();
		computed = new boolean [size];
		dates = new int [size];
		percenNV = new double [size][2];
		nbSteps = 0;
		for (Iterator i = steps.iterator (); i.hasNext ();) {
			currentStep = (Step) i.next ();
			if (currentStep.isVisible ()) {
				computed [nbSteps] = false;
				dates [nbSteps] = currentStep.getScene ().getDate ();
				percenNV [nbSteps][0] = 0d;
				percenNV [nbSteps][1] = 0d;

				if (createStructure ()) {
					if (computeStructure ()) {
						computed [nbSteps] = true;
						percenNV [nbSteps][0] = percenNT;
						percenNV [nbSteps][1] = percenVT;
					}
				}
				nbSteps++;
			}
		}
	}

	/**
	 * Show a graph of the temporal wind sensitivity
	 */
	private void viewResultAction () {
		//new MecaDInspector (this, this);
		Window w = Tools.getWindow (this);
		if (w instanceof JDialog) {
			new TemporalWindSensitivityAnalyserResultGraph ((JDialog) w, this);
		} else if (w instanceof JFrame) {
			new TemporalWindSensitivityAnalyserResultGraph ((JFrame) w, this);
		} else {
			new TemporalWindSensitivityAnalyserResultGraph (this);
		}
	}


	/**
	 * Uses biomechanics library to describe current trees with growth units
	 * according to growth data of previous steps from the root step.
	 */
	private boolean createStructure () {
		// Write a message in the display of dialog box to wait for the creation of biomechanical structure
		bar.print("Step " + currentStep.getName () + " : " + Translator.swap ("TemporalWindSensitivityAnalyser.barWaitCreation"));
		// Create the collection of considered tree ids : those of the reference step
		Collection treeIds = new Vector ();
		TreeCollection tc = (TreeCollection) currentStep.getScene ();
		if (tc.getTrees ().isEmpty ()) {
			Log.println ("\n  TemporalWindSensitivityAnalyser () : Actual TreeCollection is Empty! No compatibility!\n");
			JOptionPane.showMessageDialog (this, "Actual TreeCollection is Empty!\nNo compatibility with TemporalWindSensitivityAnalyser Extension!",
					"Error in TemporalWindSensitivityAnalyser constructor", JOptionPane.ERROR_MESSAGE );
			return false;
		} else {
			Tree tree = tc.getTrees ().iterator ().next ();
			if (!(tree instanceof MecaTreeInfo)) {
				Log.println ("\n  TemporalWindSensitivityAnalyser () : FirstTree is not a MecaTreeInfo! No compatibility!\n");
				JOptionPane.showMessageDialog (this, "FirstTree is not a MecaTreeInfo!\nNo compatibility with TemporalWindSensitivityAnalyser Extension!",
						"Error in TemporalWindSensitivityAnalyser constructor", JOptionPane.ERROR_MESSAGE );
				return false;
			}
		}

		for (Iterator i = tc.getTrees ().iterator (); i.hasNext ();) {
			Tree tree = (Tree) i.next ();
			Integer id = new Integer (tree.getId ());
			treeIds.add (id);
		}

		mecaProcess = new MecaProcess ();
		boolean processIsOk = mecaProcess.createMecaTrees (treeIds, settings, currentStep, bar);

		if (processIsOk) {
			// Write a message in the display of dialog box to confirm the creation of biomechanical structure
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barCreationOK"));
			mecaProcess.setConstraints (constraints);
			mecaProcess.setWindSpeeds ();
			return true;
		} else {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barCreationERROR"));
			return false;
		}

	}


	/**
	 * Uses biomechanics library to apply specified constraints on trees
	 * and compute their biomechanical behaviour. Then analyses damages on trees.
	 */
	private boolean computeStructure () {
		// Write a message in the display of dialog box to wait for the computation of biomechanical structure
		bar.print("Step " + currentStep.getName () + " : " + Translator.swap ("TemporalWindSensitivityAnalyser.barWaitComputation"));

		boolean processIsOk = mecaProcess.applyConstraints (constraints, bar);

		if (processIsOk) {
			// Write a message in the display of dialog box to confirm the computation of biomechanical structure
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barComputationOK"));
		} else {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barComputationERROR"));
			return false;
		}

		// damages analysis...
		bar.print("Step " + currentStep.getName () + " : " + Translator.swap ("TemporalWindSensitivityAnalyser.barWaitAnalysis"));
		processIsOk = mecaProcess.damageAnalysis (bar, false);

		if (processIsOk) {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barAnalysisOK"));
		} else {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barAnalysisERROR"));
			return false;
		}

		computePercentages ();
		//printLogPercentage ();

		// export the Mecaprocess...
		bar.print("Step " + currentStep.getName () + " : " + Translator.swap ("TemporalWindSensitivityAnalyser.barWaitExport"));
		processIsOk = mecaProcess.exportAttributes (bar);

		if (processIsOk) {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barExportOK"));
		} else {
			bar.print(Translator.swap ("TemporalWindSensitivityAnalyser.barExportERROR"));
			return false;
		}

		return true;
	}

	// Create percentages.
	//
	private void computePercentages () {
		//percentage = "";
		int nT = mecaProcess.getTreeIds ().size ();
		int nC = mecaProcess.getNbTreesWindThrow ();
		int nV = mecaProcess.getNbTreesStemBreakage ();
		double vT = mecaProcess.getStandVolume ();
		double vC = mecaProcess.getWindThrowVolume ();
		double vV = mecaProcess.getStemBreakageVolume ();

		/*NumberFormat nf2 = NumberFormat.getInstance ();
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
		*/

		percenNT = 100d * (nC + nV) / nT;
		percenVT = 100 * (vC + vV) / vT;
	}


	//public String getPercentage () {return percentage;}


	// Print the percentage of damage in the log file.
	//
/*	private void printLogPercentage () {
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
		Log.print (expstr);
	}
*/

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			if (Question.ask (MainFrame.getInstance (),
					Translator.swap ("TemporalWindSensitivityAnalyser.confirm"), Translator.swap ("TemporalWindSensitivityAnalyser.confirmClose"))) {
				dispose ();
			}
		} else if (evt.getSource ().equals (setMecaSettings)) {
			setMecaSettingsAction ();
		} else if (evt.getSource ().equals (setConstraints)) {
			setConstraintsAction ();
		} else if (evt.getSource ().equals (launchAnalysis)) {
			launchAnalysisAction ();
		} else if (evt.getSource ().equals (viewResult)) {
			viewResultAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);	// fc - 3.2.2003
		}
	}

	/**
	 * Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	 */
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("TemporalWindSensitivityAnalyser.confirm"), Translator.swap ("TemporalWindSensitivityAnalyser.confirmClose"))) {
			dispose ();
		}
	}

	public Step getStep () {return step;}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		JPanel mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());

		JPanel part1 = new JPanel ();
		part1.setLayout (new BoxLayout (part1, BoxLayout.Y_AXIS));


		JPanel l1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l1.add (new JWidthLabel (Translator.swap ("TemporalWindSensitivityAnalyser.mecaSettings")+" :", 290));
		setMecaSettings = new JButton (Translator.swap ("TemporalWindSensitivityAnalyser.setMecaSettings"));
		setMecaSettings.addActionListener (this);
		l1.add (setMecaSettings);
		part1.add (l1);

		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l2.add (new JWidthLabel (Translator.swap ("TemporalWindSensitivityAnalyser.initializeConstraints")+" :", 290));
		setConstraints = new JButton (Translator.swap ("TemporalWindSensitivityAnalyser.setConstraints"));
		setConstraints.addActionListener (this);
		l2.add (setConstraints);
		part1.add (l2);

		JPanel l3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l3.add (new JWidthLabel (Translator.swap ("TemporalWindSensitivityAnalyser.analysis")+" :", 290));
		launchAnalysis = new JButton (Translator.swap ("TemporalWindSensitivityAnalyser.launchAnalysis"));
		launchAnalysis.addActionListener (this);
		l3.add (launchAnalysis);
		part1.add (l3);

		JPanel l4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l4.add (new JWidthLabel (Translator.swap ("TemporalWindSensitivityAnalyser.result")+" :", 290));
		viewResult = new JButton (Translator.swap ("TemporalWindSensitivityAnalyser.viewResult"));
		viewResult.addActionListener (this);
		l4.add (viewResult);
		part1.add (l4);

		mainPanel.add (part1, BorderLayout.CENTER);


		// Status bar to show tasks evolution
		JPanel part5 = new JPanel ();
		part5.setLayout (new BoxLayout (part5, BoxLayout.Y_AXIS));
		bar = new StatusBar ();
		bar.print(Translator.swap ("Shared.ready"));
		part5.add (bar);
		mainPanel.add (part5, BorderLayout.SOUTH);

		// Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (close);
		pControl.add (help);

		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// layout parts
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}

