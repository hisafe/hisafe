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
package capsis.extension.modeltool;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.border.Border;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.extension.DialogModelTool;
import capsis.gui.DStringSelector;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.genetics.AlleleDiversity;
import capsis.lib.genetics.AlleleEffect;
import capsis.lib.genetics.DStringInput;
import capsis.lib.genetics.GeneticMap;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.GeneticTools;
import capsis.lib.genetics.GenoSpecies;
import capsis.lib.genetics.Genotypable;
import capsis.lib.genetics.IndividualGenotype;
import capsis.lib.genetics.MultiGenotype;
import capsis.util.QualitativeProperty;

/**	Generate more genetic data at the beginning of the simulation, 
*	just after file loading.
*
*	@author I. Seynave - October 2002,  C. Pichot - March/September 2003, F. de Coligny - November 2004
*/
public class GeneticsGeneration extends DialogModelTool implements ActionListener, ItemListener {

	static {
		Translator.addBundle("capsis.extension.modeltool.GeneticsGeneration");
	}
	
	// nb-07.08.2018
	//static public final String AUTHOR="I. Seynave, C. Pichot";
	//static public final String VERSION="1.2";
    
	public final static int MONO_SPECIFIC = Integer.MAX_VALUE;
	private Step step;
	
	private String lastSelectedOption;

	private JComboBox species;	// == null if only one species
	private int currentSpecies;

//cp march & sept. 03
	private JTextField newNuclearLoci;
	private JTextField pRecombinationLoci;
	private JTextField newMcytoLoci;
	private JTextField newPcytoLoci;
	private JButton addLoci;
	int nbGees = 0;
//cp march & sept. 03

	private JTextField newParameter;
	private JTextField lociList;
	private JButton addEffect;
	private JTextField heritability;
	private JTextField totalEnvironmentalVariance;
	private JTextField interEnvironmentalVariance;

	private Map speciesValues;	// code (Integer) - cl� de traduction (String)
	private Map label_code;	// String - Integer

	private Map newLociActions;
	private Map newPRecombinationActions;
	private Map newAlleleFrequenciesActions;
	private Map newEffectActions;

	private JScrollPane display;
	private JList displayList;
	private JButton remove;

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	private static Random random = new Random ();

	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public GeneticsGeneration () {		
		super();
	}
	
	@Override
	public void init(GModel m, Step s){

		try {
			step = s;
			label_code = new Hashtable ();

			setTitle (getName()+" - "+step.getCaption ());

			createUI ();
			pack ();	// sets the size
			setVisible (true);
			setModal (false);
			setResizable (true);
		} catch (Exception exc) {
			Log.println (Log.ERROR, "GeneticsGeneration.c ()", exc.toString (), exc);
		}
	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			Step stp = (Step) m.getProject ().getRoot ();
			if (!(stp.isLeaf ())) {return false;}	// only available on a root being also a leaf : just at the beginning

			GScene std = stp.getScene ();
			if (!(std instanceof GeneticScene)) {return false;}	// fc - 9.11.2004 - GeneticScene is enough
			
			GeneticScene scene = (GeneticScene) std;
			if (scene.getGenotypables ().isEmpty ()) {return false;}	// fc - 9.11.2004 - wee need only one genotypable

		} catch (Exception e) {
			Log.println (Log.ERROR, "GeneticsGeneration.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("GeneticsGeneration.name");
	}

	@Override
	public String getAuthor() {
		return "I. Seynave, C. Pichot";
	}

	@Override
	public String getDescription() {
		return Translator.swap("GeneticsGeneration.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**	Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	*/
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("GeneticsGeneration.confirm"), Translator.swap ("GeneticsGeneration.confirmClose"))) {
			dispose ();
		}
	}

	// Create a Map with all possible species, return the number of species
	//
	private int searchSpecies (Genotypable gee) {
		//~ if (!(gee instanceof SpeciesDefined)) {return 0;}	// no species	// fc - nov 2004 - Genotypable now defines getGenoSpecies ()

		int n = 0;
		Collection acc = Tools.getPublicAccessors (gee.getClass ());
		for (Iterator j = acc.iterator (); j.hasNext ();) {
			Method m = (Method) j.next ();

			// Card creation: one per qualitative value
			if (Tools.returnsType (m, QualitativeProperty.class)) {
				// One property accessor found...
				QualitativeProperty p = null;
				try {
					p = (QualitativeProperty) m.invoke (gee);	// fc - 2.12.2004 - varargs
				} catch (Exception e) {
					Log.println (Log.WARNING, "GeneticsGeneration ()",
							"Exception during dynamic method invocation (get...())"
							+" to retrieve a Qualitative Property on object "+gee, e);
				}

				if (! (p instanceof capsis.defaulttype.Species)) {continue;}

				// Exploring possible values and creating checkboxes
				speciesValues = p.getValues ();

				Iterator keys = speciesValues.keySet ().iterator ();
				Iterator values = speciesValues.values ().iterator ();
				while (keys.hasNext () && values.hasNext ()) {
					Integer key = (Integer) keys.next ();
					String value = (String) values.next ();

					label_code.put (Translator.swap (value), key);
					n++;
				}
			}
		}
		return n;
	}

	// 
	//
	private void addLociAction () {
		String nuclearCandidate = newNuclearLoci.getText ().trim ();
		int nbNewNuclearLoci = 0 ;
		String mCytoCandidate = newMcytoLoci.getText ().trim ();
		String pCytoCandidate = newPcytoLoci.getText ().trim ();
		String candidate = "";

		for (StringTokenizer st = new StringTokenizer (nuclearCandidate); st.hasMoreTokens ();) {
			String token = st.nextToken ();
			try {
				new Integer (token);
			} catch (Exception e) {
				MessageDialog.print (this, Translator.swap ("GeneticsGeneration.invalidFormat"));
				return;
			}
			candidate = candidate + "n" + token + " " ;
			nbNewNuclearLoci = nbNewNuclearLoci +1 ;
		}
		for (StringTokenizer st = new StringTokenizer (mCytoCandidate); st.hasMoreTokens ();) {
			String token = st.nextToken ();
			try {
				new Integer (token);
			} catch (Exception e) {
				MessageDialog.print (this, Translator.swap ("GeneticsGeneration.invalidFormat"));
				return;
			}
			candidate = candidate + "m" + token + " " ;
		}
		for (StringTokenizer st = new StringTokenizer (pCytoCandidate); st.hasMoreTokens ();) {
			String token = st.nextToken ();
			try {
				new Integer (token);
			} catch (Exception e) {
				MessageDialog.print (this, Translator.swap ("GeneticsGeneration.invalidFormat"));
				return;
			}
			candidate = candidate + "p" + token + " " ;
		}
		
// validate that pRecombination fits with newNuclearLoci
		int nbPRecombination = 0 ;
		for (StringTokenizer st = new StringTokenizer (pRecombinationLoci.getText().trim()); st.hasMoreTokens ();) {
			st.nextToken () ;
			nbPRecombination = nbPRecombination + 1;
		}

	// get geneticMap
		GeneticScene scene = (GeneticScene) step.getScene ();
		GeneticMap geneticMap = null;
		
		if (currentSpecies == MONO_SPECIFIC) {
			
			Object _t = null;
			Iterator _i = scene.getGenotypables ().iterator ();
			do {_t = _i.next ();} while (!(_t instanceof Genotypable));
			Genotypable t = (Genotypable) _t;		// phd 2003_03_17
			
			geneticMap = t.getGenoSpecies ().getGeneticMap ();
		} else {
			int first=0;
			for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
				
				Object _t = j.next ();
				if (!(_t instanceof Genotypable)) {continue;}
				Genotypable t = (Genotypable) _t;		// phd 2003_03_17
				
				//~ SpeciesDefined sd = (SpeciesDefined) t;
				//~ int esp = sd.getSpecies ().getValue ();
				try {	// fc - nov 2004 - here, GenoSpecies must also be a QualitativeProperty
					int esp = ((QualitativeProperty) t.getGenoSpecies ()).getValue ();
					
					if (esp == currentSpecies) {
						if (first == 0) {
							if (t.getGenoSpecies ().getAlleleDiversity () != null) {
								geneticMap = t.getGenoSpecies ().getGeneticMap ();
								first = 1;
							}
						}
					}
				} catch (Exception e) {
					Log.println (Log.ERROR, "GeneticsGeneration.addLociAction ()", 
							"GeneticsGeneration: GenoSpecies must also be a QualitativeProperty", e);
				}
			}
		}

		if (geneticMap == null) {
			nbPRecombination = nbPRecombination + 1 ;
		}
		if (nbPRecombination < nbNewNuclearLoci) {
			MessageDialog.print (this, Translator.swap("GeneticsGeneration.moreProba")) ;
		} else if (nbPRecombination > nbNewNuclearLoci) {
			MessageDialog.print (this, Translator.swap("GeneticsGeneration.lessProba"));
		} else {
			Collection liste = new Vector();
			liste.add ("Equiprobable") ;	// this should be translated in labels files - fc - 9.11.2004
			liste.add ("Personnalisee");	// this should be translated in labels files - fc - 9.11.2004
			liste.add ("Lshape");	// this should be translated in labels files - fc - 9.11.2004
			DStringSelector choice = new DStringSelector ("Fr�quence des all�les", "Choisir une modalit�", liste);	// this should be translated in labels files - fc - 9.11.2004
							
			if (choice.isValidDialog ()) {
				String selectedChoice = choice.getSelectedString ();

// generate allele frequencies
				double[][] alleleFrequencies = new  double[candidate.length ()][];				
				int nbAlleles = 0;
				int seq = 0;
// total number of scene in this species
				nbGees=0;
				if (currentSpecies == MONO_SPECIFIC) {
					for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
						
						Object _t = j.next ();
						if (!(_t instanceof Genotypable)) {continue;}
						Genotypable t = (Genotypable) _t;	
						
						nbGees = nbGees + (int) t.getNumber() ;	// fc - 22.8.2006 - Numberable returns double
					}
				} else {
					for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
						
						Object _t = j.next ();
						if (!(_t instanceof Genotypable)) {continue;}
						Genotypable t = (Genotypable) _t;		// phd 2003_03_17
						
						//~ SpeciesDefined sd = (SpeciesDefined) t;
						int esp = t.getGenoSpecies ().getValue ();
						
						if (esp == currentSpecies) {
							nbGees = nbGees + (int) t.getNumber() ;		// fc - 22.8.2006 - Numberable returns double
						}
					}
				}
//	System.out.println("nb d'arbres "+nbGees);
//	END total number of scene in this species
				
				if (selectedChoice == "Equiprobable") {	// this should be translated in labels files - fc - 9.11.2004
					for (StringTokenizer st = new StringTokenizer (nuclearCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						for (int j=0; j<alleleFrequencies[seq].length; j++) {
							alleleFrequencies[seq][j] = (double) 1 / alleleFrequencies[seq].length  ;
						}
						seq = seq + 1;
					}
					for (StringTokenizer st = new StringTokenizer (mCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						for (int j=0; j<alleleFrequencies[seq].length; j++) {
							alleleFrequencies[seq][j] = (double) 1 / alleleFrequencies[seq].length  ;
						}
						seq = seq + 1;
					}
					for (StringTokenizer st = new StringTokenizer (pCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						for (int j=0; j<alleleFrequencies[seq].length; j++) {
							alleleFrequencies[seq][j] = (double) 1 / alleleFrequencies[seq].length;
						}
						seq = seq + 1;
					}
				} else if (selectedChoice == "Personnalisee") {	// this should be translated in labels files - fc - 9.11.2004
					int locusOrder = 1 ;
					for (StringTokenizer st = new StringTokenizer (nuclearCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						String locus = new String ("fr�quences OU valeurs num�riques proportionnelles pour le locus nucl�aire");	// this should be translated in labels files - fc - 9.11.2004
						locus = nbAlleles + " " + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("D�finition personnalis�e des fr�quences all�liques", locus);	// this should be translated in labels files - fc - 9.11.2004
							if (input.isValidDialog ()) {
								String returnedInput = input.getSelectedString ();
								int j=0 ;
								double cumulatedFrequencies = 0;
								for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
									String token = stInput.nextToken ();
									try {
										new Double (token);
									} catch (Exception e) {
										MessageDialog.print (this, Translator.swap ("GeneticsGeneration.invalidFormat"));
										return;
									}			
									alleleFrequencies[seq][j] = (new Double ((String) token)).doubleValue ();
									cumulatedFrequencies = cumulatedFrequencies + alleleFrequencies[seq][j];
									j = j + 1 ;
								}
							// reduce to one 
								for (int j2=0;j2<alleleFrequencies[seq].length;j2++){
									alleleFrequencies[seq][j2] = alleleFrequencies[seq][j2] / cumulatedFrequencies;
								}
							}
						locusOrder = locusOrder + 1 ;
						seq = seq + 1;
					} //END nuclear
					locusOrder = 1 ;
					for (StringTokenizer st = new StringTokenizer (mCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						String locus = new String ("fr�quences OU valeurs num�riques proportionnelles pour le locus mat. cyto.");	// this should be translated in labels files - fc - 9.11.2004
						locus = nbAlleles + " " + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("D�finition personnalis�e des fr�quences all�liques", locus);	// this should be translated in labels files - fc - 9.11.2004
							if (input.isValidDialog ()) {
								String returnedInput = input.getSelectedString ();
								int j=0 ;
								double cumulatedFrequencies = 0;
								for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
									alleleFrequencies[seq][j] = (new Double ((String) stInput.nextToken ())).doubleValue ();
									cumulatedFrequencies = cumulatedFrequencies + alleleFrequencies[seq][j];
									j = j + 1 ;
								}
							//reduce to one 
								for (int j2=0; j2<alleleFrequencies[seq].length; j2++) {
									alleleFrequencies[seq][j2] = alleleFrequencies[seq][j2] / cumulatedFrequencies;
								}
							}
						locusOrder = locusOrder + 1 ;
						seq = seq + 1;
					} //fin mcyto
					locusOrder = 1 ;
					for (StringTokenizer st = new StringTokenizer (pCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue ();
						alleleFrequencies[seq] = new  double[nbAlleles];
						String locus = new String ("fr�quences OU valeurs num�riques proportionnelles pour le locus pat. cyto.");	// this should be translated in labels files - fc - 9.11.2004
						locus = nbAlleles + " " + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("D�finition personnalis�e des fr�quences all�liques", locus);	// this should be translated in labels files - fc - 9.11.2004
							if (input.isValidDialog ()) {
								String returnedInput = input.getSelectedString ();
								int j=0 ;
								double cumulatedFrequencies = 0;
								for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
									alleleFrequencies[seq][j] = (new Double ((String) stInput.nextToken ())).doubleValue ();
									cumulatedFrequencies = cumulatedFrequencies + alleleFrequencies[seq][j];
									j = j + 1 ;
								}
					//normalisation
								for (int j2=0; j2<alleleFrequencies[seq].length; j2++) {
									alleleFrequencies[seq][j2] = alleleFrequencies[seq][j2] / cumulatedFrequencies;
								}
							}
						locusOrder = locusOrder + 1 ;
						seq = seq + 1;
					} //fin pcyto
					
				} else if (selectedChoice == "Lshape") {
					int locusOrder = 1 ;
					int[][] alleleCounts = new int[candidate.length ()][10000];
					double[][] cumulatedFrequencies = new double[candidate.length ()][10000];
					double[][] alleleFrequenciesTempo = new double[candidate.length ()][10000];
					candidate = " ";	
					
					for (StringTokenizer st = new StringTokenizer (nuclearCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue (); 
						//alleleFrequencies[seq] = new  double[nbAlleles];
						//alleleCounts[seq] = new  int[nbAlleles];
						//cumulatedFrequencies[seq] = new  double[nbAlleles];
						String locus = new String (" all�les � tirer. Taux de mutation pour le locus nucl�aire");	// this should be translated in labels files - fc - 9.11.2004
						locus = 2*nbGees + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("Tirage neutraliste des fr�quences all�liques", locus);	// this should be translated in labels files - fc - 9.11.2004
						if (input.isValidDialog ()) {
							String returnedInput = input.getSelectedString ();
							int drawnAlleles = 1;
							double theta = 0;
							StringTokenizer stInput = new StringTokenizer (returnedInput); 
							//theta = (new Double ((String) stInput.nextToken())).doubleValue();
							theta = 4 * (double) nbGees * (new Double ((String) stInput.nextToken ())).doubleValue ();
							//double alleleGenerator=0;
							double uniformRandom0to1;
							alleleCounts[seq][0] = 1 ;
							//while(drawnAlleles < nbAlleles){
							for (int j=1; j < (2 * nbGees); j++) {
								//System.out.println(j);
								uniformRandom0to1 = random.nextDouble ();
								if (uniformRandom0to1 < theta/(theta + (double) j)) { //new allele
									alleleCounts[seq][drawnAlleles] = 1 ;
									drawnAlleles = drawnAlleles + 1 ;
								} else { //already drawn allele
									alleleFrequenciesTempo[seq][0] = (double) alleleCounts[seq][0] / (double) j ;
									cumulatedFrequencies[seq][0] = alleleFrequenciesTempo[seq][0];
									for (int j2 = 1; j2 < drawnAlleles; j2++) { // alleleFrequencies = alleleCounts / j
										alleleFrequenciesTempo[seq][j2]= (double) alleleCounts[seq][j2] / (double) j;
										cumulatedFrequencies[seq][j2] = cumulatedFrequencies[seq][j2-1] + alleleFrequenciesTempo[seq][j2];	
									}
									int alleleRank = 0;
									uniformRandom0to1 = random.nextDouble ();
									while (uniformRandom0to1 > cumulatedFrequencies[seq][alleleRank]) {
										alleleRank = alleleRank + 1 ;									
									}															
									alleleCounts[seq][alleleRank] = alleleCounts[seq][alleleRank] + 1 ;	
								}
							}
							nbAlleles = drawnAlleles;
							alleleFrequencies[seq] = new double[drawnAlleles];
							for (int j=0; j<drawnAlleles; j++) {
								//alleleFrequencies[seq][j] = alleleFrequenciesTempo[seq][j] ;
								//alleleFrequencies[seq][j] = (double) alleleCounts[seq][j] / (double) (2*nbGees) ;
								alleleFrequencies[seq][j] = (double) alleleCounts[seq][j] ;// counts instead of frequencies
							}
							candidate = candidate + "n" + drawnAlleles + " ";
						}
						locusOrder = locusOrder + 1;
						seq = seq + 1;
					} //fin nucleaire
					locusOrder = 1 ;
					for (StringTokenizer st = new StringTokenizer (mCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue (); 
						String locus = new String (" all�les � tirer. Taux de mutation pour le locus mat. cyto.");	// this should be translated in labels files - fc - 9.11.2004
						locus = nbGees + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("Tirage neutraliste des fr�quences all�liques",locus);	// this should be translated in labels files - fc - 9.11.2004
						if (input.isValidDialog ()) {
							String returnedInput = input.getSelectedString ();
							int drawnAlleles = 1;
							double theta = 0;
							StringTokenizer stInput = new StringTokenizer (returnedInput); 
							//theta = (new Double ((String) stInput.nextToken())).doubleValue();
							theta = 2 * (double) nbGees * (new Double ((String) stInput.nextToken ())).doubleValue ();
							//double alleleGenerator=0;
							double uniformRandom0to1;
							alleleCounts[seq][0]= 1 ;
							//while(drawnAlleles < nbAlleles){
							for (int j=1; j < nbGees; j++) {
								//System.out.println(j);
								uniformRandom0to1 = random.nextDouble ();
								if (uniformRandom0to1 < theta/(theta + (double) j)) { //new allele
									alleleCounts[seq][drawnAlleles] = 1 ;
									drawnAlleles = drawnAlleles + 1 ;
								} else { //already drawn allele
									alleleFrequenciesTempo[seq][0] = (double) alleleCounts[seq][0] / (double) j ;
									cumulatedFrequencies[seq][0] = alleleFrequenciesTempo[seq][0];
									for (int j2 = 1; j2 < drawnAlleles; j2++) { // alleleFrequencies = alleleCounts / j
										alleleFrequenciesTempo[seq][j2]= (double) alleleCounts[seq][j2] / (double) j;
										cumulatedFrequencies[seq][j2] = cumulatedFrequencies[seq][j2-1] + alleleFrequenciesTempo[seq][j2];	
									}
									int alleleRank = 0;
									uniformRandom0to1 = random.nextDouble ();
									while (uniformRandom0to1 > cumulatedFrequencies[seq][alleleRank]) {
										alleleRank = alleleRank + 1 ;									
									}															
									alleleCounts[seq][alleleRank]= alleleCounts[seq][alleleRank] + 1;	
								}
							}
							nbAlleles =  drawnAlleles;
							alleleFrequencies[seq] = new double[drawnAlleles];
							for (int j=0; j<drawnAlleles; j++) {
								//alleleFrequencies[seq][j] = alleleFrequenciesTempo[seq][j] ;
								//alleleFrequencies[seq][j] = (double) alleleCounts[seq][j] / (double) nbGees ;
								alleleFrequencies[seq][j] = (double) alleleCounts[seq][j] ;// counts instead of frequencies
							}
							candidate = candidate + "m" + drawnAlleles + " ";
						}
						locusOrder = locusOrder + 1 ;
						seq = seq + 1;
					} //fin mcyto
					locusOrder = 1 ;
					for (StringTokenizer st = new StringTokenizer (pCytoCandidate); st.hasMoreTokens ();) {
						nbAlleles = (new Integer ((String) st.nextToken ())).intValue (); 
						String locus = new String(" all�les � tirer. Taux de mutation pour le locus pat. cyto.");	// this should be translated in labels files - fc - 9.11.2004
						locus = nbGees + locus + " " + locusOrder ;
						DStringInput input = new DStringInput ("Tirage neutraliste des fr�quences all�liques", locus);	// this should be translated in labels files - fc - 9.11.2004
						if (input.isValidDialog ()) {
							String returnedInput = input.getSelectedString ();
							int drawnAlleles = 1;
							double theta = 0;
							StringTokenizer stInput = new StringTokenizer (returnedInput); 
							//theta = (new Double ((String) stInput.nextToken())).doubleValue();
							theta = 2 * (double) nbGees * (new Double ((String) stInput.nextToken ())).doubleValue ();
							//double alleleGenerator=0;
							double uniformRandom0to1;
							alleleCounts[seq][0] = 1 ;
							//while(drawnAlleles < nbAlleles){
							for (int j=1; j < nbGees; j++) {
								//System.out.println(j);
								uniformRandom0to1 = random.nextDouble ();
								if (uniformRandom0to1 < theta/(theta + (double) j)) { //new allele
									alleleCounts[seq][drawnAlleles] = 1 ;
									drawnAlleles = drawnAlleles + 1 ;
								} else { //already drawn allele
									alleleFrequenciesTempo[seq][0] = (double) alleleCounts[seq][0] / (double) j ;
									cumulatedFrequencies[seq][0] = alleleFrequenciesTempo[seq][0];
									for (int j2 = 1; j2 < drawnAlleles; j2++) { // alleleFrequencies = alleleCounts / j
										alleleFrequenciesTempo[seq][j2]= (double) alleleCounts[seq][j2] / (double) j;
										cumulatedFrequencies[seq][j2] = cumulatedFrequencies[seq][j2-1] + alleleFrequenciesTempo[seq][j2];	
									}
									int alleleRank = 0;
									uniformRandom0to1 = random.nextDouble ();
									while (uniformRandom0to1 > cumulatedFrequencies[seq][alleleRank]) {
										alleleRank = alleleRank + 1 ;									
									}															
									alleleCounts[seq][alleleRank]= alleleCounts[seq][alleleRank] + 1 ;	
								}
							}
							nbAlleles =  drawnAlleles;
							alleleFrequencies[seq] = new double[drawnAlleles];
							for (int j=0; j<drawnAlleles; j++) {
								//alleleFrequencies[seq][j] = alleleFrequenciesTempo[seq][j] ;
								//alleleFrequencies[seq][j] = (double) alleleCounts[seq][j] / (double) nbGees ;
								alleleFrequencies[seq][j] = (double) alleleCounts[seq][j];// counts instead of frequencies
							}
							candidate = candidate + "p" + drawnAlleles + " ";
						}
						locusOrder = locusOrder + 1;
						seq = seq + 1;
					} //fin pcyto
				}					
									
				candidate = "d=" + selectedChoice + " " + candidate;

				if (newAlleleFrequenciesActions == null) {
					newAlleleFrequenciesActions = new Hashtable ();
				}	
				newAlleleFrequenciesActions.put (new Integer (currentSpecies), alleleFrequencies);
		
			} // end-of-choice.isValidDialog ()
			choice.dispose ();
			
			if (newLociActions == null) {
				newLociActions = new Hashtable ();
			}	
			newLociActions.put (new Integer (currentSpecies), candidate);
			if (newPRecombinationActions == null) {
				newPRecombinationActions = new Hashtable ();
			}
			newPRecombinationActions.put (new Integer (currentSpecies), pRecombinationLoci.getText ().trim ());

//System.out.println ("--------\n"+Tools.toString (newLociActions));
			updateDisplay ();
		}
	}

	// 
	//
	private void addEffectAction () {
		String candidate = lociList.getText ().trim ();
		//~ for (StringTokenizer st = new StringTokenizer (candidate); st.hasMoreTokens ();) {
			//~ String token = st.nextToken ();
			/*try {
				new Integer (token);
			} catch (Exception e) {
				MessageDialog.promptError (Translator.swap ("GeneticsGeneration.invalidFormat"));
				return;
			}*/
		//~ }

		String parameter = newParameter.getText ().trim ();
		if (newEffectActions == null) {
			newEffectActions = new Hashtable ();
		}
		newEffectActions.put (new Integer (currentSpecies) + " - " + parameter, candidate);

//~ System.out.println ("--------\n"+Tools.toString (newEffectActions));
		updateDisplay ();
	}

	// 
	//
	private void removeAction () {
		ListModel model = displayList.getModel ();
		int l = displayList.getSelectedIndex ();
		String line = (String) model.getElementAt (l);
//~ System.out.println ("--- remove "+line);

		StringTokenizer st = new StringTokenizer (line, ":-");
		String command = st.nextToken ().trim ();
		String speciesName = st.nextToken ().trim ();
//System.out.println ("command "+command+" speciesName "+speciesName+" key "+key+" newLociActions="+newLociActions);

		if (command.equals ("newLoci")) {
			Integer key = (Integer) label_code.get (speciesName);
			newLociActions.remove (key);
		} else if (command.equals ("newEffect")) {
			String parameterName = st.nextToken ().trim ();
			Integer code = (Integer) label_code.get (speciesName);
			String key = code + " - " + parameterName;
			newEffectActions.remove (key);
		}

		updateDisplay ();
	}

	// 
	//
	private void updateDisplay () {
		Vector lines = new Vector ();

		if (newLociActions != null) {
			Iterator keysLoci = newLociActions.keySet ().iterator ();
			Iterator valuesLoci = newLociActions.values ().iterator ();
			while (keysLoci.hasNext () && valuesLoci.hasNext ()) {
				Integer code = (Integer) keysLoci.next ();
				String value = (String) valuesLoci.next ();

				String s = (String) speciesValues.get (code);

				lines.add ("newLoci - "+Translator.swap (s)+" : "+value);
			}
		}

		if (newEffectActions != null) {
			Iterator keysEffect = newEffectActions.keySet ().iterator ();
			Iterator valuesEffect = newEffectActions.values ().iterator ();
			while (keysEffect.hasNext () && valuesEffect.hasNext ()) {
				String key = (String) keysEffect.next ();
				StringTokenizer st = new StringTokenizer (key, "-");
				Integer code = new Integer (st.nextToken ().trim ());
				String parameterName = st.nextToken ().trim ();

				String value = (String) valuesEffect.next ();

				String s = (String) speciesValues.get (code);

				lines.add ("newEffect - "+Translator.swap (s)+ " - "+ parameterName + " : "+value);
			}
		}

		displayList	= new JList (lines);
		display.getViewport ().setView (displayList);
	}

	// Ok button was hit
	//
	public void okAction () {

		if (newLociActions != null) {
			for (Iterator i = newLociActions.keySet ().iterator (); i.hasNext ();) {
				GeneticScene scene = (GeneticScene) step.getScene ();
				AlleleDiversity alleleDiversity = null;
				GeneticMap geneticMap = null;
				Collection geesOfSpecies = new HashSet ();
				int spCode = ((Integer) i.next ()).intValue ();
				if (spCode == MONO_SPECIFIC) {
					
					Object _t = null;
					Iterator _i = scene.getGenotypables ().iterator ();
					do {_t = _i.next ();} while (!(_t instanceof Genotypable));
					Genotypable t = (Genotypable) _t;		// phd 2003_03_17
					
					alleleDiversity = t.getGenoSpecies ().getAlleleDiversity ();
					geneticMap = t.getGenoSpecies ().getGeneticMap ();
					geesOfSpecies = scene.getGenotypables ();
				} else {
					//String spName = (String) speciesValues.get (new Integer (spCode));

					// extract genotypables with current species
					boolean first = true;
					for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
						
						Object _t = j.next ();
						if (!(_t instanceof Genotypable)) {continue;}
						Genotypable t = (Genotypable) _t;		// phd 2003_03_17
						
						//~ SpeciesDefined sd = (SpeciesDefined) t;
						int esp = t.getGenoSpecies ().getValue ();
						if (esp == spCode) {
							geesOfSpecies.add (t);
							if (first) {
								if (t.getGenoSpecies ().getAlleleDiversity () != null) {
									alleleDiversity = t.getGenoSpecies ().getAlleleDiversity ();
									geneticMap = t.getGenoSpecies ().getGeneticMap ();
									first = false;
								}
							}
						}
					}
				}

				// read newLoci and built 3 Vector (one for each DNA) with the number of allele in each new locus
				String alleleDistribution = "";
				Double[][] alleleFrequencies ;
				Vector nuclearNewLoci = new Vector ();
				Vector pRecombinations = new Vector ();
				Vector mCytoplasmicNewLoci = new Vector ();
				Vector pCytoplasmicNewLoci = new Vector ();

				String newLoci = (String) newLociActions.get (new Integer (spCode));
				StringTokenizer st = new StringTokenizer(newLoci);
				while (st.hasMoreTokens ()) {
					String loci = (String) st.nextToken ();
					String ss = loci.substring (0, 1);
					if ("d".equals(ss)) {
						alleleDistribution = loci.substring (2,loci.length ());
						System.out.println ("distribution des alleles = "+ alleleDistribution);
					} else if ("n".equals (ss)) {
						nuclearNewLoci.add (loci.substring (1, loci.length ()));
					} else if ("m".equals (ss)) {
						mCytoplasmicNewLoci.add (loci.substring (1, loci.length ()));
					} else if ("p".equals (ss)) {
						pCytoplasmicNewLoci.add (loci.substring (1, loci.length ()));
					}
//System.out.println ("nuclearnewloci"+nuclearNewLoci);
//System.out.println ("mcytonewloci"+mCytoplasmicNewLoci);
//System.out.println ("pcytonewloci"+pCytoplasmicNewLoci);	
//System.out.println ("termin�");				
				}
				
				String newPRecombinations = (String) newPRecombinationActions.get (new Integer (spCode));
				StringTokenizer st2 = new StringTokenizer (newPRecombinations);
				while (st2.hasMoreTokens ()) {
					pRecombinations.add ((String) st2.nextToken ());
				}
			
				// complete GeneticMap
				GeneticMap newGeneticMap = completeGeneticMap (geneticMap, pRecombinations);

				// complete AlleleDiversity
				AlleleDiversity newAlleleDiversity = completeAlleleDiversity (alleleDiversity, 
						nuclearNewLoci, mCytoplasmicNewLoci, pCytoplasmicNewLoci);

				// add AlleleDiversity and GeneticMap to species
				Genotypable gee = (Genotypable) geesOfSpecies.iterator ().next ();
				gee.getGenoSpecies ().setAlleleDiversity (newAlleleDiversity);
				gee.getGenoSpecies ().setGeneticMap (newGeneticMap);

				// complete IndividualGenotype and MultiGenotype
				// draw alleles
				
				int[][] allelesToAdd = new int[nuclearNewLoci.size ()+mCytoplasmicNewLoci.size ()+pCytoplasmicNewLoci.size ()][];
				double[][] frequenciesTemp = new double[nuclearNewLoci.size ()+mCytoplasmicNewLoci.size ()+pCytoplasmicNewLoci.size ()][];
				int seq=0;
				for (Iterator j = nuclearNewLoci.iterator (); j.hasNext ();) {
					frequenciesTemp[seq] = new double[(new Integer ((String) j.next ())).intValue ()]; 
					allelesToAdd[seq] = new int[2*nbGees];
				    frequenciesTemp[seq] = ((double[][]) (newAlleleFrequenciesActions.get (new Integer (spCode))))[seq];
					allelesToAdd[seq] = GeneticTools.drawWithoutReplacement (2*nbGees, 2*nbGees, (double[]) frequenciesTemp[seq]);
					seq=seq+1;
				}
				for (Iterator j = mCytoplasmicNewLoci.iterator (); j.hasNext ();) {
					frequenciesTemp[seq] = new double[(new Integer ((String) j.next ())).intValue ()]; 
					allelesToAdd[seq] = new int[nbGees];
					frequenciesTemp[seq] = ((double[][]) (newAlleleFrequenciesActions.get (new Integer (spCode))))[seq];
					allelesToAdd[seq] = GeneticTools.drawWithoutReplacement (nbGees, nbGees, frequenciesTemp[seq]);
					seq=seq+1;
				}
				for (Iterator j = pCytoplasmicNewLoci.iterator (); j.hasNext ();) {
					frequenciesTemp[seq] = new double[(new Integer ((String) j.next ())).intValue ()]; 
					allelesToAdd[seq] = new int[nbGees];
					frequenciesTemp[seq] = ((double[][]) (newAlleleFrequenciesActions.get (new Integer (spCode))))[seq];
					allelesToAdd[seq] = GeneticTools.drawWithoutReplacement(nbGees, nbGees, frequenciesTemp[seq]);
					seq=seq+1;
				}
		
				//distribute the drawn alleles in IndividualGenotype and MultiGenotype
				int geeSeq = 0;
				for (Iterator j = geesOfSpecies.iterator (); j.hasNext ();) {
					Genotypable gee2 = (Genotypable) j.next ();
					if (gee2.getGenotype () instanceof MultiGenotype) {
						MultiGenotype newMultiGenotype = completeMultiGenotype (gee2, alleleDiversity, 
								nuclearNewLoci, mCytoplasmicNewLoci, pCytoplasmicNewLoci, allelesToAdd, geeSeq);
						gee2.setMultiGenotype (newMultiGenotype);
						geeSeq = geeSeq + (int) gee2.getNumber();	// fc - 22.8.2006 - Numberable returns double
					}
					if (gee2.getGenotype () instanceof IndividualGenotype) {
						IndividualGenotype newIndividualGenotype = completeIndividualGenotype (gee2, alleleDiversity, 
								nuclearNewLoci, mCytoplasmicNewLoci, pCytoplasmicNewLoci, allelesToAdd, geeSeq);
						gee2.setIndividualGenotype (newIndividualGenotype);
						geeSeq = geeSeq + 1 ;
					}
				}
			}
		}

		if (newEffectActions != null) {
			for (Iterator i = newEffectActions.keySet ().iterator (); i.hasNext ();) {
				GeneticScene scene = (GeneticScene) step.getScene ();
				AlleleDiversity alleleDiversity = null;
				AlleleEffect alleleEffect = null;
				GenoSpecies species = null;	// fc - 15.11.2004
				Genotypable gee = null;
				String key = (String) i.next ();
				StringTokenizer st = new StringTokenizer (key, "-");
				int spCode = (new Integer (st.nextToken ().trim ())).intValue ();
				String parameterName = st.nextToken ().trim ();
				String spName = (String) speciesValues.get (new Integer (spCode));
				if (spCode == MONO_SPECIFIC) {
					
					Iterator _i = scene.getGenotypables ().iterator ();
					Object _t = null;
					do {_t = _i.next ();} while (!(_t instanceof Genotypable));
					gee = (Genotypable) _t;		// phd 2003_03_17
					
					alleleDiversity = gee.getGenoSpecies ().getAlleleDiversity ();
					alleleEffect = gee.getGenoSpecies ().getAlleleEffect ();
				} else {
					for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
						
						Object _t = j.next ();
						if (!(_t instanceof Genotypable)) {continue;}
						gee = (Genotypable) _t;		// phd 2003_03_17
						
						//~ SpeciesDefined sd = (SpeciesDefined) gee;
						species = gee.getGenoSpecies ();
						int esp = species.getValue ();
						if (esp == spCode) {
							if (gee.getGenoSpecies ().getAlleleEffect () != null) {
								alleleEffect = gee.getGenoSpecies ().getAlleleEffect ();
							}
							alleleDiversity = gee.getGenoSpecies ().getAlleleDiversity ();
							break;
						}
					}
				}

				if (alleleDiversity == null) {
					Log.println(Log.INFO, "GeneticsGeneration.okAction ()", "the species " 
							+ spName + " is not studied on genetic level, allele effect can't be defined");
					return;
				} else {
					if (alleleEffect == null) {
						alleleEffect = new AlleleEffect ();
					}
					if (alleleEffect.getParameterEffect (parameterName) != null) {
						alleleEffect.removeParameterEffect (parameterName);
					}
					// read lociList and built 3 Vector (one for each DNA) with the order of loci that influence parameter
					Vector nuclearLociList = new Vector ();
					Vector mCytoplasmicLociList = new Vector ();
					Vector pCytoplasmicLociList = new Vector ();

					int nbn = (alleleDiversity.getNuclearAlleleDiversity ()).length;
					int nbm = (alleleDiversity.getMCytoplasmicAlleleDiversity ()).length;
					int nbp = (alleleDiversity.getPCytoplasmicAlleleDiversity ()).length;

					String lociList = (String) newEffectActions.get (key);
					StringTokenizer token = new StringTokenizer(lociList);
					while (token.hasMoreTokens()) {
						String loci = (String) token.nextToken ();
						String ss = loci.substring (0, 1);
						if ("n".equals(ss)) {
							int num = new Integer (loci.substring (1, loci.length ())).intValue ();
							nuclearLociList.add (new Integer (num));
							if (num>nbn) {
								Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", "Error during " + Translator.swap ("GeneticsGeneration.newEffectGeneration"));
								JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.noNumOnNuclearDNA"),
										Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
								return;
							}
						} else if ("m".equals(ss)) {
							int num = new Integer (loci.substring (1, loci.length ())).intValue ();
							mCytoplasmicLociList.add (new Integer (num));
							if (num>nbm) {
								Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", "Error during " + Translator.swap ("GeneticsGeneration.newEffectGeneration"));
								JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.noNumOnMCytoplasmicDNA"),
										Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
								return;
							}
						} else if ("p".equals(ss)) {
							int num = new Integer (loci.substring (1, loci.length ())).intValue ();
							pCytoplasmicLociList.add (new Integer (num));
							if (num>nbp) {
								Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", "Error during " + Translator.swap ("GeneticsGeneration.newEffectGeneration"));
								JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.noNumOnPCytoplasmicDNA"),
										Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
								return;
							}
						}
					}

					// read heritability, totalEnvironmental variance et interEnvironmentalVariance
					double iev;
					double her;
					double tev;

					try {
						iev = (double) (new Float (interEnvironmentalVariance.getText ().trim ())).floatValue ();
					} catch (Exception exc) {
						Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", 
								"Error during " + Translator.swap ("GeneticsGeneration.%vei") + "reading");
						JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.ievmustbedefined"),
								Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
						return;
					}
					try {
						her = (double) (new Float (heritability.getText ().trim ())).floatValue ();
					} catch (Exception exc) {
						Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", 
								"Error during " + Translator.swap ("GeneticsGeneration.heritability") + "reading");
						JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.hermustbedefined"),
								Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
						return;
					}
					try {
						tev = (double) (new Float (totalEnvironmentalVariance.getText ().trim ())).floatValue ();
					} catch (Exception exc) {
						Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", 
								"Error during " + Translator.swap ("GeneticsGeneration.vet") + "reading");
						JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.tevmustbedefined"),
								Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
						return;
					}

					if (iev == -1) {
						Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", 
								"Error during " + Translator.swap ("GeneticsGeneration.%vei") + "reading");
						JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.ievmustbenotequalto-1"),
								Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
						return;
					}

					if (her ==-1 && tev == -1) {
						Log.println (Log.ERROR, "GeneticsGeneration.okAction ()", 
								"Error during " + Translator.swap ("GeneticsGeneration.%vei") + Translator.swap ("GeneticsGeneration.her") + "reading");
						JOptionPane.showMessageDialog (this, Translator.swap ("GeneticsGeneration.ievandhernotbeequalto-1,onemustbedefined"),
								Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
						return;
					}
					// calculate parameterEffect

					computeParameterEffect (alleleDiversity, alleleEffect, parameterName, nuclearLociList, 
							mCytoplasmicLociList, pCytoplasmicLociList, her, tev, iev);
					gee.getGenoSpecies ().setAlleleEffect (alleleEffect);
					if (tev == -1) {
						Collection geesOfSpecies = new HashSet ();
						if (spCode == MONO_SPECIFIC) {
							geesOfSpecies = scene.getGenotypables ();
						} else {
							for (Iterator j = scene.getGenotypables ().iterator (); j.hasNext ();) {
								Object _t = j.next ();
								if (!(_t instanceof Genotypable)) {continue;}
								Genotypable gee2 = (Genotypable) _t;		// phd 2003_03_17
								
								//~ SpeciesDefined sd = (SpeciesDefined) gee2;
								int esp = gee2.getGenoSpecies ().getValue ();
								if (esp == spCode) {
									geesOfSpecies.add (gee2);
								}
							}
						}
						alleleEffect.getTotalEnvironmentalVariance (geesOfSpecies, parameterName);
					}
				}
			}
		}
		// si pb : 1 : message, 2 : return

		// quand tout est bon :
		setValidDialog (true);
	}

	/**	ActionListener interface.
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (addLoci)) {
			addLociAction ();
		} else if (evt.getSource ().equals (addEffect)) {
			addEffectAction ();
		} else if (evt.getSource ().equals (remove)) {
			removeAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	// 
	//
	private void optionChangeAction (String option) {
		currentSpecies = ((Integer) label_code.get (option)).intValue ();

		//~ System.out.println ("option="+option);
		//~ System.out.println ("  code="+currentSpecies);
	}

	/**	Called when an item is selected in species.
	*/
	public void itemStateChanged (ItemEvent evt) {
		if (evt.getSource().equals (species)) {
			Object o = evt.getItem ();
			String option = (String) o;
			if (!(option.equals (lastSelectedOption))) {
				lastSelectedOption = option;
				optionChangeAction (option);
			}
		}
	}

	/**	User interface definition
	*/
	private void createUI () {
		ColumnPanel mainPanel = new ColumnPanel ();
		Border etched = BorderFactory.createEtchedBorder ();

		ColumnPanel contextPanel = new ColumnPanel ();
		Border b1 = BorderFactory.createTitledBorder (etched,
				Translator.swap ("GeneticsGeneration.context"));
		contextPanel.setBorder (b1);

		GScene std = step.getScene ();
		GeneticScene scene = (GeneticScene) std;
		Object _t = null;
		Iterator _i = scene.getGenotypables ().iterator ();
		do {_t = _i.next ();} while (!(_t instanceof Genotypable));
		Genotypable t = (Genotypable) _t;		// phd 2003_03_17
		
		int n = searchSpecies (t);	// n : number of species

		if (n == 0) {
			contextPanel.add (new JLabel (Translator.swap ("GeneticsGeneration.allTheIndividuals")));
			currentSpecies = MONO_SPECIFIC;
		} else {
			Vector labels = new Vector (label_code.keySet ());
			species = new JComboBox (labels);
			lastSelectedOption = (String) species.getSelectedItem ();
			currentSpecies = ((Integer) label_code.get (lastSelectedOption)).intValue ();
			species.addItemListener (this);

			LinePanel l1 = new LinePanel ();

			l1.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.species")+" :", 150));
			l1.add (species);
			l1.addStrut0 ();
			contextPanel.add (l1);
		}
		contextPanel.addStrut0 ();

		ColumnPanel lociGenerationPanel = new ColumnPanel ();
		Border b2 = BorderFactory.createTitledBorder (etched,
				Translator.swap ("GeneticsGeneration.newLociGeneration"));
		lociGenerationPanel.setBorder (b2);

		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.newNuclearLoci")+" :", 300));
		newNuclearLoci = new JTextField (5);
		l2.add (newNuclearLoci);
		l2.addStrut0 ();
		lociGenerationPanel.add (l2);
		
//cp 22-03-03		
		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.pRecombinationLoci")+" :", 300));
		pRecombinationLoci = new JTextField (5);
		l3.add (pRecombinationLoci);
		l3.addStrut0 ();
		lociGenerationPanel.add (l3);

		LinePanel l3b = new LinePanel ();
		l3b.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.newMcytoLoci")+" :", 300));
		newMcytoLoci = new JTextField (5);
		l3b.add (newMcytoLoci);
		l3b.addStrut0 ();
		lociGenerationPanel.add (l3b);		

		LinePanel l3c = new LinePanel ();
		l3c.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.newPcytoLoci")+" :", 300));
		newPcytoLoci = new JTextField (5);
		l3c.add (newPcytoLoci);
		l3c.addStrut0 ();
		lociGenerationPanel.add (l3c);		

		LinePanel l4 = new LinePanel ();
		l4.add (new JWidthLabel (" ", 150));
		addLoci = new JButton (Translator.swap ("GeneticsGeneration.addLoci"));
		addLoci.addActionListener (this);
		l4.add (addLoci);
		l4.addGlue ();
		lociGenerationPanel.add (l4);
//cp 22-03-03

		lociGenerationPanel.addStrut0 ();

		ColumnPanel effectGenerationPanel = new ColumnPanel ();
		Border b3 = BorderFactory.createTitledBorder (etched,
				Translator.swap ("GeneticsGeneration.newEffectGeneration"));
		effectGenerationPanel.setBorder (b3);

		LinePanel l5 = new LinePanel ();
		l5.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.newParameter")+" :", 150));
		newParameter = new JTextField (5);
		l5.add (newParameter);
		l5.addStrut0 ();
		effectGenerationPanel.add (l5);

		LinePanel l6 = new LinePanel ();
		l6.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.lociList")+" :", 150));
		lociList = new JTextField (5);
		l6.add (lociList);
		l6.addStrut0 ();
		effectGenerationPanel.add (l6);

		LinePanel l8 = new LinePanel ();
		l8.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.heritability")+" :", 150));
		heritability = new JTextField (5);
		l8.add (heritability);
		l8.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.vet")+" :", 150));
		totalEnvironmentalVariance = new JTextField (5);
		l8.add (totalEnvironmentalVariance);
		l8.add (new JWidthLabel (Translator.swap ("GeneticsGeneration.%vei")+" :", 150));
		interEnvironmentalVariance = new JTextField (5);
		l8.add (interEnvironmentalVariance);
		effectGenerationPanel.add (l8);

		LinePanel l7 = new LinePanel ();
		l7.add (new JWidthLabel (" ", 150));
		addEffect = new JButton (Translator.swap ("GeneticsGeneration.addEffect"));
		addEffect.addActionListener (this);
		l7.add (addEffect);
		l7.addGlue ();
		effectGenerationPanel.add (l7);

		effectGenerationPanel.addStrut0 ();

		ColumnPanel displayPanel = new ColumnPanel ();
		Border b4 = BorderFactory.createTitledBorder (etched,
				Translator.swap ("GeneticsGeneration.display"));
		displayPanel.setBorder (b4);

		LinePanel l10 = new LinePanel ();

		displayList  = new JList ();
		display = new JScrollPane (displayList);
		display.setPreferredSize (new Dimension (700, 150));

		l10.add (display);

		remove = new JButton (Translator.swap ("GeneticsGeneration.remove"));
		remove.addActionListener (this);
		ColumnPanel aux = new ColumnPanel ();
		aux.add (remove);
		aux.addGlue ();

		l10.add (aux);
		l10.addStrut0 ();

		displayPanel.add (l10);
		displayPanel.addStrut0 ();

		mainPanel.add (contextPanel);
		mainPanel.add (lociGenerationPanel);
		mainPanel.add (effectGenerationPanel);
		mainPanel.add (displayPanel);
		mainPanel.addGlue ();
		getContentPane ().add (mainPanel, BorderLayout.CENTER);

		// 2. Control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);

		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}


	// ----------------------------------------------------------------------------------------------------------------------------
	//				M E T H O D S     F O R     O k A c t i o n
	// ----------------------------------------------------------------------------------------------------------------------------

	// 
	//
	// new version	02 09 03
	public static IndividualGenotype completeIndividualGenotype (Genotypable gee, AlleleDiversity ad, 
			Vector nuclearNewLoci, Vector mCytoplasmicNewLoci, Vector pCytoplasmicNewLoci, 
			int[][] allelesToAdd, int geeSeq) {
				
		IndividualGenotype g = (IndividualGenotype) gee.getGenotype ();
		int na = 0;
		int ma = 0;
		int pa = 0;
		if (g != null) {
			short[][] nuclearDNA = g.getNuclearDNA ();
			short[] mCytoplasmicDNA = g.getMCytoplasmicDNA ();
			short[] pCytoplasmicDNA = g.getPCytoplasmicDNA ();
			na = nuclearDNA.length;
			ma = mCytoplasmicDNA.length;
			pa = pCytoplasmicDNA.length;
		}

		short[][] newNuclearDNA = new short[na + nuclearNewLoci.size ()][2];
		short[] newMCytoplasmicDNA = new short[ma + mCytoplasmicNewLoci.size ()];
		short[] newPCytoplasmicDNA = new short[pa + pCytoplasmicNewLoci.size ()];

		int seq = 0;
		int j = 0;
		for (Iterator i = nuclearNewLoci.iterator (); i.hasNext ();) {
			i.next ();
			newNuclearDNA[na + j][0] = (short) allelesToAdd[seq][2*geeSeq];
			newNuclearDNA[na + j][1] = (short) allelesToAdd[seq][2*geeSeq + 1];	
			j = j + 1;
			seq = seq + 1;
		}
		j = 0;
		for (Iterator i = mCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			i.next ();
			newMCytoplasmicDNA[ma + j] = (short) allelesToAdd[seq][geeSeq];
			j = j + 1;
			seq = seq + 1;
		}
		j = 0;
		for (Iterator i = pCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			i.next ();
			newPCytoplasmicDNA[pa + j] = (short) allelesToAdd[seq][geeSeq];
			j = j + 1;
			seq = seq + 1;
		}
		if (g != null) {
			System.arraycopy (g.getNuclearDNA (), 0, newNuclearDNA, 0, g.getNuclearDNA ().length);
			System.arraycopy (g.getMCytoplasmicDNA (), 0, newMCytoplasmicDNA, 0, g.getMCytoplasmicDNA ().length);
			System.arraycopy (g.getPCytoplasmicDNA (), 0, newPCytoplasmicDNA, 0, g.getPCytoplasmicDNA ().length);
		}
		IndividualGenotype newIndividualGenotype = new IndividualGenotype (newNuclearDNA, newMCytoplasmicDNA, newPCytoplasmicDNA);
		return newIndividualGenotype;
	}

	/*public static IndividualGenotype completeIndividualGenotype (GeneticTree t, AlleleDiversity ad, Vector nuclearNewLoci, Vector mCytoplasmicNewLoci, Vector pCytoplasmicNewLoci, double[][] alleleFrequencies) {
		IndividualGenotype g = (IndividualGenotype) t.getGenotype ();
		int na = 0;
		int ma= 0;
		int pa = 0;
		if (g != null) {
			short[][] nuclearDNA = g.getNuclearDNA ();
			short[] mCytoplasmicDNA = g.getMCytoplasmicDNA ();
			short[] pCytoplasmicDNA = g.getPCytoplasmicDNA ();
			na = nuclearDNA.length;
			ma = mCytoplasmicDNA.length;
			pa = pCytoplasmicDNA.length;
		}

		short[][] newNuclearDNA = new short[na + nuclearNewLoci.size ()][2];
		short[] newMCytoplasmicDNA = new short[ma + mCytoplasmicNewLoci.size ()];
		short[] newPCytoplasmicDNA = new short[pa + pCytoplasmicNewLoci.size ()];

		int seq = 0;
		int j = 0;
		for (Iterator i = nuclearNewLoci.iterator (); i.hasNext ();) {
			int nbAllele = (new Integer ((String) i.next ())).intValue ();
			double[] cumulatedFrequencies = new double[nbAllele] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nbAllele; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}

			double proba = random.nextDouble();
			int ll=1;
			while(cumulatedFrequencies[ll-1]<proba){ll++;}
			newNuclearDNA[na + j][0] = (short) ll;
			proba = random.nextDouble();
			ll=1;
			while(cumulatedFrequencies[ll-1]<proba){ll++;}
			newNuclearDNA[na + j][1] = (short) ll;	
			j = j + 1;
			seq = seq + 1;
		}

		j = 0;
		for (Iterator i = mCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			int nbAllele = (new Integer ((String) i.next ())).intValue ();
			double[] cumulatedFrequencies = new double[nbAllele] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nbAllele; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}
			double proba = random.nextDouble();
			int ll=1;
			while(cumulatedFrequencies[ll-1]<proba){ll++;}
			newMCytoplasmicDNA[ma + j] = (short) ll;
			j = j + 1;
			seq = seq + 1;
		}
		j = 0;
		for (Iterator i = pCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			int nbAllele = (new Integer ((String) i.next ())).intValue ();
			double[] cumulatedFrequencies = new double[nbAllele] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nbAllele; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}
			double proba = random.nextDouble();
			int ll=1;
			while(cumulatedFrequencies[ll-1]<proba){ll++;}
			newPCytoplasmicDNA[pa + j] = (short) ll;
			j = j + 1;
			seq = seq + 1;
		}

		if (g != null) {
			System.arraycopy (g.getNuclearDNA (), 0, newNuclearDNA, 0, g.getNuclearDNA ().length);
			System.arraycopy (g.getMCytoplasmicDNA (), 0, newMCytoplasmicDNA, 0, g.getMCytoplasmicDNA ().length);
			System.arraycopy (g.getPCytoplasmicDNA (), 0, newPCytoplasmicDNA, 0, g.getPCytoplasmicDNA ().length);
		}
		IndividualGenotype newIndividualGenotype = new IndividualGenotype (newNuclearDNA, newMCytoplasmicDNA, newPCytoplasmicDNA);
		return newIndividualGenotype;
	}*/
	
	// 
	//
	// new  version	02 09 03
	public static MultiGenotype completeMultiGenotype (Genotypable gee, AlleleDiversity ad, 
			Vector nuclearNewLoci, Vector mCytoplasmicNewLoci, Vector pCytoplasmicNewLoci, 
			int[][] allelesToAdd, int geeSeq) {
				
		MultiGenotype mg = (MultiGenotype) gee.getGenotype ();
		int nadLength = 0;
		int madLength = 0;
		int padLength = 0;
		int number = (int) gee.getNumber ();	// fc - 22.8.2006 - Numberable returns double
		if (ad != null) {
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity ();
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity ();
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity ();
			nadLength = nuclearAlleleDiversity.length;
			madLength = mCytoplasmicAlleleDiversity.length;
			padLength = pCytoplasmicAlleleDiversity.length;
		}
		int[][] newNuclearAlleleFrequency = new int[nadLength + nuclearNewLoci.size ()][];;
		int[][] newMCytoplasmicAlleleFrequency = new int[madLength + mCytoplasmicNewLoci.size ()][];;
		int[][] newPCytoplasmicAlleleFrequency = new int[padLength + pCytoplasmicNewLoci.size ()][];

		int seq = 0;
		int k = 0;
		for (Iterator j = nuclearNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newNuclearAlleleFrequency[nadLength + k] = new int[nb];
			for (int l = 0; l<2 * number; l++) {
				newNuclearAlleleFrequency[nadLength + k][(allelesToAdd[seq][2*geeSeq+l])-1] = 
						newNuclearAlleleFrequency[nadLength + k][(allelesToAdd[seq][2*geeSeq+l])-1] + 1;
			}
			k = k + 1;
			seq = seq + 1;
		}
		k = 0;
		for (Iterator j = mCytoplasmicNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newMCytoplasmicAlleleFrequency[madLength + k] = new int[nb];						
			for (int l = 0; l<number; l++) {
				newMCytoplasmicAlleleFrequency[madLength + k][(allelesToAdd[seq][geeSeq+l])-1] = 
						newMCytoplasmicAlleleFrequency[madLength + k][(allelesToAdd[seq][geeSeq+l])-1] + 1;
			}
			k = k + 1;
			seq = seq + 1;
		}
		k = 0;
		for (Iterator j = pCytoplasmicNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newPCytoplasmicAlleleFrequency[padLength + k] = new int[nb];
			for (int l = 0; l<number; l++) {
				newPCytoplasmicAlleleFrequency[padLength + k][(allelesToAdd[seq][geeSeq+l])-1] = 
						newPCytoplasmicAlleleFrequency[padLength + k][(allelesToAdd[seq][geeSeq+l])-1] + 1;
			}
			k = k + 1;
			seq = seq + 1;		
		}
		if (mg != null) {
			System.arraycopy (mg.getNuclearAlleleFrequency (), 0, newNuclearAlleleFrequency, 0, mg.getNuclearAlleleFrequency ().length);
			System.arraycopy (mg.getMCytoplasmicAlleleFrequency (), 0, newMCytoplasmicAlleleFrequency, 0, mg.getMCytoplasmicAlleleFrequency ().length);
			System.arraycopy (mg.getPCytoplasmicAlleleFrequency (), 0, newPCytoplasmicAlleleFrequency, 0, mg.getPCytoplasmicAlleleFrequency ().length);
		}
		MultiGenotype newMultiGenotype = new MultiGenotype (newNuclearAlleleFrequency, newMCytoplasmicAlleleFrequency, newPCytoplasmicAlleleFrequency);
		return newMultiGenotype;
	}

	/*public static MultiGenotype completeMultiGenotype (GeneticTree t, AlleleDiversity ad, Vector nuclearNewLoci, Vector mCytoplasmicNewLoci, Vector pCytoplasmicNewLoci, double[][] alleleFrequencies) {
		MultiGenotype mg = (MultiGenotype) t.getGenotype ();
		int nadLength = 0;
		int madLength = 0;
		int padLength = 0;
		int number = t.getNumber ();
		if (ad != null) {
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity ();
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity ();
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity ();
			nadLength = nuclearAlleleDiversity.length;
			madLength = mCytoplasmicAlleleDiversity.length;
			padLength = pCytoplasmicAlleleDiversity.length;
		}
		int[][] newNuclearAlleleFrequency = new int[nadLength + nuclearNewLoci.size ()][];;
		int[][] newMCytoplasmicAlleleFrequency = new int[madLength + mCytoplasmicNewLoci.size ()][];;
		int[][] newPCytoplasmicAlleleFrequency = new int[padLength + pCytoplasmicNewLoci.size ()][];

		int seq = 0;
		int k = 0;
		for (Iterator j = nuclearNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newNuclearAlleleFrequency[nadLength + k] = new int[nb];
			int eff = 0;

			double[] cumulatedFrequencies = new double[nb] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nb; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}

			for (int l = 0; l<2 * number; l++) {
				double proba = random.nextDouble();
			    int ll=0;
				while(cumulatedFrequencies[ll]<proba){ll++;}
				newNuclearAlleleFrequency[nadLength + k][ll] = newNuclearAlleleFrequency[nadLength + k][ll] + 1 ;
			}
			k = k + 1;
			seq = seq + 1;
		}
		k = 0;
		for (Iterator j = mCytoplasmicNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newMCytoplasmicAlleleFrequency[madLength + k] = new int[nb];
			int eff = 0;
			
			double[] cumulatedFrequencies = new double[nb] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nb; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}	
						
			for (int l = 0; l<number; l++) {
				double proba = random.nextDouble();
			    int ll=0;
				while(cumulatedFrequencies[ll]<proba){ll++;}
				newMCytoplasmicAlleleFrequency[madLength + k][ll] = newMCytoplasmicAlleleFrequency[madLength + k][ll] + 1;
			}
			k = k + 1;
			seq = seq + 1;
		}
		k = 0;
		for (Iterator j = pCytoplasmicNewLoci.iterator (); j.hasNext ();) {
			int nb = (new Integer ((String) j.next ())).intValue ();
			newPCytoplasmicAlleleFrequency[padLength + k] = new int[nb];
			int eff = 0;
			
			double[] cumulatedFrequencies = new double[nb] ;			
			cumulatedFrequencies[0] = alleleFrequencies[seq][0];
			for (int l = 1; l<nb; l++) {
				cumulatedFrequencies[l] = cumulatedFrequencies[l-1] + alleleFrequencies[seq][l] ;
			}	

			for (int l = 0; l<number; l++) {
				double proba = random.nextDouble();
			    int ll=0;
				while(cumulatedFrequencies[ll]<proba){ll++;}
				newPCytoplasmicAlleleFrequency[padLength + k][ll] = newPCytoplasmicAlleleFrequency[padLength + k][ll] + 1;
			}
			k = k + 1;
			seq = seq + 1;		
		}
		if (mg != null) {
			System.arraycopy (mg.getNuclearAlleleFrequency (), 0, newNuclearAlleleFrequency, 0, mg.getNuclearAlleleFrequency ().length);
			System.arraycopy (mg.getMCytoplasmicAlleleFrequency (), 0, newMCytoplasmicAlleleFrequency, 0, mg.getMCytoplasmicAlleleFrequency ().length);
			System.arraycopy (mg.getPCytoplasmicAlleleFrequency (), 0, newPCytoplasmicAlleleFrequency, 0, mg.getPCytoplasmicAlleleFrequency ().length);
		}
		MultiGenotype newMultiGenotype = new MultiGenotype (newNuclearAlleleFrequency, newMCytoplasmicAlleleFrequency, newPCytoplasmicAlleleFrequency);
		return newMultiGenotype;
	}*/

	/**	
	*/
	public static GeneticMap completeGeneticMap (GeneticMap gm, Vector pRecombinations) {
		int l = 0;
		int j = 0;
		if (gm != null) {
			float[] recombinationProbas = gm.getRecombinationProbas ();
			l = recombinationProbas.length;
		}
		float[] newRecombinationProbas = new float[l + pRecombinations.size ()];
		for (Iterator i = pRecombinations.iterator (); i.hasNext ();) {
			newRecombinationProbas[l + j] = (new Float ((String) i.next ())).floatValue () ; 
			j = j + 1 ;
		}
		if (gm != null) {
			System.arraycopy (gm.getRecombinationProbas (), 0, newRecombinationProbas, 0, gm.getRecombinationProbas ().length);
		}
		GeneticMap newGeneticMap = new GeneticMap (newRecombinationProbas, null);
		return newGeneticMap;
	}

	/**	
	*/
	public static AlleleDiversity completeAlleleDiversity (AlleleDiversity ad, Vector nuclearNewLoci, 
			Vector mCytoplasmicNewLoci, Vector pCytoplasmicNewLoci) {
		
		int nadLength = 0;
		int madLength = 0;
		int padLength = 0;
		if (ad != null) {
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity ();
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity ();
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity ();
			nadLength = nuclearAlleleDiversity.length;
			madLength = mCytoplasmicAlleleDiversity.length;
			padLength = pCytoplasmicAlleleDiversity.length;
//~ System.out.println("padLength"+padLength);			
			
		}
		short[][] newNuclearAlleleDiversity = new short[nadLength + nuclearNewLoci.size ()][];
		short[][] newMCytoplasmicAlleleDiversity = new short[madLength + mCytoplasmicNewLoci.size ()][];
		short[][] newPCytoplasmicAlleleDiversity = new short[padLength + pCytoplasmicNewLoci.size ()][];

		int j = 0;
		for (Iterator i = nuclearNewLoci.iterator (); i.hasNext ();) {
			int k = (new Integer ((String) i.next ())).intValue ();
			newNuclearAlleleDiversity[nadLength + j] = new short[k];
			for (int q = 0; q<k; q++) {
				newNuclearAlleleDiversity[nadLength + j][q] = (short) (q + 1);
			}
			j = j + 1;
		}
		if (ad != null) {
			System.arraycopy (ad.getNuclearAlleleDiversity (), 0, newNuclearAlleleDiversity, 0, (ad.getNuclearAlleleDiversity ()).length);
		}

		j = 0;
		for (Iterator i = mCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			int k = (new Integer ((String) i.next ())).intValue ();
			newMCytoplasmicAlleleDiversity[madLength + j] = new short[k];
			for (int q = 0; q<k; q++) {
				newMCytoplasmicAlleleDiversity[madLength + j][q] = (short) (q + 1);
			}
			j = j + 1;
		}
		if (ad != null) {
			System.arraycopy (ad.getMCytoplasmicAlleleDiversity (), 0, newMCytoplasmicAlleleDiversity, 0, (ad.getMCytoplasmicAlleleDiversity ()).length);
		}

		j = 0;
		for (Iterator i = pCytoplasmicNewLoci.iterator (); i.hasNext ();) {
			int k = (new Integer ((String) i.next ())).intValue ();
			newPCytoplasmicAlleleDiversity[padLength + j] = new short[k];
			for (int q = 0; q<k; q++) {
				newPCytoplasmicAlleleDiversity[padLength + j][q] = (short) (q + 1);
			}
			j = j + 1;
		}
		if (ad != null) {
			System.arraycopy (ad.getPCytoplasmicAlleleDiversity (), 0, newPCytoplasmicAlleleDiversity, 0, (ad.getPCytoplasmicAlleleDiversity ()).length);
		}

		AlleleDiversity newAlleleDiversity =  new AlleleDiversity (newNuclearAlleleDiversity, newMCytoplasmicAlleleDiversity, newPCytoplasmicAlleleDiversity);
		return newAlleleDiversity;
	}

	//	
	//
	// ici proposer plusieurs distributions <- comments should be in english - fc
	public static void computeParameterEffect (AlleleDiversity alleleDiversity, AlleleEffect alleleEffect, 
			String parameterName, Vector nuclearLociList, Vector mCytoplasmicLociList, Vector pCytoplasmicLociList, 
			double heritability, double totalEnvironmentalVariance, double interEnvironmentalVariance) {
				
		short[][] nuclearAlleleEffect = new short[nuclearLociList.size ()][];
		short[][] mCytoplasmicAlleleEffect = new short[mCytoplasmicLociList.size ()][];
		short[][] pCytoplasmicAlleleEffect = new short[pCytoplasmicLociList.size ()][];

		Collection liste = new Vector ();
		liste.add ("Gaussiens") ;	// this should be translated in labels files - fc - 9.11.2004
		liste.add ("Personnalis�s") ;	// this should be translated in labels files - fc - 9.11.2004
		//liste.add ("normale") ;
		//liste.add ("Lshape") ;	// this should be translated in labels files - fc - 9.11.2004
		DStringSelector choice = new DStringSelector ("Effets des all�les", "Choisir une modalit�", liste) ;	// this should be translated in labels files - fc - 9.11.2004
								
		if (choice.isValidDialog ()) {
			String selectedChoice = choice.getSelectedString ();
if(selectedChoice=="Gaussiens"){	//cp november 05
			int ligne = 0;
			for (Iterator j = nuclearLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] nuclearAlleleDiversity = alleleDiversity.getNuclearAlleleDiversity ();
				int nbAllele = nuclearAlleleDiversity[lociNum-1].length;
				nuclearAlleleEffect[ligne] = new short[nbAllele + 1];
				nuclearAlleleEffect[ligne][0] = (short) lociNum;
				short sum = 0;
				for (int k = 0; k<nbAllele-1; k++) {
					short effect = (short) (random.nextGaussian ()*10);		//cp march 03
					nuclearAlleleEffect[ligne][k+1] = effect;
					sum = (short) (sum + effect);
				}
				short effect = (short) (-1 * sum);
				nuclearAlleleEffect[ligne][nbAllele] = effect;
				ligne = ligne + 1;
			}
			ligne = 0;
			for (Iterator j = mCytoplasmicLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] mCytoplasmicAlleleDiversity = alleleDiversity.getMCytoplasmicAlleleDiversity ();
				int nbAllele = mCytoplasmicAlleleDiversity[lociNum-1].length;
				mCytoplasmicAlleleEffect[ligne] = new short[nbAllele + 1];
				mCytoplasmicAlleleEffect[ligne][0] = (short) lociNum;
				short sum = 0;
				for (int k = 0; k<nbAllele-1; k++) {
					short effect = (short) (random.nextGaussian ()*10);		//cp march 03
					mCytoplasmicAlleleEffect[ligne][k+1] = effect;
					sum = (short) (sum + effect);
				}
				short effect = (short) (-1 * sum);
				mCytoplasmicAlleleEffect[ligne][nbAllele] = effect;
				ligne = ligne + 1;
			}
			ligne = 0;
			for (Iterator j = pCytoplasmicLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] pCytoplasmicAlleleDiversity = alleleDiversity.getPCytoplasmicAlleleDiversity ();
				int nbAllele = pCytoplasmicAlleleDiversity[lociNum-1].length;
				pCytoplasmicAlleleEffect[ligne] = new short[nbAllele + 1];
				pCytoplasmicAlleleEffect[ligne][0] = (short) lociNum;
				short sum = 0;
				for (int k = 0; k<nbAllele-1; k++) {
					short effect = (short) (random.nextGaussian ()*10);		//cp march 03
					pCytoplasmicAlleleEffect[ligne][k+1] = effect;
					sum = (short) (sum + effect);
				}
				short effect = (short) (-1 * sum);
				pCytoplasmicAlleleEffect[ligne][nbAllele] = effect;
				ligne = ligne + 1;
			}
			alleleEffect.addParameterEffect (parameterName, nuclearAlleleEffect, mCytoplasmicAlleleEffect, 
					pCytoplasmicAlleleEffect, heritability, totalEnvironmentalVariance, interEnvironmentalVariance);
		}	// end gaussien cp november 05

		else if(selectedChoice=="Personnalis�s"){//cp november 05
			int ligne = 0;
			for (Iterator j = nuclearLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] nuclearAlleleDiversity = alleleDiversity.getNuclearAlleleDiversity ();
				int nbAllele = nuclearAlleleDiversity[lociNum-1].length;
				nuclearAlleleEffect[ligne] = new short[nbAllele + 1];
				nuclearAlleleEffect[ligne][0] = (short) lociNum;
				String locus = new String ("entiers pour le locus nucl�aire");
				locus = nbAllele + " " + locus + " " + lociNum ;
				DStringInput input = new DStringInput ("D�finition personnalis�e des effets des all�les", locus);
				if (input.isValidDialog ()) {
					String returnedInput = input.getSelectedString ();
					int k = 0;
					for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
						String token = stInput.nextToken ();
						try {
							new Short (token);
						} catch (Exception e) {
							MessageDialog.print (null, Translator.swap ("GeneticsGeneration.invalidFormat"));
							return;
						}
						nuclearAlleleEffect[ligne][k+1] = (new Short ((String) token)).shortValue();
						k = k + 1 ;
					}
				}
				ligne = ligne + 1;
			}
			ligne = 0;
			for (Iterator j = mCytoplasmicLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] mCytoplasmicAlleleDiversity = alleleDiversity.getMCytoplasmicAlleleDiversity ();
				int nbAllele = mCytoplasmicAlleleDiversity[lociNum-1].length;
				mCytoplasmicAlleleEffect[ligne] = new short[nbAllele + 1];
				mCytoplasmicAlleleEffect[ligne][0] = (short) lociNum;
				String locus = new String ("entiers pour le locus maternel");
				locus = nbAllele + " " + locus + " " + lociNum ;
				DStringInput input = new DStringInput ("D�finition personnalis�e des effets des all�les", locus);
				if (input.isValidDialog ()) {
					String returnedInput = input.getSelectedString ();
					int k = 0;
					for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
						String token = stInput.nextToken ();
						try {
							new Short (token);
						} catch (Exception e) {
							MessageDialog.print (null, Translator.swap ("GeneticsGeneration.invalidFormat"));
							return;
						}
						mCytoplasmicAlleleEffect[ligne][k+1] = (new Short ((String) token)).shortValue();
						k = k + 1 ;
					}
				}
				ligne = ligne + 1;
			}
			ligne = 0;
			for (Iterator j = pCytoplasmicLociList.iterator (); j.hasNext ();) {
				int lociNum = ((Integer) j.next ()).intValue ();
				short[][] pCytoplasmicAlleleDiversity = alleleDiversity.getPCytoplasmicAlleleDiversity ();
				int nbAllele = pCytoplasmicAlleleDiversity[lociNum-1].length;
				pCytoplasmicAlleleEffect[ligne] = new short[nbAllele + 1];
				pCytoplasmicAlleleEffect[ligne][0] = (short) lociNum;
				String locus = new String ("entiers pour le locus paternel");
				locus = nbAllele + " " + locus + " " + lociNum ;
				DStringInput input = new DStringInput ("D�finition personnalis�e des effets des all�les", locus);
				if (input.isValidDialog ()) {
					String returnedInput = input.getSelectedString ();
					int k = 0;
					for (StringTokenizer stInput = new StringTokenizer (returnedInput); stInput.hasMoreTokens ();) {
						String token = stInput.nextToken ();
						try {
							new Short (token);
						} catch (Exception e) {
							MessageDialog.print (null, Translator.swap ("GeneticsGeneration.invalidFormat"));
							return;
						}
						pCytoplasmicAlleleEffect[ligne][k+1] = (new Short ((String) token)).shortValue();
						k = k + 1 ;
					}
				}
				ligne = ligne + 1;
			}
			alleleEffect.addParameterEffect (parameterName, nuclearAlleleEffect, mCytoplasmicAlleleEffect, 
					pCytoplasmicAlleleEffect, heritability, totalEnvironmentalVariance, interEnvironmentalVariance);	
			}// end personnalis�s cp november 05
		}//  valid
	}

}

