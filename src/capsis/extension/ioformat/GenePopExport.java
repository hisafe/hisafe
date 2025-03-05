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

import java.text.NumberFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.swing.RepaintManager;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.Genotypable;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import capsis.util.StandRecordSet;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.Record;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**	Exportation to GenePop for modules with genetics information and 
*	spatialized individuals, at this time Genotypables (x, y needed in export file).
*
*	@author I. Seynave - 2002, F. de Coligny - November 2004
*/
public class GenePopExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.GenePopExport");
	}

	private Map population;
	private Map dna;

	private GScene stand;	// fc - 16.5.2006
	
	private String dendroCritere;
	private String geneCritere;
	private int dendroPas;

	// Progress management - fc - 16.5.2006
	private double step = 1;
	private double progressValue = 0;

	// Generic keyword record is described in superclass: key = value

	// Export record for locus is described here
	@Import
	static public class LociRecord extends Record {
		public LociRecord () {super ();}
		public LociRecord (String line) throws Exception {super (line);}
		public String name;
	}

	// Export record for Genotypables is described here
	@Import
	static public class GenotypableRecord extends Record {
		public GenotypableRecord () {super ();}
		public GenotypableRecord (String line) throws Exception {super (line);}
		public String genotype;
	}


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public GenePopExport () {}

	/**	Script mode constructor. 
	*	Use: new GenePopExport (starter).save (exportFileName);
	*/
	public GenePopExport (Step step,
			GScene std,	
			String groupName,
			boolean dendroFilter,
			int dendroClassWith,
			String dendroCriterium,
			boolean geneFilter,
			String geneCriterium,
			boolean exportNuclearDNA,
			boolean exportMCytoplasmicDNA,
			boolean exportPCytoplasmicDNA) throws Exception {
	
		try {
			stand = std;
			
			ProgressDispatcher.setMinMax (0, 100);
			StatusDispatcher.print (Translator.swap ("GenePopExport.exporting")+"...");
			
			createMaps (groupName, dendroFilter, dendroClassWith, dendroCriterium, 
					geneFilter, geneCriterium, exportNuclearDNA, 
					exportMCytoplasmicDNA, exportPCytoplasmicDNA);

			ProgressDispatcher.setValue (90);
			StatusDispatcher.print (Translator.swap ("GenePopExport.writingFile")+"...");
				
			// create the record set
			createRecordSet (population, dna);
			
			ProgressDispatcher.setValue (100);
			ProgressDispatcher.stop ();
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "GenePopExportDialog.c (GenePopExportStarter s)", "error in constructor", e);
			throw e;
		}
		
	}
	
	
	
	/**	Official constructor. 
	*	Format in Export mode needs a Stand in starter (then call save (fileName)). 
	*	Format in Import mode needs fileName in starter (then call load (GModel)). 
	*/
	public void initExport(GModel m, Step s) throws Exception {
	
		if (s.getScene () != null) {
			// Export mode
			setHeaderEnabled (false);	// fc - 4.12.2003 -> Just 1 line in header (SOM) (see createRecordSet ())
			
			//~ GStand stand = s.getStand ();
			stand = s.getScene ();	// fc - 16.5.2006
			
			try {
				GenePopExportDialog dlg = new GenePopExportDialog (stand);
				if (!dlg.isValidDialog ()) {return;}	// user canceled dialog -> stop
				
				// retrieve values in dialog - fc - 16.5.2006
				String groupName = dlg.getGroupName ();
				boolean dendroFilter = dlg.isDendroFilter ();
				int dendroClassWith = dlg.getDendroClassWith ();
				String dendroCriterium = dlg.getDendroCriterium ();
				boolean geneFilter = dlg.isGeneFilter ();
				String geneCriterium = dlg.getGeneCriterium ();
				boolean exportNuclearDNA = dlg.isExportNuclearDNA ();
				boolean exportMCytoplasmicDNA = dlg.isExportMCytoplasmicDNA ();
				boolean exportPCytoplasmicDNA = dlg.isExportPCytoplasmicDNA ();

				RepaintManager.currentManager (dlg).paintDirtyRegions ();	// ok!
				dlg.dispose ();
				
				ProgressDispatcher.setMinMax (0, 100);
				StatusDispatcher.print (Translator.swap ("GenePopExport.exporting")+"...");
				
				// create population and dna maps - fc - 16.5.2006
				try {
					createMaps (groupName, dendroFilter, dendroClassWith, dendroCriterium, 
							geneFilter, geneCriterium, exportNuclearDNA, exportMCytoplasmicDNA, exportPCytoplasmicDNA);
				} catch (Exception e) {
					Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "error in okAction", e);
					MessageDialog.print (this, e.getMessage ());
					return;
				}
				
				ProgressDispatcher.setValue (90);
				StatusDispatcher.print (Translator.swap ("GenePopExport.writingFile")+"...");
				
				// create the record set
				createRecordSet (population, dna);
				
				ProgressDispatcher.setValue (100);
				ProgressDispatcher.stop ();
				
			} catch (Exception e) {
				Log.println (Log.ERROR, "GenePopExport.c ()", "An error occured during export", e);
				MessageDialog.print (this, Translator.swap ("GenePopExport.errorDuringExport"), e);
			}
			
		} else {
			throw new Exception ("Unable to recognize mode Import/Export.");
		}
	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeList)) {return false;}	// we need x, y : spatialized trees
			if (!(s instanceof GeneticScene)) {return false;}	// we need genotypables
			
			// fc - 6.10.2003 - further tests will inform user in config 
			// dialog if no Genotypables are found
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "GenePopExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**	
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("GenePopExport.name");
	}

	/**	
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "I. Seynave";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("GenePopExport.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.2";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.2";

	// fc + so - 15.5.2006 - createMaps made from Dialog okAction to be usable from a script
	public void createMaps (String groupName, 	// a groupName / null
			boolean dendroFilter, 				// true / false
			int dendroClassWith, 				// int
			String dendroCriterium, 			// age / height / diameter
			boolean geneFilter, 				// true / false
			String geneCriterium, 				// mId / pId / creationDate
			boolean exportNuclearDNA, 
			boolean exportMCytoplasmicDNA, 
			boolean exportPCytoplasmicDNA) throws Exception {
		
					ProgressDispatcher.setValue ((int) progressValue);
		
		// 1. If user gave no group, 
		// population <-  the trees with genotype: Individual and != null
		//
		//~ if (!grouperChooser.isGrouperSelected ()) {
		if (groupName == null) {
			population = new Hashtable ();
			Set set = new HashSet ();
			
			step = 20d / ((TreeList) stand).getTrees ().size ();	// fc - 16.5.2006 - progress management
			
			for (Iterator i = ((TreeList) stand).getTrees ().iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				
				progressValue += step;
				ProgressDispatcher.setValue ((int) progressValue);
				
				if (t instanceof Genotypable) {	// fc - 6.10.2003 - maybe some trees are not GeneticTrees
					Genotypable tree = (Genotypable) t;
					Genotype g = tree.getGenotype ();
					if (g != null && g instanceof IndividualGenotype) {set.add (tree);}
				}
			}
			population.put ("allTreeWithGenotype", set);

		// 2. If user gave one group, 
		// population <-  the trees in the group with genotype: Individual and != null
		//
		} else {
			Map temporaire = new Hashtable ();
			
			GrouperManager gm = GrouperManager.getInstance ();
			//~ Grouper grouper = gm.getGrouper (grouperChooser.getGrouperName ());	// return null if not found
			Grouper grouper = gm.getGrouper (groupName);	// return null if not found
			
			Collection allTrees = ((TreeCollection) stand).getTrees ();
			boolean not = groupName.toLowerCase ().startsWith ("not ");	// fc - 16.5.2006
			Collection trees = grouper.apply (allTrees, not);	// fc - 9.4.2004
			
			step = 20d / trees.size ();	// fc - 16.5.2006 - progress management
			
			Set allTreesInGroup= new HashSet ();
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				
				progressValue += step;
				ProgressDispatcher.setValue ((int) progressValue);
				
				if (t instanceof Genotypable) {	// fc - 6.10.2003 - maybe some trees are not GeneticTrees
					Genotypable tr = (Genotypable) t;
					if (tr.getGenotype () != null && tr.getGenotype () instanceof IndividualGenotype) {
						allTreesInGroup.add (tr);
					}
				}
			}
			//~ String grouperName = "Group"+grouperChooser.getGrouperName ();
			String grouperName = "Group"+groupName;
			temporaire.put (grouperName, allTreesInGroup);
			population = temporaire;
		}

		ProgressDispatcher.setValue ((int) progressValue);
		
		// From this point, we have only Genotypable GTrees in the population (see upper) - fc
		
		// 3. If all trees in the group have not same species, a message is writen in the log
		//
		Set name = population.keySet ();
		String s = (String) name.iterator ().next ();
		
		Set trees = (Set) population.get (s);
		
		// fc - 9.11.2004
		if (trees.isEmpty ()) {
			throw new Exception (Translator.swap ("GenePopExportDialog.noTreesSelected"));
		}
		
		// Species of first tree is "none" or something else - fc
		Genotypable tr = (Genotypable) trees.iterator ().next ();
		
		String speciesName = tr.getGenoSpecies ().toString ();
			
		step = 20d / trees.size ();	// fc - 16.5.2006 - progress management
			
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			// Species of current tree is "none" or something else - fc
			tr = (Genotypable) i.next ();	// cast ok: case 1. and 2. upper ensure each tree in set is a Genetic tree - fc
				
			progressValue += step;
			ProgressDispatcher.setValue ((int) progressValue);
				
			String speciesOfTree = tr.getGenoSpecies ().toString ();
			
			// if species of current tree is different from species of first tree, trouble - fc
			if (!speciesName.equals (speciesOfTree)) {
				throw new Exception (Translator.swap ("GenePopExportDialog.allTreesMustHaveSameSpecies"));
			}
		}

		ProgressDispatcher.setValue ((int) progressValue);
		
		// 4. If requested, build populations from dendrometric parameter
		//
		double maxDendro = 0;
		double minDendro = 100000;
		int nbClasse = 0;
		int minFirstClasse = 0;
		if (dendroFilter) {
			dendroPas = dendroClassWith;
			dendroCritere = dendroCriterium;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree tree = (Tree) i.next ();	// fc - 6.10.2003 - GeneticTree -> GTree
				double dendro = getCritereDendro (tree, dendroCritere);
				maxDendro = Math.max (maxDendro, dendro);
				minDendro = Math.min (minDendro, dendro);
			}
			nbClasse = ((int) ((maxDendro/dendroPas)+1) - (int) (minDendro/dendroPas));
			minFirstClasse = (int) ((int) (minDendro/dendroPas)) * dendroPas;
		//~ }							// fc - 6.10.2003 (useless)
		//~ if (checkDendro.isSelected ()) {	// fc - 6.10.2003 (useless)
			Map temporaire = new Hashtable ();
			
			step = 20d / population.keySet ().size ();	// fc - 16.5.2006 - progress management
			
			for (Iterator i = (population.keySet ()).iterator (); i.hasNext ();) {
				String nameKey = (String) i.next ();
				Set value = (Set) population.get (nameKey);
				
				progressValue += step;
				ProgressDispatcher.setValue ((int) progressValue);
				
				for (Iterator j = value.iterator (); j.hasNext ();) {
					Tree tree = (Tree) j.next ();	// fc - 6.10.2003 - GeneticTree -> GTree
					double dendro = getCritereDendro (tree, dendroCritere);
					for (int k = 0; k<nbClasse; k++) {
						if (dendro<(minFirstClasse +(k+1)*dendroPas) && dendro>(minFirstClasse +k*dendroPas)) {
							if (! temporaire.keySet ().contains (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas))) {
								String newNameKey = nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas);
								Set temp1 = new HashSet ();
								temporaire.put (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas), temp1);
							}
							Set temp2 = (Set) temporaire.get (nameKey+"_"+dendroCritere+(k*dendroPas)+":"+((k+1)*dendroPas));
							temp2.add (tree);
						}
					}
				}
			}
			population = temporaire;	// fc - 6.10.2003 - one line: dendro class - trees in the class
		}

		ProgressDispatcher.setValue ((int) progressValue);

		// 5. If requested, build populations from genotypic parameter
		//
		//~ if (checkGene.isSelected ()) {
		if (geneFilter) {
			geneCritere = geneCriterium;
			//~ geneCritere = (String) geneList.getSelectedItem ();
			//~ if (geneCritere == null) {
				//~ Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "genetics parameters must be defined");
				//~ JOptionPane.showMessageDialog (this, Translator.swap ("GenePopExportDialog.geneticParameterDefined"),
						//~ Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				//~ return;
			//~ }
			Map temporaire = new Hashtable ();
			
			step = 20d / population.keySet ().size ();	// fc - 16.5.2006 - progress management
			
			for (Iterator i = (population.keySet ()).iterator (); i.hasNext ();) {
				String nameKey = (String) i.next ();
				Set value = (Set) population.get (nameKey);
				
				progressValue += step;
				ProgressDispatcher.setValue ((int) progressValue);
				
				for (Iterator j = value.iterator (); j.hasNext ();) {
					//~ GeneticTree tree = (GeneticTree) j.next ();	// cast ok, only GeneticTrees in population (see upper) - fc
					Genotypable tree = (Genotypable) j.next ();	// fc - 15.5.2006 // cast ok, only Genotypable GTrees in population (see upper) - fc
					//~ double gene = getCritereGene (tree, geneCritere);
					int gene = getCritereGene (tree, geneCritere);	// fc - 7.10.2003
					if (! temporaire.keySet ().contains (nameKey+"_"+geneCritere+"="+gene)) {
						String newNameKey = nameKey+"_"+geneCritere+"="+gene;
						Set temp1 = new HashSet ();
						temporaire.put (nameKey+"_"+geneCritere+"="+gene, temp1);
					}
					Set temp2 = (Set) temporaire.get (nameKey+"_"+geneCritere+"="+gene);
					temp2.add (tree);
				}
			}
			population = temporaire;
		}

		ProgressDispatcher.setValue ((int) progressValue);

		// If requested, check if every tree in population has NuclearDNA - fc
		//
		dna = new Hashtable ();
		String test = "0";
		//~ if (checkN.isSelected ()) {
		if (exportNuclearDNA) {
			test = "1";
			Set ss = (Set) population.get ((population.keySet ()).iterator ().next ());
			Genotypable tt = (Genotypable) ss.iterator ().next ();
			IndividualGenotype gg = (IndividualGenotype) tt.getGenotype ();
			if ((gg.getNuclearDNA ()).length == 0) {
				Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "no genes on mCytoplasmicDNA");
				throw new Exception (Translator.swap ("GenePopExportDialog.nogenesonNuclearDNA"));
			}
		}
		dna.put ("nuclearDNA", test);
		
		// If requested, check if every tree in population has MCytoplasmicDNA - fc
		//
		test = "0";
		//~ if (checkM.isSelected ()) {
		if (exportMCytoplasmicDNA) {
			test = "1";
			Set ss = (Set) population.get ((population.keySet ()).iterator ().next ());
			Genotypable tt = (Genotypable) ss.iterator ().next ();
			IndividualGenotype gg = (IndividualGenotype) tt.getGenotype ();
			if ((gg.getMCytoplasmicDNA ()).length == 0) {
				Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "no genes on mCytoplasmicDNA");
				throw new Exception (Translator.swap ("GenePopExportDialog.nogenesonmCytoplasmicDNA"));
			}
		}
		dna.put ("mCytoplasmicDNA", test);
		
		// If requested, check if every tree in population has PCytoplasmicDNA - fc
		//
		test = "0";
		//~ if (checkP.isSelected ()) {
		if (exportPCytoplasmicDNA) {
			test = "1";
			Set ss = (Set) population.get ((population.keySet ()).iterator ().next ());
			Genotypable tt = (Genotypable) ss.iterator ().next ();
			IndividualGenotype gg = (IndividualGenotype) tt.getGenotype ();
			if ((gg.getPCytoplasmicDNA ()).length == 0) {
				Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "no genes on mCytoplasmicDNA");
				throw new Exception (Translator.swap ("GenePopExportDialog.nogenesonpCytoplasmicDNA"));
			}
		}
		dna.put ("pCytoplasmicDNA", test);
		
		// If nothing requested, trouble - fc
		//
		//~ if (!checkN.isSelected () && !checkM.isSelected () && !checkP.isSelected ()) {
		if (!exportNuclearDNA && !exportMCytoplasmicDNA && !exportPCytoplasmicDNA) {
			Log.println (Log.ERROR, "GenePopExportDialog.okAction ()", "give genotype to export");
			throw new Exception (Translator.swap ("GenePopExportDialog.oneDNAMustBeSelected"));
		}
		//~ settings = new GenePopExportSettings (population, dna);

	}

	// Return the value for the given criterion on the tree
	private double getCritereDendro (Tree tree, String critere) {	// fc - 6.10.2003 - changed GeneticTree -> GTree
		double result;
		if (critere == Translator.swap ("age")) {			// fc - 16.5.2006 - key was "GenePopExportDialog.age"
			result = (double) tree.getAge ();
		} else if (critere == Translator.swap ("dbh")) {	// fc - 16.5.2006 - key was "GenePopExportDialog.dbh"
			result = (double) tree.getDbh ();
		} else	{											// fc - 16.5.2006 - "height"
			result = (double) tree.getHeight ();
		}
		return result;
	}

	// Return the value for the given criterion on the tree
	private int getCritereGene (Genotypable tree, String critere) {
		int result;
		if (critere == Translator.swap ("mId")) {		// fc - 16.5.2006 - key was "GenePopExportDialog.mId"
			result = tree.getMId ();
		} else if (critere == Translator.swap ("pId")) {	// fc - 16.5.2006 - key was "GenePopExportDialog.pId"
			result = tree.getPId ();
		} else {										// fc - 16.5.2006 - "creationDate"
			result = tree.getCreationDate ();		
		}
		return result;
	}

	/**	Exportation (see constructor).
	*/
	public void createRecordSet (Map population, Map dna) throws Exception {
		
		// fc - 7.10.2003 - Note: In many cases, this method can raise an exception
		// ex: no Genotypable in population, no Genotypable with IndividualGenotype in population...
		// It has been designed to throw the exception to the caller
		//
		// Used some StringBuffers instead of String concatenation (faster)
		// Used number format to ensure allele is exactly 3 digits long in export
		//
		
		// fc - 4.12.2003 -> Just 1 line in header (SOM)
		add (new CommentRecord (" Capsis "+Engine.getVersion ()+" generated file - "
					+new Date ().toString ()));
		
		// Coordinates formater : decimal with no more than 3 fraction digits
		NumberFormat nf = NumberFormat.getNumberInstance (Locale.ENGLISH);
		nf.setMaximumFractionDigits (3);
		nf.setGroupingUsed (false);
		
		// Allele formater : integer with 3 digits exactly
		NumberFormat af = NumberFormat.getNumberInstance (Locale.ENGLISH);
		af.setMaximumFractionDigits (0);
		af.setMaximumIntegerDigits (3);
		af.setMinimumIntegerDigits (3);
		af.setGroupingUsed (false);
		
		// 1. Get first Genotypable with IndividualGenotype
		//
		//Genotypable t = (Genotypable) trees.iterator ().next ();
		String n = (String) population.keySet ().iterator ().next ();
		Set s = (Set) population.get (n);
		Iterator i  = s.iterator ();
		Genotypable t = (Genotypable) s.iterator ().next ();
		Genotype g = t.getGenotype ();
		while (!(g instanceof IndividualGenotype)) {
			t = (Genotypable) i.next ();
			g = t.getGenotype ();
		}
		
		// 2. Add LociRecords for each locus name for nuclearDNA, mCytoplasmicDNA 
		// and pCytoplasmicDNA when requested
		//
		Set loci = new HashSet ();
		if ("1".equals (dna.get ("nuclearDNA"))) {	//if (dna.get ("nuclearDNA") == "1") {
			short [][] nuclearDNA = ((IndividualGenotype) g).getNuclearDNA ();
			int nbLoci = nuclearDNA.length;
			for (int k = 1; k<=nbLoci; k++) {
				LociRecord l = new LociRecord ();
				l.name = "n" + k;
				String locus = "n_"+k;
				loci.add (locus);
				add (l);
			}
		}
		if ("1".equals (dna.get ("mCytoplasmicDNA"))) {	//if (dna.get ("mCytoplasmicDNA") == "1") {
			short [] mCytoplasmicDNA = ((IndividualGenotype) g).getMCytoplasmicDNA ();
			int nbLoci = mCytoplasmicDNA.length;
			for (int k = 1; k<=nbLoci; k++) {
				LociRecord l = new LociRecord ();
				l.name = "m" + k;
				add (l);
			}
		}
		if ("1".equals (dna.get ("pCytoplasmicDNA"))) {	//if (dna.get ("pCytoplasmicDNA") == "1") {
			short [] pCytoplasmicDNA = ((IndividualGenotype) g).getPCytoplasmicDNA ();
			int nbLoci = pCytoplasmicDNA.length;
			for (int k = 1; k<=nbLoci; k++) {
				LociRecord l = new LociRecord ();
				l.name = "p" + k;
				add (l);
			}
		}
		
		// For each population
		//     Add "pop" line
		//     For each tree with IndividualGenotype
		//         Create and add a GenotypableRecord with 
		//         - population name, 
		//         - additional tree info (before ',')
		//         - genetic info as requested
		//
//~ System.out.println ("GenePopExport: report ----------------------------------------");	// fc - report - please do not remove (comment)
		for (Iterator j = population.keySet ().iterator (); j.hasNext ();) {
			LociRecord lr = new LociRecord ();
			lr.name = "pop";
			add (lr);
			String name = (String) j.next ();	// Population name
			
//~ System.out.print ("key: "+name);	// fc - report - please do not remove (comment)
			
			Set trees = (Set) population.get (name);
			for (Iterator k = trees.iterator (); k.hasNext ();) {
				t = (Genotypable) k.next ();
				
				// fc - 7.10.2003 - adding additional info before ','
				StringBuffer additionalInfo = new StringBuffer ();
				additionalInfo.append (" tree_");
				additionalInfo.append (t.getId ());
				
				// fc - november 2004
				String species = t.getGenoSpecies ().toString ();
					
				//~ String species = null;
				//~ if (t instanceof SpeciesDefined) {species = ((SpeciesDefined) t).getSpecies ().toString ();}
				//~ if (species != null) {	
				
					//~ additionalInfo.append ("(");
					//~ additionalInfo.append (species);
					//~ additionalInfo.append (")");
				
				//~ }
				
				additionalInfo.append (" MId=");
				additionalInfo.append (t.getMId ());
				additionalInfo.append (" PId=");
				additionalInfo.append (t.getPId ());
				
				if (t instanceof Spatialized) {
					Spatialized sp = (Spatialized) t;
					additionalInfo.append (" x=");
					additionalInfo.append (nf.format (sp.getX ()));
					additionalInfo.append (" y=");
					additionalInfo.append (nf.format (sp.getY ()));
				}
				
//~ String species2 = "none";	// fc - report - please do not remove (comment)
//~ if (t instanceof SpeciesDefined) {species2 = ((SpeciesDefined) t).getSpecies ().toString ();}	// fc - report - please do not remove (comment)
//~ System.out.print (" t:"+t.getId ()+"("+species2+")");	// fc - report - please do not remove (comment)
				
				g = t.getGenotype ();
				if (g instanceof IndividualGenotype) {
					GenotypableRecord r = new GenotypableRecord ();
					StringBuffer geno = new StringBuffer (name);
					geno.append (additionalInfo.toString ());	// fc - 7.10.2003 - adding additional info before ','
					geno.append (" ,");
					
					if ("1".equals (dna.get ("nuclearDNA"))) {	//if (dna.get ("nuclearDNA") == "1") {
						short [][] nuclearDNA = ((IndividualGenotype) g).getNuclearDNA ();
						geno.append (" ");
						//~ geno.append (("00"+nuclearDNA[0][0]).substring (("00"+nuclearDNA[0][0]).length()-3));
						//~ geno.append (("00"+nuclearDNA[0][1]).substring (("00"+nuclearDNA[0][1]).length()-3));
						geno.append (af.format (nuclearDNA[0][0]));
						geno.append (af.format (nuclearDNA[0][1]));
						for (int l = 1; l<nuclearDNA.length; l++) {
							geno.append (" ");
							//~ geno.append (("00"+nuclearDNA[l][0]).substring (("00"+nuclearDNA[l][0]).length()-3));
							//~ geno.append (("00"+nuclearDNA[l][1]).substring (("00"+nuclearDNA[l][1]).length()-3));
							geno.append (af.format (nuclearDNA[l][0]));
							geno.append (af.format (nuclearDNA[l][1]));
						}
					}
					if ("1".equals (dna.get ("mCytoplasmicDNA"))) {	//if (dna.get ("mCytoplasmicDNA") == "1") {
						short [] mCytoplasmicDNA = ((IndividualGenotype) g).getMCytoplasmicDNA ();
						geno.append (" ---");
						//~ geno.append (("00"+mCytoplasmicDNA[0]).substring (("00"+mCytoplasmicDNA[0]).length()-3));
						geno.append (af.format (mCytoplasmicDNA[0]));
						for (int l = 1; l<mCytoplasmicDNA.length; l++) {
							geno.append (" ---");
							//~ geno.append (("00"+mCytoplasmicDNA[l]).substring (("00"+mCytoplasmicDNA[l]).length()-3));
							geno.append (af.format (mCytoplasmicDNA[l]));
						}
					}
					if ("1".equals (dna.get ("pCytoplasmicDNA"))) {	//if (dna.get ("pCytoplasmicDNA") == "1") {
						short [] pCytoplasmicDNA = ((IndividualGenotype) g).getPCytoplasmicDNA ();
						geno.append (" ---");
						//~ geno.append (("00"+pCytoplasmicDNA[0]).substring (("00"+pCytoplasmicDNA[0]).length()-3));
						geno.append (af.format (pCytoplasmicDNA[0]));
						for (int l = 1; l<pCytoplasmicDNA.length; l++) {
							geno.append (" ---");
							//~ geno.append (("00"+pCytoplasmicDNA[l]).substring (("00"+pCytoplasmicDNA[l]).length()-3));
							geno.append (af.format (pCytoplasmicDNA[l]));
						}
					}
					r.genotype = geno.toString ();
					add (r);
				}
			}
//~ System.out.println ();	// fc - report - please do not remove (comment)
		}
	}

	/**	RecordSet -> VtxStand
	*	Implementation here.
	*/
	public GScene load (GModel model) throws Exception {
		return null;
	}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

}
