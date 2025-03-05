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

import java.text.NumberFormat;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.lib.genetics.GeneticTree;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import capsis.util.StandRecordSet;


/**	
 * Export to Spagedi software. Compatible with Genetics modules.
 * 
 * @author F. de Coligny - December 2003
 */
public class LubSpagediExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.LubSpagediExport");
	}

	private static final int NUCLEAR = 1;
	private static final int CYTO_M = 2;
	private static final int CYTO_P = 3;

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public LubSpagediExport () {
		source = "";
		commentMark = "//";
		setHeaderEnabled (false);
	}


	/**	Official constructor. 
	*	Format in Export mode needs a Stand in starter (then call save (fileName)). 
	*	Format in Import mode needs fileName in starter (then call load (GModel)). 
	*/
	public void initExport(GModel m, Step s) throws Exception {
		
		if (s.getScene () != null) {	// stand to be exported
			// Export mode
			//
			// retrieveSettings ();
			GScene stand = s.getScene ();
			
			try {
				LubSpagediExportDialog dlg = new LubSpagediExportDialog (stand);
				if (!dlg.isValidDialog ()) {	// user canceled dialog -> stop
					dlg.dispose ();
					return;
				}
				
				Collection selectedTrees = dlg.getSelectedTrees ();
				int dnaCode = 0;
				if (dlg.isNuclearSelected ()) {
					dnaCode = NUCLEAR;
				} else if (dlg.isCytoMSelected ()) {
					dnaCode = CYTO_M;
				} else if (dlg.isCytoPSelected ()) {
					dnaCode = CYTO_P;
				} 
				createRecordSet (selectedTrees, dnaCode);
				dlg.dispose ();
				
			} catch (Exception e) {
				Log.println (Log.ERROR, "LubSpagediExport.initExport ()", "An error occured during export", e);
				MessageDialog.print (this, Translator.swap ("LubSpagediExport.errorDuringExport"), e);
			}
			
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
			if (!(s instanceof TreeList)) {return false;}
			// fc - 6.10.2003 - further tests will inform user in config 
			// dialog if no Genetic trees are found
			
			// fc-7.9.2015 restricting compatibility as expected
			TreeList stand = (TreeList) s;
			for (Iterator i = stand.getTrees ().iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				if (t instanceof GeneticTree) {
					Genotype g = ((GeneticTree) t) .getGenotype ();
					// Found at least one IndividualGenotype -> ok
					if (g != null && g instanceof IndividualGenotype) {return true;}
				}
			}
			return false;
			// fc-7.9.2015 restricting compatibility as expected

		} catch (Exception e) {
			Log.println (Log.ERROR, "LubSpagediExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
//		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("LubSpagediExport.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("LubSpagediExport.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	/**	Export here.
	*/
	public void createRecordSet (Collection selectedTrees, int dnaCode) throws Exception {
		
		// fc - 7.10.2003 - Note: In many cases, this method can raise an exception
		// ex: no GeneticTree in population, no GeneticTree with IndividualGenotype in population...
		// It has been designed to throw the exception to the caller
		//
		// Used some StringBuffers instead of String concatenation (faster)
		// Used number format to ensure allele is exactly 3 digits long in export
		//
		
		GeneticTree firstTree = (GeneticTree) selectedTrees.iterator ().next ();
		IndividualGenotype g = (IndividualGenotype) firstTree.getGenotype ();
		
		// We will consider only one of the three following variables
		//
		short[][] nuclearDNA = g.getNuclearDNA ();
		short[] mCytoplasmicDNA = g.getMCytoplasmicDNA ();
		short[] pCytoplasmicDNA = g.getPCytoplasmicDNA ();
			
		// Header
		//
		int ploidie;
		int nbLocus;
		int nbDigits;		
		if (dnaCode == NUCLEAR) {
			ploidie = 2;
			nbLocus = nuclearDNA.length;
			nbDigits = (""+nuclearDNA[0][0]).length ();
		} else {
			ploidie = 1;
			if (dnaCode == CYTO_M) {
				nbLocus = mCytoplasmicDNA.length;
				nbDigits = (""+mCytoplasmicDNA[0]).length ();
			} else {
				nbLocus = pCytoplasmicDNA.length;
				nbDigits = (""+pCytoplasmicDNA[0]).length ();
			}
		}
		
		add (new CommentRecord (" (lines beginning by // are comment lines)"));
		add (new CommentRecord (" #individus	#categories	#coordonnees	#locus	#decimales_locusloc ploidie"));
		StringBuffer b = new StringBuffer ();
		b.append (selectedTrees.size ());
		b.append ("\t");
		// SO 31-05-05 : 3 cohort in Luberon
		b.append ("3");
		b.append ("\t");
		b.append ("2");
		b.append ("\t");
		b.append (nbLocus);
		b.append ("\t");
		b.append (nbDigits);	// ex: 172 : l=3
		b.append ("\t");
		b.append (ploidie);	
		
		add (new FreeRecord (b.toString ()));
		add (new FreeRecord ("-0"));
		
		StringBuffer b2 = new StringBuffer ();
		b2.append ("Ind\t");
		b2.append ("Cohort\t");
		b2.append ("Lat\t");
		b2.append ("Long\t");
		
		if (dnaCode == NUCLEAR) {
			for (int i = 0; i < nuclearDNA.length; i++) {
				b2.append ("n_");	// nucleus, position 0: locus n_1
				b2.append ((i+1));
				b2.append ("\t");
			}
		} else if (dnaCode == CYTO_M) {
			for (int i = 0; i < mCytoplasmicDNA.length; i++) {
				b2.append ("m_");	// mCyto, position 0: locus m_1
				b2.append ((i+1));
				b2.append ("\t");
			}
		} else {
			for (int i = 0; i < pCytoplasmicDNA.length; i++) {
				b2.append ("p_");	// pCyto, position 0: locus p_1
				b2.append ((i+1));
				b2.append ("\t");
			}
		}
		add (new FreeRecord (b2.toString ()));
		
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
		
		for (Iterator k = selectedTrees.iterator (); k.hasNext ();) {
			GeneticTree t = (GeneticTree) k.next ();
			g = (IndividualGenotype) t.getGenotype ();
			
			StringBuffer b3 = new StringBuffer ();
			b3.append (t.getId ());
			b3.append ("\t");
			
			//S0 31-05-05: groupe = cohorte
			if (t.getCreationDate () == -1) {b3.append ("G0\t");}
			else if ( (t.getMId ()<= 2451) && (t.getPId ()<= 2451) ) {b3.append ("G1\t");}
			else if ( (t.getMId ()> 2451) || (t.getPId ()> 2451) ) {b3.append ("G2\t");}
			
			b3.append (nf.format (t.getX ()));
			b3.append ("\t");
			b3.append (nf.format (t.getY ()));
			b3.append ("\t");
			
			if (dnaCode == NUCLEAR) {
				
				nuclearDNA = g.getNuclearDNA ();	// fc - 20.1.2004
				for (int i = 0; i < nuclearDNA.length-1; i++) {  // TO TAKE ALL LOCI BUT THE LAST
					//~ for (int j = 0; j < 2; j++) {
						//~ short allele = (short) Math.max (0, nuclearDNA[i][j]);
						//~ b3.append (af.format (allele));
					//~ }
					short allele1 = (short) Math.max (0, nuclearDNA[i][0]);
					short allele2 = (short) Math.max (0, nuclearDNA[i][1]);
					if (allele1 == 0) {
						b3.append (af.format (allele2));	// fc (S.Gerber) - 23.2.2004 - unknown allele -> 000 and never in first position
						b3.append (af.format (allele1));
					} else {
						b3.append (af.format (allele1));
						b3.append (af.format (allele2));
					}
					
					b3.append ("\t");
				}
				
			} else if (dnaCode == CYTO_M) {
				
				mCytoplasmicDNA = g.getMCytoplasmicDNA ();	// fc - 20.1.2004
				for (int i = 0; i < mCytoplasmicDNA.length; i++) {
					short allele = (short) Math.max (0, mCytoplasmicDNA[i]);
					b3.append (af.format (allele));
					b3.append ("\t");
				}
				
			} else {
				
				pCytoplasmicDNA = g.getPCytoplasmicDNA ();	// fc - 20.1.2004
				for (int i = 0; i < pCytoplasmicDNA.length; i++) {
					short allele = (short) Math.max (0, pCytoplasmicDNA[i]);
					b3.append (af.format (allele));
					b3.append ("\t");
				}
				
			}
			
			add (new FreeRecord (b3.toString ()));
		}
		
		add (new FreeRecord ("END"));
		
	}

	/**	Import here.
	*/
	public GScene load (GModel model) throws Exception {return null;}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

}
