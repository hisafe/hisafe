/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import capsis.defaulttype.TreeCollection;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.Flag;
import capsis.util.StandRecordSet;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.HgProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.ProdGProvider;
import capsis.util.methodprovider.ProdVProvider;
import capsis.util.methodprovider.SHBProvider;
import capsis.util.methodprovider.TreeVProvider;
import capsis.util.methodprovider.VProvider;

/**
 * StandTableExport : exportation of stand table data.
 *
 * @author T. Labb�, C. Meredieu september 2003
 */
public class StandTableExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.StandTableExport");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected MethodProvider methodProvider;
	protected NumberFormat formater;
	protected NumberFormat formater3;

	// Generic keyword record is described in superclass: key = value

	// Export stand record is described here
	@Import
	static public class StandRecord extends Record {
		public StandRecord () {super ();}
		public StandRecord (String line) throws Exception {super (line);}
		public String getSeparator () {return ",";}	// to change default "\t" separator

		public int date;
		public int Nha;
		public String Gha;
		public String Vha;
		public int N;
		public String G;
		public String V;
		public String S;
		public String Hdom;
		public String Ddom;
		public String Hg;
		public String Dg;
		public String Vg;
		public String thiNha;
		public String thiGha;
		public String thiVha;
		public String thiN;
		public String thiG;
		public String thiV;
		public String thiVm;
		public String thiDg;
		public String proVhaEcl;
		public String proVha;
		public String proGha;
		public String proVecl;
		public String proV;
		public String proG;

		}

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public StandTableExport () {
		setHeaderEnabled (false);
		commentMark = "";

		formater = NumberFormat.getInstance (Locale.US); // to impose decimal dot instead of "," for french number format
		formater.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
		formater.setMinimumFractionDigits (MAX_FRACTION_DIGITS);
		formater.setGroupingUsed (false);

		formater3 = NumberFormat.getInstance (Locale.US); // to impose decimal dot instead of "," for french number format
		formater3.setMaximumFractionDigits (3);
		formater3.setMinimumFractionDigits (3);
		formater3.setGroupingUsed (false);
	}

	public StandTableExport(GScene lastStand) throws Exception {
		this();
		prepareExport(lastStand);
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider ();
			if (!(mp instanceof NProvider)) {return false;}
			if (!(mp instanceof GProvider)) {return false;}		// to be refined

		} catch (Exception e) {
			Log.println (Log.ERROR, "StandTableExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("StandTableExport.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "T. Labbé, C. Meredieu";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("StandTableExport.description");
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

	/**
	 * Export: Stand -> RecordSet - Implementation here.
	 * (RecordSet -> File in superclass)
	 */
	@Override
	public void prepareExport (GScene stand) throws Exception {
//		super.createRecordSet (stand);		//  See modification below
		methodProvider = stand.getStep ().getProject ().getModel ().getMethodProvider ();
		Flag flag = new Flag ();	// for method providers return codes - fc - 29.9.2005
		
		double coefHa = 10000 / stand.getArea ();

		// Retrieve Steps from root to this step
		Vector steps = stand.getStep ().getProject ().getStepsFromRoot (stand.getStep ());

		// 0. Comment Records (instead of header of StandRecordSet.CreateRecordSet)
//		We don't use the standard header lines decribed in StandRecordSet : so we comment :
//		super.createRecordSet (stand);		// deals with RecordSet'stand source
		add (new CommentRecord ("Model : " +  stand.getStep ().getProject ().getModel ().getPackageName ()
		+ new StandRecord ().getSeparator ()
		+ "Project : " + stand.getStep ().getProject ().getName ()
		+ new StandRecord ().getSeparator ()
		+ "Step : " + stand.getStep ().getName ()));
		add (new EmptyRecord ());

		// 1. KeyRecords (none at the moment)

		// 2. StandRecords
		add (new FreeRecord ("Date" + new StandRecord ().getSeparator ()
		+ "N/ha" + new StandRecord ().getSeparator ()
		+ "G/ha" + new StandRecord ().getSeparator ()
		+ "V/ha" + new StandRecord ().getSeparator ()
		+ "N" + new StandRecord ().getSeparator ()
		+ "G" + new StandRecord ().getSeparator ()
		+ "V" + new StandRecord ().getSeparator ()
		+ "S" + new StandRecord ().getSeparator ()
		+ "Hdom" + new StandRecord ().getSeparator ()
		+ "Ddom" + new StandRecord ().getSeparator ()
		+ "Hg" + new StandRecord ().getSeparator ()
		+ "Dg" + new StandRecord ().getSeparator ()
		+ "Vg" + new StandRecord ().getSeparator ()
		+ "ThiN/ha" + new StandRecord ().getSeparator ()
		+ "ThiG/ha" + new StandRecord ().getSeparator ()
		+ "ThiV/ha" + new StandRecord ().getSeparator ()
		+ "ThiN" + new StandRecord ().getSeparator ()
		+ "ThiG" + new StandRecord ().getSeparator ()
		+ "ThiV" + new StandRecord ().getSeparator ()
		+ "ThiVm" + new StandRecord ().getSeparator ()
		+ "ThiDg" + new StandRecord ().getSeparator ()
		+ "VEcl/ha" + new StandRecord ().getSeparator ()
		+ "VPrd/ha" + new StandRecord ().getSeparator ()
		+ "GPrd/ha" + new StandRecord ().getSeparator ()
		+ "VEcl" + new StandRecord ().getSeparator ()
		+ "VProd" + new StandRecord ().getSeparator () + "GProd"));

//		add (new EmptyRecord ());

		double cumulThiVha = 0d;
		double cumulThiGha = 0d;
		double cumulThiV = 0d;
		double cumulThiG = 0d;
		String buffer = "";

		for (Iterator w = steps.iterator (); w.hasNext ();) {
			Step step = (Step) w.next ();

			boolean interventionStep = step.getScene ().isInterventionResult ();

			// Previous step
			Step prevStep = (Step) step.getFather ();

			// Consider restriction to one particular group if needed
			GScene s = step.getScene ();
			Collection trees = null;	// fc - 24.3.2004
			
			// fc - 29.9.2005 - if no groups, trees must be null when passed to method providers (convention)
			try {trees = ((TreeCollection) s).getTrees ();} catch (Exception e) {}
			// fc - 29.9.2005 - previous line should be commented, correct method providers before
			// fc - 29.9.2005 - they should accept "trees" = null and work on "stand"

			GScene prevStand = null;
			try {prevStand = prevStep.getScene ();
					} catch (Exception e) {}
			Collection prevTrees = null;	// fc - 24.3.2004
			// fc - 29.9.2005 - if no groups, trees must be null when passed to method providers (convention)
			try {prevTrees = ((TreeCollection) prevStand).getTrees ();} catch (Exception e) {}	// fc - 24.3.2004
			// fc - 29.9.2005 - previous line should be commented, correct method providers before
			// fc - 29.9.2005 - they should accept "trees" = null and work on "stand"
				
			StandRecord r = new StandRecord ();

			// Stand variables ------------------------------------------------------------------

			r.date = s.getDate ();

			double Nha = -1d;					//----- N is always computed
			try {Nha = ((NProvider) methodProvider).getN (s, trees) * coefHa;} catch (Exception e) {}
			r.Nha = (int) Nha;	// N integer (phd)

			double Gha = -1d;					//----- G is always computed
			try {Gha = ((GProvider) methodProvider).getG (s, trees) * coefHa;} catch (Exception e) {}	// used for proG
			r.Gha = ""+formater.format (Gha);

			double Vha = -1d;					//-----  Vha default = "not calculable"
			try {Vha = ((VProvider) methodProvider).getV (s, trees) * coefHa;} catch (Exception e) {}	// used for proV
			r.Vha = ""+formater.format (Vha);

			double N = -1d;					//----- N is always computed
			try {N = ((NProvider) methodProvider).getN (s, trees);} catch (Exception e) {}
			r.N = (int) N;	// N integer (phd)

			double G = -1d;					//----- G is always computed
			try {G = ((GProvider) methodProvider).getG (s, trees);} catch (Exception e) {}	// used for proG
			r.G = ""+formater.format (G);

			double V = -1d;					//----- V default = "not calculable"
			try {V = ((VProvider) methodProvider).getV (s, trees);} catch (Exception e) {}	// used for proV
			r.V = ""+formater.format (V);

			double S = -1d;		// default = "not calculable"
			try {S = ((SHBProvider) methodProvider).getSHB (s, trees) ;} catch (Exception e) {}
			r.S = ""+formater.format (S);

			double Hdom = -1d;		// default = "not calculable"
			try {Hdom = ((HdomProvider) methodProvider).getHdom (s, trees);} catch (Exception e) {}
			r.Hdom = ""+formater.format (Hdom);

			double Ddom = -1d;		// default = "not calculable"
			try {Ddom = ((DdomProvider) methodProvider).getDdom (s, trees);} catch (Exception e) {}
			r.Ddom = ""+formater.format (Ddom);

			double Hg = -1d;		// default = "not calculable"
			try {Hg = ((HgProvider) methodProvider).getHg (s, trees);} catch (Exception e) {}
			r.Hg = ""+formater.format (Hg);

			double Dg = -1d;		// default = "not calculable"
			try {Dg = ((DgProvider) methodProvider).getDg (s, trees);} catch (Exception e) {}
			r.Dg = ""+formater.format (Dg);

			double Vg = -1d;		// default = "not calculable"
			try {Vg = ((TreeVProvider) methodProvider).getTreeV (((DgProvider) methodProvider).getDg (s, trees), ((HgProvider) methodProvider).getHg (s, trees), s);} catch (Exception e) {}
//			if (V != -1 && N != -1) {Vg = V / N;}
			r.Vg = ""+formater3.format (Vg);

			// Thinning variables ------------------------------------------------------------------

			double thiNha = -1;				//----- thiN is always computed
			if (interventionStep) {
				double prevNha = -1d;
				try {prevNha = ((NProvider) methodProvider).getN (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Nha != -1 && prevNha != -1) {thiNha = prevNha - Nha;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+ (int) (thiNha);
			}
			r.thiNha = buffer;

			double thiGha = -1;				//----- thiG is always computed
			if (interventionStep) {
				double prevGha = -1d;
				try {prevGha = ((GProvider) methodProvider).getG (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Gha != -1 && prevGha != -1) {thiGha = prevGha - Gha;}
				if (thiGha != -1) {cumulThiGha += thiGha;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (thiGha);
			}
			r.thiGha = buffer;

			double thiVha = -1;				//----- thiV default = "not calculable"
			if (interventionStep) {
				double prevVha = -1d;
				try {prevVha = ((VProvider) methodProvider).getV (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Vha != -1 && prevVha != -1) {thiVha = prevVha - Vha;}
				if (thiVha != -1) {cumulThiVha += thiVha;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (thiVha);
			}
			r.thiVha = buffer;

			double thiN = -1;				//----- thiN is always computed
			if (interventionStep) {
				double prevN = -1d;
				try {prevN = ((NProvider) methodProvider).getN (prevStand, prevTrees);} catch (Exception e) {}
				if (N != -1 && prevN != -1) {thiN = prevN - N;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+ (int) (thiN);
			}
			r.thiN = buffer;

			double thiG = -1;				//----- thiG is always computed
			if (interventionStep) {
				double prevG = -1d;
				try {prevG = ((GProvider) methodProvider).getG (prevStand, prevTrees);} catch (Exception e) {}
				if (G != -1 && prevG != -1) {thiG = prevG - G;}
				if (thiG != -1) {cumulThiG += thiG;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (thiG);
			}
			r.thiG = buffer;

			double thiV = -1;				//----- thiV default = "not calculable"
			if (interventionStep) {
				double prevV = -1d;
				try {prevV = ((VProvider) methodProvider).getV (prevStand, prevTrees);} catch (Exception e) {}
				if (V != -1 && prevV != -1) {thiV = prevV - V;}
				if (thiV != -1) {cumulThiV += thiV;}
			}
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (thiV);
			}
			r.thiV = buffer;

			//----- thiVm
			buffer = "";
			if (interventionStep) {
				double thiVm = -1;
				if (thiV != -1 && thiN != -1) {thiVm = thiV / thiN;}
				buffer = ""+formater3.format (thiVm);
			}
			r.thiVm = buffer;

			//----- thiDg
			buffer = "";
			if (interventionStep) {
				double thiDg = -1;
				if (thiG != -1 && thiN != -1) {thiDg = (Math.sqrt((thiG / thiN) / Math.PI) * 2) * 100;}
				buffer = ""+formater.format (thiDg);
			}
			r.thiDg = buffer;

			// Production variables ------------------------------------------------------------------

			double proVhaEcl = cumulThiVha;
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (proVhaEcl);
			}
			r.proVhaEcl = buffer;

			buffer = "";
			if (!interventionStep) {
				double proVha = -1d;
				// if (Vha != -1) {proVha = Vha + cumulThiVha;}  //supp sp 12/01/04
				try {proVha = ((ProdVProvider) methodProvider).getProdV (s, trees, flag) * coefHa;} catch (Exception e) {} // add sp 12/01/04
				buffer = ""+formater.format (proVha);
			}
			r.proVha = buffer;

			buffer = "";
			if (!interventionStep) {
				double proGha = -1d;
				// if (Gha != -1) {proGha = Gha + cumulThiGha;}   //supp sp 12/01/04
				try {proGha = ((ProdGProvider) methodProvider).getProdG (s, trees) * coefHa;} catch (Exception e) {} // add sp 12/01/04
				buffer = ""+formater.format (proGha);
			}
			r.proGha = buffer;

			double proVecl = cumulThiV;
			buffer = "";
			if (interventionStep) {
				buffer = ""+formater.format (proVecl);
			}
			r.proVecl = buffer;

			buffer = "";
			if (!interventionStep) {
				double proV = -1d;
				// if (V != -1) {proV = V + cumulThiV;}  //supp sp 12/01/04
				try {proV = ((ProdVProvider) methodProvider).getProdV (s, trees, flag);} catch (Exception e) {} // add sp 12/01/04
				buffer = ""+formater.format (proV);
			}
			r.proV = buffer;

			buffer = "";
			if (!interventionStep) {
				double proG = -1d;
				// if (G != -1) {proG = G + cumulThiG;}   //supp sp 12/01/04
				try {proG = ((ProdGProvider) methodProvider).getProdG (s, trees);} catch (Exception e) {} // add sp 12/01/04
				buffer = ""+formater.format (proG);
			}
			r.proG = buffer;

			add (r);
		}

	}

	/**
	 * Import: RecordSet -> Stand - Implementation here.
	 * (File -> RecordSet in superclass).
	 */
	public GScene load (GModel model) throws Exception {
		return null;
	}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

}
