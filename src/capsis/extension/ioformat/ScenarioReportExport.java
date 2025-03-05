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
import capsis.util.StandRecordSet;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.HgProvider;
import capsis.util.methodprovider.KgProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.VProvider;

/**
 * ScenarioReportExport: exportation of stand table data.
 *
 * @author T. Labbe, C. Meredieu - May 2005
 */
public class ScenarioReportExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.ScenarioReportExport");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected MethodProvider methodProvider;
	protected NumberFormat formater;
	protected NumberFormat formater3;

	// Generic keyword record is described in superclass: key = value

	// Type of record : HTML ; to change , separator or \t separator - TL 19.05.2005

	// Export initial stand record is described here
	@Import
	static public class InitialStandRecord extends Record {
		public InitialStandRecord () {super ();}
		public InitialStandRecord (String line) throws Exception {super (line);}
		public String getSeparator () {return "</TD><TD>";}	// to change default "\t" separator


		public String date;
		public String Hdom;
		public String Hg;
		public String Nha;
		public String Cdom;
		public String Cg;
		public String Gha;
		}

	// Export harvest stand record is described here
	@Import
	static public class HarvestStandRecord extends Record {
		public HarvestStandRecord () {super ();}
		public HarvestStandRecord (String line) throws Exception {super (line);}
		public String getSeparator () {return "</TD><TD>";}	// to change default "\t" separator

		public String date;
		public String thiNha;
		public String thiGha;
		public String thiVha;
		public String thiCg;
		}

	// Export thin stand record is described here
	@Import
	static public class ThinStandRecord extends Record {
		public ThinStandRecord () {super ();}
		public ThinStandRecord (String line) throws Exception {super (line);}
		public String getSeparator () {return "</TD><TD>";}	// to change default "\t" separator

		public String date;
		public String Kg;
		public String beforeHg;
		public String beforeHdom;
		public String beforeNha;
		public String beforeCg;
		public String beforeCdom;
		public String beforeGha;
		public String currentHg;  // "current" for the stand after thinning : alphabetic order before then current; TL 10052005
		public String currentHdom;
		public String currentNha;
		public String currentCg;
		public String currentCdom;
		public String currentGha;
		}

	// Export production stand record is described here
	@Import
	static public class ProductionStandRecord extends Record {
		public ProductionStandRecord () {super ();}
		public ProductionStandRecord (String line) throws Exception {super (line);}
		public String getSeparator () {return "</TD><TD>";}	// to change default "\t" separator

		public String VProdha;
		public String MeanVProdha;
		}


	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public ScenarioReportExport () {
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

			// fc+bc-15.1.2024
			// The list of all MethodProvider interfaces used lower should be tested here
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "ScenarioReportExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("ScenarioReportExport.name");
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
		return Translator.swap ("ScenarioReportExport.description");
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

		double coefHa = 10000 / stand.getArea ();

		// Retrieve Steps from root to this step
		Vector steps = stand.getStep ().getProject ().getStepsFromRoot (stand.getStep ());

		// 0. Comment Records (instead of header of StandRecordSet.CreateRecordSet)
//		We don't use the standard header lines decribed in StandRecordSet : so we comment :
//		super.createRecordSet (stand);		// deals with RecordSet'stand source
		add (new CommentRecord ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">"));
		add (new CommentRecord ("<HTML>"));
		add (new CommentRecord ("<HEAD>"));
		add (new CommentRecord ("<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=iso-8859-1\">"));
		add (new CommentRecord ("<TITLE>"+Translator.swap ("ScenarioReportExport")+"</TITLE>"));
		add (new CommentRecord ("<META NAME=\"Generator\" CONTENT=\"Capsis 4\">"));
		add (new CommentRecord ("<META NAME=\"Author\" CONTENT=\""+getAuthor ()+"\">"));
		add (new CommentRecord ("<META NAME=\"Description\" CONTENT=\""+Translator.swap ("ScenarioReportExport.description")+"\">"));
		add (new CommentRecord ("</HEAD>"));
		add (new CommentRecord ("<BODY>"));
		add (new CommentRecord ("<H1><CENTER>Model : " +  stand.getStep ().getProject ().getModel ().getPackageName ()
		+ ", Project : " + stand.getStep ().getProject ().getName ()
		+ ", Step : " + stand.getStep ().getName ()+"</CENTER></H1>"));

		add (new EmptyRecord ());

		// 1. KeyRecords (none at the moment)

		// 2. InitialStandRecords
		add (new FreeRecord ("<H2>" + Translator.swap ("ScenarioReportExport.initialStandTable") + "</H2>"));
		add (new FreeRecord ("<TABLE BORDER=1 CELLSPACING=0>"));
		add (new FreeRecord ("<TR ALIGN=\"middle\"><TH>Date</TH>"+
		"<TH>Hdom</TH>"+
		"<TH>Hg</TH>"+
		"<TH>N/ha</TH>"+
		"<TH>Cdom</TH>"+
		"<TH>Cg</TH>"+
		"<TH>G/ha</TH></TR>"));

//		add (new EmptyRecord ());

		double initialVha = -1d;		// default = "not calculable"
		double finalVha = -1d;		// default = "not calculable"
		double cumulThiVha = 0d;
		String buffer = "";
		boolean initialStep = true;
		int initialAge = 0;
		int finalAge = 0;

		for (Iterator w = steps.iterator (); w.hasNext ();) {
			Step step = (Step) w.next ();

			boolean interventionStep = step.getScene ().isInterventionResult ();

			// Previous step
			Step prevStep = (Step) step.getFather ();

			// Consider restriction to one particular group if needed
			GScene s = step.getScene ();
			Collection trees = null;	// fc - 24.3.2004
			try {trees = ((TreeCollection) s).getTrees ();} catch (Exception e) {}	// fc - 24.3.2004

			GScene prevStand = null;
			try {prevStand = prevStep.getScene ();
					} catch (Exception e) {}
			Collection prevTrees = null;	// fc - 24.3.2004
			try {prevTrees = ((TreeCollection) prevStand).getTrees ();
					} catch (Exception e) {}	// fc - 24.3.2004

			if (initialStep) {

				InitialStandRecord r =new InitialStandRecord ();

				// Stand variables ------------------------------------------------------------------

				r.date = "<TR ALIGN=\"middle\"><TD>" + s.getDate ();

				double Hdom = -1d;		// default = "not calculable"
				try {Hdom = ((HdomProvider) methodProvider).getHdom (s, trees);} catch (Exception e) {}
				if (Hdom == -1d) r.Hdom = ""; else r.Hdom = ""+formater.format (Hdom);

				double Hg = -1d;		// default = "not calculable"
				try {Hg = ((HgProvider) methodProvider).getHg (s, trees);} catch (Exception e) {}
				if (Hg == -1d) r.Hg = ""; else r.Hg = ""+formater.format (Hg);

				double Nha = -1d;					//----- N is always computed
				try {Nha = ((NProvider) methodProvider).getN (s, trees) * coefHa;} catch (Exception e) {}
				if (Nha == -1d) r.Nha = ""; else r.Nha = ""+(int) Nha;	// N integer (phd)

				double Cdom = -1d;		// default = "not calculable"
				try {Cdom = Math.PI * ((DdomProvider) methodProvider).getDdom (s, trees);} catch (Exception e) {}
				if (Cdom == -1d) r.Cdom = ""; else r.Cdom = ""+formater.format (Cdom);

				double Cg = -1d;		// default = "not calculable"
				try {Cg = Math.PI * ((DgProvider) methodProvider).getDg (s, trees);} catch (Exception e) {}
				if (Cg == -1d) r.Cg = ""; else r.Cg = ""+formater.format (Cg);

				double Gha = -1d;					//----- G is always computed
				try {Gha = ((GProvider) methodProvider).getG (s, trees) * coefHa;} catch (Exception e) {}
				if (Gha == -1d) r.Gha = "</TD></TR>"; else r.Gha = ""+formater.format (Gha)+ "</TD></TR>";

				add (r);

				initialAge = s.getDate ();
				try {initialVha = ((VProvider) methodProvider).getV (s, trees) * coefHa;} catch (Exception e) {}

				initialStep = false;
				add (new FreeRecord ("</TABLE>"));


				//add (new EmptyRecord ());


				// 3. HarvestStandRecords
				add (new FreeRecord ("<H2>" + Translator.swap ("ScenarioReportExport.harvestTable") + "</H2>"));
				add (new FreeRecord ("<TABLE BORDER=1 CELLSPACING=0>"));
				add (new FreeRecord ("<TR ALIGN=\"middle\"><TH>Date</TH>"+
				"<TH>N/ha</TH>"+
				"<TH>G/ha</TH>"+
				"<TH>V/ha</TH>"+
				"<TH>Cg</TH></TR>"));

			}

			if (interventionStep) {
				HarvestStandRecord r =new HarvestStandRecord ();
				r.date = "<TR ALIGN=\"middle\"><TD>" + s.getDate ();

				// Thinning variables ------------------------------------------------------------------

				double Nha = -1d;					//----- N is always computed
				try {Nha = ((NProvider) methodProvider).getN (s, trees) * coefHa;} catch (Exception e) {}
				double thiNha = -1d;				//----- thiN is always computed
				double prevNha = -1d;
				try {prevNha = ((NProvider) methodProvider).getN (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Nha != -1 && prevNha != -1) {thiNha = prevNha - Nha;}
				buffer = ""+ (int) (thiNha);
				if (thiNha == -1d) r.thiNha = ""; else r.thiNha = buffer;

				double Gha = -1d;					//----- G is always computed
				try {Gha = ((GProvider) methodProvider).getG (s, trees) * coefHa;} catch (Exception e) {}
				double thiGha = -1d;				//----- thiG is always computed
				double prevGha = -1d;
				try {prevGha = ((GProvider) methodProvider).getG (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Gha != -1 && prevGha != -1) {thiGha = prevGha - Gha;}
				buffer = ""+formater.format (thiGha);
				if (thiGha == -1d) r.thiGha = ""; else r.thiGha = buffer;

				double Vha = -1d;				//----- V default = "not calculable"
				try {Vha = ((VProvider) methodProvider).getV (s, trees) * coefHa;} catch (Exception e) {}
				double thiVha = -1d;				//----- thiV default = "not calculable"
				double prevVha = -1d;
				try {prevVha = ((VProvider) methodProvider).getV (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
				if (Vha != -1 && prevVha != -1) {thiVha = prevVha - Vha;}
				if (thiVha != -1) {cumulThiVha += thiVha;}
				buffer = ""+formater.format (thiVha);
				if (thiVha == -1d) r.thiVha = ""; else r.thiVha = buffer;

				//----- thiCg
				double thiCg = -1d;
				if (thiGha != -1 && thiNha != -1) {thiCg = Math.PI * (Math.sqrt((thiGha / thiNha) / Math.PI) * 2) * 100;}
				buffer = ""+formater.format (thiCg)+"</TD></TR>";
				if (thiCg == -1d) r.thiCg = "</TD></TR>"; else r.thiCg = buffer;

				add (r);

			}

			if (!w.hasNext ()) {

				finalAge = s.getDate ();

				HarvestStandRecord r =new HarvestStandRecord ();

				r.date = "<TR ALIGN=\"middle\"><TD>" + s.getDate ();

				// Final variables ------------------------------------------------------------------

				double Nha = -1d;					//----- N is always computed
				try {Nha = ((NProvider) methodProvider).getN (s, trees) * coefHa;} catch (Exception e) {}
				buffer = ""+ (int) (Nha);
				if (Nha == -1d) r.thiNha = ""; else r.thiNha = buffer;

				double Gha = -1d;					//----- G is always computed
				try {Gha = ((GProvider) methodProvider).getG (s, trees) * coefHa;} catch (Exception e) {}
				buffer = ""+formater.format (Gha);
				if (Gha == -1d) r.thiGha = ""; else r.thiGha = buffer;

				try {finalVha = ((VProvider) methodProvider).getV (s, trees) * coefHa;} catch (Exception e) {}
				buffer = ""+formater.format (finalVha);
				if (finalVha == -1d) r.thiVha = ""; else r.thiVha = buffer;

				double Cg = -1d;		// default = "not calculable"
				try {Cg = Math.PI * ((DgProvider) methodProvider).getDg (s, trees);} catch (Exception e) {}
				buffer = ""+formater.format (Cg)+"</TD></TR>";
				if (Cg == -1d) r.thiCg = "</TD></TR>"; else r.thiCg = buffer;

				add (r);
			}

		}
		add (new FreeRecord ("</TABLE>"));
		//add (new EmptyRecord ());

		// 4. ThinStandRecords

		add (new FreeRecord ("<H2>" + Translator.swap ("ScenarioReportExport.thinningTable") + "</H2>"));
		add (new FreeRecord ("<TABLE BORDER=1 CELLSPACING=0>"));
		add (new FreeRecord ("<TR ALIGN=\"middle\"><TH>Date</TH>"+
		"<TH>Kg</TH>"+
		"<TH>Hg " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>Hdom " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>N/ha " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>Cg " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>Cdom " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>G/ha " + Translator.swap ("ScenarioReportExport.before")+ "</TH>"+
		"<TH>Hg " + Translator.swap ("ScenarioReportExport.after")+ "</TH>"+
		"<TH>Hdom " + Translator.swap ("ScenarioReportExport.after")+ "</TH>"+
		"<TH>N/ha " + Translator.swap ("ScenarioReportExport.after")+ "</TH>"+
		"<TH>Cg " + Translator.swap ("ScenarioReportExport.after")+ "</TH>"+
		"<TH>Cdom " + Translator.swap ("ScenarioReportExport.after")+ "</TH>"+
		"<TH>G/ha " + Translator.swap ("ScenarioReportExport.after")+ "</TH></TR>"));


		for (Iterator w = steps.iterator (); w.hasNext ();) {
			Step step = (Step) w.next ();

			boolean interventionStep = step.getScene ().isInterventionResult ();

			// Previous step
			Step prevStep = (Step) step.getFather ();

			// Consider restriction to one particular group if needed
			GScene s = step.getScene ();
			Collection trees = null;	// fc - 24.3.2004
			try {trees = ((TreeCollection) s).getTrees ();} catch (Exception e) {}	// fc - 24.3.2004

			GScene prevStand = null;
			try {prevStand = prevStep.getScene ();
					} catch (Exception e) {}
			Collection prevTrees = null;	// fc - 24.3.2004
			try {prevTrees = ((TreeCollection) prevStand).getTrees ();
					} catch (Exception e) {}	// fc - 24.3.2004

			if (!interventionStep) {continue;} // next iteration

			ThinStandRecord r =new ThinStandRecord ();
			r.date = "<TR ALIGN=\"middle\"><TD>" + s.getDate ();

			double Kg = -1d;		// default = "not calculable"
			try {Kg = ((KgProvider) methodProvider).getKg (s, trees);} catch (Exception e) {}
			buffer = ""+formater.format (Kg);
			if (Kg == -1d) r.Kg = ""; else r.Kg = buffer;

			double beforeHg = -1d;		// default = "not calculable"
			try {beforeHg = ((HgProvider) methodProvider).getHg (prevStand, prevTrees);} catch (Exception e) {}
			buffer = ""+formater.format (beforeHg);
			if (beforeHg == -1d) r.beforeHg = ""; else r.beforeHg = buffer;

			double beforeHdom = -1d;		// default = "not calculable"
			try {beforeHdom = ((HdomProvider) methodProvider).getHdom (prevStand, prevTrees);} catch (Exception e) {}
			buffer = ""+formater.format (beforeHdom);
			if (beforeHdom == -1d) r.beforeHdom = ""; else r.beforeHdom = buffer;

			double beforeNha = -1d;				//----- before_N is always computed
			try {beforeNha = ((NProvider) methodProvider).getN (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
			buffer = ""+ (int) (beforeNha);
			if (beforeNha == -1d) r.beforeNha = ""; else r.beforeNha = buffer;

			double beforeCg = -1d;		// default = "not calculable"
			try {beforeCg = Math.PI * ((DgProvider) methodProvider).getDg (prevStand, prevTrees);} catch (Exception e) {}
			buffer = ""+formater.format (beforeCg);
			if (beforeCg == -1d) r.beforeCg = ""; else r.beforeCg = buffer;

			double beforeCdom = -1d;		// default = "not calculable"
			try {beforeCdom = Math.PI * ((DdomProvider) methodProvider).getDdom (prevStand, prevTrees);} catch (Exception e) {}
			buffer = ""+formater.format (beforeCdom);
			if (beforeCdom == -1d) r.beforeCdom = ""; else r.beforeCdom = buffer;

			double beforeGha = -1d;				//----- before_G is always computed
			try {beforeGha = ((GProvider) methodProvider).getG (prevStand, prevTrees) * coefHa;} catch (Exception e) {}
			buffer = ""+formater.format (beforeGha);
			if (beforeGha == -1d) r.beforeGha = ""; else r.beforeGha = buffer;

			double currentHg = -1d;		// default = "not calculable"
			try {currentHg = ((HgProvider) methodProvider).getHg (s, trees);} catch (Exception e) {}
			buffer = ""+formater.format (currentHg);
			if (currentHg == -1d) r.currentHg = ""; else r.currentHg = buffer;

			double currentHdom = -1d;		// default = "not calculable"
			try {currentHdom = ((HdomProvider) methodProvider).getHdom (s, trees);} catch (Exception e) {}
			buffer = ""+formater.format (currentHdom);
			if (currentHdom == -1d) r.currentHdom = ""; else r.currentHdom = buffer;

			double currentNha = -1d;				//----- after_N is always computed
			try {currentNha = ((NProvider) methodProvider).getN (s, trees) * coefHa;} catch (Exception e) {}
			buffer = ""+ (int) (currentNha);
			if (currentNha == -1d) r.currentNha = ""; else r.currentNha = buffer;

			double currentCg = -1d;		// default = "not calculable"
			try {currentCg = Math.PI * ((DgProvider) methodProvider).getDg (s, trees);} catch (Exception e) {}
			buffer = ""+formater.format (currentCg);
			if (currentCg == -1d) r.currentCg = ""; else r.currentCg = buffer;

			double currentCdom = -1d;		// default = "not calculable"
			try {currentCdom = Math.PI * ((DdomProvider) methodProvider).getDdom (s, trees);} catch (Exception e) {}
			buffer = ""+formater.format (currentCdom);
			if (currentCdom == -1d) r.currentCdom = ""; else r.currentCdom = buffer;

			double currentGha = -1d;				//----- after_G is always computed
			try {currentGha = ((GProvider) methodProvider).getG (s, trees) * coefHa;} catch (Exception e) {}
			buffer = ""+formater.format (currentGha)+"</TD></TR>";
			if (currentGha == -1d) r.currentGha = "</TD></TR>"; else r.currentGha = buffer;

			add (r);

		}
		add (new FreeRecord ("</TABLE>"));
		//add (new EmptyRecord ());

		// 5. ProductionStandRecords

		add (new FreeRecord ("<H2>" + Translator.swap ("ScenarioReportExport.productionTableFrom")+ " "  + initialAge+ " "
			+ Translator.swap ("ScenarioReportExport.productionTableTo")+ " " + finalAge + " "
			+ Translator.swap ("ScenarioReportExport.productionTableYears") + "</H2>"));

		add (new FreeRecord ("<TABLE BORDER=1 CELLSPACING=0>"));
		add (new FreeRecord ("<TR ALIGN=\"middle\"><TH>ProdV/ha</TH>"+
		"<TH>ProdV/ha/year</TH></TR>"));

		ProductionStandRecord r =new ProductionStandRecord ();

		double prodVha = -1d;		// default = "not calculable"
		if (finalVha != -1 && cumulThiVha != -1 && initialVha != -1) {prodVha = finalVha + cumulThiVha - initialVha;}
		buffer = "<TR ALIGN=\"middle\"><TD>"+formater.format (prodVha);
		if (prodVha == -1d) r.VProdha = "<TR ALIGN=\"middle\"><TD>"; else r.VProdha = buffer;

		double meanProdVha = -1d;		// default = "not calculable"
		if (prodVha != -1) {
			if (finalAge != initialAge) {meanProdVha = prodVha / (finalAge - initialAge);} else {meanProdVha = 0d;}
		}
		buffer = ""+formater.format (meanProdVha)+"</TD></TR>";
		if (meanProdVha == -1d) r.MeanVProdha = "</TD></TR>"; else r.MeanVProdha = buffer;

		add (r);
		add (new FreeRecord ("</TABLE>"));
		add (new CommentRecord ("</BODY>"));
		add (new CommentRecord ("</HTML>"));

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
