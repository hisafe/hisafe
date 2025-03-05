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

package capsis.extension.dataextractor;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CoverProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GDiameterClassProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.VProvider;

/**
 * A stand table report.
 * 
 * @author B.Courbaud - April 2004
 */
public class DEStructureType extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEStructureType");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;

	protected NumberFormat f;

	/**
	 * Constructor.
	 */
	public DEStructureType() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEStructureType(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = new ArrayList();
			// Used to format decimal part with 2 digits only
			f = NumberFormat.getInstance(Locale.ENGLISH);
			f.setGroupingUsed(false);
			f.setMaximumFractionDigits(3);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEStructureType.c ()", "Exception during construction : ", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;

			GScene stand = ((Step) m.getProject().getRoot()).getScene();
			if (!(stand instanceof TreeCollection)) {
				return false;
			}

			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof NProvider)) {
				return false;
			}
			if (!(mp instanceof GProvider)) {
				return false;
			}
			if (!(mp instanceof VProvider)) {
				return false;
			}
			if (!(mp instanceof HdomProvider)) {
				return false;
			}
			if (!(mp instanceof DgProvider)) {
				return false;
			}
			if (!(mp instanceof CoverProvider)) {
				return false;
			}
			if (!(mp instanceof GDiameterClassProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEStructureType.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEStructureType.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B.Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEStructureType.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// addConfigProperty (DataExtractor.HECTARE);
		addBooleanProperty("visibleStepsOnly");
		addDoubleProperty("minThresholdInCm", 17.5);

		addBooleanProperty("typoStand_N");
		addBooleanProperty("typoStand_G");
		addBooleanProperty("typoStand_V");
		addBooleanProperty("typoStand_Hdom");
		addBooleanProperty("typoStand_Dg");
		addBooleanProperty("typoStand_Cover1");
		addBooleanProperty("typoStand_Cover2");
		addBooleanProperty("typoStand_Cover3");
		addBooleanProperty("typoStand_Cover4");
		addBooleanProperty("typoStand_GBig");
		addBooleanProperty("typoStand_Code", true);
		addBooleanProperty("typoThi_N");
		addBooleanProperty("typoThi_V");
		addBooleanProperty("typoThi_G");
		addBooleanProperty("typoThi_KG", true);
		/*
		 * addBooleanProperty ("typoLight_ClarkEvans",true); addBooleanProperty
		 * ("typoLight_MeanIrradiance"); addBooleanProperty
		 * ("typoLight_IrradianceGapArea"); addBooleanProperty
		 * ("typoLight_IndexL1"); addBooleanProperty ("typoLight_IndexL2");
		 * addBooleanProperty ("typoLight_IndexL3"); addBooleanProperty
		 * ("typoLight_IndexL4");
		 */
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the basal area of the stand versus
	 * date from the root Step to this one.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		// ~ System.out.println ("DEStructureType.doExtraction ()");
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// ~ System.out.println ("upToDate="+upToDate+" step="+step);

		// Retrieve method provider
		// methodProvider = MethodProviderFactory.getMethodProvider
		// (step.getScenario ().getModel ());
		methodProvider = step.getProject().getModel().getMethodProvider();

		Log.println("DEStructureType : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			// if (isSet("perHectare")) {
			coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			// }
			double standArea = getSceneArea(step.getScene()); // fc-2.6.2020

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			int n = steps.size();
			if (isSet("visibleStepsOnly")) {
				n = 0;
				for (Iterator i = steps.iterator(); i.hasNext();) {
					Step s = (Step) i.next();
					if (s.isVisible()) {
						n++;
					}
				}
			}

			int sizeStand = 1;
			if (isSet("typoStand_N")) {
				sizeStand++;
			}
			if (isSet("typoStand_G")) {
				sizeStand++;
			}
			if (isSet("typoStand_V")) {
				sizeStand++;
			}
			if (isSet("typoStand_Hdom")) {
				sizeStand++;
			}
			if (isSet("typoStand_Dg")) {
				sizeStand++;
			}
			if (isSet("typoStand_Cover1")) {
				sizeStand++;
			}
			if (isSet("typoStand_Cover2")) {
				sizeStand++;
			}
			if (isSet("typoStand_Cover3")) {
				sizeStand++;
			}
			if (isSet("typoStand_Cover4")) {
				sizeStand++;
			}
			if (isSet("typoStand_GBig")) {
				sizeStand++;
			}
			if (isSet("typoStand_Code")) {
				sizeStand++;
			}
			int sizeThinning = 0;
			if (isSet("typoThi_N")) {
				sizeThinning++;
			}
			if (isSet("typoThi_V")) {
				sizeThinning++;
			}
			if (isSet("typoThi_G")) {
				sizeThinning++;
			}
			if (isSet("typoThi_KG")) {
				sizeThinning++;
			}
			/*
			 * int sizeLight = 0; if (isSet ("typoLight_ClarkEvans"))
			 * {sizeLight++;} if (isSet ("typoLight_MeanIrradiance"))
			 * {sizeLight++;} if (isSet ("typoLight_IrradianceGapArea"))
			 * {sizeLight++;} if (isSet ("typoLight_IndexL1")) {sizeLight++;} if
			 * (isSet ("typoLight_IndexL2")) {sizeLight++;} if (isSet
			 * ("typoLight_IndexL3")) {sizeLight++;} if (isSet
			 * ("typoLight_IndexL4")) {sizeLight++;}
			 */
			n += 1; // add first line for columns headers
			String[][] tabStand = null;
			String[][] tabThinning = null;
			// String [][] tabLight = null;
			if (sizeStand != 0) {
				tabStand = new String[n][sizeStand];
			}
			if (sizeThinning != 0) {
				tabThinning = new String[n][sizeThinning];
			}
			// if (sizeLight != 0) {tabLight = new String[n][sizeLight];}

			// Tables titles
			titles.clear();
			if (sizeStand != 0) {
				titles.add(Translator.swap("DEStructureType.stand"));
			}
			if (sizeThinning != 0) {
				titles.add(Translator.swap("DEStructureType.thinning"));
			}
			// if (sizeLight != 0) {titles.add (Translator.swap
			// ("DEStructureType.light"));}

			// Column headers
			int c = 0; // column number
			tabStand[0][c++] = "Date";
			if (isSet("typoStand_N")) {
				tabStand[0][c++] = "N/ha";
			}
			if (isSet("typoStand_G")) {
				tabStand[0][c++] = "G/ha";
			}
			if (isSet("typoStand_V")) {
				tabStand[0][c++] = "V/ha";
			}
			if (isSet("typoStand_Hdom")) {
				tabStand[0][c++] = "Hdom";
			}
			if (isSet("typoStand_Dg")) {
				tabStand[0][c++] = "Dg";
			}
			if (isSet("typoStand_Cover1")) {
				tabStand[0][c++] = "Cover 1";
			}
			if (isSet("typoStand_Cover2")) {
				tabStand[0][c++] = "Cover 2";
			}
			if (isSet("typoStand_Cover3")) {
				tabStand[0][c++] = "Cover 3";
			}
			if (isSet("typoStand_Cover4")) {
				tabStand[0][c++] = "Cover 4";
			}
			if (isSet("typoStand_GBig")) {
				tabStand[0][c++] = "GBig";
			}
			if (isSet("typoStand_Code")) {
				tabStand[0][c++] = "code";
			}

			c = 0; // column number
			if (isSet("typoThi_N")) {
				tabThinning[0][c++] = "N%";
			}
			if (isSet("typoThi_V")) {
				tabThinning[0][c++] = "V%";
			}
			if (isSet("typoThi_G")) {
				tabThinning[0][c++] = "G%";
			}
			if (isSet("typoThi_KG")) {
				tabThinning[0][c++] = "KG";
			}

			/*
			 * c = 0; // column number if (isSet ("typoLight_ClarkEvans"))
			 * {tabLight[0][c++] = "Clark-Evans";} if (isSet
			 * ("typoLight_MeanIrradiance")) {tabLight[0][c++] =
			 * "MeanIrradiance";} if (isSet ("typoLight_IrradianceGapArea"))
			 * {tabLight[0][c++] = "Irradiance Gap Area";} if (isSet
			 * ("typoLight_IndexL1")) {tabLight[0][c++] = "Index L1";} if (isSet
			 * ("typoLight_IndexL2")) {tabLight[0][c++] = "Index L2";} if (isSet
			 * ("typoLight_IndexL3")) {tabLight[0][c++] = "Index L3";} if (isSet
			 * ("typoLight_IndexL4")) {tabLight[0][c++] = "Index L4";}
			 */

			// Data extraction

			int line = 1;
			String buffer = "";
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step step = (Step) i.next();
				// if (isSet ("visibleStepsOnly") && !step.isVisible ())
				// {continue;} // next iteration

				boolean interventionStep = step.getScene().isInterventionResult();

				// Previous step
				Step prevStep = null;
				// if (isSet ("visibleStepsOnly")) {
				// prevStep = (Step) step.getVisibleFather ();
				// } else {
				prevStep = (Step) step.getFather();
				// }

				double minThreshold = getDoubleProperty("minThresholdInCm");

				// 1. Retrieve trees in the stand under the step
				GScene stand = step.getScene();
				Collection trees = new ArrayList();

				Iterator i1 = (((TreeCollection) stand).getTrees()).iterator();
				while (i1.hasNext()) {
					Tree t = (Tree) i1.next();
					double d = t.getDbh(); // dbh : cm
					if (d > minThreshold) {
						trees.add(t);
					}
				}

				// 2. Retrieve trees in the stand under the previous step
				GScene prevStand = null;
				Collection prevTrees = new ArrayList(); // list based on table

				// Caution : if step = root, prevStep = null
				try {
					prevStand = prevStep.getScene();
					Iterator i2 = (((TreeCollection) prevStand).getTrees()).iterator();
					while (i2.hasNext()) {
						Tree t = (Tree) i2.next();
						double d = t.getDbh(); // dbh : cm
						if (d > minThreshold) {
							prevTrees.add(t);
						}
					}
				} catch (Exception e) {
				}

				// Stand variables
				// ------------------------------------------------------------------
				c = 0; // column number
				int date = stand.getDate();
				tabStand[line][c++] = "" + date;

				double N = -1d; // ----- N is always computed
				try {
					N = ((NProvider) methodProvider).getN(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("typoStand_N")) {
					tabStand[line][c++] = "" + (int) N; // N integer (phd)
				}

				double G = -1d; // ----- G is always computed
				try {
					G = ((GProvider) methodProvider).getG(stand, trees) * coefHa;
				} catch (Exception e) {
				} // used for pro_G
				if (isSet("typoStand_G")) {
					tabStand[line][c++] = "" + f.format(G);
				}

				double V = -1d; // ----- V is always computed
				try {
					V = ((VProvider) methodProvider).getV(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("typoStand_V")) {
					tabStand[line][c++] = "" + f.format(V);
				}

				double Hdom = -1d; // ----- Hdom is always computed
				try {
					Hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);
				} catch (Exception e) {
				}
				if (isSet("typoStand_Hdom")) {
					tabStand[line][c++] = "" + f.format(Hdom);
				}

				double Dg = -1d; // default = "not calculable"
				if (isSet("typoStand_Dg")) {
					try {
						Dg = ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					tabStand[line][c++] = "" + f.format(Dg);
				}

				Vector coverStatus = ((CoverProvider) methodProvider).getCover(stand);
				double cover1 = -1d; // default = "not calculable"
				try {
					cover1 = ((Double) coverStatus.get(4)).doubleValue();
				} catch (Exception e) {
				}
				cover1 = cover1 * coefHa / 100;
				if (isSet("typoStand_Cover1")) {
					tabStand[line][c++] = "" + f.format(cover1);
				}
				double cover2 = -1d; // default = "not calculable"
				try {
					cover2 = ((Double) coverStatus.get(3)).doubleValue();
				} catch (Exception e) {
				}
				cover2 = cover2 * coefHa / 100;
				if (isSet("typoStand_Cover2")) {
					tabStand[line][c++] = "" + f.format(cover2);
				}
				double cover3 = -1d; // default = "not calculable"
				try {
					cover3 = ((Double) coverStatus.get(2)).doubleValue();
				} catch (Exception e) {
				}
				cover3 = cover3 * coefHa / 100;
				if (isSet("typoStand_Cover3")) {
					tabStand[line][c++] = "" + f.format(cover3);
				}
				double cover4 = -1d; // default = "not calculable"
				try {
					cover4 = ((Double) coverStatus.get(1)).doubleValue();
				} catch (Exception e) {
				}
				cover4 = cover4 * coefHa / 100;
				if (isSet("typoStand_Cover4")) {
					tabStand[line][c++] = "" + f.format(cover4);
				}
				double cover = -1d; // default = "not calculable"
				try {
					cover = ((Double) coverStatus.get(0)).doubleValue();
				} catch (Exception e) {
				}
				cover = cover * coefHa / 100;
				if (isSet("typoStand_Cover")) {
					tabStand[line][c++] = "" + f.format(cover);
				}
				double GBig = -1d; // default = "not calculable"
				Vector GDiameterClass = ((GDiameterClassProvider) methodProvider).getGDiameterClass(stand);
				try {
					double G1 = ((Double) GDiameterClass.get(0)).doubleValue();
					double G2 = ((Double) GDiameterClass.get(1)).doubleValue();
					double G3 = ((Double) GDiameterClass.get(2)).doubleValue();
					double G4 = ((Double) GDiameterClass.get(3)).doubleValue();
					double G5 = ((Double) GDiameterClass.get(4)).doubleValue();
					double G6 = ((Double) GDiameterClass.get(5)).doubleValue();
					GBig = 100 * (G5 + G6) / (G1 + G2 + G3 + G4 + G5 + G6);
				} catch (Exception e) {
					GBig = -1d;
					Log.println(Log.WARNING, "DEStructureType.doExtraction ()", "Error during GBig calculation : ", e);
				}
				if (isSet("typoStand_GBig")) {
					tabStand[line][c++] = "" + f.format(GBig);
				}
				String code = "Error";

				// Eric Mermin - 2005

				// G < 10m�/ha
				if ((G < 10) && (cover < 10)) {
					code = "P/A";
				} else if ((G < 10) && (cover >= 10) && (Hdom < 15)) {
					code = "CH";
				} else if ((G < 10) && (cover >= 10) && ((cover3 + cover4 < 50) || (N < 800))) {
					code = "L";
				} else if ((G < 10) && (cover >= 10) && ((cover3 + cover4 >= 50) || (N >= 800))) {
					code = "R";
				}
				// G >= 10m�/ha
				// Strate 1 pr�pond�rante
				else if ((G >= 10) && (cover1 >= 50) && (GBig < 60)) {
					code = "1";
				} else if ((G >= 10) && (cover1 >= 50) && (GBig >= 60)) {
					code = "1GB";
				}
				// Strate 1 majoritaire
				else if ((G >= 10) && (cover1 > 20) && (cover2 <= 20) && (cover3 <= 20) && (GBig < 60)) {
					code = "1";
				} else if ((G >= 10) && (cover1 >= 20) && (cover2 <= 20) && (cover3 <= 20) && (GBig >= 60)) {
					code = "1GB";
				}

				// Strate 2 pr�pond�rante
				else if ((G >= 10) && (cover2 >= 50) && (GBig < 60)) {
					code = "2";
				} else if ((G >= 10) && (cover2 >= 50) && (GBig >= 60)) {
					code = "2GB";
				}
				// Strate 2 majoritaire
				else if ((G >= 10) && (cover1 <= 20) && (cover2 > 20) && (cover3 <= 20) && (GBig < 60)) {
					code = "2";
				} else if ((G >= 10) && (cover1 <= 20) && (cover2 > 20) && (cover3 <= 20) && (GBig >= 60)) {
					code = "2GB";
				}

				// Strate 3 pr�pond�rante
				else if ((G >= 10) && (cover3 >= 50) && (GBig < 60)) {
					code = "3";
				} else if ((G >= 10) && (cover3 >= 50) && (GBig >= 60)) {
					code = "3GB";
				}
				// Strate 3 majoritaire
				else if ((G >= 10) && (cover1 <= 20) && (cover2 <= 20) && (cover3 > 20) && (GBig < 60)) {
					code = "3";
				} else if ((G >= 10) && (cover1 <= 20) && (cover2 <= 20) && (cover3 > 20) && (GBig >= 60)) {
					code = "3GB";
				}

				// Strate 1 d�ficitaire
				else if ((G >= 10) && (cover1 <= 20) && (cover2 > 20) && (cover3 > 20)) {
					code = "2-3";
				}
				// Strate 2 d�ficitaire
				else if ((G >= 10) && (cover1 > 20) && (cover2 <= 20) && (cover3 > 20) && (GBig < 60)) {
					code = "1-3";
				} else if ((G >= 10) && (cover1 > 20) && (cover2 <= 20) && (cover3 > 20) && (GBig >= 60)) {
					code = "1-3GB";
				}
				// Strate 3 d�ficitaire
				else if ((G >= 10) && (cover1 > 20) && (cover2 > 20) && (cover3 <= 20) && (GBig < 60)) {
					code = "1-2";
				} else if ((G >= 10) && (cover1 > 20) && (cover2 > 20) && (cover3 <= 20) && (GBig >= 60)) {
					code = "1-2GB";
				}
				// 3 strates d�ficitaires
				else if ((G >= 10) && (cover1 <= 20) && (cover2 <= 20) && (cover3 <= 20)) {
					code = "C";
				}

				// Strates 1,2,3 ni pr�pond�rantes ni d�ficitaires
				else if ((G >= 10) && (cover1 > 20) && (cover2 > 20) && (cover3 > 20) && (GBig < 60)) {
					code = "J";
				} else if ((G >= 10) && (cover1 > 20) && (cover2 > 20) && (cover3 > 20) && (GBig >= 60)) {
					code = "JGB";
				}

				/*
				 * Ancien code de Benoit Courbaud pour calcul de la typo if (G <
				 * 10) { if (((cover3+cover4)<45) || (N<800)) { code = "L"; }
				 * else { code = "R"; }
				 * 
				 * 
				 * } else { // G>=10 m2/ha if ((cover1 < 25) && (cover2 < 25) &&
				 * (cover3 < 25)) { // DDD code = "C"; } else if ( ((cover1 <
				 * 25) && (cover2 < 25) && (cover3 < 45)) || ((cover1 < 25) &&
				 * (cover2 < 45) && (cover3 < 25)) || ((cover1 < 45) && (cover2
				 * < 25) && (cover3 < 25)) ) {// DDM, DMD or MDD code = "C";
				 * 
				 * } else if ((cover1 < 25) && (cover2 < 45) && (cover3 < 45)) {
				 * // DMM if (GBig < 60) { code = "2-3"; } else { code =
				 * "2-3 GB"; } } else if ((cover1 < 45) && (cover2 < 25) &&
				 * (cover3 < 45)) { // MDM if (GBig < 60) { code = "1-3"; } else
				 * { code = "1-3 GB"; } } else if ((cover1 < 45) && (cover2 <
				 * 45) && (cover3 < 25)) { // MMD if (GBig < 60) { code = "1-2";
				 * } else { code = "1-2 GB"; }
				 * 
				 * } else if ((cover1 < 45) && (cover2 < 45) && (cover3 < 45)) {
				 * // MMM if (GBig < 60) { code = "J"; } else { code = "J GB"; }
				 * 
				 * } else if ((cover1 >= 45) && (cover1 >= cover2) && (cover1 >=
				 * cover3)) { // PMM ou PMp ou PpM ou PMD ou PDM ou PpD ou PDp
				 * if (GBig < 60) { code = "1"; } else { code = "1 GB"; } } else
				 * if ((cover2 >= 45) && (cover2 >= cover1) && (cover2 >=
				 * cover3)) { if (GBig < 60) { code = "2"; } else { code =
				 * "2 GB"; } } else if ((cover3 >= 45) && (cover3 >= cover2) &&
				 * (cover3 >= cover1)) { if (GBig < 60) { code = "3"; } else {
				 * code = "3 GB"; } } }
				 */

				if (isSet("typoStand_Code")) {
					tabStand[line][c++] = code;
				}

				// Thinning variables
				// ------------------------------------------------------------------
				c = 0; // column number
				double typoThi_N = -1; // ----- typoThi_N is always computed
				if (interventionStep) {
					// double N = -1d;
					double prevN = -1d;
					double prevG = -1d;
					double prevV = -1d;
					try {
						prevN = ((NProvider) methodProvider).getN(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}
					try {
						prevG = ((GProvider) methodProvider).getG(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}
					try {
						prevV = ((VProvider) methodProvider).getV(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}

					if (N != -1 && prevN != -1) {
						typoThi_N = 100 * (prevN - N) / prevN;
					}
					if (isSet("typoThi_N")) {
						buffer = "";
						if (interventionStep) {
							buffer = "" + (int) (typoThi_N);
						}
						tabThinning[line][c++] = buffer;
					}
					double typoThi_V = -1; // ----- typoThi_V is always computed
					if (V != -1 && prevV != -1) {
						typoThi_V = 100 * (prevV - V) / prevV;
					}
					if (isSet("typoThi_V")) {
						buffer = "";
						if (interventionStep) {
							buffer = "" + f.format(typoThi_V);
						}
						tabThinning[line][c++] = buffer;
					}
					double typoThi_G = -1; // ----- typoThi_V is always computed
					if (G != -1 && prevG != -1) {
						typoThi_G = 100 * (prevG - G) / prevG;
					}
					if (isSet("typoThi_G")) {
						buffer = "";
						if (interventionStep) {
							buffer = "" + f.format(typoThi_G);
						}
						tabThinning[line][c++] = buffer;
					}
					double typoThi_KG = -1;
					if (G != -1 && prevG != -1) {
						double thinnedGmean = (prevG - G) / (prevN - N);
						double prevGmean = prevG / prevN;
						typoThi_KG = thinnedGmean / prevGmean;
					}
					if (isSet("typoThi_KG")) {
						buffer = "";
						if (interventionStep) {
							buffer = "" + f.format(typoThi_KG);
						}
						tabThinning[line][c++] = buffer;
					}

				}

				// Spatial structure and radiation interception variables
				// ------------------------------------------------------------------
				/*
				 * c = 0; // column number double CE = -1d; // default =
				 * "not calculable" if (isSet ("typoLight_ClarkEvans")) {
				 * 
				 * // works on the iterator made with the trees of the stand
				 * Iterator treesIter = trees.iterator (); int pointnb =
				 * trees.size();
				 * 
				 * // Limited in size! (java initializes each value to 0) double
				 * x[] = new double[pointnb+1]; double y[] = new
				 * double[pointnb+1]; double xmi=stand.getOrigin().x; double
				 * xma=xmi+stand.getWidth(); double ymi=stand.getOrigin().y;
				 * double yma=ymi+stand.getHeight();
				 * 
				 * // Create output data int j=-1; while (treesIter.hasNext ())
				 * { j+=1; GMaddTree t = (GMaddTree) treesIter.next ();
				 * x[j]=t.getX(); // x : m // FG y[j]=t.getY(); // y : m // FG }
				 * int pointNumber=j+1;
				 * 
				 * CE = ClarkEvans.computeCE(x,y,pointNumber,xmi,xma,ymi,yma);
				 * if (Double.isNaN(CE)) { tabLight[line][c++] = "-"; } else {
				 * tabLight[line][c++] = ""+f.format (CE); } }
				 * 
				 * double irrad = -1d; // default = "not calculable" result in %
				 * if (isSet ("typoLight_MeanIrradiance")) { try {irrad =
				 * ((MeanIrradianceProvider) methodProvider).getMeanIrradiance
				 * (stand);} catch (Exception e) {} tabLight[line][c++] =
				 * ""+f.format (irrad); }
				 * 
				 * double irradAreaSup20 = -1d; // default = "not calculable"
				 * Result in % of totalArea if (isSet
				 * ("typoLight_IrradianceGapArea")) {
				 * 
				 * GPlot plot = stand.getPlot (); Collection cells =
				 * plot.getCells (); Iterator ite = cells.iterator (); int tab[]
				 * = new int[5]; int totalCellNb = 0; int numbers = 0;
				 * 
				 * // Create output data while (ite.hasNext ()) { GCell cell =
				 * (GCell) ite.next (); double relEnergy =
				 * ((CellRelativeHorizontalEnergyProvider) methodProvider).
				 * getCellRelativeHorizontalEnergy (cell); if (relEnergy == 100)
				 * {relEnergy = 99.9;} int category = (int) (relEnergy / 20);
				 * tab [category] += 1; totalCellNb +=1; } irradAreaSup20 = 100
				 * * (tab[4]+tab[3]+tab[2]+tab[1]) / totalCellNb;
				 * tabLight[line][c++] = ""+f.format (irradAreaSup20); }
				 * 
				 * 
				 * Vector layerInterceptionIndex =
				 * ((LayerInterceptionIndexProvider)
				 * methodProvider).getLayerInterceptionIndex (stand, trees);
				 * 
				 * double l1Ix = -1d; // default = "not calculable" try {l1Ix =
				 * ((Double) layerInterceptionIndex.get (0)).doubleValue ();}
				 * catch (Exception e) {} if (isSet ("typoLight_IndexL1")) { if
				 * (l1Ix==-1) {tabLight[line][c++] = "-";} else
				 * {tabLight[line][c++] = ""+f.format (l1Ix);} } double l2Ix =
				 * -1d; // default = "not calculable" try {l2Ix = ((Double)
				 * layerInterceptionIndex.get (1)).doubleValue ();} catch
				 * (Exception e) {} if (isSet ("typoLight_IndexL2")) { if
				 * (l2Ix==-1) {tabLight[line][c++] = "-";} else
				 * {tabLight[line][c++] = ""+f.format (l2Ix);} } double l3Ix =
				 * -1d; // default = "not calculable" try {l3Ix = ((Double)
				 * layerInterceptionIndex.get (2)).doubleValue ();} catch
				 * (Exception e) {} if (isSet ("typoLight_IndexL3")) { if
				 * (l3Ix==-1) {tabLight[line][c++] = "-";} else
				 * {tabLight[line][c++] = ""+f.format (l3Ix);} } double l4Ix =
				 * -1d; // default = "not calculable" try {l4Ix = ((Double)
				 * layerInterceptionIndex.get (3)).doubleValue ();} catch
				 * (Exception e) {} if (isSet ("typoLight_IndexL4")) { if
				 * (l4Ix==-1) {tabLight[line][c++] = "-";} else
				 * {tabLight[line][c++] = ""+f.format (l4Ix);} }
				 */

				line++;
			}

			tables.clear();
			if (tabStand != null) {
				tables.add(tabStand);
			}
			if (tabThinning != null) {
				tables.add(tabThinning);
			}
			// if (tabLight != null) {tables.add (tabLight);}

			// ~ System.out.println ("DEStructureType : extraction done");

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEStructureType.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	// fc - 23.4.2004
	// ~ protected String getNamePrefix () {
	// ~ String prefix = "";
	// ~ try {
	// ~ if (isCommonGroup ()
	// ~ && isGroupMode ()
	// ~ && GroupManager.getInstance ().getGroupNames ().contains (getGroupName
	// ())) {
	// ~ prefix += getGroupName ()+" - ";
	// ~ }
	// ~ if (isSet("perHectare")) {prefix += "/ha - ";}
	// ~ } catch (Exception e) {} // if trouble, prefix is empty
	// ~ return prefix;
	// ~ }

	/**
	 * From DataFormat interface.
	 */
	// ~ public String getCaption () {
	// ~ return getStep ().getCaption ();
	// ~ }

	/**
	 * From DFTables interface.
	 */
	public Collection getTables() {
		return tables;
	}

	/**
	 * From DFTables interface.
	 */
	public Collection getTitles() {
		return titles;
	}

}
