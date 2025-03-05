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

import java.util.Collection;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.Genotypable;
import capsis.lib.genetics.LoiselleKinshipCoefficient;
import capsis.util.methodprovider.FijLoiselleProvider;

/**
 * Distribution of Fij Loiselle coefficients across distance classes
 * 
 * @author S. Oddou-Muratorio - April 2007
 */
public class DEDistanceClassLoiselle extends PaleoDataExtractor implements DFCurves {
	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDistanceClassLoiselle"); // axis
																						// translation
	}

	protected Vector curves; // Integer for x and Double pour y
	private Vector labels;

	/**
	 * Constructor.
	 */
	public DEDistanceClassLoiselle() {
	} // habitual for extensions, not operational

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEDistanceClassLoiselle(GenericExtensionStarter s) {
		super(s);

		curves = new Vector();
		labels = new Vector();

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			Step root = (Step) m.getProject().getRoot();
			if (!(root.getScene() instanceof GeneticScene)) {
				return false;
			}
			if (!(mp instanceof FijLoiselleProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDistanceClassLoiselle.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		// fc - 13.9.2004
		return getNamePrefix() + Translator.swap("DEDistanceClassLoiselle.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "S. Oddou-Muratorio";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEDistanceClassLoiselle.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.0";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRScatterPlot";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// addGroupProperty (FishGroupHelper.GROUP_FISH, DataExtractor.COMMON); // fc -
		// 13.9.2004
		// addGroupProperty (FishGroupHelper.GROUP_FISH, DataExtractor.INDIVIDUAL); // fc -
		// 13.9.2004
		addBooleanProperty("logDistance", true);
		addIntProperty("maxPairsNumber", 1000);
		// add here a property type String for upper bounds: ex: 20,56,148...
		// TODO
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// Retrieve method provider
		// ~ methodProvider = step.getScenario ().getModel ().getMethodProvider
		// ();
		try {
			GScene stand = step.getScene();
			GModel m = step.getProject().getModel();

			GeneticScene genoStand = (GeneticScene) stand;
			Collection<Genotypable> gees = genoStand.getGenotypables();

			double maxDistance = Math.sqrt(stand.getXSize() * stand.getXSize() + stand.getYSize() * stand.getYSize());

			double[] upperBounds = new double[6];
			upperBounds[0] = 20;
			upperBounds[1] = 54.6;
			upperBounds[2] = 148.4;
			upperBounds[3] = 403.4;
			upperBounds[4] = 1096;
			upperBounds[5] = maxDistance;

			int maxPairsNumber = getIntProperty("maxPairsNumber");

			MethodProvider mp = m.getMethodProvider();
			FijLoiselleProvider p = (FijLoiselleProvider) mp;

			LoiselleKinshipCoefficient.DistanceClass[] dc = p.getFijLoiselle(gees, upperBounds, maxPairsNumber);

			// Data extraction : points with (Double, Double) coordinates
			curves.clear();

			Vector xs = new Vector();
			for (int i = 0; i < upperBounds.length; i++) {
				if (isSet("logDistance")) {
					// TODO: check that ub > 0
					xs.add(Math.log(upperBounds[i]));
				} else {
					xs.add(upperBounds[i]);
				}
			}

			Vector ys = new Vector();
			for (int i = 0; i < dc.length; i++) {
				ys.add(dc[i].FijMean);
			}

			curves.add(xs);
			curves.add(ys);

			labels.clear(); // if you tell nothing, labels are calculated from x
							// series

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDistanceClassLoiselle.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		if (isSet("logDistance")) {
			v.add(Translator.swap("DEDistanceClassLoiselle.xLabel") + " "
					+ Translator.swap("DEDistanceClassLoiselle.log"));
		} else {
			v.add(Translator.swap("DEDistanceClassLoiselle.xLabel"));
		}
		v.add(Translator.swap("DEDistanceClassLoiselle.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1;
	}

}
