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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.AgeDomProvider;
import capsis.util.methodprovider.AgeGProvider;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;

/**
 * Ring width versus time: (1)- obj : ring width of objective trees (the set of
 * trees at the chosen step) at time t (average, min and max) ; (2)- Ddom : ring
 * width of dominant tree (= Ddom / AgeDom / 2) (needs AgeDomProvider) ; (3)- Dg
 * : ring width of mean tree (= Dg / AgeG / 2) (needs AgeGProvider) ; Ring width
 * is estimated by (dbh2 - dbh1) / (age2 - age1) / 2 (i.e. bark included !!). If
 * meanPB is selected, ring width is the average from pith to time t (dbh / age)
 * Should work with numberable trees.
 * 
 * @author F. Mothe - July 2008
 */
public class DETimeRW extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRW");
	}
	protected List<List<? extends Number>> curves;
	protected List<List<String>> labels;

	/**
	 * Constructor.
	 */
	public DETimeRW() {
	}

	/**
	 * Constructor 2,
	 */
	public DETimeRW(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new ArrayList<List<? extends Number>>();
			labels = new ArrayList<List<String>>();

			if (step != null) {
				MethodProvider mp = step.getProject().getModel().getMethodProvider();
				boolean withDdom = (mp instanceof DdomProvider) && (mp instanceof AgeDomProvider);
				boolean withDg = (mp instanceof DgProvider) && (mp instanceof AgeGProvider);
				setPropertyEnabled("DETimeRW.n01Ddom", withDdom);
				setPropertyEnabled("DETimeRW.n02Dg", withDg);
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRW.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			Step step = (Step) ((GModel) referent).getProject().getRoot();
			if (!(step.getScene() instanceof TreeList))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRW.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeRW.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. Mothe";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeRW.description");
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
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		// TODO : group selection in two tabs ?
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration

		// Properties in the form boxTitle_componentTitle :
		addBooleanProperty("DETimeRW.n03obj_avg", true);
		addBooleanProperty("DETimeRW.n03obj_minmax", false);
		addBooleanProperty("DETimeRW.n01Ddom", true);
		addBooleanProperty("DETimeRW.n02Dg", true);
		addBooleanProperty("DETimeRW.n04selected", true);
		addBooleanProperty("DETimeRW.n09meanPB", false);
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes Number of trees in the stand
	 * versus Date.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		
		return true;
	}

	/**
	 * Ring width for a given tree considering previous dbh at previous age.
	 * Returns the radial increment or the mean from pith.
	 */
	private static double getRingWidth_mm(Tree t, double previousDbh, int previousAge, boolean withMeanPB) {
		double rw;
		if (withMeanPB) {
			rw = getRingWidth_mm(t.getDbh(), t.getAge());
		} else {
			int nbYears = t.getAge() - previousAge;
			double incDbh = Math.max(0., t.getDbh() - previousDbh);
			rw = getRingWidth_mm(incDbh, nbYears);
		}
		return rw;
	}

	/**
	 * Ring width for a given increment in diameter during a given period.
	 */
	private static double getRingWidth_mm(double incDiam_cm, double incAge) {
		return incAge > 0. ? incDiam_cm / (.2 * incAge) : 0.; // mm
	}

	/**
	 * Add a data serie to curves and labels. label may be null for x
	 * coordinates.
	 */
	private void addSerie(Vector<? extends Number> coordinates, String label) {
		curves.add(coordinates);
		Vector<String> yLabels = new Vector<String>();
		if (label != null) {
			yLabels.add(label);
		}
		labels.add(yLabels);
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector<String> v = new Vector<String>();
		v.add(Translator.swap("DETimeRW.xLabel"));
		v.add(Translator.swap("DETimeRW.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
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
		return labels;
	}

}
