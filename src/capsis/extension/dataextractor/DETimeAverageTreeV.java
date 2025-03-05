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
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.TotalAboveGroundVolumeProvider;
import capsis.util.methodprovider.VMerchantProviderWithName;
import capsis.util.methodprovider.VProvider;
import capsis.util.methodprovider.VProviderWithName;

/**
 * Average volume per tree along with time. Inspired from extractor DETimeV
 * (author B. Courbaud)
 * 
 * @author M. Fortin - August 2009
 */
public class DETimeAverageTreeV extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeAverageTreeV");
	}

	protected boolean totalVolumeMode;
	protected boolean merchantVolume;
	protected List<List<String>> labels;

	/**
	 * Constructor.
	 */
	public DETimeAverageTreeV() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeAverageTreeV(GenericExtensionStarter s) {
		super(s);

		labels = new ArrayList<List<String>>();

		documentationKeys.add("VProvider"); // fc - 29.3.2005
		documentationKeys.add("NProvider"); // mf - 28.8.2009

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
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof VProvider)) {
				return false;
			}
			if (!(mp instanceof NProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAverageTreeV.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeAverageTreeV.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeAverageTreeV.description");
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

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
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

		double dm3Factor = 1000; // factor to ensure the conversion from m3 to
									// dm3

		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		totalVolumeMode = false;
		if (methodProvider instanceof TotalAboveGroundVolumeProvider) {
			totalVolumeMode = true;
		}
		merchantVolume = false;
		if (methodProvider instanceof VMerchantProviderWithName) {
			merchantVolume = true;
		}

		try {
			// Retrieve Steps from root to this step
			Vector<Step> steps = step.getProject().getStepsFromRoot(step);

			Vector<Integer> c1 = new Vector<Integer>(); // x coordinates
			Vector<Double> c2 = new Vector<Double>(); // y coordinates from
														// regular VProvider
			Vector<Double> c3 = new Vector<Double>(); // y' coordinates from
														// TotalAboveGroundVolumeProvider
			Vector<Double> c4 = new Vector<Double>(); // optional merchant
														// volume coordinates
														// from
														// VMerchantProviderWithName

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator<Step> i = steps.iterator(); i.hasNext();) {
				Step s = i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				double N = ((NProvider) methodProvider).getN(stand, trees);
				double v0 = ((VProvider) methodProvider).getV(stand, trees);
				if (N > 0) {
					c2.add(new Double(v0 / N * dm3Factor));
					if (totalVolumeMode) {
						double v1 = ((TotalAboveGroundVolumeProvider) methodProvider).getTotalAboveGroundVolume(stand,
								trees);
						c3.add(new Double(v1 / N * dm3Factor));
					}
					if (merchantVolume) {
						double v2 = ((VMerchantProviderWithName) methodProvider).getVMerchant(stand, trees);
						c4.add(new Double(v2 / N * dm3Factor));
					}
				} else {
					c2.add(0.0);
					if (totalVolumeMode) {
						c3.add(0.0);
					}
					if (merchantVolume) {
						c4.add(0.0);
					}
				}
				c1.add(new Integer(date));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);
			if (totalVolumeMode) {
				curves.add(c3);
			}
			if (merchantVolume) {
				curves.add(c4);
			}

			String volumeName = Translator.swap("DETimeAverageTreeV.volumeName");
			if (methodProvider instanceof VProviderWithName) {
				volumeName = ((VProviderWithName) methodProvider).getVolumeName();
			}

			labels.clear();
			labels.add(new Vector<String>()); // no x labels
			Vector<String> y1Labels = new Vector<String>();
			y1Labels.add(volumeName);
			labels.add(y1Labels);

			if (totalVolumeMode) {
				Vector<String> y2Labels = new Vector<String>();
				y2Labels.add(Translator.swap("DETimeAverageTreeV.totalVolumeName"));
				labels.add(y2Labels);
			}

			if (merchantVolume) {
				Vector<String> y4Labels = new Vector<String>();
				y4Labels.add(((VMerchantProviderWithName) methodProvider).getVMerchantName());
				labels.add(y4Labels);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeAverageTreeV.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector<String> v = new Vector<String>();
		v.add(Translator.swap("DETimeAverageTreeV.xLabel"));
		v.add(Translator.swap("DETimeAverageTreeV.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
		// ~ if (totalVolumeMode) {
		// ~ return 2;
		// ~ } else {
		// ~ return 1;
		// ~ }
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
