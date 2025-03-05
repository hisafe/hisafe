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
import capsis.util.TextInterface;
import capsis.util.methodprovider.TotalAboveGroundVolumeProvider;
import capsis.util.methodprovider.VMerchantProviderWithName;
import capsis.util.methodprovider.VProvider;
import capsis.util.methodprovider.VProviderWithName;

/**
 * Volume versus Date.
 * 
 * @author B. Courbaud - September 2001
 */
public class DETimeV extends DETimeG implements TextInterface {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeV");
	}

	protected boolean totalVolumeMode;
	protected boolean merchantVolume;
	protected Vector labels;

	protected String text;

	/**
	 * Constructor.
	 */
	public DETimeV() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeV(GenericExtensionStarter s) {
		super(s);
		try {

			labels = new Vector();

			documentationKeys.add("VProvider");

			// Additional text for documentation
			methodProvider = step.getProject().getModel().getMethodProvider();
			if (methodProvider != null && methodProvider instanceof VProviderWithName) {

				VProviderWithName p = (VProviderWithName) methodProvider;
				text = p.getVolumeName();
			}
		} catch (Exception e) { 
			// fc-30.8.2021 Added try / catch to check an error with Modis
			Log.println(Log.WARNING, "DETimeV.c ()", "Error in constuctor", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof VProvider))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeV.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeV.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud, P. Vallet";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeV.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-09.08.2018
	// public static final String VERSION = "1.2";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
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
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y' coordinates
			Vector c4 = new Vector(); // optional merchant volume coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				double V = ((VProvider) methodProvider).getV(stand, trees);

				c1.add(new Integer(date));
				c2.add(new Double(V * coefHa));

				if (totalVolumeMode) {
					double v1 = ((TotalAboveGroundVolumeProvider) methodProvider).getTotalAboveGroundVolume(stand,
							trees);
					c3.add(new Double(v1 * coefHa));
				}

				if (merchantVolume) {
					double v2 = ((VMerchantProviderWithName) methodProvider).getVMerchant(stand, trees);
					c4.add(new Double(v2 * coefHa));
				}
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

			// String volumeName = Translator.swap("DETimeV.volumeName");
			// if (methodProvider instanceof VProviderWithName) {
			// volumeName = ((VProviderWithName) methodProvider)
			// .getVolumeName();
			// if (volumeName.length() > 20) {
			// volumeName = volumeName.substring(0, 20) + "...";
			// }
			// }

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			// y1Labels.add(volumeName);
			y1Labels.add("");
			labels.add(y1Labels);

			if (totalVolumeMode) {
				Vector y2Labels = new Vector();
				y2Labels.add(Translator.swap("DETimeV.totalVolumeName"));
				labels.add(y2Labels);
			}

			if (merchantVolume) {
				Vector y4Labels = new Vector();
				y4Labels.add(((VMerchantProviderWithName) methodProvider).getVMerchantName());
				labels.add(y4Labels);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeV.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeV.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeV.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeV.yLabel"));
		}
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

	@Override
	public String getText() {
		return text;
	}

}
