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

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.TotalAboveGroundVolumeProvider;
import capsis.util.methodprovider.VLogProvider;
import capsis.util.methodprovider.VMerchantProviderWithName;
import capsis.util.methodprovider.VProvider;
import capsis.util.methodprovider.VProviderWithName;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Total volume versus Date.
 * 
 * @author P. Vallet - Autumn 2002
 */
// fc-22.9.2021 Clearer to extend PaleoDataExtractor than DETimeG
public class DETimeProducedV extends PaleoDataExtractor implements DFCurves {
//public class DETimeProducedV extends DETimeG {

	private static int NUMBER_DIGITS = 2;

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeProducedV");
	}

	protected List<List<? extends Number>> curves; // fc-22.9.2021
	protected Vector labels;
	protected MethodProvider methodProvider; // fc-22.9.2021

	/**
	 * Constructor.
	 */
	public DETimeProducedV() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeProducedV(GenericExtensionStarter s) {
		super(s);

		curves = new ArrayList<List<? extends Number>>(); // fc-22.9.2021
		labels = new Vector();

		documentationKeys.add("VProvider"); // tl cm - 29.1.2007
		documentationKeys.add("VLogProvider"); // tl cm - 20.7.2009
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
			Log.println(Log.ERROR, "DETimeProducedV.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeProducedV.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "P. Vallet";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeProducedV.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.3";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.3";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // fc - 9.4.2004

		// Properties in the form boxTitle_componentTitle :
		addBooleanProperty("DETimeProducedV.totalv_totag", false);
		addBooleanProperty("DETimeProducedV.prodv_normal", true);
		addBooleanProperty("DETimeProducedV.prodv_merchant", false);
		addBooleanProperty("DETimeProducedV.prodv_totag", true);
		addBooleanProperty("DETimeProducedV.prodv_log", true);
		
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		// fc-20.9.2021 Ask the properties to be shown to user in the order they were
		// declared here (not for the legacy properties, optional)
//		setPropertyDeclarationOrderKept();

		// fc-22.9.2021 Disabling properties when feature is not available
		if (step != null) {
			// Retrieve method provider
			methodProvider = step.getProject().getModel().getMethodProvider();
			
			setPropertyEnabled("DETimeProducedV.prodv_log", methodProvider instanceof VLogProvider);
			setPropertyEnabled("DETimeProducedV.prodv_merchant", methodProvider instanceof VMerchantProviderWithName);

			setPropertyEnabled("DETimeProducedV.prodv_totag", methodProvider instanceof TotalAboveGroundVolumeProvider);
			setPropertyEnabled("DETimeProducedV.totalv_totag",
					methodProvider instanceof TotalAboveGroundVolumeProvider);
		}
		// fc-22.9.2021
		
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

		boolean normalVolumeMode = isSet("DETimeProducedV.prodv_normal");
		boolean merchantVolumeMode = isSet("DETimeProducedV.prodv_merchant")
				&& methodProvider instanceof VMerchantProviderWithName;
		boolean totalVolumeMode = (isSet("DETimeProducedV.prodv_totag") || isSet("DETimeProducedV.totalv_totag"))
				&& methodProvider instanceof TotalAboveGroundVolumeProvider;
		boolean logVolumeMode = (isSet("DETimeProducedV.prodv_log") // cm tl
																	// 15.07.2009
				&& methodProvider instanceof VLogProvider);

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c0 = new Vector(); // x coordinates
			Vector c1 = new Vector(); // y1 coordinates (normal collected)
			Vector c2 = new Vector(); // y2 coordinates (merchant collected)
			Vector c3 = new Vector(); // y3 coordinates (total AG collected)
			Vector c4 = new Vector(); // y4 coordinates (total AG collected +
										// alive)
			Vector c5 = new Vector(); // y5 coordinates (total exported log
										// volume

			double producedBFVolume = 0;
			double producedLogVolume = 0;
			double previousBFVolume = 0;
			double producedMerchantVolume = 0;
			double previousMerchantVolume = 0;
			double producedTAGVolume = 0;
			double previousTAGVolume = 0;

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				// c0.add (new Integer (date));
				c0.add(date);

				if (normalVolumeMode) {
					double bfVolume = ((VProvider) methodProvider).getV(stand, trees);

					if (stand.isInterventionResult()) {
						producedBFVolume += Math.max(0., previousBFVolume - bfVolume);
					}
					previousBFVolume = bfVolume;
					c1.add(formatNumber(producedBFVolume * coefHa, NUMBER_DIGITS));
				}

				if (merchantVolumeMode) {
					double merchantVolume = ((VMerchantProviderWithName) methodProvider).getVMerchant(stand, trees);
					if (stand.isInterventionResult()) {
						producedMerchantVolume += Math.max(0., previousMerchantVolume - merchantVolume);
					}
					previousMerchantVolume = merchantVolume;

					c2.add(formatNumber(producedMerchantVolume * coefHa, NUMBER_DIGITS));
				}

				if (totalVolumeMode) {
					double totalAGVolume = ((TotalAboveGroundVolumeProvider) methodProvider)
							.getTotalAboveGroundVolume(stand, trees);
					if (stand.isInterventionResult()) {
						producedTAGVolume += Math.max(0., previousTAGVolume - totalAGVolume);
					}
					previousTAGVolume = totalAGVolume;

					if (isSet("DETimeProducedV.prodv_totag")) {
						c3.add(formatNumber(producedTAGVolume * coefHa, NUMBER_DIGITS));
					}
					if (isSet("DETimeProducedV.totalv_totag")) {
						c4.add(formatNumber((producedTAGVolume + totalAGVolume) * coefHa, NUMBER_DIGITS));
					}
				}

				if (logVolumeMode) {
					double logVolume = ((VLogProvider) methodProvider).getTotalStandLogWood(stand, trees);
					producedLogVolume += logVolume;

					c5.add(formatNumber(producedLogVolume * coefHa, NUMBER_DIGITS));
				}

			}

			curves.clear();
			labels.clear();
			addSerie(null, c0); // no x labels

			if (normalVolumeMode) {
				String n1;
				if (methodProvider instanceof VProviderWithName) {
					n1 = ((VProviderWithName) methodProvider).getVolumeName();
				} else {
					n1 = Translator.swap("DETimeProducedV.y1Label");
				}
				addSerie(n1, c1);
			}

			if (merchantVolumeMode) {
				String n2 = ((VMerchantProviderWithName) methodProvider).getVMerchantName();
				addSerie(n2, c2);
			}
			if (totalVolumeMode) {
				if (isSet("DETimeProducedV.prodv_totag")) {
					addSerie(Translator.swap("DETimeProducedV.y3Label"), c3);
				}
				if (isSet("DETimeProducedV.totalv_totag")) {
					addSerie(Translator.swap("DETimeProducedV.y4Label"), c4);
				}
			}
			if (logVolumeMode) {
				addSerie(Translator.swap("DETimeProducedV.y5Label"), c5);
			}
		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeProducedV.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * Add a data serie to curves and labels. label may be null for x coordinates.
	 */
	private void addSerie(String label, Vector coordinates) {
		curves.add(coordinates);
		Vector yLabels = new Vector();
		if (label != null) {
			yLabels.add(label);
		}
		labels.add(yLabels);
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeProducedV.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeProducedV.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeProducedV.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

	@Override
	public List<List<? extends Number>> getCurves() { // fc-22.9.2021
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

	private double formatNumber(double number, int decimal) {
		double factor = Math.pow(10d, decimal);
		double result = Math.round(number * factor) / factor;
		return result;
	}

}
