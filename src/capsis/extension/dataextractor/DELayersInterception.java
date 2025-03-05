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

import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.LayerEnergyProvider;

/**
 * Energy intercepted by each canopy layer.
 * 
 * @author B. Courbaud - March 2002
 */
public class DELayersInterception extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DELayersInterception");
	}

	private Vector labels;

	/**
	 * Constructor.
	 */
	public DELayersInterception() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DELayersInterception(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;

			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof LayerEnergyProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELayersInterception.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DELayersInterception.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DELayersInterception.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.0";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
//		addConfigProperty(PaleoDataExtractor.PERCENTAGE);

		addBooleanProperty("percentage", false); // fc-23.8.2021
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

		// System.out.println ("DELayersInterception : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;

			// fc-23.8.2021 replaced PERCENTAGE
//			if (!settings.percentage && isSet("perHectare")) {
			if (isSet("perHectare") && !isSet("percentage"))
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			methodProvider = step.getProject().getModel().getMethodProvider();
			Vector energyVector = ((LayerEnergyProvider) methodProvider).getLayerEnergy(step.getScene());

			double totalEnergy = ((Double) energyVector.get(0)).doubleValue();
			double layer1Energy = ((Double) energyVector.get(1)).doubleValue();
			double layer2Energy = ((Double) energyVector.get(2)).doubleValue();
			double layer3Energy = ((Double) energyVector.get(3)).doubleValue();
			double layer4Energy = ((Double) energyVector.get(4)).doubleValue();
			double soilEnergy = ((Double) energyVector.get(5)).doubleValue();

			if (isSet("percentage")) {
//			if (settings.percentage) {
				layer1Energy = layer1Energy * 100 / totalEnergy;
				layer2Energy = layer2Energy * 100 / totalEnergy;
				layer3Energy = layer3Energy * 100 / totalEnergy;
				layer4Energy = layer4Energy * 100 / totalEnergy;
				soilEnergy = soilEnergy * 100 / totalEnergy;
			}

			// Consider restriction to one particular group if needed
			// GPlot plot = step.getStand ().getPlot ();

			c1.add(new Integer(1));
			c1.add(new Integer(2));
			c1.add(new Integer(3));
			c1.add(new Integer(4));
			c1.add(new Integer(5));

			c2.add(new Double(layer1Energy * coefHa));
			c2.add(new Double(layer2Energy * coefHa));
			c2.add(new Double(layer3Energy * coefHa));
			c2.add(new Double(layer4Energy * coefHa));
			c2.add(new Double(soilEnergy * coefHa));

			l1.add(Translator.swap("DELayersInterception.layer1"));
			l1.add(Translator.swap("DELayersInterception.layer2"));
			l1.add(Translator.swap("DELayersInterception.layer3"));
			l1.add(Translator.swap("DELayersInterception.layer4"));
			l1.add(Translator.swap("DELayersInterception.soil"));

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DELayersInterception.doExtraction ()", "Exception: ", exc);
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
		v.add(Translator.swap("DELayersInterception.xLabel"));

		// fc-29.7.2021 Check ha first to prevent ha and % to appear together (title
		// bar)
		// fc-23.8.2021 replaced PERCENTAGE
		if (isSet("perHectare")) {
			v.add(Translator.swap("DELayersInterception.yLabel") + " (ha)");
		} else if (isSet("percentage")) {
//		if (settings.percentage) {
			v.add(Translator.swap("DELayersInterception.yLabel") + " (%)");
		} else {
			v.add(Translator.swap("DELayersInterception.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
