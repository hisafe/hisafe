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

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.modeltool.rockfornet.Rockfornet;
import capsis.extension.modeltool.rockfornet.RockfornetResult;
import capsis.extension.modeltool.rockfornet.RockfornetSettings;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.RockfornetStand;

/**
 * Rockfornet Probable Residual Rockfall Hazard over time.
 * 
 * @author F. de Coligny - July 2007
 */
public class DETimeRockfornetPRRH extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRockfornetPRRH");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	private Map rockTypeMap;
	private Map rockShapeMap;

	/**
	 * Constructor.
	 */
	public DETimeRockfornetPRRH() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeRockfornetPRRH(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			initMaps();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRockfornetPRRH.c ()", "Exception during construction", e);
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
			GScene std = ((Step) m.getProject().getRoot()).getScene();
			if (!(std instanceof RockfornetStand))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRockfornetPRRH.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeRockfornetPRRH.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeRockfornetPRRH.description");
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

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// ~ addConfigProperty (DataExtractor.HECTARE);
		// ~ addConfigProperty (DataExtractor.TREE_GROUP); // group
		// multiconfiguration
		// ~ addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration

		initMaps();

		// fc-14.9.2021 Note: Removed the ordered properties (starting with o- wil have
		// no particular effect), reviewed the translation files to get a correct user
		// interface in configuration panel

		addDoubleProperty("o-1-0-rockDiameter1", 0);
		addDoubleProperty("o-1-1-rockDiameter2", 0);
		addDoubleProperty("o-1-2-rockDiameter3", 0);

		LinkedList list1 = new LinkedList(rockTypeMap.keySet());
		addComboProperty("o-4-0-rockType", list1);

		LinkedList list2 = new LinkedList(rockShapeMap.keySet());
		addComboProperty("o-5-0-rockShape", list2);

		addDoubleProperty("o-6-0-slope", 0);
		addDoubleProperty("o-7-0-heightCliff", 0);
		addDoubleProperty("o-8-0-lengthForestedSlope", 0);
		addDoubleProperty("o-9-0-lengthNonForestedSlope", 0);

	}

	private void initMaps() {

		// fc+bc-20.9.2016 review: keys must not be translated in
		// comboProperties
		rockTypeMap = new HashMap();
		rockTypeMap.put("DETimeRockfornetPRRH.granite", 2800);
		rockTypeMap.put("DETimeRockfornetPRRH.basalte", 2900);
		rockTypeMap.put("DETimeRockfornetPRRH.amphibolite", 3000);
		rockTypeMap.put("DETimeRockfornetPRRH.diorite", 2850);
		rockTypeMap.put("DETimeRockfornetPRRH.dolomite", 2700);
		rockTypeMap.put("DETimeRockfornetPRRH.gneiss", 2800);
		rockTypeMap.put("DETimeRockfornetPRRH.gypsum", 2300);
		rockTypeMap.put("DETimeRockfornetPRRH.limestone", 2500);
		rockTypeMap.put("DETimeRockfornetPRRH.marlstone", 2400);
		rockTypeMap.put("DETimeRockfornetPRRH.micaschist", 2700);
		rockTypeMap.put("DETimeRockfornetPRRH.sandstone", 2500);
		rockTypeMap.put("DETimeRockfornetPRRH.shale", 2450);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.granite"),
		// 2800);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.basalte"),
		// 2900);
		// rockTypeMap.put (Translator.swap
		// ("DETimeRockfornetPRRH.amphibolite"), 3000);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.diorite"),
		// 2850);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.dolomite"),
		// 2700);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.gneiss"),
		// 2800);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.gypsum"),
		// 2300);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.limestone"),
		// 2500);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.marlstone"),
		// 2400);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.micaschist"),
		// 2700);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.sandstone"),
		// 2500);
		// rockTypeMap.put (Translator.swap ("DETimeRockfornetPRRH.shale"),
		// 2450);

		rockShapeMap = new HashMap();
		rockShapeMap.put("DETimeRockfornetPRRH.sphere", RockfornetSettings.SPHERE);
		rockShapeMap.put("DETimeRockfornetPRRH.disc", RockfornetSettings.DISC);
		rockShapeMap.put("DETimeRockfornetPRRH.ellipsoid", RockfornetSettings.ELLIPSOID);
		rockShapeMap.put("DETimeRockfornetPRRH.rectangular", RockfornetSettings.RECTANGULAR);
		// rockShapeMap.put (Translator.swap ("DETimeRockfornetPRRH.sphere"),
		// RockfornetSettings.SPHERE);
		// rockShapeMap.put (Translator.swap ("DETimeRockfornetPRRH.disc"),
		// RockfornetSettings.DISC);
		// rockShapeMap.put (Translator.swap ("DETimeRockfornetPRRH.ellipsoid"),
		// RockfornetSettings.ELLIPSOID);
		// rockShapeMap.put (Translator.swap
		// ("DETimeRockfornetPRRH.rectangular"),
		// RockfornetSettings.RECTANGULAR);

	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the basal area of the stand versus date
	 * from the root Step to this one.
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
			// per Ha computation
			// ~ double coefHa = 1;
			// ~ if (isSet("perHectare")) {
			// ~ coefHa = 10000 / getSceneArea(step.getStand ()); // fc-2.6.2020
			// ~ }

			double rockDiameter1 = getDoubleProperty("o-1-0-rockDiameter1");
			double rockDiameter2 = getDoubleProperty("o-1-1-rockDiameter2");
			double rockDiameter3 = getDoubleProperty("o-1-2-rockDiameter3");

			String rockTypeName = getComboProperty("o-4-0-rockType");
			System.out.println("derockfornet: combo rockTypeName=" + rockTypeName);
			if (rockTypeName == null) {
				curves = null;
				return true;
			}

			String rockShapeName = getComboProperty("o-5-0-rockShape");
			System.out.println("derockfornet: combo rockShapeName=" + rockShapeName);
			if (rockShapeName == null) {
				curves = null;
				return true;
			}

			double slope = getDoubleProperty("o-6-0-slope");
			double heightCliff = getDoubleProperty("o-7-0-heightCliff");
			double lengthForestedSlope = getDoubleProperty("o-8-0-lengthForestedSlope");
			double lengthNonForestedSlope = getDoubleProperty("o-9-0-lengthNonForestedSlope");

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				RockfornetStand std = (RockfornetStand) stand;
				// ~ Collection trees = doFilter (stand); // fc - 5.4.2004

				// Rockfornet calculation for this stand
				RockfornetSettings settings = new RockfornetSettings(std);
				settings.rockDiameter1 = rockDiameter1;
				settings.rockDiameter2 = rockDiameter2;
				settings.rockDiameter3 = rockDiameter3;
				settings.rockType = (Integer) rockTypeMap.get(rockTypeName);
				settings.rockShape = (Integer) rockShapeMap.get(rockShapeName);
				settings.slope = slope;
				settings.heightCliff = heightCliff;
				settings.lengthForestedSlope = lengthForestedSlope;
				settings.lengthNonForestedSlope = lengthNonForestedSlope;
				Rockfornet r = new Rockfornet(settings);
				r.execute();
				RockfornetResult result = r.getResult();

				int date = stand.getDate();
				double prrh = result.getProbableResidualRockfallHazard();

				c1.add(new Integer(date));
				c2.add(new Double(prrh));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeRockfornetPRRH.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
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
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeRockfornetPRRH.xLabel"));
		// ~ if (isSet("perHectare")) {
		// ~ v.add (Translator.swap ("DETimeRockfornetPRRH.yLabel")+" (ha)");
		// ~ } else {
		v.add(Translator.swap("DETimeRockfornetPRRH.yLabel"));
		// ~ }
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1;
	}

}
