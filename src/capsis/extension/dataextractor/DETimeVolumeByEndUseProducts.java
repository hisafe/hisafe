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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import lerfob.carbonbalancetool.productionlines.EndUseWoodProductCarbonUnitFeature.UseClass;
import capsis.extension.dataextractor.superclass.DEMultiTimeX;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.util.methodprovider.VolumeByEndProductsProvider;

/**
 * Volumes by end use product categories against time.
 * 
 * @author M. Fortin - August 2010
 */
public class DETimeVolumeByEndUseProducts extends DEMultiTimeX {

	
	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeVolumeByEndUseProducts");
	}

	// nb-07.08.2018
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR = "M. Fortin";

	private Map<GScene, Map<UseClass, Double>> products;

	// private VolumeByEndProductsProvider provider;

	@Override
	public void init(GModel m, Step s) throws Exception {
		super.init(m, s);
		// provider = (VolumeByEndProductsProvider) m.getMethodProvider();
		products = new HashMap<GScene, Map<UseClass, Double>>();
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public static boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof VolumeByEndProductsProvider))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVolumeByLogCategories.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}
	
	@Override
	public String getName() {
		return Translator.swap("DETimeVolumeByEndUseProducts.name");
	}

	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DETimeVolumeByEndUseProducts.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public boolean doExtraction() {
		if (!upToDate) {
			products = new HashMap<GScene, Map<UseClass, Double>>();
		}
		return super.doExtraction();
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(STATUS);
		addConfigProperty(TREE_GROUP);
		addConfigProperty(I_TREE_GROUP); // group individual configuration
	}

	@Override
	public List<String> getAxesNames() {
		List<String> v = new ArrayList<String>();
		v.add(Translator.swap("DETimeVolumeByEndUseProducts.xLabel"));

		if (isSet("perHectare")) {
			v.add(getYLabel() + " / ha)");
		} else {
			v.add(getYLabel() + ")");
		}

		return v;
	}

	@SuppressWarnings("unchecked")
	protected List<Object> getTypes(GScene stand) {
		try {
			// List<UseClass> list = provider.getEndProductClasses(stand);
			// list.remove(UseClass.NONE);
			// return (List) list;
			return null;
		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVolumeByLogCategory.getTypes",
					"Error while retrieving log categories from tree logger", e);
			return null;
		}
	}

	@SuppressWarnings({ "unchecked", "unused" })
	protected Number getValue(GModel m, GScene stand, int typeId, Object type) {
		double areaFactor = 1d;
		if (isSet("perHectare")) {
			areaFactor = 10000d / getSceneArea(stand); // fc-2.6.2020
//			areaFactor = 10000d / stand.getArea();
		}

		Collection trees = doFilter(stand);

		if (!products.containsKey(stand)) {
			// products.put(stand,
			// provider.getVolumeByEndUseProductClasses(stand, trees));
		}

		if (products.get(stand).containsKey((UseClass) type)) {
			return products.get(stand).get((UseClass) type) * areaFactor;
		} else {
			return 0d;
		}
	}

}
