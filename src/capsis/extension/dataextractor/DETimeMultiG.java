/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2012  Thomas Bronner
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

import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.superclass.DETimeYs;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.methodprovider.GCoppiceProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.GStandardsProvider;

/**
 * Separate basal area over time.
 * 
 * @author T. Bronner - June 2012
 */
public class DETimeMultiG extends DETimeYs {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeMultiG");
	}

	// nb-06.08.2018
	//public static final String NAME = "DETimeMultiG";
	//public static final String DESCRIPTION = "DETimeMultiG.descritpion";
	//public static final String AUTHOR = "T. Bronner";
	//public static final String VERSION = "1.0";

	private static final String[] GType = new String[] { "G", "GStandards", "GCoppice", "GSumBS" };

	public static boolean matchWith(Object referent) {
		if (!(referent instanceof GModel))
			return false;

		// fc-2.8.2013 changed compatibility with X. Morin
		MethodProvider mp = ((GModel) referent).getMethodProvider();
		if ((mp instanceof GCoppiceProvider || mp instanceof GStandardsProvider) && mp instanceof GProvider)
			return true;

		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("DETimeMultiG.name");
	}

	@Override
	public String getAuthor() {
		return "T. Bronner";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DETimeMultiG.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public String[] getYAxisVariableNames() {
		String[] translatedNames = new String[GType.length];
		for (int i = 0; i < GType.length; i++) {
			translatedNames[i] = Translator.swap(GType[i]);
		}
		return translatedNames;
	}

	@Override
	public void setConfigProperties() {
		for (int i = 0; i < GType.length; i++) {
			addBooleanProperty(GType[i], true);
		}
		addBooleanProperty("perHa", true);
	}

	@Override
	protected String getYLabel() {
		if (isSet("perHa")) {
			return "G m2/ha";
		} else {
			return "G m2";
		}
	}

	@Override
	protected Number getValue(GModel m, GScene stand, int date, int i) {
		MethodProvider mp = m.getMethodProvider();
		double value = Double.NaN;
		switch (i) {
		case 0:// G
			if (isSet(GType[0]) && mp instanceof GProvider) {
				value = ((GProvider) mp).getG(stand, null);
			}
			break;
		case 1:// "GStandards"
			if (isSet(GType[1]) && mp instanceof GStandardsProvider) {
				value = ((GStandardsProvider) mp).getStandardsG(stand, null);
			}
			break;
		case 2:// "GCoppice"
			if (isSet(GType[2]) && mp instanceof GCoppiceProvider) {
				value = ((GCoppiceProvider) mp).getCoppiceG(stand, null);
			}
			break;
		case 3:// "GStandards+GCoppice (should be = GStandards)"
			if (isSet(GType[3]) && mp instanceof GStandardsProvider && mp instanceof GCoppiceProvider) {
				value = ((GStandardsProvider) mp).getStandardsG(stand, null)
						+ ((GCoppiceProvider) mp).getCoppiceG(stand, null);
			}
			break;
		}
		if (isSet("perHa")) {
			// fc-2.6.2020
			value /= getSceneArea(stand) / 10000;
//			value /= stand.getArea() / 10000;
		}
		return value;
	}

}
