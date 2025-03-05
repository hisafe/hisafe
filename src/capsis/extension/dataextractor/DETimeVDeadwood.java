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
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.VProvider;

/**
 * Volume (living, fresh standing deadwood, decayed deadwood) versus Date.
 * 
 * @author G. Ligot - October 2011
 */
public class DETimeVDeadwood extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeVDeadwood");
	}

	protected List<List<? extends Number>> curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeVDeadwood() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeVDeadwood(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector<String>();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVDeadwood.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {


		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeVDeadwood.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "G. Ligot";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeVDeadwood.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		// addConfigProperty (PaleoDataExtractor.TREE_GROUP);
		// addConfigProperty (PaleoDataExtractor.I_TREE_GROUP); // group
		// individual configuration

		addBooleanProperty("livingVolume", true);
		addBooleanProperty("totalVolume");
		addBooleanProperty("freshDeadwoodVolume");
		addBooleanProperty("decayedDeadwoodVolume");
		addBooleanProperty("totalDeadwoodVolume");
	}

	/**
	 * From DataExtractor SuperClass. Computes the data series. This is the real
	 * output building. It needs a particular Step. Return false if trouble
	 * while extracting.
	 */
	public boolean doExtraction() {
		
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeVDeadwood.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeVDeadwood.yLabel") + " (perHa)");
		} else {
			v.add(Translator.swap("DETimeVDeadwood.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public int getNY() {
		return curves.size() - 1;
	}

	@Override
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<List<String>> getLabels() {
		return labels;
	}

}
