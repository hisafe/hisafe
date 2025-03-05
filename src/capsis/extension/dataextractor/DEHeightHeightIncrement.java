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
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.HeightIncrementProvider;

/**
 * Tree diameter increment diameter scatterplot.
 * 
 * @author B. Courbaud - May 2002
 */
public class DEHeightHeightIncrement extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEHeightHeightIncrement");
	}

	private Vector labels;

	/**
	 * Constructor.
	 */
	public DEHeightHeightIncrement() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEHeightHeightIncrement(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

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
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {
				return true;
			} // bare soil problem
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return false;
			}
			if (!(t instanceof HeightIncrementProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEHeightHeightIncrement.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEHeightHeightIncrement.name");
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
		return Translator.swap("DEHeightHeightIncrement.description");
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
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes tree height diameter
	 * scatterplot.
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

		// System.out.println
		// ("DEHeightHeightIncrement : extraction being made");

		try {
			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// Restriction to a group if needed
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Create output data
			while (trees.hasNext()) {
				Tree t = (Tree) trees.next();
				double h = t.getHeight(); // h : m
				HeightIncrementProvider t2 = (HeightIncrementProvider) t;
				double dh = t2.getHeightIncrement(); // h increment in m

				c1.add(new Double(h));
				c2.add(new Double(dh));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEHeightHeightIncrement.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DEHeightHeightIncrement.xLabel"));
		v.add(Translator.swap("DEHeightHeightIncrement.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

}
