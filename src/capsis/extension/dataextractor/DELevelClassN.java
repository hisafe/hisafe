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
import capsis.util.methodprovider.LevelProvider;

/**
 * Numbers of trees per level classes.
 * 
 * @author S. Turbis - November 2000
 */
public class DELevelClassN extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DELevelClassN");
	}

	private Vector labels;

	/**
	 * Constructor.
	 */
	public DELevelClassN() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DELevelClassN(GenericExtensionStarter s) {
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
				return false;
			}
			// if (!(tc.getTrees ().iterator ().next () instanceof MaddTree))
			// {return false;}
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return false;
			}
			if (!(t instanceof LevelProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELevelClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DELevelClassN.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "S. Turbis";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DELevelClassN.description");
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
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes Numbers of trees per level
	 * classes.
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

		// System.out.println ("DELevelClassN : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			int n1 = 0; // Dominant (D)
			int n2 = 0; // Codominant (C)
			int n3 = 0; // Intermediate (I)
			int n4 = 0; // Oppressed (O)
			int n5 = 0; // Veteran (V)

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: Dominant, ...)

			// Consider restriction to one particular group if needed
			GScene stand = step.getScene();
			Collection trees = doFilter(stand);

			for (Iterator i = trees.iterator(); i.hasNext();) {
				Tree t = (Tree) i.next();
				// PreTree t = (PreTree) i.next (); // for the moment until I do
				// an interface
				LevelProvider t2 = (LevelProvider) t;
				// classes numbers
				// MaddSpecies s = (MaddSpecies) t.getSpecies ();
				char level = t2.getLevel();
				if (level == 'D') {
					n1++;
				} else if (level == 'C') {
					n2++;
				} else if (level == 'I') {
					n3++;
				} else if (level == 'O') {
					n4++;
				} else if (level == 'V') {
					n5++;
				}
			}

			c1.add(new Integer(1));
			c1.add(new Integer(2));
			c1.add(new Integer(3));
			c1.add(new Integer(4));
			c1.add(new Integer(5));

			c2.add(new Integer((int) (n1 * coefHa)));
			c2.add(new Integer((int) (n2 * coefHa)));
			c2.add(new Integer((int) (n3 * coefHa)));
			c2.add(new Integer((int) (n4 * coefHa)));
			c2.add(new Integer((int) (n5 * coefHa)));

			l1.add(Translator.swap("DELevelClassN.Dominant"));
			l1.add(Translator.swap("DELevelClassN.Codominant"));
			l1.add(Translator.swap("DELevelClassN.Intermediate"));
			l1.add(Translator.swap("DELevelClassN.Oppressed"));
			l1.add(Translator.swap("DELevelClassN.Veteran"));

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DELevelClassN.doExtraction ()", "Exception caught : ", exc);
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
		// v.add (Translator.swap (Translator.swap
		// (MaddSpecies.getStaticPropertyName ())));
		// v.add (labels);
		v.add(Translator.swap("DELevelClassN.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DELevelClassN.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DELevelClassN.yLabel"));
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
