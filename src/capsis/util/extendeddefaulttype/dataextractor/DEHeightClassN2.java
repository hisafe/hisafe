/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype.dataextractor;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.DETimeG;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.methodprovider.NProvider;

/**
 * Numbers of trees per height class.
 * This class is a modified version of the original DEHeightClassN (author B. Courbaud). It is based on the NProvider.
 * @author M. Fortin - August 2009
 */
public class DEHeightClassN2 extends DETimeG {

	/**
	 * This enum variable provides the different labels for the graph.
	 * @author Mathieu Fortin - April 2012
	 */
	public static enum MessageID implements TextableEnum {
		xLabel("H (m)", "H (m)"),
		yLabel("N", "N"),
		Title("Height distribution", 
				"Distribution des hauteurs"),
		Description("Tree frequencies per height classes", 
				"Fr\u00E9quences des arbres par classes de hauteur");

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
	}
	
	
	// fc - 30.4.2003 - added classWidth / minThreshold properties
	public static final int MAX_FRACTION_DIGITS = 2;
	public static final int MAX_NB_CLASSES = 200;

	private List<List<String>> labels;
	protected NumberFormat formater;


	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public DEHeightClassN2() {}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DEHeightClassN2(GenericExtensionStarter s) {
		super(s);
		labels = new ArrayList<List<String>>();
		methodProvider = s.getStep().getProject().getModel().getMethodProvider();				// instanciate the methodprovider member
		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance ();
		formater.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof ExtModel)) {return false;}
			ExtModel m = (ExtModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof NProvider)) {return false;}					// the extractor requires the NProvider which implements the different methods to calculate the number of trees
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "DEHeightClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return REpiceaTranslator.getString(MessageID.Title);
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "M. Fortin";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return REpiceaTranslator.getString(MessageID.Description);
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	// nb-15.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties () {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty (PaleoDataExtractor.STATUS);		// fc - 22.4.2004
		addConfigProperty (PaleoDataExtractor.TREE_GROUP);
		addConfigProperty (PaleoDataExtractor.I_TREE_GROUP);		// group individual configuration
		addDoubleProperty ("classWidthInM", 5d);
		addDoubleProperty ("minThresholdInM", 0d);
		addBooleanProperty ("centerClasses");
		addBooleanProperty ("roundN");	// fc - 22.8.2006 - n may be double (NZ1) -> round becomes an option
	}

	/**
	 * From DataExtractor SuperClass.
	 *
	 * Computes the data series. This is the real output building.
	 * It needs a particular Step.
	 * This extractor computes Numbers of trees per diameter classes.
	 *
	 * Return false if trouble while extracting.
	 */
	@SuppressWarnings("unchecked")
	public boolean doExtraction () {
		if (upToDate) {return true;}
		if (step == null) {return false;}

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / step.getScene ().getArea ();
			}

			double minThreshold = getDoubleProperty ("minThresholdInM");
			double classWidth = getDoubleProperty ("classWidthInM");

			// Security
			if (classWidth < 0d) {classWidth = 1d;}

			double shift = 0;
			if (isSet ("centerClasses")) {shift = classWidth/2;}

			Vector<Integer> c1 = new Vector<Integer> ();	// x coordinates
			Vector<Double> c2 = new Vector<Double>();		// y coordinates
			Vector<String> l1 = new Vector<String>();		// labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			Collection aux = doFilter (step.getScene ());
			
			// Array of collections for the different diameter class;
			Collection<Tree>[] collectionsByDiameterClass = new Collection[DEHeightClassN2.MAX_NB_CLASSES];

			// instanciate all the collections in the array;
			for (int i = 0; i < collectionsByDiameterClass.length; i++ ) {
				collectionsByDiameterClass[i] = new ArrayList<Tree>();
			};

			int maxCat = 0;
			int minCat = 200;

			// Create output data
			for (Iterator trees = aux.iterator(); trees.hasNext();) {
				Tree t = (Tree) trees.next ();
				// bug correction : some maid trees may have number == 0 -> ignore them - tl 12/09/2005
				if ((t instanceof Numberable) && (((Numberable) t).getNumber ()==0)) continue; // next iteration

				double height = t.getHeight ();		// height : m

				if (height < minThreshold) {continue;}	// fc - 9.4.2003

				int category = (int) ((height-shift) / classWidth);
				collectionsByDiameterClass[category].add(t);			// add the tree to the appropriate collection

				if (category > maxCat) {maxCat = category;}
				if (category < minCat) {minCat = category;}
			}

			for (int i = minCat; i <= maxCat; i++) {
				//~ c1.add (new Integer (anchor++));
				c1.add(new Integer (i));	// fc - 7.8.2003 - bug correction (from PVallet)
				double numberProvided = ((NProvider) this.methodProvider).getN(step.getScene (), collectionsByDiameterClass[i]);
				// New option: if (roundN), N is rounded to the nearest int
				double numbers = 0.0f;
				if (isSet ("roundN")) {
					numbers = (int) (numberProvided * coefHa + 0.5);	// fc - 29.9.2004 : +0.5 (sp)
				} else {
					numbers = numberProvided * coefHa;	// fc - 22.8.2006 - Numberable is now double
				}
				c2.add (new Double (numbers));

				double classBase = shift + i*classWidth;
				l1.add (""+formater.format (classBase)+"-"+formater.format ((classBase+classWidth)));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "DEHeightClassN.doExtraction ()", "Exception caught : ",exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames () {
		Vector<String> v = new Vector<String> ();
		v.add(REpiceaTranslator.getString(MessageID.xLabel));
		if (isSet("perHectare")) {
			v.add (REpiceaTranslator.getString(MessageID.yLabel) + " / ha");
		} else {
			v.add (REpiceaTranslator.getString(MessageID.yLabel));
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
