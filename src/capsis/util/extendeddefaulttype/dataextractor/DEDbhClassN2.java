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

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.DEDbhClassN;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.NProvider;
import jeeb.lib.util.Log;
import repicea.simulation.covariateproviders.plotlevel.TreeStatusCollectionsProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * Numbers of trees per diameter classes.
 * This extractor has been modified from its original version (DEDbhClassN v1.5, author F. de Coligny) to use the NProvider
 * 
 * @author M. Fortin - August 2009
 */
public class DEDbhClassN2 extends DEDbhClassN { // fc - 30.4.2003 - added classWidth / minThreshold properties
	
	/**
	 * This enum variable provides the different labels for the graph.
	 * @author Mathieu Fortin - April 2012
	 */
	public static enum MessageID implements TextableEnum {
		xLabel("Diameter (cm)", "Diam\u00E8tre (cm)"),
		yLabel("N", "N"),
		Title("Diameter distribution", 
				"Distribution des diam\u00E8tres"),
		Description("Tree frequencies per diameter classes", 
				"Fr\u00E9quences des arbres par classes de diam\u00E8tre"),
		Status("Status", "Etat")		
		;

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
	}

	public static final int MAX_FRACTION_DIGITS = 2;
	public static final int MAX_NB_CLASSES = 200;

	private List<List<String>> labels;
	protected NumberFormat formater;

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public DEDbhClassN2() {}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DEDbhClassN2(GenericExtensionStarter s) {
		super(s);
		methodProvider = s.getStep().getProject().getModel().getMethodProvider();				// instanciate the methodprovider member
		labels = new ArrayList<List<String>> ();

		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance ();
		formater.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
	}

	@Override
	public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof NProvider)) {return false;}					// the extractor requires the NProvider which implements the different methods to calculate the number of trees
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			if (!(s instanceof TreeStatusCollectionsProvider)) {return false;}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DEDbhClassN.matchWith ()", "Error in matchWith () (returned false)", e);
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
	public String getAuthor () {return "M. Fortin";}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {return REpiceaTranslator.getString(MessageID.Description);}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "2.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "2.0";

	// nb-15.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	@Override
	public void setConfigProperties () {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
//		addConfigProperty (PaleoDataExtractor.STATUS);		// fc - 22.4.2004
		addConfigProperty (PaleoDataExtractor.TREE_GROUP);
		addConfigProperty (PaleoDataExtractor.I_TREE_GROUP);		// group individual configuration
		addDoubleProperty ("classWidthInCm", 5d);
		addDoubleProperty ("minThresholdInCm", 0d);
		addBooleanProperty ("centerClasses");
		setPropertyEnabled("centerClasses", false);
		addBooleanProperty ("roundN");	// fc - 22.8.2006 - n may be double (NZ1) -> round becomes an option
		setPropertyEnabled("roundN", false);
//		LinkedList list = new LinkedList();
//		list = new LinkedList();
//		for (StatusClass var : StatusClass.values()) {
//			list.add(var.toString());
//		}
//		addComboProperty(MessageID.Status.toString(), list);
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
				coefHa = 10000 / step.getScene().getArea ();
			}

			double minThreshold = getDoubleProperty ("minThresholdInCm");
			double classWidth = getDoubleProperty ("classWidthInCm");

			// Security
			if (classWidth <= 0d) {classWidth = 1d;}

//			double shift = 0;
//			if (isSet ("centerClasses")) {shift = classWidth/2;}

			Vector<Double> c1 = new Vector<Double>();		// x coordinates
			Vector<Double> c2 = new Vector<Double> ();		// y coordinates
			Vector<String> l1 = new Vector<String> ();		// labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			Collection aux = doFilter (step.getScene ());					// collection of trees with respect to the filter (grouper)

			// Array of collections for the different diameter class;
			Collection<Tree>[] collectionsByDiameterClass = new Collection[DEDbhClassN2.MAX_NB_CLASSES];

			// instanciate all the collections in the array;
			for (int i = 0; i < collectionsByDiameterClass.length; i++ ) {
				collectionsByDiameterClass[i] = new ArrayList<Tree>();
			};

			// Limited in size! (java initializes each value to 0)
//			double tab[] = new double[DEDbhClassN2.MAX_NB_CLASSES];	// fc - 22.8.2006
			int maxCat = 0;
			int minCat = 200;

			for (Iterator trees = aux.iterator(); trees.hasNext();) {
				Tree t = (Tree) trees.next();
				
				// bug correction : some maid trees may have number == 0 -> ignore them - tl 12/09/2005
				if ((t instanceof Numberable) && (((Numberable) t).getNumber ()==0)) {	
					continue; 				// next iteration
				}

				double d = t.getDbh ();		// dbh : cm

				if (d < minThreshold) {		// fc - 9.4.2003
					continue;
				}	

				int category = (int) ((d-minThreshold) / classWidth);
				collectionsByDiameterClass[category].add(t);			// add the tree to the appropriate collection
	
				if (category > maxCat) {maxCat = category;}
				if (category < minCat) {minCat = category;}
			}

			for (int i = minCat; i <= maxCat; i++) {
				c1.add((i + .5) * classWidth + minThreshold);	// fc - 7.8.2003 - bug correction (from PVallet)
				double numberProvided = ((NProvider) this.methodProvider).getN(step.getScene (), collectionsByDiameterClass[i]);
				// New option: if (roundN), N is rounded to the nearest int
				double numbers = 0.0f;
				if (isSet ("roundN")) {
					numbers = (int) (numberProvided * coefHa + 0.5);	// fc - 29.9.2004 : +0.5 (sp)
				} else {
					numbers = numberProvided * coefHa;	// fc - 22.8.2006 - Numberable is now double
				}
				c2.add (new Double (numbers));
				// fc - 22.8.2006 - Numberable is now double

				double classBase = minThreshold + i*classWidth;
				l1.add (""+formater.format (classBase)+"-"+formater.format ((classBase+classWidth)));
			}

			curves.clear ();
			curves.add (c1);
			curves.add (c2);

			labels.clear ();
			labels.add (l1);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "DEDbhClassN.doExtraction ()", "Exception caught : ",exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/*
	 * This method has been overriden in order to allow the grouping on the status class (non-Javadoc)
	 * @see capsis.extension.AbstractDataExtractor#doFilter(capsis.kernel.GScene, java.lang.String)
	 */
	@Override
	public Collection doFilter (GScene stand, GroupableType type) { 

		Collection living = new ArrayList();
		Collection all = new ArrayList();
		
		for (StatusClass status : StatusClass.values()) {
			Collection currentStatusCollection = ((TreeStatusCollectionsProvider) stand).getTrees(status);
			all.addAll(currentStatusCollection);
			if (status == StatusClass.alive) {
				living.addAll(currentStatusCollection);
			}
		}
		
		if (!isGrouperMode ()) {return living;} // by default

		GrouperManager gm = GrouperManager.getInstance ();
		Grouper g = gm.getGrouper (getGrouperName ()); // if group not found,
														// return a DummyGrouper

		// fc-16.11.2011 - use a copy of the grouper (several data extractors
		// are updated in several threads, avoid concurrence problems)
		Grouper copy = g.getCopy ();

		Collection output = copy.apply (all, getGrouperName ().toLowerCase ().startsWith ("not ")); // the
																									// unchanged
		return output;
	}

//	@Override
//	public Collection doFilter (GScene stand) { // fc -17.9.2004
//		StatusClass statusClass = AbstractDETime.getStatus(getComboProperty(MessageID.Status.toString()));
////		System.out.println("Status set to " + statusClass.toString());
//		if (statusClass == StatusClass.alive) {
//			if (settings.c_grouperType == null) {
//				Log.println (Log.ERROR, "DataExtractor.doFilter (stand)", "settings.c_grouperType == null, used TREE instead");
//				settings.c_grouperType = TreeList.GROUP_ALIVE_TREE;
//			}
//			return doFilter(stand, settings.c_grouperType);
//		} else {
//			return ((TreeStatusCollectionsProvider) stand).getTrees(statusClass);
//		}
//	}

	/**
	 * From DFCurves interface.
	 */
	@Override
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
	@Override
	public List<List<String>> getLabels () {
		return labels;
	}

}

