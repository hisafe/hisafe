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

import java.lang.reflect.Method;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.QualitativeProperty;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.group.Group;

/**
 * Numbers of individuals per values of one given EnumProperty.
 * 
 * @author F. de Coligny - September 2004
 */
public class DEEnumPropertyClassN extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEEnumPropertyClassN");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected Vector curves;
	protected Vector labels;
	protected NumberFormat formater;

	private Map methodName2accessor; // LubTree1.getQPSpecies -> getQPSpecies ()
										// of LubTree1 (another entry for
										// LubTree2)
	private Map propName2methName; // "Espece" -> "getQPSpecies" - fc -
									// 21.9.2005

	private QualitativeProperty qp;

	/**
	 * Constructor.
	 */
	public DEEnumPropertyClassN() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEEnumPropertyClassN(GenericExtensionStarter s) {
		super(s);

		curves = new Vector();
		labels = new Vector();

		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance();
		formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);

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

			// fc-11.10.2021 Checked (returning true could also be considered here)
			if (tc.getTrees().isEmpty())
				return false;
			
			createMethodName2accessor(tc.getTrees());
			Collection accessors = methodName2accessor.keySet();
			if (accessors.isEmpty()) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEEnumPropertyClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEEnumPropertyClassN.name");
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
		return Translator.swap("DEEnumPropertyClassN.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.1";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() { // if begins with an i_ then in
										// individual
		// This method is called by super constructor
		createMethodName2accessor(((TreeCollection) step.getScene()).getTrees());
		removeComboProperty("methodName");

		// process keys to discard the className
		// process keys to discard the className
		// process keys to discard the className

		// ~ addComboProperty ("methodName", new LinkedList
		// (methodName2accessor.keySet ()));
		addComboProperty("methodName", new LinkedList(propName2methName.keySet())); // fc
																					// -
																					// 21.9.2005

		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		// Should accept all groups types compatible with referent

		addGroupProperty(TreeList.GROUP_ALIVE_TREE, PaleoDataExtractor.COMMON);
		addGroupProperty(TreeList.GROUP_ALIVE_TREE, PaleoDataExtractor.INDIVIDUAL);
		// // fc-16.9.2004
		// addGroupProperty(TreeList.GROUP_ALIVE_TREE,
		// PaleoDataExtractor.COMMON);
		// addGroupProperty(TreeList.GROUP_ALIVE_TREE,
		// PaleoDataExtractor.INDIVIDUAL);
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
		// methodProvider = step.getScenario ().getModel ().getMethodProvider
		// ();

		try {
			createMethodName2accessor(((TreeCollection) step.getScene()).getTrees());

			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) 
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			GScene stand = step.getScene();
			Collection trees = doFilter(stand, TreeList.GROUP_ALIVE_TREE); // fc - 16.9.2004

			Method method = null;

			String propName = getComboProperty("methodName");
			String methodName = (String) propName2methName.get(propName);
			// ~ Method method = (Method) methodName2accessor.get (propName);

			if (trees.size() > 0) {
				Iterator k = trees.iterator();
				Object indiv = k.next();

				Object result = null;
				try {
					method = (Method) methodName2accessor.get(indiv.getClass().getName() + "." + methodName);

					result = method.invoke(indiv); // fc - 2.12.2004 - varargs
				} catch (Exception e) {
					// May happen if several mosels are loaded -> interrupt
					throw new NoSuchMethodException();
				}
				qp = (QualitativeProperty) result;

				// Regelight returned null for some qp
				// This will result in "Check configuration"
				if (qp == null) {
					Log.println(Log.ERROR, "DEEnumPropertyClassN.doExtraction ()",
							"One individual returns a null QualitativeProperty...");
					Log.println(Log.ERROR, "DEEnumPropertyClassN.doExtraction ()", "Check: " + indiv);
				}

				Map possibleValues = qp.getValues(); // value -> name

				// make a conversion table for QP values -> indices from 0 to
				// possibleValues.size () - 1
				int convert[] = new int[possibleValues.size()];
				int m = 0;
				for (Iterator z = possibleValues.keySet().iterator(); z.hasNext();) {
					Integer i0 = (Integer) z.next();
					convert[m++] = i0.intValue();
				}

				int tab[] = new int[possibleValues.size()];

				Class previousClass = null;

				do {

					int userValue = qp.getValue();
					int index = getIndexValue(convert, userValue);

					if (indiv instanceof Numberable) {
						Numberable nu = (Numberable) indiv;
						tab[index] += nu.getNumber();
					} else {
						tab[index]++;
					}

					indiv = null;
					if (k.hasNext()) {
						indiv = k.next();
						// ~ System.out.println
						// ("DEEnumPropertyClassN: indiv type="+indiv.getClass
						// ().getName ());

						if (!indiv.getClass().equals(previousClass)) {
							method = (Method) methodName2accessor.get(indiv.getClass().getName() + "." + methodName);
							previousClass = indiv.getClass();
						}

						result = method.invoke(indiv); // fc - 2.12.2004 -
														// varargs
						qp = (QualitativeProperty) result;
					}

				} while (indiv != null);

				Iterator i = possibleValues.keySet().iterator();
				while (i.hasNext()) {
					Integer vI = (Integer) i.next();
					int vi = vI.intValue();

					int index = getIndexValue(convert, vi);
					int number = (int) (tab[index] * coefHa + 0.5);
					
					// fc-29.8.2019 better result if we omit 0 values on the distribution graph
					int n = new Integer(number);
					if (n <= 0)
						continue;
					
					c1.add(vI);
					c2.add(n);

					String name = (String) possibleValues.get(vI);
					l1.add(Translator.swap(name));

				}
				
				// fc-29.8.2019 former loop code before change
//				while (i.hasNext()) {
//					Integer vI = (Integer) i.next();
//					int vi = vI.intValue();
//					c1.add(vI);
//
//					int index = getIndexValue(convert, vi);
//					int number = (int) (tab[index] * coefHa + 0.5);
//					c2.add(new Integer(number));
//
//					String name = (String) possibleValues.get(vI);
//					l1.add(Translator.swap(name));
//
//				}
				
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (NoSuchMethodException exc) {
			return false; // not an error

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEEnumPropertyClassN.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	// Build methodName2accessor for the given individuals
	//
	private void createMethodName2accessor(Collection individuals) {
		
		// fa-09.07.2021: moved here from below, to allow for scene without any tree
		// TODO: !!! to be checked by Francois !!!
		methodName2accessor = new HashMap();
		propName2methName = new HashMap();
		
		// ~ System.out.println ("DEEnumPropertyClassN...");
		if (individuals == null || individuals.isEmpty()) {
			return;
		}

		// fa-09.07.2021: moved above
//		methodName2accessor = new HashMap();
//		propName2methName = new HashMap();

		// Get one representative per class in the collection
		Collection reps = Tools.getRepresentatives(individuals);

		for (Iterator k = reps.iterator(); k.hasNext();) {

			// ~ Object o = individuals.iterator ().next ();
			Object o = k.next();

			Collection accessors = Tools.getAccessors(o.getClass(), QualitativeProperty.class);
			for (Iterator i = accessors.iterator(); i.hasNext();) {
				Method m = (Method) i.next();

				String methodName = m.getName(); // "getQPSpecies"
				String propertyName = methodName; // default

				// ~ System.out.println ("methodName="+methodName);

				// try to get the name of the property
				try {
					Object result = m.invoke(o); // fc - 2.12.2004 - varargs
					qp = (QualitativeProperty) result;
					propertyName = Translator.swap(qp.getPropertyName()); // "Espece"
				} catch (Exception e) {
				}
				// ~ System.out.println ("propertyName="+propertyName);

				// "Espece" -> "getQPSpecies"
				propName2methName.put(propertyName, methodName);

				// LubTree1.getQPSpecies -> getQPSpecies() (of LubTree1)
				// LubTree2.getQPSpecies -> getQPSpecies() (of LubTree2)
				methodName2accessor.put(o.getClass().getName() + "." + methodName, m);
			}
		}
		return;
	}

	// Conversion user -> index
	//
	private int getIndexValue(int[] convert, int user) {
		for (int i = 0; i < convert.length; i++) {
			if (convert[i] == user) {
				return i;
			} // i.e. the index value
		}
		return -1; // error
	}

	// Conversion index -> user
	//
	private int getUserValue(int[] convert, int index) {
		try {
			return convert[index]; // i.e. the user value
		} catch (Exception e) {
			return -1; // error
		}
	}

	/**
	 * From MultiConfigurable interface.
	 */
	public ConfigurationPanel getSharedConfigPanel(Object param) {

		createMethodName2accessor(((TreeCollection) step.getScene()).getTrees());
		removeComboProperty("methodName");

		// process keys to discard the className
		// process keys to discard the className
		// process keys to discard the className

		// ~ addComboProperty ("methodName", new LinkedList
		// (methodName2accessor.keySet ()));
		addComboProperty("methodName", new LinkedList(propName2methName.keySet())); // fc
																					// -
																					// 21.9.2005

		return super.getSharedConfigPanel(param);
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() { // has to be defined here,
														// is no more inherited
														// from DETimeG.
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() { // has to be defined here, is no more
											// inherited from DETimeG.
		Vector v = new Vector();

		v.add(Translator.swap(qp.getPropertyName())); // x axis label
		
		// fc-13.9.2021 added /ha if needed
		String yAxisLabel = Translator.swap("DEEnumPropertyClassN.yLabel");
		if (isSet("perHectare"))
			yAxisLabel += " (/ha)";
		v.add(yAxisLabel);
		
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() { // has to be defined here, is no
											// more inherited from DETimeG.
		return labels;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() { // has to be defined here, is no more inherited from
							// DETimeG.
		return 1;
	}

}
