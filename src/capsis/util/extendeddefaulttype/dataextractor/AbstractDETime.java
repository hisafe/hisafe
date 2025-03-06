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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Vector;

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.TextInterface;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.Log;
import repicea.simulation.covariateproviders.plotlevel.TreeStatusCollectionsProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

abstract class AbstractDETime extends PaleoDataExtractor implements DFCurves, TextInterface {

	protected List<List<? extends Number>> curves;
	protected MethodProvider methodProvider;

	protected Vector labels;
	protected String text;
	
	protected enum MessageID implements TextableEnum {
		Time("Time", "Temps"),
		Ha(" (ha)", " (ha)");

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	/**
	 * Phantom constructor. Only to ask for extension properties (authorName,
	 * version...).
	 */
	protected AbstractDETime() {}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	protected AbstractDETime(GenericExtensionStarter s) {
		super(s);
		labels = new Vector();
		curves = new ArrayList<List<? extends Number>> ();
	}


	protected abstract String getYAxisLabelName();
	
	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
	 * 
	 * Return false if trouble while extracting.
	 */
	@Override
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();


		try {
			Vector<Vector<? extends Number>> values = extractValues();
			
			curves.clear();
			for (Vector<? extends Number> vec : values) {
				curves.add(vec);
			}

			labels.clear();
			Vector<String> labelNames = extractLabels(values);
			labels.add(new Vector()); // no x labels
			Vector label;
			for (String labelName : labelNames) {
				 label = new Vector();
				 label.add(labelName);
				 labels.add(label);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeV.doExtraction ()",
					"Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}


	protected abstract Vector<String> extractLabels(Vector values);

	protected abstract Vector<Vector<? extends Number>> extractValues();

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(MessageID.Time.toString());
		if (isSet("perHectare")) {
			v.add(getYAxisLabelName() + MessageID.Ha.toString());
		} else {
			v.add(getYAxisLabelName());
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

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<List<String>> getLabels() {
		return labels;
	}

	/**	
	 * From DFCurves interface.
	 */
	@Override
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	
	protected static StatusClass getStatus(String var) {
		for (StatusClass variable : StatusClass.values()) {
			if (variable.toString().equals(var)) {
				return variable;
			}
		}
		return null;
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

}

