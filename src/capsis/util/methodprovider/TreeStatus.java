/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2013  Mathieu Fortin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.util.methodprovider;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.util.EnumProperty;

/**
 * The TreeStatus class is an attempt to harmonize the tree status in the different models.
 * @author Mathieu Fortin - May 2013
 */
public class TreeStatus extends EnumProperty implements Serializable {
	
	

	private static enum StatusProperty implements TextableEnum {
		Status("Tree status", "Etat");

		StatusProperty(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

		@Override
		public void setText (String englishText, String frenchText) {
			REpiceaTranslator.setString (this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString (this);
		}
	}

	public static final String PROPERTY_NAME = StatusProperty.Status.toString ();

	@Deprecated
	public static enum StatusClass implements TextableEnum {
		alive("alive", "vivant"), 
		cut("cut", "coup\u00E9"), 
		dead("dead", "mort"),
		windfall("windfall", "chablis");

		StatusClass(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

//		public String getName() {return name().trim().toLowerCase();}
//		public String getCompleteName() {return toString();}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString (this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}
		
	}

	static private final Map<Integer, TreeStatus> VALUES;
	static {
		VALUES = new HashMap<Integer, TreeStatus> ();
		TreeStatus treeStatus;
		for (TreeStatusProvider.StatusClass statusClass : TreeStatusProvider.StatusClass.values()) {
			if (VALUES.isEmpty()) {
				treeStatus = new TreeStatus(statusClass, null);
			} else {
				treeStatus = new TreeStatus(statusClass, VALUES.values().iterator().next());
			}
			VALUES.put(statusClass.ordinal(), treeStatus);
		}
	}

	private TreeStatusProvider.StatusClass statusClass;

	/**
	 * Private constructor for limited number of instances.
	 * @param statusClass a StatusClass enum
	 * @param model
	 */
	private TreeStatus(TreeStatusProvider.StatusClass statusClass, EnumProperty model) {
		super (statusClass.ordinal(), statusClass.toString(), model, PROPERTY_NAME);
		this.statusClass = statusClass;
	}		

	/**
	 * This method returns the TreeStatus instance that corresponds to the requested StatusClass instance.
	 * @param statusClass a StatusClass enum variable
	 * @return a TreeStatus instance from the VALUE map
	 */
	public static TreeStatus getStatus(TreeStatusProvider.StatusClass statusClass) {
		if (VALUES.containsKey(statusClass.ordinal())) {
			return VALUES.get(statusClass.ordinal());
		} else return null;
	}

	/**
	 * This method returns the StatusClass enum.
	 * @param i an integer that corresponds to the ordinal of the enum
	 * @return a StatusClass enum or null if there is no StatusClass enum corresponding to the integer
	 */
	public static TreeStatusProvider.StatusClass getStatusClass(int i) {
		if (VALUES.containsKey(i)) {
			return VALUES.get(i).statusClass;
		} else {
			return null;
		}
	}

	@Override
	public boolean equals (Object obj) {
		boolean equal = false;
		if (((TreeStatus) obj).getValue () == getValue ()) {
			equal = true;
		}
		return equal;
	}

	@Override
	public String toString() {return statusClass.toString();}
	
}