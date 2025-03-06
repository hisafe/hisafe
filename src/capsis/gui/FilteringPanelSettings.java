/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
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

package capsis.gui;

import java.util.StringTokenizer;

import capsis.kernel.AbstractSettings;

/**
 * Filtering panel settings.
 * Format : booleans and ints separed by "_" : detailSet, minimumSet, maximumSet, 
 * detailValue, minimumValue, maximumValue, userMinimumValue, userMaximumValue.
 * 
 * @author F. de Coligny - may 2002
 */
public class FilteringPanelSettings extends AbstractSettings {
	
	public boolean isDetailSet;
	public boolean isMinimumSet;
	public boolean isMaximumSet;
	
	public int detailValue;
	public int minimumValue;
	public int maximumValue;

	public int userMinimumValue;
	public int userMaximumValue;
	
	/**
	 * Create a default settings object to configure a FilteringPanel.
	 */
	public FilteringPanelSettings () {
		this (null);	// default values
	}
	
	/**
	 * Create a settings object from an encoded string to configure a FilteringPanel.
	 */
	public FilteringPanelSettings (String encodedString) {
		super ();
		try {	
			
			StringTokenizer st = new StringTokenizer (encodedString, "_");
			
			isDetailSet = new Boolean (st.nextToken ()).booleanValue ();
			isMinimumSet = new Boolean (st.nextToken ()).booleanValue ();
			isMaximumSet = new Boolean (st.nextToken ()).booleanValue ();
			
			detailValue = new Integer (st.nextToken ()).intValue ();
			minimumValue = new Integer (st.nextToken ()).intValue ();
			maximumValue = new Integer (st.nextToken ()).intValue ();
			
			userMinimumValue = new Integer (st.nextToken ()).intValue ();
			userMaximumValue = new Integer (st.nextToken ()).intValue ();
			
		} catch (Exception e) {
			secure ();
			return;
		}
		
	}

	/**
	 * Initialisation : user min and max bounds.
	 */
	public void init (int min, int max) {
		minimumValue = min;
		maximumValue = max;
		
		userMinimumValue = min;
		userMaximumValue = max;
		
	}

	/**
	 * Return the current state as an encoded string.
	 */
	public String getEncodedString () {
		StringBuffer b = new StringBuffer ();
		b.append (isDetailSet);
		b.append ("_");
		b.append (isMinimumSet);
		b.append ("_");
		b.append (isMaximumSet);
		b.append ("_");
		b.append (detailValue);
		b.append ("_");
		b.append (minimumValue);
		b.append ("_");
		b.append (maximumValue);
		b.append ("_");
		b.append (userMinimumValue);
		b.append ("_");
		b.append (userMaximumValue);
		
		return b.toString ();
	}

	/**
	 * In case of trouble, use conservative values (i.e. disable filtering).
	 */
	public void secure () {
		isDetailSet = false;
		isMinimumSet = false;
		isMaximumSet = false;
		detailValue = 0;
		minimumValue = 0;
		maximumValue = 150;
		userMinimumValue = 0;
		userMaximumValue = 150;
	}

	public String toString () {
		return getEncodedString ();
	}

}

