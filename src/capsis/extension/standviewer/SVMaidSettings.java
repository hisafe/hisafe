/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2003  Francois de Coligny
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

package capsis.extension.standviewer;

import java.awt.Color;

import jeeb.lib.util.Log;
import capsis.kernel.AbstractSettings;

/**
 * SVMaidSettings are settings for SVMaid stand viewer.
 * 
 * @author F. de Coligny - august 2001
 */
public class SVMaidSettings extends AbstractSettings {

	public final static int VARIABLES_BOTTOM = 1;
	public final static int VARIABLES_RIGHT = 2;
	//~ public final static int VARIABLES_SIZE = 170;
	//~ public static final int CURVE_HEIGHT = 60;

	public static final int AGGREGATE_CLASS_WIDTH = 1;
	public static final int AGGREGATE_CLASS_NUMBER = 2;

	protected boolean showAxisNames;
	protected boolean showPreviousStep;
	protected boolean zeroOnX;			// x axis begins on zero
	protected boolean showGirth;
	protected boolean showVariables;
	protected int variablesPosition;
	protected Color selectionColor;
	protected Color color1;
	protected Color color2;
	protected boolean grouperMode;
	protected boolean grouperModeNot;	// fc - 21.4.2004
	protected String grouperName;
	protected boolean selectUnderlyingTrees;
	protected boolean perHectare;

	protected boolean aggregate;
	protected int aggregateMode;
	protected double aggregateClassWidth;
	protected int aggregateClassNumber;
	protected boolean isAggregateMinThreshold;
	protected boolean isAggregateMaxThreshold;
	protected double aggregateMinThreshold;
	protected double aggregateMaxThreshold;

	//~ protected int variablesSize;	// fc - 6.1.2005
	//~ protected int curveHeight;	// fc - 6.1.2005
	
	protected boolean enlargeBars;
	
	//	protected int detailThreshold;		// in pixels (ex: detailled trees if dbh>3pixels)
	//	protected int lowThreshold;			// in cm. (ex: trees with dbh<lT are not displayed)
	//	protected int highThreshold;		// in cm. (ex: trees with dbh>hT are not displayed)


	/**	Constructor.
	*/
	public SVMaidSettings () {
		resetSettings ();
	}
	
	public boolean isAggregateClassWidth () {return aggregateMode == AGGREGATE_CLASS_WIDTH;}
	public boolean isAggregateClassNumber () {return aggregateMode == AGGREGATE_CLASS_NUMBER;}
	
	/**	Reset all values to default values.
	*/
	public void resetSettings () {
		showAxisNames = true;
		showPreviousStep = true;
		zeroOnX = false;
		showGirth = false;
		showVariables = true;
		variablesPosition = VARIABLES_RIGHT;
		selectionColor = new Color (153, 51, 0);	// dark red
		color1 = new Color (102, 102,102);	// dark gray
		color2 = new Color (204, 204, 204);	// light gray
		grouperMode = false;
		grouperModeNot = false;		// fc - 21.4.2004
		grouperName = "";
		selectUnderlyingTrees = false;
		perHectare = true;
		
		aggregate = true;
		aggregateMode = AGGREGATE_CLASS_WIDTH;
		aggregateClassNumber = 5;
		aggregateMinThreshold = 0;
		aggregateClassWidth = 5;
		aggregateMaxThreshold = 100;
		isAggregateMinThreshold = false;
		isAggregateMaxThreshold = false;

		//~ variablesSize = VARIABLES_SIZE;
		//~ curveHeight = CURVE_HEIGHT;
		enlargeBars = true;
		
		//	detailThreshold = 3;
		//	lowThreshold = 0;
		//	highThreshold = VARIABLES_SIZE;
	}

	/**	Clone the settings object.
	*/
	public Object clone () {
		SVMaidSettings o = null;
		try {
			o = (SVMaidSettings) super.clone ();
			o.selectionColor = new Color (selectionColor.getRed (), selectionColor.getGreen (), selectionColor.getBlue ());
			o.color1 = new Color (color1.getRed (), color1.getGreen (), color1.getBlue ());
			o.color2 = new Color (color2.getRed (), color2.getGreen (), color2.getBlue ());
			o.grouperName = new String (grouperName);
		} catch (Exception exc) {
			Log.println (Log.WARNING, 
					"SVMaidSettings.clone ()", 
					"Trouble while cloning SVMaidSettings. Object to clone : \n"+toString ());
		}

		return (Object) o;
	}
	
	public String toString () {
		return "SVMaid settings="
			+"(showAxisNames "+showAxisNames
			+" showPreviousStep "+showPreviousStep
			+" zeroOnX "+zeroOnX
			+" showGirth="+showGirth
			+" showVariables "+showVariables
			+" variablesPosition "+variablesPosition
			//~ +" variablesSize "+variablesSize
			+" selectionColor "+selectionColor
			+" color1 "+color1
			+" color2 "+color2
			+" grouperMode "+grouperMode
			+" grouperModeNot "+grouperModeNot
			+" grouperName "+grouperName
			+" selectUnderlyingTrees "+selectUnderlyingTrees
			+" perHectare "+perHectare
			
			+" aggregate "+aggregate
			+" aggregateMode "+aggregateMode
			+" aggregateClassNumber "+aggregateClassNumber
			+" aggregateClassWidth "+aggregateClassWidth
			+" isAggregateMinThreshold "+isAggregateMinThreshold
			+" aggregateMinThreshold "+aggregateMinThreshold
			+" isAggregateMaxThreshold "+isAggregateMaxThreshold
			+" aggregateMaxThreshold "+aggregateMaxThreshold
			//~ +" curveHeight "+curveHeight
			+" enlargeBars "+enlargeBars
		//	+" detailThreshold "+detailThreshold
		//	+" lowThreshold "+lowThreshold
		//	+" highThreshold "+highThreshold
			+")";
	} 

}

