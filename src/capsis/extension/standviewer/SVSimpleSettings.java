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
import capsis.util.Panel2DSettings;

/**
 * SVSimpleSettings are the current settings for SVSimple. They are also
 * the general option settings for SVSimple subclasses.
 *
 * @author F. de Coligny - jun 2000 / feb 2001 / may 2002 / dec 2003
 */
public class SVSimpleSettings extends AbstractSettings {

	public boolean showLabels;
	public int maxLabelNumber;	// fc - 18.12.2003
	public boolean showDiameters;
	protected boolean showTransparency;	// fc - 15.11.2005
	public Color labelColor;
	public Color treeColor;
	public Color cellColor;
	public Color selectionColor;
	public boolean grouperMode;
	public boolean grouperNot;	// fc - 21.4.2001
	public String grouperName;

	public Panel2DSettings panel2DSettings;


	/**	Constructor
	*/
	public SVSimpleSettings () {
		resetSettings ();
	}

	/**	Reset to default values
	*/
	public void resetSettings () {
		showLabels = true;
		maxLabelNumber = 50;
		showDiameters = true;
		showTransparency = false;
		labelColor = Color.BLUE;
		treeColor = Color.GREEN;
		cellColor = new Color (130, 130, 130); // was Color.ORANGE fc-3.9.2012 
		selectionColor = Color.RED;
		grouperMode = false;
		grouperNot = false;
		grouperName = "";

		panel2DSettings = new Panel2DSettings ();
	}

	/**	Clone settings
	*/
	public Object clone () {
		SVSimpleSettings o = null;
		try {
			o = (SVSimpleSettings) super.clone ();	// clones primitive type variables

			// Clone objects manually
			//
			o.labelColor = new Color (labelColor.getRed (), labelColor.getGreen (), labelColor.getBlue ());
			o.treeColor = new Color (treeColor.getRed (), treeColor.getGreen (), treeColor.getBlue ());
			o.cellColor = new Color (cellColor.getRed (), cellColor.getGreen (), cellColor.getBlue ());
			o.selectionColor = new Color (selectionColor.getRed (), selectionColor.getGreen (), selectionColor.getBlue ());
			o.grouperName = ""+grouperName;	// fc - 5.4.2004
		} catch (Exception exc) {
			Log.println (Log.WARNING,
					"SVSimpleSettings.clone ()",
					"Trouble while cloning SVSimpleSettings. Object to clone : \n"+toString (),
					exc);
		}
		return (Object) o;
	}

	/**	String representation
	*/
	public String toString () {
		StringBuffer b = new StringBuffer ("SVSimpleSettings-[");
		b.append ("showLabels=");
		b.append (showLabels);
		b.append (" maxLabelNumber=");
		b.append (maxLabelNumber);
		b.append (" showDiameters=");
		b.append (showDiameters);
		b.append (" showTransparency=");
		b.append (showTransparency);
		b.append (" labelColor=");
		b.append (labelColor);
		b.append (" treeColor=");
		b.append (treeColor);
		b.append (" cellColor=");
		b.append (cellColor);
		b.append (" selectionColor=");
		b.append (selectionColor);
		b.append (" grouperMode=");
		b.append (grouperMode);
		b.append (" grouperNot=");
		b.append (grouperNot);
		b.append (" grouperName=");
		b.append (grouperName);
		b.append (" panel2DSettings=");
		b.append (panel2DSettings);
		b.append ("]");
		return b.toString ();
	}

}

