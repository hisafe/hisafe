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

package capsis.extension.lollypop;

import java.awt.Color;
import java.awt.event.ActionListener;
import java.io.Serializable;

import capsis.kernel.extensiontype.GenericExtensionStarter;

/**	GenericLollypopStarter: parameters for a GenericLollypop
*
*	@author F. de Coligny - march 2006
*/
public class GenericLollypopStarter extends GenericExtensionStarter
		implements Serializable, Cloneable {

	// listener must not be serialized : transient
	// it must be reset after deserialization
	transient public ActionListener listener;	// the tool using the Drawer, interested in config changes

	public boolean labelEnabled = true;		// label visible or not (default: ids)
	public boolean labelFrequencyEnabled = true;	// draw only some labels considering a frequency
	public int labelFrequency = 10;			// frequency of label drawing
	public boolean labelId = true;			// label is tree id
	public boolean labelDbh = false;		// label is tree dbh

	public boolean trunkEnabled = true;		// trunk visible or not
	public double trunkMagnifyFactor = 1;	// magnify the trunk

	public boolean crownEnabled = true;		// crown visible or not
	public boolean crownOutline = false;	// fast drawing: drawings not crownFilled
	public boolean crownFilled = true;		// fill the lollypop	// fc - 1.2.2008 - changed default option to crownFilled
	public boolean crownFilledFlat = true;	// fill with flat color
	public boolean crownFilledLight = false;	// fill with light aspect
	public boolean crownFilledTransparent = false;	// fill with transparency
	public int crownAlphaValue = 200;				// transparency parameter [0..255]

	public Color labelColor = new Color (51, 0, 102);
	public Color trunkColor = Color.BLACK;
	public Color crownColor = new Color (0, 102, 0);
	public Color cellColor = Color.GRAY;
	public Color selectionColor = new Color (207, 74, 7);


	/**	Constructor.
	*	Listener is needed.
	*/
	public GenericLollypopStarter (ActionListener listener) {
		super ();
		this.listener = listener;
	}

	// Starter must be cloned when memorized by Extension manager
	// in order not to be then shared by two distinct extensions
	public Object clone () {	// to be redefined in subclasses
		GenericLollypopStarter o = null;
		try {
			o = (GenericLollypopStarter) super.clone ();
			o.listener = null;	// forgotten

			o.labelColor = new Color (labelColor.getRed (), labelColor.getGreen (), labelColor.getBlue ());
			o.trunkColor = new Color (trunkColor.getRed (), trunkColor.getGreen (), trunkColor.getBlue ());
			o.crownColor = new Color (crownColor.getRed (), crownColor.getGreen (), crownColor.getBlue ());
			o.cellColor = new Color (cellColor.getRed (), cellColor.getGreen (), cellColor.getBlue ());
			o.selectionColor = new Color (selectionColor.getRed (), selectionColor.getGreen (), selectionColor.getBlue ());
		} catch (Exception exc) {}
		return o;
	}

}


