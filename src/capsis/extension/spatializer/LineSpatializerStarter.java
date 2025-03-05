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

package capsis.extension.spatializer;

import java.io.Serializable;

import capsis.kernel.extensiontype.GenericExtensionStarter;

/**	LineSpatializerStarter: parameters for a LineSpatializer
*
*	@author F. de Coligny - july 2006
*/
public class LineSpatializerStarter extends GenericExtensionStarter
		implements Serializable, Cloneable {

	public double x0;		// bounds.x0
	public double x1;		// bounds.x1
	public double y0;		// bounds.y0
	public double y1;		// bounds.y1
	
	public double x0Border = 0;		// space between left border and first line
	public double x1Border = 0;
	public double y0Border = 0;
	public double y1Border = 0;
	
	public boolean interXEnabled = true;
	public double interX = 5;

	public boolean interYEnabled = false;
	public double interY = 5;

	/**	Constructor.
	*	Listener is needed.
	*/
	public LineSpatializerStarter () {
		super ();
	}

	// Starter must be cloned when memorized by Extension manager
	// in order not to be then shared by two distinct extensions
	public Object clone () {	// to be redefined in subclasses
		LineSpatializerStarter o = null;
		try {
			o = (LineSpatializerStarter) super.clone ();

		} catch (Exception exc) {}
		return o;
	}

}


