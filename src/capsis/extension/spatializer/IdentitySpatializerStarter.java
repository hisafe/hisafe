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

/**	IdentitySpatializerStarter: parameters for a IdentitySpatializer
*
*	@author F. de Coligny - july 2006
*/
public class IdentitySpatializerStarter extends GenericExtensionStarter
		implements Serializable, Cloneable {

	// No special parameters from now on - fc - 10.7.2006
	

	/**	Constructor.
	*	Listener is needed.
	*/
	public IdentitySpatializerStarter () {
		super ();
	}

	// Starter must be cloned when memorized by Extension manager
	// in order not to be then shared by two distinct extensions
	public Object clone () {	// to be redefined in subclasses
		IdentitySpatializerStarter o = null;
		try {
			o = (IdentitySpatializerStarter) super.clone ();

		} catch (Exception exc) {}
		return o;
	}

}


