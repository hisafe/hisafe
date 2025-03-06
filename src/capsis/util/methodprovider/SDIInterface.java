/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2011  Francois de Coligny
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
package capsis.util.methodprovider;

import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Vertex2d;
import capsis.kernel.GScene;

/**
 * An interface to get the Stand density index of a GScene.
 * 
 * @author T. Fonseca, F. de Coligny - may 2011
 */
public interface SDIInterface {

	/**
	 * Returns the Stand density index of the scene (%)
	 */
	public double getSDI(GScene scene);

	/**
	 * Returns two lines to be drawn on the SDI graphs. Specific to
	 * ModisPinaster optimisation. Optional. If not needed, return null and this
	 * will be ignored.
	 */
	public Vector<List<Vertex2d>> getSDIBand(GScene scene);

}
