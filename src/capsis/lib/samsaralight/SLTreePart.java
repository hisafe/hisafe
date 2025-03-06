/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
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

package capsis.lib.samsaralight;

import java.io.Serializable;

/**
 * SLTreePart - A super class for all tree parts.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public interface SLTreePart extends Serializable, Cloneable {

//	// fc+fa-25.4.2017 samsaralight resolution
//	// e.g. CROWN, CROWN_UP, CROWN_DOWN, TRUNK...
//	public SLTag getTag ();
	
	/**
	 * Evaluates if the given ray intercepts this tree part, previously
	 * relocated with the given shifts. Returns the interception path length and
	 * a distance to the origin (i.e. the target cell center) or null if no
	 * interception.
	 * 
	 * @param xShift
	 * 			  : m, to relocate the crown part for the duration of this interception
	 * @param yShift
	 *            : m, to relocate the crown part for the duration of this interception
	 * @param zShift
	 *            : m, to relocate the crown part for the duration of this interception
	 * @param elevation
	 *            : angle, rad
	 * @param azimuth
	 *            : angle, rad, trigonometric 0 and counter clockwise
	 */
	public double[] intercept(double xShift, double yShift, double zShift,
			double elevation, double azimuth);

	public void addDirectEnergy(double e); // MJ
	public double getDirectEnergy(); // direct beam energy in MJ

	public void addDiffuseEnergy(double e); // MJ
	public double getDiffuseEnergy(); // diffuse beam energy in MJ

	
	
	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//	public double getPotentialEnergy(); // in MJ, energy intercepted by this
//										// part but without neighbours
//	public void addPotentialEnergy(double e); // MJ
	
	
	
	
	// fc-23.11.2012 
	public SLTreePart getCopy (double xShift, double yShift, double zShift);

	public SLTreePart clone () throws CloneNotSupportedException;
	
	// fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy ();
	
}
