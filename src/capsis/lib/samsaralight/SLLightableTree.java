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

import java.util.List;

/**
 * SLLightableTree - a tree with a SLTreeLightResult light properties object.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public interface SLLightableTree {

	/**
	 * Returns the species code of this tree.
	 */
	public int getSpeciesId(); // fc+fa-26.4.2017, see SLBeam.ladProportion

	/**
	 * Returns the object containing the results of the radiative balance on
	 * this tree.
	 */
	public SLTreeLightResult getLightResult();

	public void setLightResult(SLTreeLightResult t);

	/**
	 * The crown parts of the tree (e.g. 1 ellipsoide, 2 half-ellipsoids, a set
	 * of vexels, 8 fractions of crown).
	 */
	public List<SLCrownPart> getCrownParts();

	/**
	 * The trunk (a cylinder so far)
	 */
	public SLTrunk getTrunk();

	/**
	 * Crown transmissivity, [0,1], MJ/MJ
	 */
	public double getCrownTransmissivity();

	/**
	 * To reset the results of the previous radiative balance run when starting
	 * a new run.
	 */
	// fc-10.1.2018 REMOVED, refactored, call tree.getLightResult ().resetEnergy () instead
	//public void resetEnergy();

}
