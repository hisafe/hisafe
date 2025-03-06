/* 
 * Biomechanics library for Capsis4.
 * 
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;


/**
 * MecaTreeInfo - Interface (abstract class) for tree biomechanics data.
 *
 * @author Ph. Ancelin - october 2001
 */
public interface MecaTreeInfo {
//checked for c4.1.1_08 - fc - 4.2.2003

	// crown density , trunk, deltaH deltaD

	/**
	 * Return current height increment of a tree (m).
	 * Method to implement in the tree class of compatible models.
	 */
	public double getHeightIncrement ();

	/**
	 * Return current diameter (dbh) increment of a tree (cm).
	 * Method to implement in the tree class of compatible models.
	 */
	public double getDbhIncrement ();
	// no use : MecaTools.getDiameter (tree, height) provides trunk diameter increments at any height...


	/**
	 * Return current Crown Base Height of a tree (m).
	 * Method to implement in the tree class of compatible models.
	 */
	public double getCrownBaseHeight ();

	/**
	 * Return current Crown Radius At height h of a tree (m).
	 * Method to implement in the tree class of compatible models.
	 */
	public double getCrownRadiusAt (double h);

}


