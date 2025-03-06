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

/**
 * An interface for objects able to return a leaf area development proportion (ladProp)
 * or a green leaf proportion (greenProp) value in [0, 1] for a given treeId and a given day of year (doy).
 * 
 * @author F. André, F. de Coligny - May 2017
 */
public interface SLFoliageStateManager {
	// cr+fc-01.04.2022 added Exceptions

	// Now this method works at tree level instead of species level. nb-06.11.2019
	/**
	 * Returns a lad proportion value at tree level.
	 */
	public double getLadProportionOfNextYear(int treeId, int doy) throws Exception;

	// nb-18.12.2019
//	/**
//	 * Returns a lad proportion value at species level.
//	 */
//	public double getLadProportionOfNextYearAtSpeciesLevel(int speciesId, int doy);
	
	//fa-19.06.2017
	/**
	 * Returns a green proportion value.
	 */
	public double getGreenProportionOfNextYear(int treeId, int doy) throws Exception;

	// nb-18.12.2019
//	/**
//	 * Returns a green proportion value at species level.
//	 */
//	public double getGreenProportionOfNextYearAtSpeciesLevel(int speciesId, int doy);
}
