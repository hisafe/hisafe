/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.util.methodprovider;

import jeeb.lib.util.AdditionMap;

/**
 * Rockfornet stand description To be compatible with the Rockfornet modeltool
 * extension, a capsis module must have its Stand description implementing this
 * interface.
 *
 * @author Eric Mermin, Eric Maldonado - november 2006
 */
public interface RockfornetStand {

	public static final int STAND_BASAL_AREA = 1;
	public static final int MEAN_STEM_DBH = 2;

	/**
	 * Return mean stand density. (/ha ?...)
	 */
	public double getMeanStandDensity();

	/**
	 * Returns STAND_BASAL_AREA or MEAN_STEM_DBH.
	 */
	public int getMode();

	/**
	 * Return stand basal area (m2) (/ha ?...) Must be provided if mode ==
	 * STAND_BASAL_AREA
	 */
	public double getStandBasalArea_m2ha();

	/**
	 * Return mean stem dbh (cm) Must be provided if mode == MEAN_STEM_DBH
	 */
	public double getMeanStemDbh_cm();

	/**
	 * Return species distribution. key: RockfornetSpecies.REFERENCE_PICEA,
	 * REFERENCE_ABIES, REFERENCE_FAGUS or REFERENCE_OTHER. 
	 * value: percentage of trees of this species (/ha ?...)
	 */
	public AdditionMap<String> getSpeciesDistribution(); // 4 entries

}
