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

package capsis.util.methodprovider;

import capsis.extension.treelogger.GPieceDisc;
import capsis.kernel.MethodProvider;

/**	Mass per height unit (kg/m) of a set of rings in a given GPieceDisc
*	(used by WoodQualityWorkshop.GeoLog)
*	@author F. Mothe - august 2008
*/

public interface DiscMassProvider extends MethodProvider {

	/**	Computes mass per height unit (kg/m) of a set of rings.
	*	cambialAgeMin and cambialAgeMax should be valid (wood, bark or pith)
	*/
	public double getDiscMass_kgpm (GPieceDisc disc,
			int cambialAgeMin, int cambialAgeMax);
}

