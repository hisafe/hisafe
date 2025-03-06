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

import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.util.Flag;

/**	This method provides data on the initial settings of the model, required by some data extractors.
*
*	@author J. Labonne - October 2005
*/
public interface FishSettingsProvider extends MethodProvider {


	// labonne doit modifier ce commentaire
	/**	ProdV : total production volume (m3) since the beginning of the scenario
	*	(must be cumulated), including mortality and thinnings (cut trees).
	*	Parameters:
	*	"stand": ProdV must be calculated for this stand,
	*	"trees": may be null (ignore it). If not null, ProdV must be calculated for
	*		these trees (a group),
	*	"flag": contains the return code, 0 : "correct", -1: "s == null",
	*		-2: "trees: unsupported groups".
	*	Returned value: ProdV if flag is 0, -1 otherwise.
	*/

	// this table has 8 values
	// carrying capacity for 2, 3 , 4, 5, 6, 7-12, 12-24, 24 + months old fish.
	// set by user

	public int[] getCarryingCapacity (GModel model, Flag flag);

	public boolean getGeneticsEnabled(GModel model, Flag flag);

	/**	Table with Settings growth parameters
	 */
	public double[] getGrowthModel (GModel model, Flag flag);

	/**	Table with Settings fecundity slope
	 */
	public double getFecundityCoefficient (GModel model, Flag flag)	;

//	public void setScene (FishStand stand) ;		// fc - 3.10.2005

	/**	Table with Settings fecundity intercept
	 */
	public double getFecundityIntercept (GModel model, Flag flag) ;


}


