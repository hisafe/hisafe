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

/**
 * Stump treatment availability
 *
 * @author C. Meredieu T. Labbe May 2013
 */
public interface StumpTreatmentAvailabilityProvider {

	/**	Stump treatment availability
	*/
	public boolean getStumpTraitment (GModel model);
	public double getProbSporeInfection (GModel model);
	public double getTreatedFailure (GModel model);


	/**	Stump treatment chosen by user for thinnning : can be treated, removed or crushed
	*/
	public void setStumpTraitment (boolean areStumpTreated, GModel model);
	public void setProbSporeInfection (double probSporeInfection, GModel model);
	public void setTreatedFailure (double treatedFailure, GModel model);

}


