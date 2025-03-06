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

import java.util.Collection;

import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;

/**
 * Stand volume for a species with Perron parameters (used in Quebec, Canada)
 * ref:Perron, J.-Y., 1983, Tarif du cubage général - Volume marchand brut.
 *	   Ministère de l'Énergie et des Ressources (Québec), Canada. 52p.
 *
 * @author S. Turbis June 2004
 */
public interface VPerronProvider extends MethodProvider {

	/**	Stand trees from which we can calculate volume
	*	from Perron parameters
	*/
	public double getVPerron (GScene stand, Collection trees, double minDbh);

}
