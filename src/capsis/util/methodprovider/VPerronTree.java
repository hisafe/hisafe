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

import capsis.kernel.MethodProvider;


/**
 * Individual tree volume with Perron parameters (used in Quebec, Canada)
 * ref:Perron, J.-Y., 1983, Tarif du cubage général - Volume marchand brut.
 *	   Ministère de l'Énergie et des Ressources (Québec), Canada. 52p.
 *
 * @author S. Turbis June 2004
 */
public interface VPerronTree extends MethodProvider {

	/**	F1 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF1 ();

	/**	F2 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF2 ();

	/**	F3 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF3 ();

	/**	F4 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF4 ();


	/**	F5 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF5 ();

	/**	F6 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF6 ();

	/**	F7 parameter of the calculation of individual tree volume with
	*	Perron parameters (used in Quebec, Canada)
	*/
	public double getF7 ();


	/**	Dbh of tree in cm
	*/
	public double getDbh ();


	/**	Height of tree in m
	*/
	public double getHeight ();


}