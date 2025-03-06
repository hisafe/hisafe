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
 * Individual tree volume with Honer parameters (used in Ontario and Quebec, Canada)
 * ref: Honer, T.G., M.F. Ker et I.S. Alemdag, 1983, Metric timber tables for the
 * commercial tree species of Central and Eastern Canada. Environment Canada
 * Canadian Forestry service, Information Report
 * M-X-140. 139p.
 *
 * @author S. Turbis June 2004
 */
public interface VHonerTree  extends MethodProvider{

	/**	VHc1 parameter of the calculation of individual tree volume with
	*	Honer parameters (used in Ontario and Quebec, Canada)
	*/
	public double getVHc1 ();

	/**	VHc2 parameter of the calculation of individual tree volume with
	*	Honer parameters (used in Ontario and Quebec, Canada)
	*/
	public double getVHc2 ();

	/**	VHb2 parameter of the calculation of individual tree volume with
	*	Honer parameters (used in Ontario and Quebec, Canada)
	*/
	public double getVHb2 ();

	/**	Dbh of tree in cm
	*/
	public double getDbh ();


	/**	Height of tree in m
	*/
	public double getHeight ();


}