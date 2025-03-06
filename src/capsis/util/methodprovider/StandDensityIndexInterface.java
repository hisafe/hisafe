/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2011  Francois de Coligny
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


/**
 * Information needed to parametrize a thinning from the Stand density index.
 * 
 * @author T. Fonseca, F. de Coligny - may 2011
 */
public interface StandDensityIndexInterface {
	
	/**	
	 * Returns N after thinning from the given sdi, basalArea and N before thinning.
	 */
	public int getNafter (double sdi, double Gbefore, double Nbefore);
	
	/**
	 * Returns the basalArea after thinning from the basal area before thinning  
	 * and number of trees before and after thinning.
	 */
	public double getGafter (double Gbefore, int Nbefore, int Nafter);
	
	/**
	 * Returns the number of trees from the given dominant 
	 * height and stand density index.
	 */
	public int getN (double hd, double sdi);
	
	/**
	 * See Reineke 1993.
	 */
	public double getSelfThinningSlopeCoefficient ();
	
	/**
	 * Returns the max number of living trees for this species.
	 */
	public int getMaxNumberOfTreesOn1HaForDg25cm ();
	
}
