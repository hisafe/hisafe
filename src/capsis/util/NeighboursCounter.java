/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import capsis.extension.modeltool.rivermaker.Reach;
import jeeb.lib.util.Node;

/**	A tool able to count the number of neighbours around each patch or reach in a network (fishstand/fishmodel)
*	it includes 3 methods.
*
*	@author J. Labonne - january 2007
*/
public class NeighboursCounter    {



	/**	Method that computes the mean number and variance of neighbour reaches in a given range.
		In this model, the distance is equivalent to the number of reach to cross.
		jl - 18.09.2006
	*/
	public double [] computeLocalConnectivity (Collection nodes, int radius) {
		//Collection newReaches = nodes.values();
		double [] localConnectivity = new double [2];
		double [] value = new double [nodes.size()];
		int count = 0;
		for (Iterator j = nodes.iterator();j.hasNext();) {
			Node n = (Node) j.next();
			if (n instanceof Reach) {
				Reach r =(Reach)n;
				value [count] = computeNeighbours(r,radius);
				count++;
			} else if (n instanceof GReach) {
				GReach r =(GReach)n;
				value [count] = computeNeighbours(r,radius);
				count++;
			}
		}

		return value;
	}

	/**	Method that computes the mean number and variance of neighbour reaches in a given range.
		In this model, the distance is equivalent to the number of reach to cross.
		jl - 18.09.2006
	*/
	public double [][] connectivityStats (Collection nodesTotal) {
		Collection nodes = new ArrayList();
		for (Iterator k = nodesTotal.iterator(); k.hasNext();) {
			Node n = (Node) k.next();
			if(n instanceof Reach || n instanceof GReach) {
				nodes.add(n);
			}
		}
		double [][] stats = new double [5][2];
		for (int i =0; i<5; i++) {
			double [] inter = computeLocalConnectivity (nodes, i+1);
System.out.println("i is "+ i);
			double mean = 0;
			for (int h = 0; h<inter.length; h++) {
				mean = mean+inter[h];
			}
			mean= mean/inter.length;
			double var=  0;
			for (int h = 0; h<inter.length; h++) {
				var = var+Math.pow((inter[h]-mean),2);
			}
			var= var/inter.length;
			stats [i][0] = mean;
			stats [i][1] = var;
//System.out.println("connectivity vector is "+stats [0][0]+stats [1][0]+stats [2][0]+stats [3][0]+stats [4][0]
//+stats [0][1]+stats [1][1]+stats [2][1]+stats [3][1]+stats [4][1]);
//System.out.println("mean connected is "	+ mean);
//System.out.println("var connected is "+ var);
		}

		return stats;


	}


	/**	Method that computes the number of neighbour reaches around a single reach.
		In this model, the distance is equivalent to the number of reach to cross.
		jl - 18.09.2006
	*/
	public int computeNeighbours (GReach reach, int mag) {

		Collection up = reach.getUpstream(mag);
		Collection down = reach.getDownstream(mag);
		int neig = 0;
		if (!up.isEmpty()) {
			for (Iterator k = up.iterator(); k.hasNext();) {
				GReach u = (GReach) k.next();
				neig++;
			}
		}
		if (down!=null && !down.isEmpty()) {
			int count = 0;
			for (Iterator k = down.iterator(); k.hasNext();) {
				GReach u = (GReach) k.next();
				neig++;
				count++;
				if (u.getBrothers()!=null && !u.getBrothers().isEmpty()) {
					Iterator j = u.getBrothers().iterator();		// here we do not iterate : in this model, a reach can have at most one brother.
					GReach b = (GReach)j.next();
					int bros = 1+b.getUpstream(mag-count-1).size();
					neig = neig + bros;
				}
			}
		}
		return neig;
	}

	/**	Method that computes the number of neighbour reaches around a single reach.
		In this model, the distance is equivalent to the number of reach to cross.
		jl - 18.09.2006
	*/
	public int computeNeighbours (Reach reach, int mag) {

		Collection up = reach.getUpstream(mag);
		Collection down = reach.getDownstream(mag);
		int neig = 0;
		if (!up.isEmpty()) {
			for (Iterator k = up.iterator(); k.hasNext();) {
				Reach u = (Reach) k.next();
				neig++;
			}
		}
		if (down!=null && !down.isEmpty()) {
			int count = 0;
			for (Iterator k = down.iterator(); k.hasNext();) {
				Reach u = (Reach) k.next();
				neig++;
				count++;
				if (u.getBrothers()!=null && !u.getBrothers().isEmpty()) {
					Iterator j = u.getBrothers().iterator();		// here we do not iterate : in this model, a reach can have at most one brother.
					Reach b = (Reach)j.next();
					int bros = 1+b.getUpstream(mag-count-1).size();
					neig = neig + bros;
				}
			}
		}
		return neig;
	}





}


