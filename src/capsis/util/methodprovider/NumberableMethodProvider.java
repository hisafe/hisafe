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
import java.util.Iterator;

import jeeb.lib.util.Log;
import capsis.defaulttype.NumberableTree;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;

/**
 * Default method provider for MaidModels. It is possible to use these implementations 
 * in the MethodProvider of your modules by copying and adapting them. Redirections (your method 
 * calls one here) are no more promoted : a change in this class would change the way 
 * your module computes the related property, which does not seem desirable.
 *
 * @author F. de Coligny - april 2001
 */
public class NumberableMethodProvider  implements MethodProvider, 
		GProvider,
		DgProvider,
		NProvider {


	/**	This constructor is private to comply with the Singleton pattern.
	*	To get the instance of MaidMethodProvider, use MaidMethodProvider.getInstance ();
	*/
	public NumberableMethodProvider () {super();}	// Singleton pattern


	/**	Basal area ("Surface terrière") (m2) : Sum(pi r^2) for all the given trees.
	*	Dbh (diameter 1.3m) must be in cm.
	*/
	// fc - 24.3.2004 - Use given trees
	public double getG (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}	// if no trees, G = 0
			double radius;
			double G = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				NumberableTree t = (NumberableTree) i.next ();
				radius = t.getDbh () / 2;		// dbh & radius : cm
				G += Math.PI * radius * radius * t.getNumber();
			}
			return G/10000;		// G : m2
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaidMethodProvider.getG ()", "Error while computing G", e);
			return 0d;
		}
	}


	/**	Diameter of the mean tree (cm) : sqrt (Sum(d^2) / n).
	 */
	public double getDg (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}	// if no trees, Dg = 0
	
			NumberableTree t;
			double d = 0;	// fc , h = 0;
			double cum = 0;
			double Dg = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				t = (NumberableTree) i.next ();
				d = t.getDbh ();
				cum += d*d * t.getNumber ();
			}
			Dg = Math.sqrt (cum / getN (stand, trees));
			return Dg;
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaidMethodProvider.getDg ()", "Error while computing Dg", e);
			return -1d;
		}
	}


	/**	Number of trees.
	*/
	public double getN (GScene stand, Collection trees) {	// fc - 22.8.2006 - Numberable is double
		double N = 0;	// fc - 22.8.2006 - Numberable is double
		NumberableTree t;
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			t = (NumberableTree) i.next ();
			N += t.getNumber ();
		}
		return N;
	}


}
