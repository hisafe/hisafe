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
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Tree;
import capsis.kernel.GScene;

/**
 * Default method provider for MaddModels. It is possible to use these implementations 
 * in the MethodProvider of your modules by copying and adapting them. Redirections (your method 
 * calls one here) are no more promoted : a change in this class would change the way 
 * your module computes the related property, which does not seem desirable.
 * 
 * @author F. de Coligny - april 2000
 * modified B.Courbaud - september 2001
 * modified F. de Coligny - september 2002 - Singleton pattern / final class
 */
public class SpatializedMethodProvider  implements
		GProvider, 
		DgProvider, 
		HgProvider, 
		MeanHProvider,
		NProvider, 
		VProvider, 
		MeanVProvider,
		GTreeVProvider {

	
	/**	This constructor is private to comply with the Singleton pattern.
	*	To get the instance of MaddMethodProvider, use MaddMethodProvider.getInstance ();
	*/
	public SpatializedMethodProvider () {super();}	// Singleton pattern
	
		
	/**	Basal area ("Surface terrière") (m2) : Sum(pi r^2) for all trees in the stand.
	*	Dbh (diameter 1.3m) must be in cm.
	*	Author: F. de Coligny - 2001
	*/
	// fc - 24.3.2004 - Use given trees
	public double getG (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}	// if no trees, G = 0
			
			double radius;
			double G = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				radius = t.getDbh () / 2;		// dgh & radius : cm
				G += Math.PI * radius * radius;
			}
			return G/10000;		// G : m2
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaddMethodProvider.getG ()", "Error while computing G", e);
			return -1d;
		}
		
	}


	/**	Dg : Diameter of the mean tree (cm) : sqrt (Sum(d^2) / n).
	 *	Author: F. de Coligny & Ph. Dreyfus - 2001
	 */
	public double getDg (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}	// if no trees, Dg = 0
			
			Tree t;
			double d = 0;
			double cum = 0;
			double Dg = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				t = (Tree) i.next ();
				d = t.getDbh ();
				cum += d*d;
			}
			Dg = Math.sqrt (cum / trees.size ());
			return Dg;
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaddMethodProvider.getDg ()", "Error while computing Dg", e);
			return -1d;
		}
	}


	/**	Mean height for all the given trees (m)
	 *	Author: F. de Coligny - june 2004
	 */
	public double getMeanH (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}
			
			double cum = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				cum += t.getHeight ();
			}
			
			return cum / trees.size ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaddMethodProvider.getMeanH ()", "Error while computing MeanH", e);
			return -1d;
		}
	}

	/**	Hg : Height of the mean tree (m) : sqrt (Sum(h^2) / n).
	*	 Author: F. de Coligny & Ph. Dreyfus - 2001
	*	@deprecated Hg is the height of the tree with its diameter equal to Dg - fc - 7.4.2004
	*/
	public double getHg (GScene stand, Collection trees) {
		return -1d;
	}


	/**	Number of trees.
	*	Author: F. de Coligny - 2001
	*/
	public double getN (GScene stand, Collection trees) {	// fc - 22.8.2006 - Numberable is double
		return trees.size ();
	}


	/**	Trees volume (m3).
	*	Author: B. Courbaud - september 2001
	*/
	public double getV (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}	// if no elements, V = 0
	
			SpatializedTree t;
			double V = 0;
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				t = (SpatializedTree) i.next ();
				
				// Default taper coefficient is used if tree not instance of TaperCoefProvider
				double dRatio = 0.7;
				
				if (t instanceof TaperCoefProvider) {
					TaperCoefProvider t2 = (TaperCoefProvider) t;
					dRatio = t2.getTaperCoef ();
				}
				
				// This implementation relies on getGTreeV ()
				V += getGTreeV (dRatio, t);	
			}
			return V;		// V : m3
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaddMethodProvider.getV ()", "Error while computing V", e);
			return -1d;
		}
	}


	/**	Arithmetic mean tree volume (m3).
	*	Author: B. Courbaud - september 2001
	*/
	public double getMeanV (GScene stand, Collection trees) {
		try {
			double meanV = 0;
			double totV = getV (stand, trees);
			double N = getN (stand, trees);	// fc - 22.8.2006 - Numberable is double
			if (N != 0) {
				meanV = totV / N;
			}
			return meanV;
		} catch (Exception e) {
			Log.println (Log.ERROR, "MaddMethodProvider.getMeanV ()","Error while computing meanV",e);
			return -1d;
		}
	}


	/**	Individual tree volume.
	*	Author: B. Courbaud - september 2001
	*/
	public double getGTreeV (double taperCoef, Tree tree) {
		// Volume is calculated from diameter at medium height
		return Math.PI/4 * tree.getHeight() * Math.pow ((taperCoef*tree.getDbh() / 100), 2);	// (m3)
	}


}
	
