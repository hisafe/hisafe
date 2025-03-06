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

package capsis.util.diagram2d;

import java.awt.Rectangle;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex2d;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.util.TreeDbhThenIdComparator;

/**
 * An histogram built from a Capsis Step for the MAID models.
 * It need a GTCStand instance under the step  and GMaidTree instances
 * in it. It takes trees dbhs in x[], trees numbers in y[] and trees height in z[].
 *
 * Enhancement: works with GTree(s) too : if tree is not instanceof Numberable (gerNumber ()),
 * number is set to 1. Allows SVMaid compatibility with MADD models. fc - 11.2.2002
 *
 * How to use it : Build a MaidHisto on a given step using its constructor. You can then get its
 * bounding boxes with getNBounds () and getHBounds (), and get its bars by getNBars () and
 * getHBars (). You can retrieve real step xMin and xMax by using matching accessors.
 *
 * It's possible to restrict the histogram from a given xMin and to a given xMax if some
 * smaller or bigger trees are unneeded. You can also change class width, resulting in a
 * cumulation of several bars in one single. In that case, we sum numbers and calculate
 * the average for heights.
 *
 * If needed, it's possible to reset the histogram to its first state (one tree per bar), by using the
 * reset () method.
 *
 * @author F. de Coligny - january 2002
 */
public class MaidHisto {
	public static final int MAX_FRACTION_DIGITS = 2;
	public final static double EPSILON = 0.001d;
	public final static double INACTIVE = Double.MIN_VALUE;

	protected SortedSet treeSet;
	protected double forcedXMin;
	protected double forcedXMax;
	protected double classWidth;
	protected boolean xBeginsAtZero;
	protected boolean girthMode;	// if true, girth everywhere instead of dbh
	protected double hectareCoefficient;
	
	protected double realXMin;	// the real min dbh value in the treeSet (always dbh, girth is locally computed in methods)
	protected double realXMax;	// the real max dbh value in the treeSet
	protected double[] x;
	protected double[] y;
	protected double[] z;
	protected Collection[] uTrees;	// one bar -> the underlying maid trees
	protected String[] xLabels;		// optional: Maid histo can propose x labels (ex: 0 - 10 / 10 - 20 ...)

	protected Rectangle.Double nBounds;
	protected Rectangle.Double hBounds;
	protected java.util.List nBars;
	protected java.util.List hBars;

	protected NumberFormat formater;	// to control number of decimals


	/**	Constructor.
	*/
	public MaidHisto (Collection trees) {
		setTrees (trees);
		hectareCoefficient = 1;		// default : not per hectare
		formater = NumberFormat.getInstance ();
		formater.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
		nBounds = new Rectangle.Double ();
		hBounds = new Rectangle.Double ();
		nBars = new ArrayList ();
		hBars = new ArrayList ();

		reset ();
	}

	/**	Resets the histogram on the referent step values for trees dbh and numbers.
	*/
	public void reset () {
		if (isEmpty ()) {return;}

		x = null;
		y = null;
		z = null;
		uTrees = null;
		xLabels = null;

		forcedXMin = INACTIVE;
		forcedXMax = INACTIVE;
		nBounds.setRect (0, 0, 0, 0);
		hBounds.setRect (0, 0, 0, 0);
		classWidth = 0d;		// reset -> classWidth = 0;
		double approxClassWidth = Double.MIN_VALUE;

//System.out.println ("MaidHisto, reset() (1), xBeginsAtZero="+xBeginsAtZero);

		try {
			int n = treeSet.size ();
			x = new double[n];
			y = new double[n];
			z = new double[n];
			uTrees = new Collection[n];
			double xMin = Double.MAX_VALUE;
			double xMax = Double.MIN_VALUE;
			double yMin = Double.MAX_VALUE;
			double yMax = Double.MIN_VALUE;
			double zMin = Double.MAX_VALUE;
			double zMax = Double.MIN_VALUE;

			int k = 0;
			for (Iterator i = treeSet.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();

				double number = 1;	// fc - 22.8.2006 - Numberable returns double
				try {number = ((Numberable) t).getNumber ();} catch (Exception e) {}

				// bug correction : some maid trees may have number == 0 -> ignore them - fc - 6.1.2005
				if (number == 0) {continue;}

				x[k] = t.getDbh ();
				y[k] = number * hectareCoefficient;		// coef = 1 or not according to current settings
				z[k] = t.getHeight ();
				memoUTree (k, t);	// add a reference to the underlying tree in uTrees

				if (girthMode) {x[k] = x[k] * Math.PI;}		// girth mode management

				if (k > 0 && approxClassWidth == Double.MIN_VALUE) {	// second loop only
					approxClassWidth = x[1] - x[0];
				}

				xMin = Math.min (xMin, x[k]);
				xMax = Math.max (xMax, x[k]);
				yMin = Math.min (yMin, y[k]);
				yMax = Math.max (yMax, y[k]);
				if (zMin !=0) {		// for height bounds, we want the accurate min value (not zero)
					zMin = Math.min (zMin, z[k]);
				}
				zMax = Math.max (zMax, z[k]);
				k++;
			}

			double x0 = xMin - approxClassWidth / 2;
			if (xBeginsAtZero) {x0 = 0d;}
			nBounds.setRect (x0, 0, xMax - x0 + approxClassWidth / 2, yMax);
			hBounds.setRect (x0, zMin, xMax - x0 + approxClassWidth / 2, zMax);
			makeBars ();
			
//System.out.println ("MaidHisto, reset() (2), xMin="+xMin);

		} catch (Exception e) {
			Log.println (Log.ERROR, "MaidHisto.reset ()", "Error while reseting histo on stand "+treeSet, e);
		}
	}
	
	/**	Calculates X max depending on current options.
	*/
	public double getXMax () {
		double xMax = this.realXMax;
		if (girthMode) {xMax = xMax * Math.PI;}
		if (forcedXMax != INACTIVE) {xMax = forcedXMax;}
		return xMax;
	}

	/**	Calculates X min depending on current options.
	*/
	public double getXMin () {
		double realXMin = this.realXMin;
		if (forcedXMin > this.realXMax) {forcedXMin = INACTIVE;}			// Ph. Dreyfus 2015-02-02
		if (girthMode) {realXMin = realXMin * Math.PI;}
		// If x min is not forced, make xMin tally with an immediately lower
		// than realXMin multiple of classWidth
		// This will help to draw two MaidHisto in the same Diagram2D
		double xMin = ((int) (realXMin/classWidth)) * classWidth;
		if (forcedXMin != INACTIVE) {xMin = forcedXMin;}
		return xMin;
	}

	/**	Recalculates the histogram with given forcedXMin, forcedXMax and classWidth.
	*/
	public void update () {
		if (isEmpty ()) {return;}

		// Trace...
	/*	System.out.println ("classWidth = "+classWidth);
		System.out.println ("forcedXMin = "+forcedXMin);
		System.out.println ("forcedXMax = "+forcedXMax);
		System.out.println ("treeSet = "+treeSet);*/

		// Trivial cases are implemented in reset method
		if ((Math.abs (classWidth) <= EPSILON)		// i.e. classWidth == 0...
				|| (forcedXMin != INACTIVE && forcedXMax != INACTIVE && forcedXMin >= forcedXMax)
				|| treeSet == null || treeSet.isEmpty ()) {
			reset ();
			return;
		}

		Iterator trees = treeSet.iterator ();

		double xMax = getXMax ();
		double xMin = getXMin ();

		double yMax = 0;
		double zMin = Double.MAX_VALUE;
		double zMax = Double.MIN_VALUE;

		// Calculate n (number of bars in histos)
		// NOTE: forced values can be used to change X interval
		int n = (int) ((xMax - xMin) / classWidth) + 1;

		// Added this security - fc - 9.4.2003
		if (n <= 0) {reset ();}
		
		// fc-8.7.2013 there is a bug here when MaidHisto is opened in GirthMode.
		// Workaround: start capsis -no
		// TO BE FIXED

		x = new double[n];
		y = new double[n];
		z = new double[n];
		uTrees = new Collection[n];
		xLabels = new String [n];

		// First tree (at least one tree, see upper if (treeSet.isEmpty ()))
		Tree t = (Tree) trees.next ();

		double number = 1;	// fc - 22.8.2006 - Numberable returns double
		try {number = ((Numberable) t).getNumber ();} catch (Exception e) {}

		double tx = t.getDbh ();
		double ty = number;
		double tz = t.getHeight ();
		if (girthMode) {tx = tx * Math.PI;}		// girth mode management

		// Loop on classes
		double k = xMin;	// min limit of first class
		int index = 0;		// first class
		double lastKMin = 0;

		while (t != null && k <= xMax) {

			if (forcedXMax != INACTIVE && tx >= forcedXMax) {break;}	// don't reach forcedXMax

			int m = 0;			// to help compute average heights during aggregation
			double kMin = k;
			double kMax = k + classWidth;	// we know that classWidth > 0 (EPSILON)
			xLabels[index] = ""+formater.format (kMin)+"-"+formater.format (kMax);

			x[index] = kMin + classWidth/2;	// class anchor = class middle
			lastKMin = kMin;	// last class begin value

			// Add trees in one class [kMin, kMax[
			while (t != null && tx < kMax) {

				if (forcedXMax != INACTIVE && tx >= forcedXMax) {break;}	// don't reach forcedXMax

				// Add tree in class
				//~ if (tx >= kMin && ty >= 0) {	// fc - 6.1.2005 - if ty (number) == 0, ignore the tree
				if (tx >= kMin) {
					y[index] += ty;
					z[index] += tz;
					m++;
					memoUTree (index, t);	// add a reference to the underlying tree in uTrees
				}

				// Next tree
				if (trees.hasNext ()) {
					t = (Tree) trees.next ();

					number = 1;
					try {number = ((Numberable) t).getNumber ();} catch (Exception e) {}

					tx = t.getDbh ();
					ty = number;
					tz = t.getHeight ();
					if (girthMode) {tx = tx * Math.PI;}		// girth mode management

				} else {
					t = null;	// no more trees
				}

			}

			y[index] *= hectareCoefficient;	// coef = 1 or not according to current settings

			// Memo max y for bounds
			yMax = Math.max (yMax, y[index]);

			// Compute average height for the class
			if (m > 1) {
				z[index] /= m;
			}

			// Memo min & max z for bounds
			if (z[index] !=0) {		// for height bounds, we want the accurate min value (not zero)
				zMin = Math.min (zMin, z[index]);
			}
			zMax = Math.max (zMax, z[index]);

			// Next class
			index++;
			k += classWidth;
		}

		// xMax correction according to last class
		xMax = lastKMin + classWidth;	// i.e. last class upper value

		double x0 = xMin;	// - classWidth / 2;
		if (xBeginsAtZero) {x0 = 0d;}
	//	nBounds.setRect (x0, 0, xMax - x0 + classWidth / 2, yMax);
	//	hBounds.setRect (x0, zMin, xMax - x0 + classWidth / 2, zMax - zMin);
		nBounds.setRect (x0, 0, xMax - x0, yMax);
		hBounds.setRect (x0, zMin, xMax - x0, zMax - zMin);
		makeBars ();

		// Trace : resum numbers and write in console.
		int sum = 0;
		for (int i = 0; i < y.length; i++) {
			sum += y[i];
		}
		//~ System.out.println ("update (): re-sum gives N = "+sum);
		// End of trace
	}

	/**	Return true if this histo and the given one are fusionable, i.e. can
	*	be drawn on the same graphic.
	*/
	public boolean isFusionableWith (MaidHisto histo) {
		if (histo == null) {return false;}
		if (classWidth == 0) {return false;}
		if (classWidth != histo.getClassWidth ()) {return false;}
		if ((getXMin () - histo.getXMin ()) % classWidth != 0) {return false;}
		return true;
	}

	//	Add a reference to the underlying tree in uTrees
	//
	private void memoUTree (int index, Tree t) {
		if (uTrees == null) {return;}
		if (uTrees[index] == null) {uTrees[index] = new ArrayList ();}
		uTrees[index].add (t);
	}

	/**	Return the underlying maid trees under bar matching the given index.
	*/
	public Collection getUTrees (int index) {
		try {
			return uTrees[index];
		} catch (Exception e) {
			return null;
		}
	}

	/**	Change step. Then, call update ().
	*/
	public void setTrees (Collection trees) {
		try {
			treeSet = new TreeSet (new TreeDbhThenIdComparator ());	// we sort trees by ascending dbh
//			treeSet.addAll (trees);
// bug correction : some maid trees may have number == 0 -> ignore them - tl 12/09/2005
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();
				if ((t instanceof Numberable) && (((Numberable) t).getNumber ()==0)) continue;
				treeSet.add (t);
			}

			if (!treeSet.isEmpty ()) {
				realXMin = ((Tree) treeSet.first ()).getDbh ();
				realXMax = ((Tree) treeSet.last ()).getDbh ();
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "MaidHisto.setTrees ()",
					"Error while retrieving treeSet (should be a Collection with GTrees in it)", e);
			return;
		}
	}

	/**	Empty MaidHisto if no trees in it.
	*/
	public boolean isEmpty () {return treeSet == null || treeSet.isEmpty ();}

	/**	Change classWidth. Then, call update ().
	*/
	public void setClassWidth (double classWidth) {this.classWidth = classWidth;}

	/**	Change forced beginning x for histogram. Then, call update ().
	*/
	public void setForcedXMin (double forcedXMin) {this.forcedXMin = forcedXMin;}

	/**	Change forced ending x for histogram. Then, call update ().
	*/
	public void setForcedXMax (double forcedXMax) {this.forcedXMax = forcedXMax;}

	/**	Force bounds to begin at zero on X axis.
	*/
	public void setXBeginsAtZero (boolean b) {this.xBeginsAtZero = b;}

	/**	Girth instead of Dbh.
	*/
	public void setGirthMode (boolean b) {this.girthMode = b;}

	/**	Calculate per hectare.
	*/
	public void setHectareCoefficient (double c) {this.hectareCoefficient = c;}

	/**	Calculate per hectare.
	*/	// fc - 5.9.2005 - added this accessor
	public double getHectareCoefficient () {return hectareCoefficient;}

	/**	Return optional xLabels, may return null.
	*/
	public String [] getXLabels () {return xLabels;}

	/**	Return the N bounds computed last time.
	*/
	public Rectangle.Double getNBounds () {return nBounds;}

	/**	Return the H bounds computed last time.
	*/
	public Rectangle.Double getHBounds () {return hBounds;}

	/**	Return the N bars collection.
	*/
	public java.util.List getNBars () {return nBars;}

	/**	Return the H bars collection.
	*/
	public java.util.List getHBars () {return hBars;}

	/**	Return histo class width.
	*/
	public double getClassWidth () {return classWidth;}

	/**	Return real x min (computed in setTrees : always match current step).
	*/
	public double getRealXMin () {return realXMin;}

	/**	Return real x max (computed in setTrees : always match current step).
	*/
	public double getRealXMax () {return realXMax;}

	/**	Return forced x min.
	*/
	public double getForcedXMin () {return forcedXMin;}

	/**	Return forced x max.
	*/
	public double getForcedXMax () {return forcedXMax;}

	/**	Return true if X axis begins at zero.
	*/
	public boolean isXBeginsAtZero () {return xBeginsAtZero;}

	/**	Return true if hectare coefficient is not equal to 1.
	*/
	public boolean isPerHectare () {return hectareCoefficient != 1;}

	/**	Return true girth mode is on.
	*/
	public boolean isGirthMode () {return girthMode;}

	//	Create the bars collections (to be called at the end of reset () or update ().
	//
	protected void makeBars () {
		nBars.clear ();
		hBars.clear ();
		for (int i = 0; i < x.length; i++) {
			Vertex2d b = new Vertex2d (x[i], y[i]);
			nBars.add (b);
			Vertex2d c = new Vertex2d (x[i], z[i]);
			hBars.add (c);
		}

	}

	/**
	 * Returns the values in the maidHisto as doubles
	 */
	public double[] getDoubleValues () {  // fc-16.5.2011 (tf)
		List bars = this.getNBars ();
		int histoSize = bars.size ();
		double[] initialValues = new double[histoSize];
		int k = 0;
		for (Object o : bars) {
			Vertex2d bar = (Vertex2d) o;
			initialValues[k++] = bar.y;  // the value for each class
		}
		return initialValues;
	}

	/**
	 * Returns the values in the maidHisto as ints
	 */
	public int[] getIntValues () {  // fc-16.5.2011 (tf)
		double[] doubles = getDoubleValues ();
		int[] initialValues = new int[doubles.length];
		int k = 0;
		for (double v : doubles) {
			initialValues[k++] = (int) Math.round (v);  // the value for each class
		}
		return initialValues;
	}

	
	/**	Trace.
	*/
	public String toString () {
		double totalNumbers = -1;
		try {for (int i = 0; i < y.length; i++) {totalNumbers+=y[i];}} catch (Exception e) {};
		String s = "MaidHisto n="+nBars.size ()+" totalNumbers="+totalNumbers;
		s+=" nBars="+getNBars ();
		s+=" nBounds="+getNBounds ();
		s+=" hBars="+getHBars ();
		s+=" hBounds="+getHBounds ();
		s+=" classWidth="+classWidth;
		s+=" realXMin="+realXMin;
		s+=" realXMax="+realXMax;
		s+=" forcedXMin="+forcedXMin;
		s+=" forcedXMax="+forcedXMax;
		return s;
	}

}

