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

package capsis.extension.datarenderer.drcurves;

import java.awt.FontMetrics;

import capsis.util.Interval;


//_________________________________________________________________________
public class GraduationLengthApproximer {
	public static final int MAXIMUM_FRACTION_DIGITS = 2;

	/**	Graduation length approximation.
	 */
	static public int approximateGradLength (Interval interval, FontMetrics fm, boolean valuesAreIntegers) {
		// heuristic for wYGrad, wZGrad, hXGrad computation (try to be not so bad...)

		// bug correction - fc - 4.10.2002
		// (bc) "some grads may be cut along Y axis" - fixed
		//
		StringBuffer a = new StringBuffer ();
		StringBuffer b = new StringBuffer ();

		// sign
		//
		if (interval.a < 0) {a.append ('-');}
		if (interval.b < 0) {b.append ('-');}

		// integer part
		//
		a.append (Math.abs ((int) interval.a));
		b.append (Math.abs ((int) interval.b));

		// decimal part
		//
		if (valuesAreIntegers) {
			a.append ('8');	// some vertical grads on X axis could be trucated in small graphics
			b.append ('8');
		} else {
			if (MAXIMUM_FRACTION_DIGITS > 0) {	// decimal point
				a.append ('.');
				b.append ('.');
			}
			for (int i = 0; i < MAXIMUM_FRACTION_DIGITS; i++) {	// max size with all possible fraction digits
				a.append ('8');
				b.append ('8');
			}
		}

		// total potential sizes
		//
		int la = fm.stringWidth (a.toString ());
		int lb = fm.stringWidth (b.toString ());

		int result = Math.max (la, lb);

		String minString = "88";
		int lMinString = fm.stringWidth (minString);

		result = Math.max (result, lMinString);	// minimum 2 digits (needed for X axis)

		return result;
	}
}

//_________________________________________________________________________










