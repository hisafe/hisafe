/*
 * mathutil library for Capsis4.
 *
 * Copyright (C) 2004 Francois de Coligny.
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

package capsis.lib.math;

import java.io.Serializable;
/**
 * An enveloped double
 * @author A. Piboule - october 2004
 */

public class DoubleObject implements Comparable, Serializable  {

	public double value;

	/**
	 * Standard constructor.
	 */
	public DoubleObject (double v) {
		value = v;
	}

	/**
	 * Default constructor.
	 */
	public DoubleObject () {
		value = 0d;
	}


	public int compareTo (Object o) throws ClassCastException, ArithmeticException {
		if (o instanceof DoubleObject) {
			if (value < (((DoubleObject) o).value)) {
				return -1;
			} else if (value == (((DoubleObject) o).value)) {
				return 0;
			} else if (value > (((DoubleObject) o).value)) {
				return 1;
			} else {
				throw new ArithmeticException (o+" value is not a comparable number (NaN)");
			}
		} else {
			throw new ClassCastException (o+" is not a DoubleObject");
		}
	}


}