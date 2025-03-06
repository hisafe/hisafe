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


/**
 * Converter for simple type from Wrapper types.
 * 
 * @author F. de Coligny - august 2000
 */
public class Converter {
	public double value;

	public static Converter create (Object a) {
		if (a instanceof Double) {
			return new Converter ((Double) a);
		} else if (a instanceof Float) {
			return new Converter ((Float) a);
		} else if (a instanceof Integer) {
			return new Converter ((Integer) a);
		}
		return null;
	}	

	public Converter (Double a) {
		value = a.doubleValue ();
	}
	
	public Converter (Float a) {
		value = (double) a.floatValue ();
	}
	
	public Converter (Integer a) {
		value = new Double (a.intValue ()).doubleValue ();
	}

	public double doubleValue () {
		return value;
	}
	
	public float floatValue () {
		return (float) value;
	}
	
	public int intValue () {
		return (int) value;
	}


}

