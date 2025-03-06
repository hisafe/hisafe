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

import jeeb.lib.util.Check;
import jeeb.lib.util.Log;

/**	AxBString proposes a collection of static methods.
*	The purpose is to manage String of type Value1 x Value2.
*	These values are double (see WxHString for int values). 
*	@author F. de Coligny - october 2007
*/
public class AxBString {
	private String string;

	public AxBString (String s) {
		string = s;
	}
	public AxBString (String a, String b) {
		StringBuffer sb = new StringBuffer ();
		sb.append (a.trim ());
		sb.append ("x");
		sb.append (b.trim ());
		string = sb.toString ();
	}
	public AxBString (double a, double b) {
		this (""+a, ""+b);
	}

	public double getA () {
		try {
			return Check.doubleValue (string.substring (0, string.indexOf ("x")));
		} catch (Exception exc) {
			Log.println (Log.WARNING, "AxBString.getA ()", 
					"Error while reading first part of formatted string, returned 0."
					+" string="+string
					+" Exception="+exc.toString ());
			return 0d;
		}
	}

	public double getB () {
		try {
			return Check.doubleValue (string.substring (string.indexOf ("x")+1, string.length ()));
		} catch (NumberFormatException exc) {
			Log.println (Log.WARNING, "AxBString.getB ()", 
					"Error while reading second part of formatted string, returned 0."
					+" string="+string
					+" Exception="+exc.toString ());
			return 0d;
		}
	}
	
	public String toString () {
		return string;
	}

}
