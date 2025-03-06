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

import jeeb.lib.util.Vertex2d;

/**
 * A Serializable point with double coordinates. Do not use this class. Use Vertex2d instead.
 * 
 * @author F. de Coligny - march 2001
 */
public abstract class Point2D {
	public static class Double extends Vertex2d {
		
		public Double (double a, double b) {super (a, b);}
		public Double (String s) throws Exception {super (s);}
			
	
	}
	
	// for test only
	public static void main (String[] a) {
		try {
			Point2D.Double p2 = new Point2D.Double ("  (  1.25  ,   2.34  )  ");
			System.out.println (p2.toString ());
		} catch (Exception e) {
			System.out.println (e);
		}
	}

}
	
	