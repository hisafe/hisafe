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
package capsis.lib.math;

/**	Tools arround integration.
*
*	@author F. de Coligny - april 2005
*/
public class Integration {

	/**	Numerical integration using the trapeziodal rule
	*/
	public static double trapezium (Function f1, double a, double b, double dX) {
		double sum = 0d;
		double yPrev = f1.f (a);
		for (double x = a+dX; x <= b; x += dX) {
			double y = f1.f (x);
			sum += dX*(y+yPrev)/2;
			yPrev = y;
		}
		return sum;
	}
	
	
	public static void main (String[] args) {
		Function line = new Line (1, 0);
		System.out.println ("line.f (4) = "+line.f (4));
		
		double i = Integration.trapezium (line, 2, 5, 1);
		System.out.println ("Integration.trapezium (line, 2, 5, 1) = "+i);
		
		i = Integration.trapezium (line, 2, 5, 0.25);
		System.out.println ("Integration.trapezium (line, 2, 5, 0.25) = "+i);
		
		
	}
	
}