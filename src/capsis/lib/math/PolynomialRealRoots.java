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

import java.util.Vector;

/**
 * Second, third and fourth order polynomials REAL roots calculation
 *
 * @author A. Piboule - october 2004
 */


public abstract class PolynomialRealRoots {

	public static final double THIRD = 1d/3d;

	// solve a 2° order equation : ax² + bx + c = 0
	public static double[] solveOrder2 (double a, double b, double c) {
		if (a==0d) {
			if (b==0d) {
				return new double[0];
			} else {
				double[] t = new double[1];
				t[0] = -c/b;
				return t;
			}
		}

		double[] roots;

		double delta = b*b - 4d*a*c;

		if (delta>0d) {
			double deltaSqrt = Math.sqrt (delta);
			roots = new double[2];
			roots[0] = (-b + deltaSqrt)/(2d*a);
			roots[1] = (-b - deltaSqrt)/(2d*a);

		} else if (delta==0d) {
			roots = new double[2];
			roots[0] = -b/(2d*a);
			roots[1] = roots[0];
		} else {
			roots = new double[0];
		}

		return roots;
	}


	// solve a 3° order equation : ax3 + bx2 + cx + d = 0
	public static double[] solveOrder3 (double a, double b, double c, double d) {
		if (a==0d) {
			return solveOrder2 (b, c, d);
		}

		double[] roots;

		// we use the form x3 + A*x2 + B*x + C = 0
		final double A = b/a;
		final double B = c/a;
		final double C = d/a;
		final double Ao3 = -A/3d; // frequently used, calculate once

		// use y = x + A/3 to obtain the form y3 + P*y + Q = 0
		final double P = B - A*A/3d;
		final double Q = C + 2d*A*A*A/27d + B*Ao3;

		// calculate discrimiant DELTA
		final double DELTA = P*P*P/27d + Q*Q/4d;

		// separates differents case, depends on DELTA sign

		// only one real root
		if (DELTA>0) {
			roots = new double[1];

			final double S = -Q/2d + Math.sqrt (DELTA);
			final double T = -Q/2d - Math.sqrt (DELTA);
			double u = Math.pow (Math.abs (S), THIRD);
			double v = Math.pow (Math.abs (T), THIRD);
			if (S<0) {u *= -1d;}
			if (T<0) {v *= -1d;}
			roots[0] = Ao3 + u + v;

		// two differents real roots : one single and one double (can be equals)
		} else if (DELTA==0) {
			roots = new double[3];
			roots[0] = Ao3 + 2d*Math.pow (-Q/2d, THIRD);
			roots[1] = Ao3 -    Math.pow (-Q/2d, THIRD); // double root
			roots[2] = roots[1];

		// if DELTA<0 : three real roots
		} else {
			roots = new double[3];
			final double absPo3 = Math.abs (P)/3d;
			final double PHI = Math.acos (-Q/(2d*Math.sqrt (Math.pow (absPo3,3d))));
			roots[0] = Ao3 + 2d*Math.sqrt (absPo3)*Math.cos (PHI/3d);
			roots[1] = Ao3 - 2d*Math.sqrt (absPo3)*Math.cos ((PHI - Math.PI)/3d);
			roots[2] = Ao3 - 2d*Math.sqrt (absPo3)*Math.cos ((PHI + Math.PI)/3d);
		}

		return roots;
	}


	// solve a 4° order equation : ax4 + bx3 + cx2 + dx + e = 0
	public static double[] solveOrder4 (double a, double b, double c, double d, double e) {
		if (a==0d) {
			return solveOrder3 (b, c, d, e);
		}

		// we use the form x4 + A*x3 + B*x2 + C*X + d = 0
		final double A = b/a;
		final double B = c/a;
		final double C = d/a;
		final double D = e/a;

		// tansform to X4 + p*X2 + q*X + r = 0 (by x = X - A/4)
		final double p = B - 3d/8d*A*A;
		final double q = C - A*B/2d + 1d/8d*A*A*A;
		final double r = -3d/256d*A*A*A*A + A*A*B/16d - A*C/4d + D;
		double[] yRoots;
		Vector sol = new Vector ();

		if (q==0) { // if q=0 then solve bi-quadradic equation
			double[] tab = solveOrder2 (1d, p, r);
			for (int i=0;i<tab.length;i++) {
				yRoots = new double[2];
				if (tab[i]>=0) {
					yRoots[0] = Math.sqrt (tab[i]);
					yRoots[1] = -1d*Math.sqrt (tab[i]);
					sol.add (yRoots);
				}
			}

		} else {
			yRoots = solveOrder3 (1d,  -1d*p, -4d*r, 4d*p*r-q*q);

			double z = q/(2d*(yRoots[0]-p));

			double val1 = Math.sqrt (Math.abs (yRoots[0] -p));
			double val2 = yRoots[0]/2d;
			double val3 = z*val1;

			sol.add (solveOrder2 (1d, -val1, val2+val3));
			sol.add (solveOrder2 (1d,  val1, val2-val3));
		}


		int cpt = 0;
		for (int i =0; i<sol.size ();i++) {
			double[] tab = (double[]) sol.get (i);
			cpt += tab.length;
		}
		double[] roots = new double[cpt];

		cpt = 0;
		for (int i =0; i<sol.size ();i++) {
			double[] tab = (double[]) sol.get (i);
			for (int j=0;j<tab.length;j++) {
				roots[cpt] = tab[j]-A/4d;
				cpt++;
			}
		}

		return roots;
	}






	// for examples below, launch "java capsis.lib.mathutil.PolynomialRealRoots"
	public static void main (String[] args) {
		double a, b, c, d, e;a=b=c=d=e=0;
		double[] roots=null;

// second order examples
		System.out.println ("-------------------------------------------------");
		System.out.println ("Second Order Test (0, 1 double, or 2 real roots):");
		System.out.println ("");
		a = 2d;
		b = 2d;
		c = 4d;
		roots = solveOrder2 (a, b, c);
		System.out.println ("Real solutions of the equation "+a+"x2 + "+b+"x + "+c+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		a = 1d;
		b = 2d;
		c = 1d;
		roots = solveOrder2 (a, b, c);
		System.out.println ("Real solutions of the equation "+a+"x2 + "+b+"x + "+c+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		a = 1d;
		b = -8d;
		c = 1d;
		roots = solveOrder2 (a, b, c);
		System.out.println ("Real solutions of the equation "+a+"x2 + "+b+"x + "+c+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

// third order examples
		System.out.println ("");
		System.out.println ("-----------------------------------------------------------");
		System.out.println ("Third Order Test (1, 2 (with one double), or 3 real roots):");
		System.out.println ("");
		a = 2d;
		b = 1d;
		c = 1d;
		d = -3d;
		roots = solveOrder3 (a, b, c, d);
		System.out.println ("Real solutions of the equation "+a+"x3 + "+b+"x2 + "+c+"x + "+d+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		// case of 1 double root and a third root equal to the double root:
		a = 1d;
		b = 6d;
		c = 12d;
		d = 8d;
		roots = solveOrder3 (a, b, c, d);
		System.out.println ("Real solutions of the equation "+a+"x3 + "+b+"x2 + "+c+"x + "+d+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		a = 1d;
		b = 2d;
		c = -5d;
		d = -6d;
		roots = solveOrder3 (a, b, c, d);
		System.out.println ("Real solutions of the equation "+a+"x3 + "+b+"x2 + "+c+"x + "+d+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");


// fourth order examples
		System.out.println ("");
		System.out.println ("-----------------------------------");
		System.out.println ("Fourth Order Test (0-4 real roots):");
		System.out.println ("");
		a = 1d;
		b = 2d;
		c = 3d;
		d = 4d;
		e = 5d;
		roots = solveOrder4 (a, b, c, d, e);
		System.out.println ("Real solutions of the equation "+a+"x4 + "+b+"x3 + "+c+"x2 + "+d+"x + "+e+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		a = -1d;
		b = -2d;
		c = 3d;
		d = 4d;
		e = 5d;
		roots = solveOrder4 (a, b, c, d, e);
		System.out.println ("Real solutions of the equation "+a+"x4 + "+b+"x3 + "+c+"x2 + "+d+"x + "+e+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");

		a = 1d;
		b = 5d;
		c = -7d;
		d = -29d;
		e = 30d;
		roots = solveOrder4 (a, b, c, d, e);
		System.out.println ("Real solutions of the equation "+a+"x4 + "+b+"x3 + "+c+"x2 + "+d+"x + "+e+" = 0 :");
		for (int i=0 ; i< roots.length;i++) {
			System.out.println (roots[i]);
		}
		if (roots.length==0) {System.out.println ("No real solution!");}
		System.out.println ("");
	}




}