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

import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.util.Vector;


/**
 * Geometrical ellipse definition for ellipse-ellipse intersection
 *
 * @author A. Piboule - october 2004
 */

/*an ellipse is defined by: a center (x0, y0),
							an (x) semi axis length a
							an (y) semi axis length b
/* alpha is the angle between x axis and a axis of the ellipse (in radians) (counterClockWise)
*/
public class Ellipse {

	// Pi/4 constant (frequently used in calculations)
	public static final double PIo4 = Math.PI/4d;
	public static double tolerance = 0.001d; // by default 1/1000 of ellipse radius in the considered direction

	public static void setTolerance (double t) {
		tolerance = t;
	}

	public double x0;
	public double y0;
	public double a;
	public double b;
	public double alpha;

	public double a0; // algebraic ellipse eqaution coefficients (see below)
	public double a1;
	public double a01;
	public double b0;
	public double b1;
	public double c0;

	public Ellipse (double xt, double yt, double at, double bt, double alp) {
		x0 = xt;
		y0 = yt;
		a = at;
		b = bt;
		alpha=alp;

		// ellipse equation is (X)²/a² + (Y)²/b² = 1 (in ellipse coordinate system)

		// developped : a0*x² + a1*y² + 2*a01*x*y+ b0*x + b1*y + c0 = 0
		double aa=a*a; double bb=b*b;
		double cosA = Math.cos (alpha); double cos2A = cosA*cosA;
		double sinA = Math.sin (alpha); double sin2A = sinA*sinA;

		a0 = bb*cos2A + aa*sin2A;

		a1 = bb*sin2A + aa*cos2A;

		a01= cosA*sinA*(bb-aa); //if alpha=0 then a01=0

		b0 = -2d*(x0*a0 + y0*a01);

		b1 = -2d*(y0*a1 + x0*a01);

		c0 = x0*x0*a0 + y0*y0*a1 + 2d*x0*y0*a01 - aa*bb;
	}



	// gives a drawable shape for the ellipse
	public Shape getShape () {
		Shape eBase = new Ellipse2D.Double (x0-a, y0-b, 2d*a, 2d*b);
		AffineTransform at = new AffineTransform ();
		at.rotate (alpha, x0, y0);
		return at.createTransformedShape (eBase);
	}

	// gives the area of the ellipse
	public double getArea () {
		return Math.PI*a*b;
	}


	// Gives a list of intersection beetween the two specified ellipses (0-4 intersections)
	// If there is no intersection: ellipses can be separated, or one can be included in the other
	public static Vector ellipsesIntersection (Ellipse e0, Ellipse e1) {

		Vector result = new Vector ();

		double v0  = 2d*(e0.a0*e1.a01-e1.a0*e0.a01);
		double v1  = e0.a0*e1.a1 - e1.a0*e0.a1;
		double v2  = e0.a0*e1.b0 - e1.a0*e0.b0;
		double v3  = e0.a0*e1.b1 - e1.a0*e0.b1;
		double v4  = e0.a0*e1.c0 - e1.a0*e0.c0;
		double v5  = 2d*(e0.a01*e1.a1-e1.a01*e0.a1);
		double v6  = 2d*(e0.a01*e1.b1-e1.a01*e0.b1);
		double v7  = 2d*(e0.a01*e1.c0-e1.a01*e0.c0);
		double v8  = e0.a1*e1.b0 - e1.a1*e0.b0;
		double v9  = e0.b0*e1.b1 - e1.b0*e0.b1;
		double v10 = e0.b0*e1.c0 - e1.b0*e0.c0;

		double u0 = v2*v10 - v4*v4;
		double u1 = v0*v10 + v2*(v7 + v9) - 2d*v3*v4;
		double u2 = v0*(v7 + v9) + v2*(v6 - v8) - v3*v3 - 2d*v1*v4;
		double u3 = v0*(v6 - v8) + v2*v5 - 2d*v1*v3;
		double u4 = v0*v5 - v1*v1;
		double[] ySol = PolynomialRealRoots.solveOrder4 (u4, u3, u2, u1, u0);


		for (int i =0;i<ySol.length;i++) {
			double y = ySol[i];

			double a = e0.a0;
			double b = 2d*e0.a01*y + e0.b0;
			double c = e0.a1*y*y + e0.b1*y + e0.c0;

			double[] xSol = PolynomialRealRoots.solveOrder2 (a, b, c);




			for (int j=0;j<xSol.length;j++) {
				double x = xSol[j];


			// For each ellipse, test that distance to potential intersection point
			// is not >> or << to ellipse radius in the direction of this point
				// ellipse e0
				double dx = x-e0.x0;
				double dy = y-e0.y0;

				double l0 = Math.sqrt (Math.pow (dx, 2) + Math.pow (dy, 2));
				double a0;

				if (dx==0) {
					if (dy>0) {a0 = Math.PI/2d;}
					else      {a0 = 3d*Math.PI/2d;}
				} else if (dx>0) {
					a0 = Math.atan (dy/dx);
				} else {
					a0 = Math.PI + Math.atan (dy/dx);
				}
				if (a0<0) {a0 += Math.PI*2d;}

				a0 -= e0.alpha;
				if (a0<0) {a0 += Math.PI*2d;}

				double xEll0 = e0.b*Math.cos (a0);
				double yEll0 = e0.a*Math.sin (a0);
				double lEll0 = Math.sqrt ((e0.a*e0.a*e0.b*e0.b)/(Math.pow (xEll0, 2) + Math.pow (yEll0, 2)));

				// ellipse e1
				dx = x-e1.x0;
				dy = y-e1.y0;

				double l1 = Math.sqrt (Math.pow (dx, 2) + Math.pow (dy, 2));
				double a1;

				if (dx==0) {
					if (dy>0) {a1 = Math.PI/2d;}
					else      {a1 = 3d*Math.PI/2d;}
				} else if (dx>0) {
					a1 = Math.atan (dy/dx);
				} else {
					a1 = Math.PI + Math.atan (dy/dx);
				}
				if (a1<0) {a1 += Math.PI*2d;}

				a1 -= e1.alpha;
				if (a1<0) {a1 += Math.PI*2d;}

				double xEll1 = e1.b*Math.cos (a1);
				double yEll1 = e1.a*Math.sin (a1);
				double lEll1 = Math.sqrt ((e1.a*e1.a*e1.b*e1.b)/(Math.pow (xEll1, 2) + Math.pow (yEll1, 2)));


				double delta0 = Math.abs (lEll0-l0)/lEll0;
				double delta1 = Math.abs (lEll1-l1)/lEll1;
				if ((delta0<tolerance) && (delta1<tolerance)) {
					result.add (new Point2D.Double (x, y));
				}


			}

/*
			if (xSol.length>0) {
				double x = xSol[0];
				double val1 = Math.abs (e0.a0*x*x + e0.a1*y*y + 2d*e0.a01*x*y + e0.b0*x + e0.b1*y + e0.c0)
							+ Math.abs (e1.a0*x*x + e1.a1*y*y + 2d*e1.a01*x*y + e1.b0*x + e1.b1*y + e1.c0);
System.out.println ("VAL1="+val1);
				if (xSol.length==2) {
					double val2 = Math.abs (e0.a0*xSol[1]*xSol[1] + e0.a1*y*y + 2d*e0.a01*xSol[1]*y + e0.b0*xSol[1] + e0.b1*y + e0.c0)
								+ Math.abs (e1.a0*xSol[1]*xSol[1] + e1.a1*y*y + 2d*e1.a01*xSol[1]*y + e1.b0*xSol[1] + e1.b1*y + e1.c0);
System.out.println ("VAL2="+val2);
					if (val2<val1) {
						x = xSol[1];
					}

				}

				result.add (new Point2D.Double (x, y));
			}
*/
		}

		// eliminate duplicate intersections
		for (int i=0;i<result.size ();i++) {
			Point2D.Double pt = (Point2D.Double) result.get (i);
			for (int j=0;j<i;j++) {
				Point2D.Double pt2 = (Point2D.Double) result.get (j);
				if ((pt.x==pt2.x) && (pt.y==pt2.y)) {
					result.remove (pt);
					i--;
					break;
				}
			}

		}

		return result;
	}


	// Gives a list of intersection beetween the considered ellipse and the segment [(x1, y1);(x2,y2)]
	// it can be 0-2 intersections
	public Vector ellipseSegmentIntersection (double x1, double y1, double x2, double y2) {
		Vector result = new Vector ();

		double xmin;
		double xmax;
		double ymin;
		double ymax;

		// define min and max segment bounds for x and y
		if (x1<x2) {
			xmin = x1;
			xmax = x2;
		} else {
			xmin = x2;
			xmax = x1;
		}

		if (y1<y2) {
			ymin = y1;
			ymax = y2;
		} else {
			ymin = y2;
			ymax = y1;
		}

		// calculate intersection between segment and the ellipse
		if (x1!=x2) {
			double slope = (y1-y2)/(x1-x2);
			double ord   = y1 - slope*x1;

			double coefa = a0 + a1*slope*slope + 2d*a01*slope;
			double coefb = 2d*a1*slope*ord + b0 + b1*slope + 2d*a01*ord;
			double coefc = a1*ord*ord + b1*ord + c0;

			double[] xSol = PolynomialRealRoots.solveOrder2 (coefa, coefb, coefc);

			for (int i=0;i<xSol.length;i++) {
				double ySol = slope*xSol[i] + ord;

				if ((xSol[i]>=xmin) && (xSol[i]<=xmax) &&
				    (ySol   >=ymin) && (ySol   <=ymax)) {

					result.add (new Point2D.Double (xSol[i], ySol));
				}
			}

		// case of x=constant=x1=x2 (vertical segment)
		} else {

			double coefa = a1;
			double coefb = b1 + 2d*a01*x1;
			double coefc = a0*x1*x1 + b0*x1 + c0;

			double[] ySol = PolynomialRealRoots.solveOrder2 (coefa, coefb, coefc);

			for (int i=0;i<ySol.length;i++) {

				if ((ySol[i]>=ymin) && (ySol[i]<=ymax)) {

					result.add (new Point2D.Double (x1, ySol[i]));
				}
			}
		}


		return result;
	}

	// Gives the ellipse radius in beta direction (in radians)
	// beta is the angle from x axis counterClockWise (in radians)
	public double getEllipseRadiusAt (double betaBase) {

			double beta = betaBase - alpha;
			if (beta<0) {beta += 2d*Math.PI;}

			double cosBeta = Math.cos (beta);
			double sinBeta = Math.sin (beta);

			return Math.sqrt (1/(cosBeta*cosBeta/(a*a)+sinBeta*sinBeta/(b*b)));
	}



	// for the example below about ellipses intersections, launch "java capsis.lib.mathutil.Ellipse"
	public static void main (String[] args) {

		//Create two ellipses
		double angle = Math.PI/2d;
		Ellipse e1;
		Ellipse e2;
		Vector res;

		// Example 1
		System.out.println ("Ex0: 2 intersections (inclined ellipse)");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (2d, 2d, 3d, 2d, angle);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");


		// Example 1
		System.out.println ("Ex1: 0 intersections");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (7d, 0d, 3d, 2d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");

		// Example 2
		System.out.println ("Ex2: 1 intersections");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (6d, 0d, 3d, 2d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");

		// Example 3
		System.out.println ("Ex3: 2 intersections");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (5d, 0d, 3d, 2d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");

		// Example 4
		System.out.println ("Ex4: 3 intersections");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (1d, 0d, 2d, 3d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");

		// Example 5
		System.out.println ("Ex5: 4 intersections");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (1d, 0d, 1d, 3d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");

		// Example 6
		System.out.println ("Ex6: 4 intersections (same y same x)");
		e1 = new Ellipse (0d, 0d, 3d, 2d, 0d);
		e2 = new Ellipse (0d, 0d, 1d, 3d, 0d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");



		// TEST
		System.out.println ("TEST");
		e1 = new Ellipse (303.2940979003906d, 256.4143981933594d, 1.1460696458816528d, 1.1460696458816528d, 6.159231150718744d);
		e2 = new Ellipse (314.4324951171875d, 257.27191162109375d, 0.06788381934165955d, 0.06788381934165953d, 3.7308320649983724d);
		// find intersections
		res = Ellipse.ellipsesIntersection (e1, e2);
		System.out.println ("There are "+res.size ()+" intersections between specified ellipses");
		for (int i=0;i<res.size ();i++) {
			Point2D.Double pt = (Point2D.Double) res.get (i);
			System.out.println ("x="+pt.x+" y="+pt.y);
		}
		System.out.println ("");



		System.out.println ("Now launching visual demonstration...");

		try {
			Thread.sleep (2000);
		} catch (Exception e) {}


		EllipseDemo.DemoWindow myFrame = new EllipseDemo.DemoWindow ("Ellipse intersection demonstration", e1, e2);

	}



}
