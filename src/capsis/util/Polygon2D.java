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

import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.Vector;

import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;

/**
 * A 2D polygon : each vertex is a Vertex2d instance.
 *
 * @author F. de Coligny - march 2001
 */
public class Polygon2D implements Serializable {

	private Collection vertices;		// contains Vertex2d instances

	/**
	 * Vertices is a Collection of Vertex2d instances.
	 */
	public Polygon2D (Collection vertices) throws Exception {
		if (vertices.size () < 3) {throw new Exception ("Can not build a Polygon2D with less than 3 vertices."
				+" Vertices="+vertices);}
		if (!(vertices.iterator ().next () instanceof Vertex2d)) {
				throw new Exception ("Polygon2D vertices must be of type Vertex2d.");}
		this.vertices = vertices;
	}

	/**
	 * Return the origin (Vertex2d) for polygons in Capsis : most left (first) bottom (if several) vertex.
	 */
	public Vertex2d getOrigin () {
		Vertex2d r2 = new Vertex2d (Double.MAX_VALUE, Double.MAX_VALUE);
		for (Iterator i = vertices.iterator (); i.hasNext ();) {
			Vertex2d v2 = (Vertex2d) i.next ();
			if ((v2.x < r2.x) || (v2.x == r2.x && v2.y < r2.y)) {r2 = v2;}
		}
		return r2;
	}

	/**
	 * Return the geometrical center of the polygon. Thanks to Ph. Borianne.
	 */
	public Vertex2d getGeometricalCenter () {
		double xc = 0d;
		double yc = 0d;
		for (Iterator i = vertices.iterator (); i.hasNext ();) {
			Vertex2d v2 = (Vertex2d) i.next ();
			xc += v2.x;
			yc += v2.y;
		}
		xc /= vertices.size ();
		yc /= vertices.size ();
		return new Vertex2d (xc, yc);
	}

	/**
	 * Computes the polygon area in m2.
	 * Author L. Saint-André, Ph. Dreyfus, F. de Coligny.
	 */
	public double getArea () {

		// To disable trace in console, change next line (false)
		//
		boolean trace = false;

		// 1. Change data structure (fc)
		//
		int N = vertices.size () + 1;		// maybe we need one index more to add 1st vertex at the end
		int n = 0;
		double[] x = new double[N];
		double[] y = new double[N];

		Vertex2d v1 = null;		// vertex one
		Vertex2d v2 = null;		// current vertex
		int i = 0;
		for (Iterator j = vertices.iterator (); j.hasNext ();) {
			v2 = (Vertex2d) j.next ();
			if (v1 == null) {v1 = v2;}		// memo first one
			x[i] = v2.x;
			y[i] = v2.y;
			i++;
		}	// on exit, v2 is last vertex

		// 2. If not already done, add first vertex at the end
		//
		if (v2.x == v1.x && v2.y == v1.y) {
			n = N - 1;		// we do not need to do it
		} else {
			n = N;
			x[i] = v1.x;	// add 1st vertex at the end
			y[i] = v1.y;
		}

		if (trace) {
			System.out.println ("Polygon2D.getArea ()------");
			System.out.println ("x="+Tools.toString (x)+" n="+n+" (N="+N+")");
			System.out.println ("y="+Tools.toString (y));
		}

		// 3. Compute area (lsa)
		//
		double area = 0d;
		if (n < 4) {	// fc - at least a triangle is needed : 3 + 1st repeated at the end = 4
			return 0d;

		} else {
			for (int k = 0; k < n-1; k++) {		// fc
				area += (x[k] * y[k+1]) - (x[k+1] * y[k]);
			}
			area = Math.abs (area / 2d);

			if (trace) {
				System.out.println ("area="+area);
			}

			return area;
		}
	}

	/**
	 * Return the origin (Vertex3d) for polygons in Capsis : most left (first) bottom (if several) vertex.
	 */
	public Vertex3d get3DOrigin () {return (Vertex3d.convert (getOrigin ()));}

	/**
	 * Return the vertices Collection (Vertex2d instances).
	 */
	public Collection getVertices () {return vertices;}

	/**
	 * Return the vertices Collection (Vertex3d instances).
	 */
	public Collection get3DVertices () {
		Collection v3s = new Vector ();
		for (Iterator i = vertices.iterator (); i.hasNext ();) {
			Vertex2d v2 = (Vertex2d) i.next ();
			Vertex3d v3 = new Vertex3d (v2.x, v2.y, 0d);
			v3s.add (v3);
		}
		return v3s;
	}

	/**
	 * Shape of the Polygon2D.
	 */
	public Shape getShape () {return getGeneralPath ();}

	/**
	 * Instance method to compute general path (Shape) of this polygon 2D.
	 */
	public GeneralPath getGeneralPath () {
		int n = vertices.size ();

		double[] xs = new double[n];
		double[] ys = new double[n];

		int j = 0;
		for (Iterator i = vertices.iterator (); i.hasNext ();) {
			//Vertex3d v3 = (Vertex3d) i.next ();
			Vertex2d v3 = (Vertex2d) i.next ();	// Vertex2d sufficient (fc - 14.3.2002)
			xs[j] = v3.x;
			ys[j] = v3.y;
			j++;
		}
		GeneralPath gp = Polygon2D.getGeneralPath (xs, ys, n);	// fc - 23.11.2001 (changed from Tools.getPolygon2D ())
		return gp;
	}

	/**
	 * Static method to compute general path (Shape) of a polygon 2D.
	 */
	public static GeneralPath getGeneralPath (double[] xPoints, double [] yPoints, int nPoints) {
		GeneralPath gp = new GeneralPath ();
		gp.moveTo ((float) xPoints[0], (float) yPoints[0]);		// first point

		for (int i = 1; i < nPoints; i++) {
			gp.lineTo ((float) xPoints[i], (float) yPoints[i]);	// lines
		}

		gp.closePath ();

		return gp;
	}

	/**
	 * This method was deprecated, use getGeneralPath (...) instead.
	 */
	public static GeneralPath getPolygon2D (double[] xPoints, double [] yPoints, int nPoints) {	// Do no use
			return getGeneralPath (xPoints, yPoints, nPoints);}		// Do no use

	/**
	 * The points must be instaneof Spatialized.
	 */
	public static Polygon2D getConvexHull (Collection points) {


		return null;
	}

	/**
	 * This method calls contains (double, double) from the Vertex2d given
	 */
	public boolean contains (Vertex2d vert) {
		return contains (vert.x, vert.y);
	}

	/**
	 * This method returns true if the point (x,y) is contained in the polygon.
	 * The method chooses an half line from the point (x,y) at random
	 * it counts the intersection between this half line and polygons borders segments
	 * if the number of intersections is odd the point is included in polygon else not
	 * if the half line causes problem for intersections calculation or is a polygon's vertex
	 * then we try another half line.
	 * Author A. Piboule - april 2004
	 */
	public boolean contains (double x, double y) {

		double a, b, a2, b2, xInter, yInter, maxx, maxy, minx, miny;
		boolean ok = false;
		Vertex2d vt1, vt2;
		int nbInter = 0;
		int nbInterVertices = 0;
		Random rdm = new Random ();
		Vector vert = new Vector (vertices);

		// if the point (x,y) is a vertex of the polygon,
		// it's contained in the polygon
		for (int i=0;i<vert.size ();i++) {
			vt1 = (Vertex2d) vert.get (i);
			if ((x==vt1.x) && (y==vt1.y)) {
				return true;
			}
		}

		if (vert.size ()>0) {
			// add the fist element at the end
			vert.add (vert.get(0));

			while (!ok) {
				// choose a half line
				a = rdm.nextDouble ();
				b = y - a*x;

				ok = true;
				nbInter = 0;
				nbInterVertices = 0;

				// run over the borders of the polygons
				for (int i=0;i<(vert.size ()-1);i++) {
					vt1 = (Vertex2d) vert.get (i);
					vt2 = (Vertex2d) vert.get (i+1);

					if (vt1.x!=vt2.x) {
						a2 = (vt1.y - vt2.y) / (vt1.x - vt2.x);
						b2 = vt1.y - a2*vt1.x;

						if (a!=a2) {
							xInter = (b2-b)/(a-a2);
						} else { // we should choose another half line...
							ok = false;
							break;
						}
					} else {
						xInter = vt1.x;
					}

					// intersection between the actual border line and the half line choosed
					yInter = a*xInter + b;

					// we only consider a HALF line
					if (xInter<=x) {

						if (vt1.x>vt2.x) {maxx = vt1.x; minx = vt2.x;}
							else {maxx = vt2.x; minx = vt1.x;}

						if (vt1.y>vt2.y) {maxy = vt1.y; miny = vt2.y;}
							else {maxy = vt2.y; miny = vt1.y;}

						// is the intersection include in the actual border segment
						if ((xInter>=minx) && (xInter<=maxx) && (yInter>=miny) && (yInter<=maxy)) {

							// count the intersection
							nbInter = nbInter + 1;

						}
					}

					// dont accept intersection which are a vertex of the polygon
					for (int j=0;j<vert.size ()-1;j++) {
						vt1 = (Vertex2d) vert.get (j);

						if ((vt1.x==xInter) && (vt1.y==yInter)) {
							ok = false;
							break;
						}
					}
				}
			}

			// remove the element added at the beginning
			vert.remove (vert.get(vert.size ()-1));

			if ((nbInter==0) || ((nbInter % 2) ==0)) {return false;}
				else {return true;}
		}
		return false;
	}


}
