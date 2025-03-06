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

import java.awt.geom.Point2D;
import java.io.Serializable;
import java.text.NumberFormat;
import java.util.Locale;
/**
 * A vertex with 2 double coordinates. The first coordinate represents the angle from the
 * vertical (CLOCKWISE), and the second representes the length from origin point
 * to the relative point represented by the RadianPolar2D object.
 * @author A. Piboule - october 2004
 */
public class RadianPolar2D implements Serializable, Cloneable {

	public final static double PIx2 = 2d*Math.PI;


	public double angle; // in radians
	public double length; // in meters

	/**
	 * Default constructor.
	 */
	public RadianPolar2D () {this (0d, 0d);}

	/**
	 * Usual constructor.
	 */
	public RadianPolar2D (double an, double le) {
		angle = an;
		length = le;
	}


	public Object clone () {
		return new RadianPolar2D (angle, length);
	}


	public String toString () {
		NumberFormat nf = NumberFormat.getNumberInstance (Locale.ENGLISH);
		nf.setMaximumFractionDigits (2);
		nf.setGroupingUsed (false);

		StringBuffer b = new StringBuffer ();
		b.append ("(");
		b.append (nf.format(angle));
		b.append (", ");
		b.append (nf.format(length));
		b.append (")");
		return b.toString ();
	}



	public static Point2D.Double RadianPolar2DToCoord (Point2D.Double origin, RadianPolar2D point) {
		double x;
		double y;

		x = Math.sin (point.angle) * point.length;
		y = Math.cos (point.angle) * point.length;

		return new Point2D.Double (origin.x + x, origin.y + y);
	}



	public static RadianPolar2D coordToRadianPolar2D (Point2D.Double origin, Point2D.Double point) {
		double x;
		double y;
		double l;
		double a;
		double arcCosa;
		double arcSina;

		x = point.x - origin.x;
		y = point.y - origin.y;
		l = Math.sqrt ((x*x + y*y));

		arcCosa = Math.acos (y/l);
		arcSina = Math.asin (x/l);

		if (arcSina >= 0) {a = arcCosa;}
		else {a = (PIx2) - arcCosa;}


		if (l==0) {return new RadianPolar2D (0d,0d);}

		return new RadianPolar2D (a, l);
	}


	public static double angleBetween (RadianPolar2D p1, RadianPolar2D p2) {

		double result = 0d;
		double result2 = 0d;

		if (p1.angle==p2.angle) {return 0d;}

		result = p1.length - p2.length;
		result2 = p2.length - p1.length;

		if (p1.angle>p2.angle) {
			result2 += PIx2;
		} else {
			result += PIx2;
		}

		if (result<result2) {
			return result;
		} else {
			return result2;
		}


	}


	public static double invertAngle (double angle){
		double a = angle + Math.PI;
		if (a < PIx2) {
			return a;
		} else {
			return a - PIx2;
		}
	}


	public static RadianPolar2D invertRadianPolar2D (RadianPolar2D p) {
		return new RadianPolar2D (invertAngle (p.angle),p.length);
	}


	// for test only
	public static void main (String [] a) {
		RadianPolar2D p1 = new RadianPolar2D (-Math.PI/4d, Math.sqrt (18d));
		RadianPolar2D p2 = new RadianPolar2D (-3d*Math.PI/4d, Math.sqrt (18d));
		RadianPolar2D p3 = new RadianPolar2D (-5d*Math.PI/4d, Math.sqrt (18d));
		RadianPolar2D p4 = new RadianPolar2D (-7d*Math.PI/4d, Math.sqrt (18d));
		Point2D.Double v0 = new Point2D.Double (0, 0);
		Point2D.Double v1 = new Point2D.Double (3, 3);
		Point2D.Double v2 = new Point2D.Double (3, -3);
		Point2D.Double v3 = new Point2D.Double (-3, -3);
		Point2D.Double v4 = new Point2D.Double (-3, 3);

		System.out.println (coordToRadianPolar2D (v0,v1));
		System.out.println (coordToRadianPolar2D (v0,v2));
		System.out.println (coordToRadianPolar2D (v0,v3));
		System.out.println (coordToRadianPolar2D (v0,v4));

		System.out.println (RadianPolar2DToCoord (v0,p1));
		System.out.println (RadianPolar2DToCoord (v0,p2));
		System.out.println (RadianPolar2DToCoord (v0,p3));
		System.out.println (RadianPolar2DToCoord (v0,p4));

		System.out.println (invertAngle (Math.PI));
		System.out.println (invertAngle (0));
		System.out.println (invertAngle (Math.PI/2d));
		System.out.println (invertAngle (-Math.PI/3d));



	}

}
