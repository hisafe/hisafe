/*
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.lib.samsaralight;

import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;

/**
 * SLEllipsoidalCrownPart - A description of a part of a crown. A crown may
 * contain one or two such objects, one for the top and one for the bottom.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLEllipsoidalCrownPart extends SLCrownPart implements SLShiftable {

//	public static final SLTag ELLISOIDAL_CROWN = new SLTag ("ELLISOIDAL_CROWN");
//	public static final SLTag ELLISOIDAL_CROWN_TOP = new SLTag ("ELLISOIDAL_CROWN_TOP");
//	public static final SLTag ELLISOIDAL_CROWN_BOTTOM = new SLTag ("ELLISOIDAL_CROWN_BOTTOM");

	// Ellipsoid center
	public double x;
	public double y;
	public double z;

	// Ellipsoid parameters (semi-principal axes)
	public double a; // on X axis
	public double b; // on Y axis
	public double c; // on Z axis (heights)

	private double zTop; // z or z + c
	private double zBottom; // z or z - c

	private boolean halfEllipsoid;
	private boolean halfTopPart;

	private double leafAreaDensity; // m2/m3
	private double extinctionCoefficient;

	private double directEnergy; // MJ
	private double diffuseEnergy; // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//	private double potentialEnergy; // MJ

	private boolean isSetLeafAreaDensity = false;

	/**
	 * Constructor for a full ellipse.
	 */
	public SLEllipsoidalCrownPart(double x, double y, double z, double a, double b, double c) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.a = a;
		this.b = b;
		this.c = c;

		zTop = z + c;
		zBottom = z - c;

		halfEllipsoid = false;
		halfTopPart = false;

//		this.tag = ELLISOIDAL_CROWN; 
	}

	/**
	 * Constructor for half an ellipse for the top or the bottom of a crown.
	 * 
	 * @param topPart : if true, this half ellipse is the top of a crown, if false,
	 *                the bottom.
	 */
	public SLEllipsoidalCrownPart(double x, double y, double z, double a, double b, double c, boolean topPart) {
		this(x, y, z, a, b, c);

		if (topPart) {
			zTop = z + c;
			zBottom = z;
		} else {
			zTop = z;
			zBottom = z - c;
		}

		halfEllipsoid = true;
		halfTopPart = topPart;

//		// Tag default value
//		if (topPart)
//			this.tag = ELLISOIDAL_CROWN_TOP;
//		else 
//			this.tag = ELLISOIDAL_CROWN_BOTTOM;

	}

	// fc-23.11.2012
	public SLEllipsoidalCrownPart getCopy(double xShift, double yShift, double zShift) {

		SLEllipsoidalCrownPart copy = new SLEllipsoidalCrownPart(x + xShift, y + yShift, z + zShift, a, b, c);

		copy.zTop = zTop; // fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!
		copy.zBottom = zBottom; // fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!

		copy.halfEllipsoid = halfEllipsoid;
		copy.halfTopPart = halfTopPart;

		copy.leafAreaDensity = leafAreaDensity;
		copy.extinctionCoefficient = extinctionCoefficient;

//		copy.setTag(this.tag);

		// energy fields stay equal to zero
		return copy;
	}

	// fc-21.11.2013 to make copies (Heterofor, sometimes radiative balance not
	// every years)
	public SLEllipsoidalCrownPart clone() throws CloneNotSupportedException {
		return (SLEllipsoidalCrownPart) super.clone();
	}

	// fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy() {
		directEnergy = 0;
		diffuseEnergy = 0;

		// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//		potentialEnergy = 0;
	}

	/**
	 * Evaluates if the given ray intercepts this crown part, previously relocated
	 * with the given shifts. Returns the interception path length and a distance to
	 * the origin (i.e. the target cell center) or null if no interception.
	 * 
	 * @param xShift    , yShift, zShift: m, to relocate the crown part for the
	 *                  duration of this interception
	 * @param elevation : angle, rad
	 * @param azimuth   : angle, rad, trigonometric 0 and counter clockwise
	 */
	public double[] intercept(double xShift, double yShift, double zShift, double elevation, double azimuth) {

		double x2 = x + xShift;
		double y2 = y + yShift;
		double z2 = z + zShift;
		double zTop2 = zTop + zShift;
		double zBottom2 = zBottom + zShift;
		Vertex3d v0shifted = new Vertex3d(x2, y2, z2);

		double cosElevation = Math.cos(elevation);
		double sinElevation = Math.sin(elevation);
		double cosAzimuth = Math.cos(azimuth);
		double sinAzimuth = Math.sin(azimuth);

		double A = cosElevation * cosElevation * cosAzimuth * cosAzimuth / (a * a)
				+ cosElevation * cosElevation * sinAzimuth * sinAzimuth / (b * b)
				+ sinElevation * sinElevation / (c * c);
		double B = -2 * x2 * cosElevation * cosAzimuth / (a * a) - 2 * y2 * sinAzimuth * cosElevation / (b * b)
				- 2 * z2 * sinElevation / (c * c);
		double C = x2 * x2 / (a * a) + y2 * y2 / (b * b) + z2 * z2 / (c * c) - 1;

		double[] r = SLModel.solveQuadraticEquation(A, B, C);

		if (r.length == 0)
			return null; // no intersection
		if (r.length == 1)
			return null; // tangent, ignored

		// intersection (distance from the target point)
		double root1 = r[0];
		double root2 = r[1];

		// intersection with the horizontal crown part section
		double lCenterPlane = z2 / sinElevation; // always at ellipsoid center...

		// Compute point coordinates of the intersection point with crown part limits
		Vertex v1 = getVertex(root1, elevation, azimuth);
		Vertex v2 = getVertex(root2, elevation, azimuth);
		Vertex vCenterPlane = getVertex(lCenterPlane, elevation, azimuth);
		Vertex vTargetPoint = getVertex(0, elevation, azimuth); // (0,0,0)

		// correction to avoid rounding errors
		vCenterPlane.z = z2;

		// trunk limit
		Vertex3d min = new Vertex3d(x2 - a, y2 - b, zBottom2); // box around shifted ellipsoid part
		Vertex3d max = new Vertex3d(x2 + a, y2 + b, zTop2);

		// check all potential points
		List<Vertex> list = new ArrayList<Vertex>();
		if (inBBox(min, max, v1) && v1.z >= 0)
			list.add(v1);
		if (inBBox(min, max, v2) && v2.z >= 0)
			list.add(v2);
		if (inBBox(min, max, vCenterPlane) && inEllipsoid(v0shifted, vCenterPlane) && isHalfEllipsoid()
				&& vCenterPlane.z > 0)
			list.add(vCenterPlane);
		if (inBBox(min, max, vTargetPoint) && inEllipsoid(v0shifted, vTargetPoint))
			list.add(vTargetPoint);
		if (list.size() == 0)
			return null;

		if (list.size() != 2) {
			// Particular cases
			boolean particularCase1 = (zTop2 == 0); // the ellipse center is at the same z as the target point
													// and the rest of the crown part is below the target point
			boolean ParticularCase2 = false;
			if (list.size() == 1) {
				Vertex v = list.get(0);
				if (v == v1 || v == v2) {
					if (v.z == 0)
						ParticularCase2 = true; // The root is at the periphery of the crown part and the second root is
												// below the target point
				}
			}

			if (!particularCase1 && !ParticularCase2) {
				SLReporter.printInLog(Log.WARNING, "SLEllipsoidalCrownPart.intercept ()",
						"The number of interception if neither 0, 2 or an expected particular case. The list size is "
								+ list.size());
				SLReporter.printInStandardOutput("SLEllipsoidalCrownPart.intercept () - "
						+ "The number of interception if neither 0, 2 or an expected particular case. A check is needed! The list size is "
						+ list.size());
			}
			return null;

		} else {
			Vertex w1 = list.get(0);
			Vertex w2 = list.get(1);
			double l1 = w1.l;
			double l2 = w2.l;
			double distance = (l1 + l2) / 2d;
			double pathLength = Math.abs(l1 - l2);

			return new double[] { pathLength, distance };
		}

		/// ----------------------------------------------------------
		// old algorithm (gives exactly the same results)
		// the target point has L=0 and z=0
//		boolean interceptTargetPoint = isInsideCrownPart (getVertex(0,elevation,azimuth), x2, y2, z2, zTop2, zBottom2);
//		
//		// intersception with the horizontal plane of ellipsoid center
//		double lCenterPlane = z2 / sinElevation; //always at ellipsoid center...
//		boolean interceptCenterPlane = isInsideCrownPart (getVertex(lCenterPlane,elevation,azimuth), x2, y2, z2, zTop2, zBottom2) 
//				&& (z2 >= 0); //The target point must be above the ellipsoid center to be really intercepted
//				
//		// the 2 interception points are inside the crown part limits and above the target point
//		if (zTop2 >= iz1 && iz1 > zBottom2 && zTop2 >= iz2 && iz2 > zBottom2 && iz1 > 0 && iz2 > 0) {
//			double pathLength = Math.abs (root1 - root2);
//			double distance = (root1 + root2) / 2d; // to sort later the trees along the ray
//			return new double[] {pathLength, distance};
//
//			
//		// Only one 1 interception (iz1) is within the crown part limits and above the target point
//		} else if (zTop2 >= iz1 && iz1 > zBottom2 && iz1 >=0) {
//			double iz3 = 0;
//			if (interceptTargetPoint) iz3 = 0;
//			else if(interceptCenterPlane && isHalfEllipsoid ()) iz3 = z2;
//			else System.out.println ("SLEllipsoidalCrownPart - An interception was not computed properly!");
//			double root3 = iz3 / sinElevation;
//			double pathLength = Math.abs (root1 - root2);
//			double distance = (root1 + root3) / 2d;
//			return new double[] {pathLength, distance};
//
//			
//		// Only one 1 interception (iz2) is within the crown part limits and above the target point
//		} else if (zTop2 >= iz2 && iz2 > zBottom2 && iz2 >= 0) {
//			double iz3 = 0;
//			if (interceptTargetPoint) iz3 = 0;
//			else if(interceptCenterPlane && isHalfEllipsoid ()) iz3 = z2;
//			else System.out.println ("SLEllipsoidalCrownPart - An interception was not computed properly!");
//			double root3 = iz3 / sinElevation;
//			double pathLength = Math.abs (root2 - root3);
//			double distance = (root2 + root3) / 2d;
//			return new double[] {pathLength, distance};
//
//		// None of the two roots are within the crown part limit and above the target point
//		} else {
//			// a vertical ray might go through the horizonthal section of a "half-bottom" ellipsoid
//			// and stop at the target point located within the crown part
//			if (interceptTargetPoint && interceptCenterPlane && z2 > 0){
//				double root3 = z2 / sinElevation; //distance from the target point to the plane
//				double pathLength = Math.abs(root3);
//				double distance = (root3) / 2d;
//				return new double[] {pathLength, distance};
//				
//				
//			}else{
//				//no valid interception
//				return null;
//			}
//		}
	}

	/**
	 * check if a vertex is included in the ellispoid of the crownPart
	 */
	private boolean inEllipsoid(Vertex3d vs, Vertex v) {
		double value = (v.x - vs.x) * (v.x - vs.x) / (a * a) + (v.y - vs.y) * (v.y - vs.y) / (b * b)
				+ (v.z - vs.z) * (v.z - vs.z) / (c * c);
		boolean inEllipsoid = (value < 1);
		return inEllipsoid;
	}

//	/**check if a point (x,y) is inside the crown part. Ellipsoid coordinates are given to take into account a potential shift. 
//	 * GL Feb 2013
//	 */
//	public boolean isInsideCrownPart(Vertex point, double x2, double y2, double z2, double zTop2, double zBottom2){
//		double x = point.x;
//		double y = point.y;
//		double z = point.z;
//		boolean isInsideEllipsoid = (x2 - x) * (x2 - x) / (a * a) + (y2 - y) * (y2 - y) / (b * b) + (z2 - z) * (z2 - z) / (c * c) <= 1;
//		boolean isbetweenShiftedTopAndBottom = z2 <= zTop2 && z2 >= zBottom2;
//		return isInsideEllipsoid && isbetweenShiftedTopAndBottom;
//	}

	/**
	 * Compute the coordinates of the interception point from the distance to the
	 * target cell and beam direction.
	 * 
	 * @param length
	 * @param elevation
	 * @param azimuth
	 * @return a vertex containing the coordinates of the interception point and its
	 *         distance to the target cell
	 * @author GL Feb 2013
	 */
	private Vertex getVertex(double length, double elevation, double azimuth) {
		double cosElevation = Math.cos(elevation);
		double sinElevation = Math.sin(elevation);
		double cosAzimuth = Math.cos(azimuth);
		double sinAzimuth = Math.sin(azimuth);
		double x = length * cosElevation * cosAzimuth;
		double y = length * cosElevation * sinAzimuth;
		double z = length * sinElevation;
		return new Vertex(x, y, z, length);
	}

	/**
	 * Inner class: a vertex with an associated length
	 */
	private static class Vertex {
		public double x;
		public double y;
		public double z;
		public double l; // a length

		public Vertex(double x, double y, double z, double l) {
			this.x = x;
			this.y = y;
			this.z = z;
			this.l = l;
		}

		public String toString() {
			return "Vertex x: " + x + " y: " + y + " z: " + z + " l: " + l;
		}
	}

	@Override
	public double getVolume() {
		double volume = 4d / 3d * Math.PI * a * b * c; // full ellipsoid volume

		// Complete ellipsoide (fc-25.6.2012)
		if (!isHalfEllipsoid()) {
			return volume;
		}

		// Half an ellipsoide (fc-25.6.2012)
		double halfVolume = volume / 2d;
		return halfVolume;
	}

	@Override
	public double getExtinctionCoefficient() {
		return this.extinctionCoefficient;
	}

	@Override
	public void setExtinctionCoefficient(double extinctionCoefficient) {
		this.extinctionCoefficient = extinctionCoefficient;

	}

	public double getDirectEnergy() {
		return directEnergy;
	} // direct beam energy in MJ

	public double getDiffuseEnergy() {
		return diffuseEnergy;
	} // diffuse beam energy in MJ

	// fc+fa-3.8.2017 added synchronized for Parallel mode
	synchronized public void addDirectEnergy(double e) {
		this.directEnergy += e;
	} // MJ

	// fc+fa-3.8.2017 added synchronized for Parallel mode
	synchronized public void addDiffuseEnergy(double e) {
		this.diffuseEnergy += e;
	} // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//	public double getPotentialEnergy () {
//		return potentialEnergy;
//	} // in MJ, energy intercepted by this part but without neighbours
//	public void addPotentialEnergy (double e) {
//		this.potentialEnergy += e;
//	} // MJ

	/**
	 * A flag to determine whether the half ellipsoid is topPart or not throws an
	 * exception if the crown part is not a half ellipsoid
	 * 
	 * @author GL
	 * @pre must be a half ellipsoid
	 */
	public boolean isHalfTopPart() {
		if (!isHalfEllipsoid()) {
			SLReporter.printInLog(Log.WARNING,
					"SLEllipsoidalCrownPart.isHalfTopPart(): this method was called on a full ellipsoid. The method returned false.",
					null);
			return false;
		}
		return halfTopPart;
	}

	/**
	 * Returns true if this crown part is located in the lower part of the crown,
	 * used by SamsaraLight.
	 */
	public boolean isLower() {
		return halfEllipsoid && !halfTopPart;
	}

	/**
	 * Returns true if this crown part is located in the upper part of the crown,
	 * used by SamsaraLight.
	 */
	public boolean isUpper() {
		return !halfEllipsoid || (halfEllipsoid && halfTopPart); // fc+fa-16.5.2017
	}

	/**
	 * check if a vertex point is included between max and min vertex points
	 */
	private boolean inBBox(Vertex3d min, Vertex3d max, Vertex v) {
		boolean inBBox = (min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y && min.z <= v.z && v.z <= max.z);
		return inBBox;
	}

	public double getZTop() {
		return zTop;
	}

	public double getZBottom() {
		return zBottom;
	}

	public double getLeafAreaDensity() {
		return leafAreaDensity;
	}

	public void setLeafAreaDensity(double leafAreaDensity) {
		this.leafAreaDensity = leafAreaDensity;
		this.isSetLeafAreaDensity = true;
	}

	public boolean isHalfEllipsoid() {
		return halfEllipsoid;
	}

	/**
	 * get Leaf area m2
	 * 
	 * @pre LAD must be set
	 */
	public double getLeafArea() {
		if (isSetLeafAreaDensity) {
			return leafAreaDensity * getVolume();
		} else {
			SLReporter.printInLog(Log.WARNING,
					"SLEllipsoidalCrownPart.getLeafArea(): this has been called before setting crown part LAD. It has returned 0.",
					null);
			return 0;
		}
	}

	@Override
	public boolean isIndsideCrownPart(double xp, double yp, double zp) {
		double x0 = this.x;
		double y0 = this.y;
		double z0 = this.z;
		Vertex3d v = new Vertex3d(x0, y0, z0);
		Vertex3d vs = new Vertex3d(xp, yp, zp);
		double value = (v.x - vs.x) * (v.x - vs.x) / (a * a) + (v.y - vs.y) * (v.y - vs.y) / (b * b)
				+ (v.z - vs.z) * (v.z - vs.z) / (c * c);
		boolean inEllipsoid = (value < 1);

		// gl-16.05.2019: I corrected the two following lines so as the bounding box
		// depends on b and h also
		// this function was not used anymore and should be verified if it is used in
		// the future.
		Vertex3d min = new Vertex3d(Math.min(x0, x0 + a), Math.min(y0, y0 + b), Math.min(z0, z0 + c)); // box
		Vertex3d max = new Vertex3d(Math.max(x0, x0 + a), Math.max(y0, y0 + b), Math.max(z0, z0 + c));
		boolean inBBox = (min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y && min.z <= v.z && v.z <= max.z);
		return inEllipsoid && inBBox;
	}
	
	/**
	 * This method is specific to Samsara2 and the PeripheryCreator
	 * tool: some trees are cloned to be located in the periphery of the stand,
	 * their location must be updated.
	 */
	public void setXY(double x, double y) {
		this.x = x;
		this.y = y;
	}
	
	/**
	 * fc+bc-22.6.2022 This method is specific to Samsara2 and the PeripheryCreator
	 * tool: some trees are cloned to be located in the periphery of the stand,
	 * their z must be updated.
	 */
	public void shiftZ(double zShift) {
		z += zShift;
		zTop += zShift;
		zBottom += zShift;
	}

}
