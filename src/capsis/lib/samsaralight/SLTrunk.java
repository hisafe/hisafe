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
 * SLTrunk - A description of the trunk to compute light intersection.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLTrunk implements SLTreePart {

	// public static final SLTag TRUNK = new SLTag ("TRUNK");

	// private SLTag tag; // fc+fa-25.4.2017 samsaralight resolution

	// Cylinder center
	private double x;
	private double y;
	private double z;
	private double radius; // trunk radius in meters
	private double crownBase; // height of the crown base in meters

	private double directEnergy; // MJ
	private double diffuseEnergy; // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
	// private double potentialEnergy; // MJ

	private static double EPSILON = 0.000001; // used to compare double with 6
												// decimal

	/**
	 * default constructor
	 * 
	 * @param dbh
	 *            in cm, crownBase in m
	 */
	public SLTrunk(double x, double y, double z, double dbh, double crownBase) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.crownBase = crownBase;
		this.radius = dbh / 200; // cm -> m

		// // Tag default value
		// this.tag = SLTrunk.TRUNK;
	}

	// fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy() {
		directEnergy = 0;
		diffuseEnergy = 0;

		// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
		// potentialEnergy = 0;
	}

	// fc-23.11.2012
	public SLTrunk getCopy(double xShift, double yShift, double zShift) {
		SLTrunk copy = new SLTrunk(x + xShift, y + yShift, z + zShift, radius * 200, crownBase);
		// copy.setTag(this.tag);
		return copy;
		// energy fields stay equal to zero
	}

	// fc-21.11.2013 to make copies (Heterofor, sometimes radiative balance not
	// every years)
	public SLTrunk clone() throws CloneNotSupportedException {
		return (SLTrunk) super.clone();
	}

	// @Override
	// public SLTag getTag() {
	// return tag;
	// }
	//
	// public void setTag(SLTag tag) {
	// this.tag = tag;
	// }

	/**
	 * Evaluates if the given ray intercepts this crown part, previously
	 * relocated with the given shifts. Returns the intersection path length and
	 * a distance to the origin (i.e. the target cell center) or null if no
	 * intersection.
	 */
	public double[] intercept(double xShift, double yShift, double zShift, double elevation, double azimuth) {

		double x2 = x + xShift;
		double y2 = y + yShift;
		double z2 = z + zShift; // trunk base in the new system of coordinates
		Vertex3d v0Shifted = new Vertex3d(x2, y2, z2);
		double crownBase2 = z2 + crownBase; // crown base in the new system of
											// coordinates

		double cosElevation = Math.cos(elevation);
		double sinElevation = Math.sin(elevation);
		double cosAzimuth = Math.cos(azimuth);
		double sinAzimuth = Math.sin(azimuth);

		double A = cosElevation * cosElevation;
		double B = -2 * x2 * cosElevation * cosAzimuth - 2 * y2 * cosElevation * sinAzimuth;
		double C = x2 * x2 + y2 * y2 - radius * radius;

		double[] r = SLModel.solveQuadraticEquation(A, B, C);

		if (r.length == 0)
			return null; // no intersection
		if (r.length == 1)
			return null; // tangent, ignored

		double root1 = r[0];
		double root2 = r[1];

		// Intersection point with the horizontal plane of the crown base
		double lCrownBase = crownBase2 / sinElevation;

		// Interception point with the horizontal plane of the trunk base
		double lTrunkBase = z2 / sinElevation;

		// Compute point coordinates of the intersection point with crown part
		// limits
		Vertex v1 = getVertex(root1, elevation, azimuth);
		Vertex v2 = getVertex(root2, elevation, azimuth);
		Vertex vCrownBase = getVertex(lCrownBase, elevation, azimuth);
		Vertex vTrunkBase = getVertex(lTrunkBase, elevation, azimuth);
		Vertex vTargetPoint = getVertex(0, elevation, azimuth);

		// trunk limit
		Vertex3d min = new Vertex3d(x2 - radius, y2 - radius, z2); // box around
																	// shifted
																	// cylinder
																	// center
																	// and crown
																	// limit
		Vertex3d max = new Vertex3d(x2 + radius, y2 + radius, crownBase2);

		// check all potential points
		List<Vertex> list = new ArrayList<Vertex>();
		if (inBBox(min, max, v1) && v1.z >= z2 && v1.z >= 0)
			list.add(v1);
		if (inBBox(min, max, v2) && v2.z >= z2 && v2.z >= 0)
			list.add(v2);
		if (isInsideCylinder(v0Shifted, vCrownBase) && crownBase2 >= z2 && crownBase2 >= 0)
			list.add(vCrownBase);
		if (isInsideCylinder(v0Shifted, vTrunkBase) && z2 >= 0)
			list.add(vTrunkBase);
		if (inBBox(min, max, vTargetPoint) && isInsideCylinder(v0Shifted, vTargetPoint)) {// vTargetPoint.z
																							// =
																							// 0
			// if(vTargetPoint.x != vTrunkBase.x || vTargetPoint.y !=
			// vTrunkBase.y || vTargetPoint.z
			// != vTrunkBase.z){//check that these two points are different!
			if (vTargetPoint.z != vCrownBase.z && vTargetPoint.z != vTrunkBase.z) { // gl
																					// 25-0-2013???
				list.add(vTargetPoint);
				// System.out.println
				// ("SLTRunk.intercept() - target point was added");
			}
		}
		if (list.size() == 0)
			return null; // intersected the cylinder outside the limits of the
							// trunk

		if (list.size() != 2) {
			boolean particularCase1 = (crownBase2 == 0); // one intersection
															// with crownBase
															// which in
															// in the plane of
															// the origin
			boolean particularCase2 = false; // The only root is at the
												// periphery of the cylinder in
												// the plane of the origin or
												// the crownBase (tangent
												// intersection)

			if (list.size() == 1) {
				Vertex v = list.get(0);
				if (v == v1 || v == v2) {
					if (v.z == 0 || v.z == crownBase2)
						particularCase2 = true;
				}
			}

			// System.out.println ("Inside the cylinder " +
			// isInsideCylinder(v0Shifted,
			// vTrunkBase));
			// System.out.println ("abobe trunk base : " + (vTrunkBase.z >=
			// z2));
			// System.out.println ("above origin : " + (vTrunkBase.z >= 0));
			// System.out.println ("all the condition : " +
			// (isInsideCylinder(v0Shifted, vTrunkBase)
			// && vTrunkBase.z >= z2 && vTrunkBase.z >= 0));

			if (!particularCase1 && !particularCase2) {
				SLReporter.printInLog(Log.WARNING, "SLTrunk.intercept ()",
						"The number of interception if neither 0, 2, or an expected particular case. The list size is "
								+ list.size());
				SLReporter.printInStandardOutput("SLTrunk.intercept () - the number of interception if neither 0, 2, or an expected particular case."
								+ " A check is needed. The list size is " + list.size());
				SLReporter.printInStandardOutput("SLTrunk.intercept () - the target point was x = " + xShift + "; y = " + yShift);
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
		// /-----------------------------------
		// old algradiusorithm (give exactly the same results)

		// double iz1 = root1 * sinElevation;
		// double iz2 = root2 * sinElevation;
		//
		// // intersection with the upper limit of the trunk (the crown base)
		// double lCrownBase = crownBase2 / sinElevation;
		// boolean interceptCrownBase = isInsideCylinder
		// (getVertex(lCrownBase,elevation,azimuth),
		// x2, y2);
		//
		// // Is the target point within the trunk.
		// boolean interceptTargetPoint = isInsideCylinder
		// (getVertex(0,elevation,azimuth), x2, y2);
		// // L=0 and z=0 for the target point
		//
		// // The 2 intersection points are within trunk limits and above the
		// target point (z2)
		// if (iz1 < crownBase2 && iz1 > z2 && iz2 < crownBase2 && iz2 > z2 &&
		// iz1 > 0 && iz2 > 0) {
		// double pathLength = Math.abs(root1 - root2);
		// double distance = (root1 + root2) / 2d; // to sort later the tree
		// part along the ray
		// return new double[] {pathLength, distance};
		//
		//
		// // only 1 intersection (the first root : iz1) is within trunk limits
		// and above the target
		// point
		// } else if (iz1 < crownBase2 && iz1 >= 0) {
		// double iz3 = 0;
		// if (interceptCrownBase) iz3 = crownBase2;
		// else if(interceptTargetPoint) iz3 = 0; // L=0 and z=0 for the target
		// point
		// else System.out.println
		// ("SLTrunk - An interception was not computed properly!");
		// double root3 = iz3 / sinElevation;
		// double pathLength = Math.abs(root1 - root3);
		// double distance = (root1 + root3) / 2d;
		// return new double[] {pathLength, distance};
		//
		//
		// // only 1 intersection (the second root : iz2) is within trunk limits
		// and above the
		// target point
		// } else if (iz2 < crownBase2 && iz2 >= 0) {
		// double iz3 = 0;
		// if (interceptCrownBase) iz3 = crownBase2;
		// else if(interceptTargetPoint) iz3 = 0; // L=0 and z=0 for the target
		// point
		// else System.out.println
		// ("SLTrunk - An intersection was not computed properly!");
		// double root3 = iz3 / sinElevation;
		// double pathLength = Math.abs(root2 - root3);
		// double distance = (root2 + root3) / 2d;
		// returnradius new double[] { pathLength, distance };
		//
		// // none of two roots are within trunk limits and above the target
		// point
		// } else {
		// // a vertical ray might go through the trunk without intercepting its
		// periphery
		// // but this is important only if the upper limit of the trunk is
		// above the target point
		// if (interceptCrownBase && interceptTargetPoint && crownBase2 > 0){
		// double root3 = crownBase2 / sinElevation;
		// double pathLength = Math.abs(root3);
		// double distance = (root3) / 2d;
		// return new double[] {pathLength, distance};
		//
		// }else{
		// //no valid interception
		// return null;
		// }
		// }

	}

	/**
	 * check if a point (x,y) is within a vertical cylinder of radius and center
	 * (x0,y0) We used new coordinates to enable the use of a shift
	 * 
	 * @author GL Feb 2013
	 */
	public boolean isInsideCylinder(Vertex3d v0Shifted, Vertex v) {
		double x = v.x;
		double y = v.y;
		double x0 = v0Shifted.x;
		double y0 = v0Shifted.y;
		// System.out.println ("test :" + ((x0 - x) * (x0 - x) + (y0 - y) * (y0
		// - y)) + " <= " +
		// radius*radius);
		// System.out.println((x0 - x) * (x0 - x) + (y0 - y) * (y0 - y) <=
		// radius*radius);
		return (x0 - x) * (x0 - x) + (y0 - y) * (y0 - y) <= radius * radius;
	}

	/**
	 * Compute the coordinates of an interception point from the distance to the
	 * target cell and beam direction.
	 * 
	 * @param length
	 * @param elevation
	 * @param azimuth
	 * @return a vertex containing the coordinates of the interception point and
	 *         its distance to the target cell
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

	/**
	 * check if a vertex point is included between max and min vertex points
	 */
	private boolean inBBox(Vertex3d min, Vertex3d max, Vertex v) {
		boolean inBBox = (min.x <= v.x + EPSILON && v.x <= max.x + EPSILON && min.y <= v.y + EPSILON
				&& v.y <= max.y + EPSILON && min.z <= v.z + EPSILON && v.z <= max.z + EPSILON);
		return inBBox;
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
	// public double getPotentialEnergy () {
	// return potentialEnergy;
	// } // in MJ, energy intercepted by this part but without neighbours
	// public void addPotentialEnergy (double e) {
	// this.potentialEnergy += e;
	// } // MJ
	// // fc+fa-26.4.2017 SHOULD BE REMOVED for the trunk and moved in the tree
	// ?

	public double getX() {
		return x;
	}

	public double getY() {
		return y;
	}

	public double getZ() {
		return z;
	}

	public double getRadius() {
		return radius;
	}

	public double getCrownBase() {
		return crownBase;
	}

	public String toString() {
		return "SLTrunk radius: " + radius;
	}

}
