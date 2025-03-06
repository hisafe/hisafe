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
 * SLCylindricalCrownPart - A description of a part of a crown. A crown may
 * contain one or two such objects, one for the top and one for the bottom.
 * 
 * @author G. Ligot February 2014
 */
public class SLCylindricalCrownPart extends SLCrownPart {

//	public static final SLTag CYLINDRICAL_CROWN = new SLTag("CYLINDRICAL_CROWN");

	// Cylinder center
	public double x;
	public double y;
	public double z; // at the center and not at the base!

	public double r; // crown radius
	public double l; // crown length

	private double zTop; // z + l/2
	private double zBottom; // z - l/2

	private double leafAreaDensity; // m2/m3
	private double extinctionCoefficient;

	private double directEnergy; // MJ
	private double diffuseEnergy; // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
	// private double potentialEnergy; // MJ

	private boolean isSetLeafAreaDensity = false;

	/**
	 * Constructor for a full cylinder.
	 */
	public SLCylindricalCrownPart(double x, double y, double z, double r, double l) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.r = r;
		this.l = l;

		zTop = z + l / 2d;
		zBottom = z - l / 2d;

//		// Tag default value
//		this.tag = CYLINDRICAL_CROWN;
	}

	public SLCylindricalCrownPart getCopy(double xShift, double yShift, double zShift) {

		SLCylindricalCrownPart copy = new SLCylindricalCrownPart(x + xShift, y + yShift, z + zShift, r, l);

		copy.zTop = zTop; //fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!
		copy.zBottom = zBottom; //fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!

		copy.leafAreaDensity = leafAreaDensity;
		copy.extinctionCoefficient = extinctionCoefficient;

//		copy.setTag(this.tag);

		// energy fields stay equal to zero
		return copy;
	}

	// fc-21.11.2013 to make copies (Heterofor, sometimes radiative balance not
	// every years)
	public SLCylindricalCrownPart clone() throws CloneNotSupportedException {
		return (SLCylindricalCrownPart) super.clone();
	}

	// fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy() {
		directEnergy = 0;
		diffuseEnergy = 0;

		// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
		// potentialEnergy = 0;
	}

	/**
	 * Evaluates if the given ray intercepts this crown part, previously
	 * relocated with the given shifts. Returns the interception path length and
	 * a distance to the origin (i.e. the target cell center) or null if no
	 * interception.
	 * 
	 * @param xShift
	 *            , yShift, zShift: m, to relocate the crown part for the
	 *            duration of this interception
	 * @param elevation
	 *            : angle, rad
	 * @param azimuth
	 *            : angle, rad, trigonometric 0 and counter clockwise
	 */
	public double[] intercept(double xShift, double yShift, double zShift, double elevation, double azimuth) {

		double x2 = x + xShift;
		double y2 = y + yShift;
		double z2 = z + zShift; // in accordance with other crown part, this is
								// the center of the crown
		double zTop2 = zTop + zShift;
		double zBottom2 = zBottom + zShift;
		Vertex3d v0shifted = new Vertex3d(x2, y2, z2);

		double cosElevation = Math.cos(elevation);
		double sinElevation = Math.sin(elevation);
		double cosAzimuth = Math.cos(azimuth);
		double sinAzimuth = Math.sin(azimuth);

		double A = cosElevation * cosElevation;
		double B = -2 * x2 * cosElevation * cosAzimuth - 2 * y2 * sinAzimuth * cosElevation;
		double C = x2 * x2 + y2 * y2 - r * r;

		double[] r = SLModel.solveQuadraticEquation(A, B, C);

		if (r.length == 0)
			return null; // no intersection
		if (r.length == 1)
			return null; // tangent, ignored

		// intersection (distance from the target point)
		double root1 = r[0];
		double root2 = r[1];

		// intersection with the horizontal crown part section
		double ltop2 = zTop2 / sinElevation;
		double lbottom2 = zBottom2 / sinElevation;

		// Compute point coordinates of the intersection point with crown part
		// limits
		Vertex v1 = getVertex(root1, elevation, azimuth);
		Vertex v2 = getVertex(root2, elevation, azimuth);
		Vertex vUpperPlane = getVertex(ltop2, elevation, azimuth);
		Vertex vLowerPlane = getVertex(lbottom2, elevation, azimuth);

		// trunk limit
		// Vertex3d min = new Vertex3d(x2-a, y2-b, zBottom2); //box around
		// shifted ellipsoid part
		// Vertex3d max = new Vertex3d(x2+a, y2+b, zTop2);

		// check all potential points
		List<Vertex> list = new ArrayList<Vertex>();
		if (v1.z >= zBottom2 && v1.z <= zTop2)
			list.add(v1);
		if (v2.z >= zBottom2 && v2.z <= zTop2)
			list.add(v2);
		if (inCylinder(v0shifted, vUpperPlane))
			list.add(vUpperPlane);
		if (inCylinder(v0shifted, vLowerPlane))
			list.add(vLowerPlane);
		if (list.size() == 0)
			return null;

		if (list.size() != 2) {

			// if (!particularCase1 && !ParticularCase2){
			SLReporter.printInLog(Log.WARNING, "SLCylindricalCrownPart.intercept ()",
					"The number of interception if neither 0, 2 or an expected particular case. The list size is "
							+ list.size());
			SLReporter.printInStandardOutput("SLEllipsoidalCrownPart.intercept () - "
							+ "The number of interception if neither 0, 2 or an expected particular case. A check is needed! The list size is "
							+ list.size());
			// }
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
	}

	/**
	 * check if a vertex is included in the cylinder of the crownPart
	 */
	private boolean inCylinder(Vertex3d vShapeCenter, Vertex v) {
		double value = Math.pow(v.x - vShapeCenter.x, 2) + Math.pow(v.y - vShapeCenter.y, 2) - r * r;
		boolean inCylinder = (value < 0);
		return inCylinder;
	}

	/**
	 * Compute the coordinates of the interception point from the distance to
	 * the target cell and beam direction.
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

	@Override
	public double getVolume() {
		double volume = Math.PI * r * r * l; // full cylinder volume
		return volume;
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
	// public double getPotentialEnergy () {
	// return potentialEnergy;
	// } // in MJ, energy intercepted by this part but without neighbours
	// public void addPotentialEnergy (double e) {
	// this.potentialEnergy += e;
	// } // MJ

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

	/**
	 * get Leaf area m2
	 * 
	 * @pre LAD must be set
	 */
	public double getLeafArea() {
		if (isSetLeafAreaDensity) {
			return leafAreaDensity * getVolume();
		} else {
			SLReporter.printInLog(
					Log.WARNING,
					"SLCylindricalCrownPart.getLeafArea(): this has been called before setting crown part LAD. It has returned 0.",
					null);
			return 0;
		}
	}

	@Override
	public boolean isIndsideCrownPart(double x, double y, double z) {
		double x0 = this.x;
		double y0 = this.y;
		double z0 = this.z;
		Vertex3d v = new Vertex3d(x0, y0, z0);
		Vertex3d vs = new Vertex3d(x, y, z);
		double value = Math.pow(v.x - vs.x, 2) + Math.pow(v.y - vs.y, 2) - r * r;
		boolean inCylinder = (value < 0);
		boolean inBBox = (zBottom <= v.z && v.z <= zTop);
		return inCylinder && inBBox;
	}

	/**
	 * Returns true if this crown part is located in the upper part of the
	 * crown, Cylindrical crown parts are considered as upper, used by SamsaraLight.
	 */
	public boolean isUpper() {
		return true; // fc+fa-16.5.2017
	}

	/**
	 * Returns true if this crown part is located in the lower part of the
	 * crown.
	 */
	public boolean isLower() {
		return false;
	}
	
}
