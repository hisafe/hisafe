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
 * SLConicalCrownFraction - For a crown made of 4 fractions of cone.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 * This crown type was added by F. Andre - May 2019
 */
public class SLConicalCrownFraction extends SLCrownPart {

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
		
		public boolean equals(Vertex other, double EPSILON) {
			return Math.abs(x - other.x) < EPSILON && Math.abs(y - other.y) < EPSILON && Math.abs(z - other.z) < EPSILON;
		}
	}

	// Cone base center
	public double x0;
	public double y0;
	public double z0;

	// Cone parameters (semi-principal axes)
	// a and b may be positive or negative depending on the fraction
	public double a; // semi-axis of basal ellipse along X axis
	public double b; // semi-axis of basal ellipse along Y axis
	public double h; // crown height, along Z axis
	
	private double zTop; // z + h
	private double zBottom; // z

	private double leafAreaDensity; // m2/m3
	private double extinctionCoefficient;

	private double directEnergy; // MJ
	private double diffuseEnergy; // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
	// private double potentialEnergy; // MJ

	private boolean isSetLeafAreaDensity = false; // GL

	/**
	 * Constructor for a 4th of a cone. The signs of a and b determine
	 * which 4th is considered.
	 */
	public SLConicalCrownFraction(double x0, double y0, double z0, double a, double b, double h) {
		this.x0 = x0;
		this.y0 = y0;
		this.z0 = z0;
		this.a = a;
		this.b = b;
		this.h = h;

		zTop = z0 + h;
		zBottom = z0;
	}

	// fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy() {
		directEnergy = 0;
		diffuseEnergy = 0;

		// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
		// potentialEnergy = 0;

	}

	/**
	 * give a shifted copy of the crown part The shift = new - old coordinates
	 */
	public SLConicalCrownFraction getCopy(double xShift, double yShift, double zShift) {

		SLConicalCrownFraction copy = new SLConicalCrownFraction(x0 + xShift, y0 + yShift, z0 + zShift, a, b, h);

		copy.zTop = zTop; //fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!
		copy.zBottom = zBottom; //fa-08.05.2019: why not '+ zShift;' ??, to be checked with fc !!!
		
		copy.leafAreaDensity = leafAreaDensity;
		copy.extinctionCoefficient = extinctionCoefficient;

		// copy.setTag(this.tag);

		// energy fields stay equal to zero
		return copy;
	}

	// fc-21.11.2013 to make copies (Heterofor, sometimes radiative balance not
	// every years)
	public SLConicalCrownFraction clone() throws CloneNotSupportedException {
		return (SLConicalCrownFraction) super.clone();
	}

	/**
	 * Evaluates if the given ray intercepts this crown part, previously
	 * relocated with the given shifts. Returns the interception path length and
	 * a distance to the origin (i.e. the target cell center) or null if no
	 * interception.
	 * 
	 * @param xShift, yShift, zShift: m, to relocate the crown part for the
	 *            duration of this interception
	 * @param elevation : angle, rad
	 * @param azimuth : angle, rad, trigonometric 0 and counter clockwise
	 */
	public double[] intercept(double xShift, double yShift, double zShift, double elevation, double azimuth) {

		double x2 = x0 + xShift;
		double y2 = y0 + yShift;
		double z2 = z0 + zShift;
		double zTop2 = zTop + zShift;
		double zBottom2 = zBottom + zShift;

//		Vertex3d v0Shifted = new Vertex3d(x2,y2,z2);
		Vertex3d v0Shifted = new Vertex3d(x2,y2,zTop2); // zTop2, top of the cone, is the characteristic point along the z-axis

		double cosElevation = Math.cos(elevation);
		double sinElevation = Math.sin(elevation);
		double cosAzimuth = Math.cos(azimuth);
		double sinAzimuth = Math.sin(azimuth);

		double A = cosElevation * cosElevation * cosAzimuth * cosAzimuth / (a * a) + cosElevation
				* cosElevation * sinAzimuth * sinAzimuth / (b * b) - sinElevation * sinElevation / (h * h);
		double B = -2 * x2 * cosElevation * cosAzimuth / (a * a) - 2 * y2 * sinAzimuth
				* cosElevation / (b * b) + 2 * zTop2 * sinElevation / (h * h);
		double C = x2 * x2 / (a * a) + y2 * y2 / (b * b) - zTop2 * zTop2 / (h * h);

		double[] r = SLModel.solveQuadraticEquation(A, B, C);

		if (r.length == 0) return null; // no interception
		if (r.length == 1) return null; // tangent, ignored

		// Interception
		double root1 = r[0];
		double root2 = r[1];
		boolean isInfiniteRoot = false;
		if (Math.max(Math.log10(Math.abs(root1)), Math.log10(Math.abs(root2))) >= 15) // One root is infinite (the beam is (almost) parallel to one 'side' of the cone), i.e. only one intersection with the cone exists (yet, this intersection is not well estimated due to the infinite root exceeding the machine accuracy, which destabilize the solution of the intersection problem)
			isInfiniteRoot = true;

		// Interception with plane x = x0
		double lx0 = x2 / (cosElevation * cosAzimuth);

		// Interception with plane y = y0
		double ly0 = y2 / (cosElevation * sinAzimuth);

		// Interception with plane z = z0
		double lz0 = z2 / sinElevation;

		// Compute point coordinates of interception with crown part limits
		Vertex v1 = getVertex(root1, elevation, azimuth);
		Vertex v2 = getVertex(root2, elevation, azimuth);
		Vertex vx0 = getVertex(lx0, elevation, azimuth);
		Vertex vy0 = getVertex(ly0, elevation, azimuth);
		Vertex vz0 = getVertex(lz0, elevation, azimuth);
		Vertex vtarget = getVertex(0, elevation, azimuth); // the target point
															// is in L=0, z=0,
															// x=0,
															// y=0 (the origin
															// for this
															// computation)

		// Reset original x, y, z to avoid rounding errors
		vx0.x = x2;
		vy0.y = y2;
		vz0.z = z2;
		double epsilon = 1e-10; //fa-14.05.2019 - accounts for rounding errors in boolean operations below

		double xa = x2 + a;
		double yb = y2 + b;
		double zc = z2 + h;

		Vertex3d min = new Vertex3d(Math.min(x2, xa), Math.min(y2, yb), Math.min(z2, zc)); // box
																							// between
																							// shifted
																							// cone
																							// center
																							// and
																							// crown
																							// limit
		Vertex3d max = new Vertex3d(Math.max(x2, xa), Math.max(y2, yb), Math.max(z2, zc));

		List<Vertex> list = new ArrayList<Vertex>();
		if (inBBox(min, max, v0Shifted, v1, epsilon) && v1.z >= 0 && !isInfiniteRoot) // last condition: if one root is infinite, the point is excluded (intersection solution is not stable, see above), to be improved in the future!
			list.add(v1);
		if (inBBox(min, max, v0Shifted, v2, epsilon) && v2.z >= 0 && !isInfiniteRoot) // last condition: if one root is infinite, the point is excluded (intersection solution is not stable, see above), to be improved in the future!
			list.add(v2);
		if (inBBox(min, max, v0Shifted, vx0, epsilon) && inCone(v0Shifted, vx0) && vx0.z > 0)
			list.add(vx0);
//		if (inBBox(min, max, v0Shifted, vy0, epsilon) && inCone(v0Shifted, vy0) && vy0.z > 0)
		if (inBBox(min, max, v0Shifted, vy0, epsilon) && inCone(v0Shifted, vy0) && vy0.z > 0 && !vy0.equals(vx0,epsilon)) //last condition is used to avoid duplicate point if beam intersects cone axis, epsilon to consider rounding errors
			list.add(vy0);
//		if (inBBox(min, max, v0Shifted, vz0, epsilon) && inCone(v0Shifted, vz0) && vz0.z > 0)
		if (inBBox(min, max, v0Shifted, vz0, epsilon) && inCone(v0Shifted, vz0) && vz0.z > 0 && !vz0.equals(vx0,epsilon) && !vz0.equals(vy0,epsilon)) //two last conditions are used to avoid duplicate point if beam intersects cone axis, epsilon to consider rounding errors
			list.add(vz0);
		if (inBBox(min, max, v0Shifted, vtarget, epsilon) && inCone(v0Shifted, vtarget) && vtarget.z >= vz0.z)
			list.add(vtarget);
		if (list.size() == 0)
			return null; // intersected the cone outside the quarter

		if (list.size() != 2) {

			// Particular cases
			boolean particularCase1 = (zTop2 == 0); // the top of the cone is at the
													// same z as the
													// target point
													// and the rest of the crown
													// part is below the
													// target point

			boolean particularCase2 = false; // The root is at the periphery of
												// the crown part
												// and the second root is below
												// the target point

			boolean particularCase3 = false; // only the intersection with the
												// origin (0,0,0) along
												// the inner sections
			
			boolean particularCase4 = false;  // intersection with an axis (tangent to the crown fraction through this axis)


			if (list.size() == 1) {
				Vertex v = list.get(0);
				if (v == v1 || v == v2) {
					if (v.z == 0)
						particularCase2 = true;
				} else if (v == vtarget && onBBoxPeriphery(min, max, v, epsilon)) {
					particularCase3 = true;
				} else if (vy0.equals(vx0,epsilon) || vz0.equals(vx0,epsilon) || vz0.equals(vy0,epsilon)) {
						particularCase4 = true;
				}
			}
			
			if (!particularCase1 && !particularCase2 && !particularCase3 && !particularCase4) {
				
				// trace for checking	
//				if (list.size() == 1)
//					System.out.println(a + "\t" + b + "\t" + h + "\t" + xShift + "\t" + yShift + "\t" + zShift + "\t" + x0 + "\t" + y0 + "\t" + z0 + "\t" + x2 + "\t" + y2 + "\t" + z2 + "\t" + zBottom2 + "\t" + zTop2 + "\t" + elevation + "\t" + azimuth + "\t" + root1 + "\t" + root2 + "\t" + v1.x + "\t" + v1.y + "\t" + v1.z + "\t" + v2.x + "\t" + v2.y + "\t" + v2.z + "\t" + vx0.x + "\t" + vx0.y + "\t" + vx0.z + "\t" + vy0.x + "\t" + vy0.y + "\t" + vy0.z + "\t" + vz0.x + "\t" + vz0.y + "\t" + vz0.z + "\t" + list.size() + "\t" + list.get(0).x + "\t" + list.get(0).y + "\t" + list.get(0).z + "\t" + inCone(v0Shifted,vx0) + "\t" + inCone(v0Shifted,vy0) + "\t" + inCone(v0Shifted,vz0) + "\t" + inCone(v0Shifted, vtarget));
//				else if (list.size() == 2)
//					System.out.println(a + "\t" + b + "\t" + h + "\t" + xShift + "\t" + yShift + "\t" + zShift + "\t" + x0 + "\t" + y0 + "\t" + z0 + "\t" + x2 + "\t" + y2 + "\t" + z2 + "\t" + zBottom2 + "\t" + zTop2 + "\t" + elevation + "\t" + azimuth + "\t" + root1 + "\t" + root2 + "\t" + v1.x + "\t" + v1.y + "\t" + v1.z + "\t" + v2.x + "\t" + v2.y + "\t" + v2.z + "\t" + vx0.x + "\t" + vx0.y + "\t" + vx0.z + "\t" + vy0.x + "\t" + vy0.y + "\t" + vy0.z + "\t" + vz0.x + "\t" + vz0.y + "\t" + vz0.z + "\t" + list.size() + "\t" + list.get(0).x + "\t" + list.get(0).y + "\t" + list.get(0).z + "\t" + list.get(1).x + "\t" + list.get(1).y + "\t" + list.get(1).z + "\t" + inCone(v0Shifted,vx0) + "\t" + inCone(v0Shifted,vy0) + "\t" + inCone(v0Shifted,vz0) + "\t" + inCone(v0Shifted, vtarget));
//				else if (list.size() == 3)
//					System.out.println(a + "\t" + b + "\t" + h + "\t" + xShift + "\t" + yShift + "\t" + zShift + "\t" + x0 + "\t" + y0 + "\t" + z0 + "\t" + x2 + "\t" + y2 + "\t" + z2 + "\t" + zBottom2 + "\t" + zTop2 + "\t" + elevation + "\t" + azimuth + "\t" + root1 + "\t" + root2 + "\t" + v1.x + "\t" + v1.y + "\t" + v1.z + "\t" + v2.x + "\t" + v2.y + "\t" + v2.z + "\t" + vx0.x + "\t" + vx0.y + "\t" + vx0.z + "\t" + vy0.x + "\t" + vy0.y + "\t" + vy0.z + "\t" + vz0.x + "\t" + vz0.y + "\t" + vz0.z + "\t" + list.size() + "\t" + list.get(0).x + "\t" + list.get(0).y + "\t" + list.get(0).z + "\t" + list.get(1).x + "\t" + list.get(1).y + "\t" + list.get(1).z + "\t" + list.get(2).x + "\t" + list.get(2).y + "\t" + list.get(2).z + "\t" + inCone(v0Shifted,vx0) + "\t" + inCone(v0Shifted,vy0) + "\t" + inCone(v0Shifted,vz0) + "\t" + inCone(v0Shifted, vtarget));

				
				SLReporter.printInLog(Log.WARNING, "SLConicalCrownFraction.intercept ()", "The number of interception if neither 0, 2 or an expected particular case. "
								+ "It could be a tangent point with the inner section. A check is needed! The list size is "
								+ list.size());
				SLReporter.printInStandardOutput("SLConicalCrownFraction.intercept () - The number of interception if neither 0, 2 or an expected particular case. "
								+ "It could be a tangent point with the inner section. A check is needed! The list size is "
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
			
			// trace for checking	
//			System.out.println(a + "\t" + b + "\t" + xShift + "\t" + yShift + "\t" + zShift + "\t" + x0 + "\t" + y0 + "\t" + z0 + "\t" + x2 + "\t" + y2 + "\t" + z2 + "\t" + zBottom2 + "\t" + zTop2 + "\t" + elevation + "\t" + azimuth + "\t" + root1 + "\t" + root2 + "\t" + v1.x + "\t" + v1.y + "\t" + v1.z + "\t" + v2.x + "\t" + v2.y + "\t" + v2.z + "\t" + vx0.x + "\t" + vx0.y + "\t" + vx0.z + "\t" + vy0.x + "\t" + vy0.y + "\t" + vy0.z + "\t" + vz0.x + "\t" + vz0.y + "\t" + vz0.z + "\t" + list.size() + "\t" + w1.x + "\t" + w1.y + "\t" + w1.z + "\t" + w2.x + "\t" + w2.y + "\t" + w2.z + "\t" + inCone(v0Shifted,vx0) + "\t" + inCone(v0Shifted,vy0) + "\t" + inCone(v0Shifted,vz0) + "\t" + inCone(v0Shifted, vtarget));

			return new double[] { pathLength, distance };
		}

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
	 * check if a vertex point is included between max and min vertex points
	 */
	// TODO remove unused vs?
	private boolean inBBox(Vertex3d min, Vertex3d max, Vertex3d vs, Vertex v) {

		// System.out.println("SLCrownFraction inBBox (min, max, v)... ");
		// System.out.println("min: "+min);
		// System.out.println("max: "+max);
		// System.out.println("v  : "+v);

		// 1. Test with bounding box
		boolean inBBox = (min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y && min.z <= v.z && v.z <= max.z);

		// System.out.println("inBBox: "+inBBox);

		return inBBox;

	}
	
	/**
	 * check if a vertex point is included between max and min vertex points, EPSILON is used to consider rounding errors
	 * F. Andre - May 2019
	 */
	// TODO remove unused vs?
	private boolean inBBox(Vertex3d min, Vertex3d max, Vertex3d vs, Vertex v, double EPSILON) {

		// System.out.println("SLCrownFraction inBBox (min, max, v)... ");
		// System.out.println("min: "+min);
		// System.out.println("max: "+max);
		// System.out.println("v  : "+v);

		// 1. Test with bounding box
		boolean inBBox = (min.x - v.x <= EPSILON && v.x - max.x <= EPSILON && min.y - v.y <= EPSILON && v.y - max.y <= EPSILON && min.z - v.z <= EPSILON && v.z - max.z <= EPSILON);

		// System.out.println("inBBox: "+inBBox);

		return inBBox;

	}

	/**
	 * check if a vertex point is included between max and min vertex points
	 */
	private boolean onBBoxPeriphery(Vertex3d min, Vertex3d max, Vertex v) {
		boolean onBBoxPeriphery = (min.x == v.x || v.x == max.x || min.y == v.y || v.y == max.y || min.z == v.z || v.z == max.z);
		return onBBoxPeriphery;

	}
	
	/**
	 * check if a vertex point is included between max and min vertex points, EPSILON is used to consider rounding errors
	 * F. Andre - May 2019
	 */
	private boolean onBBoxPeriphery(Vertex3d min, Vertex3d max, Vertex v, double EPSILON) {
		boolean onBBoxPeriphery = (Math.abs(min.x - v.x) <= EPSILON || Math.abs(v.x - max.x) <= EPSILON || Math.abs(min.y - v.y) <= EPSILON || Math.abs(v.y - max.y) <= EPSILON || Math.abs(min.z - v.z) <= EPSILON || Math.abs(v.z - max.z) <= EPSILON);
		return onBBoxPeriphery;

	}

	/**
	 * check if a vertex is included in the cone of the crownPart (inside
	 * and not at the periphery!)
	 */
	private boolean inCone(Vertex3d vs, Vertex v) {
		double value = (v.x - vs.x) * (v.x - vs.x) / (a * a) + (v.y - vs.y)
				* (v.y - vs.y) / (b * b) - (v.z - vs.z) * (v.z - vs.z) / (h * h);
		boolean inCone = (value < 0);
		return inCone;
	}

	/**
	 * get crown part volume in m3
	 */
	@Override
	public double getVolume() {
		// fc+mj22.11.2012 added Math.abs below
		double volume = Math.abs(1d / 3d * Math.PI * a * b * h); // full cone volume
		double fractionVolume = volume / 4d; // -> divided by 4
		return fractionVolume;
	}

	public double getLeafAreaDensity() {
		return leafAreaDensity;
	}

	public void setLeafAreaDensity(double leafAreaDensity) {
		isSetLeafAreaDensity = true;
		this.leafAreaDensity = leafAreaDensity;
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
					"SLConicalCrownFraction.getLeafArea(): this has been called before setting crown part LAD. It has returned 0.", null);
			return 0;
		}
	}

	@Override
	public boolean isIndsideCrownPart(double x, double y, double z) {
		Vertex3d v = new Vertex3d(x0, y0, z0);
		Vertex3d vs = new Vertex3d(x, y, z);
		double value = (v.x - vs.x) * (v.x - vs.x) / (a * a) + (v.y - vs.y) * (v.y - vs.y) / (b * b) - (v.z - vs.z) * (v.z - vs.z) / (h * h);
		boolean inCone = (value < 0);
		
		// fa-02.05.2019: ?? to be checked -> why 'a' everywhere?
		// gl-16.05.2019: I corrected the two following lines so as the bounding box depends on b and h also
		// this function was not used anymore and should be verified if it is used in the future.
		Vertex3d min = new Vertex3d (Math.min (x0, x0+a), Math.min (y0, y0+b), Math.min (z0, z0+h)); // box
		Vertex3d max = new Vertex3d (Math.max (x0, x0+a), Math.max (y0, y0+b), Math.max (z0, z0+h));
		boolean inBBox = (min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y && min.z <= v.z && v.z <= max.z);
		return inCone && inBBox;
	}
	
}
