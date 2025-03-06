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
import java.util.Collections;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;

/**
 * SLEllipsoidalCrownPart - A description of a part of a crown. A crown may contain one or two such
 * objects, one for the top and one for the bottom.
 * 
 * @author G. Ligot november 2016
 */
public class SLVoxelCrownPart extends SLCrownPart {
	
//	public static final SLTag CROWN_VOXEL = new SLTag ("CROWN_VOXEL");

	// Voxel origin (bottom, left, front). It is not the coordinates of voxel center!
	public double x; //coordinates of the corner with min x,y,z
	public double y;
	public double z;

	// voxel parameter 
	public double size; // voxel size in m??

	private double transmissivity; //porosity

	private double leafAreaDensity; // m2/m3
	private double extinctionCoefficient;

	private double directEnergy; // MJ
	private double diffuseEnergy; // MJ

	// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//	private double potentialEnergy; // MJ

	private boolean isSetLeafAreaDensity = false;

	/**
	 * Constructor
	 */
	public SLVoxelCrownPart (double x, double y, double z, double size) {
		this.x = x; 
		this.y = y;
		this.z = z;
		this.size = size;

		// Tag default value
//		this.tag = CROWN_VOXEL;
		
	}


	public SLVoxelCrownPart getCopy (double xShift, double yShift, double zShift) {

		SLVoxelCrownPart copy = new SLVoxelCrownPart (x + xShift, y + yShift, z + zShift, size);

		copy.leafAreaDensity = leafAreaDensity;
		copy.extinctionCoefficient = extinctionCoefficient;

//		copy.setTag(this.tag);

		// energy fields stay equal to zero
		return copy;
	}

	// from SLEllipsoidalCrownPart : fc-21.11.2013 to make copies (Heterofor, sometimes radiative balance not every years)
	public SLVoxelCrownPart clone () throws CloneNotSupportedException {
		return (SLVoxelCrownPart) super.clone ();
	}

	//  from SLEllipsoidalCrownPart : fc-15.5.2014 MJ found double energy in Heterofor after an intervention
	public void resetEnergy () {
		directEnergy = 0;
		diffuseEnergy = 0;

		// fc+fa-26.4.2017 Potential energy is now in SLTreeLightResult
//		potentialEnergy = 0;
	}

	/**
	 * Evaluates if the given ray intercepts this crown part, previously relocated with the given
	 * shifts. Returns the interception path length and a distance to the origin (i.e. the target
	 * cell center) or null if no interception.
	 * 
	 * @param xShift , yShift, zShift: m, to relocate the crown part for the duration of this
	 *        interception
	 * @param elevation : angle, rad
	 * @param azimuth : angle, rad, trigonometric 0 and counter clockwise
	 */
	public double[] intercept (double xShift, double yShift, double zShift, double elevation,double azimuth) {

		double x2 = x + xShift;
		double y2 = y + yShift;
		double z2 = z + zShift;

		Vertex3d v0shifted = new Vertex3d(x2,y2,z2);

		double cosElevation = Math.cos (elevation);
		double sinElevation = Math.sin (elevation);
		double cosAzimuth = Math.cos (azimuth);
		double sinAzimuth = Math.sin (azimuth);

		//compute intersection points
		//-------------------------------
		//1) compute intersection with the six voxel planes
		// Interception with plane x = x0
		double l1 = x2 / (cosElevation * cosAzimuth);
		// Interception with plane x = x0 + size
		double l2 = (x2+size) / (cosElevation * cosAzimuth);
		// Interception with plane y = y0
		double l3 = y2 / (cosElevation * sinAzimuth);
		// Interception with plane y = y0 + size
		double l4 = (y2+size) / (cosElevation * sinAzimuth);
		// Interception with plane z = z0
		double l5 = z2 / sinElevation;
		// Interception with plane z = z0 + size
		double l6 = (z2+size) / sinElevation;

		// compute corresponding vertexes
		Vertex i1 = getVertex (l1, elevation, azimuth);
		Vertex i2 = getVertex (l2, elevation, azimuth);
		Vertex i3 = getVertex (l3, elevation, azimuth);
		Vertex i4 = getVertex (l4, elevation, azimuth);
		Vertex i5 = getVertex (l5, elevation, azimuth);
		Vertex i6 = getVertex (l6, elevation, azimuth);


		//2) check if the intersection are inside the voxel (avoid confusing/multiple intersection with adjacent voxel)
		List<Vertex> list = new ArrayList<Vertex> ();
		if (i1 !=null && inVoxel (v0shifted, i1)) list.add (i1);		
		if (i2 !=null && inVoxel (v0shifted, i2)) list.add (i2);	
		if (i3 !=null && inVoxel (v0shifted, i3)) list.add (i3);	
		if (i4 !=null && inVoxel (v0shifted, i4)) list.add (i4);	
		if (i5 !=null && inVoxel (v0shifted, i5)) list.add (i5);	
		if (i6 !=null && inVoxel (v0shifted, i6)) list.add (i6);	


		if (list.size() == 2) {

			// intersection (distance from the target point)
			Vertex root1 = list.get(0);
			Vertex root2 = list.get(1);

			double length1 = root1.l;
			double length2 = root2.l;

			double distance = (length1 + length2) / 2d;
			double pathLength = Math.abs(length1 - length2);

			return new double[] {pathLength, distance};

		} else if (list.size() > 2) {
			//this means that voxel has been hitted on one edge/corner
			//order intersection by L and take only the first and the last ones
			Collections.sort(list);
			
			Vertex root1 = list.get(0);
			Vertex root2 = list.get(list.size()-1);

			double length1 = root1.l;
			double length2 = root2.l;

			double distance = (length1 + length2) / 2d;
			double pathLength = Math.abs(length1 - length2);

			return new double[] {pathLength, distance};
			
//			System.out.println("SLVoxelCrownPart : number of intersection greater than 2. Program bug?");
//			System.out.println("x<-c(" + i1.x +","+i2.x+","+i3.x+","+i4.x+","+i5.x+","+i6.x+")");
//			System.out.println("y<-c(" + i1.y +","+i2.y+","+i3.y+","+i4.y+","+i5.y+","+i6.y+")");
//			System.out.println("z<-c(" + i1.z +","+i2.z+","+i3.z+","+i4.z+","+i5.z+","+i6.z+")");
//			System.out.println("xp<-c("+ x2 +","+ (x2+size) +")");
//			System.out.println("yp<-c("+ y2 +","+ (y2+size) +")");
//			System.out.println("zp<-c("+ z2 +","+ (z2+size) +")");
		}else if (list.size() == 1) {
			SLReporter.printInStandardOutput("SLVoxelCrownPart : number of intersection equals 1. Program bug?");
			SLReporter.printInStandardOutput("x<-c(" + i1.x +","+i2.x+","+i3.x+","+i4.x+","+i5.x+","+i6.x+")");
			SLReporter.printInStandardOutput("y<-c(" + i1.y +","+i2.y+","+i3.y+","+i4.y+","+i5.y+","+i6.y+")");
			SLReporter.printInStandardOutput("z<-c(" + i1.z +","+i2.z+","+i3.z+","+i4.z+","+i5.z+","+i6.z+")");
			SLReporter.printInStandardOutput("xp<-c("+ x2 +","+ (x2+size) +")");
			SLReporter.printInStandardOutput("yp<-c("+ y2 +","+ (y2+size) +")");
			SLReporter.printInStandardOutput("zp<-c("+ z2 +","+ (z2+size) +")");
		}

		return null;
	}



	/**
	 * Compute the coordinates of the interception point from the distance to the target cell and beam direction.
	 * @param length
	 * @param elevation
	 * @param azimuth
	 * @return a vertex containing the coordinates of the interception point and its distance to the target cell
	 * @author GL Feb 2013
	 */
	private Vertex getVertex(double length, double elevation, double azimuth) {
		if(!Double.isInfinite(length)){ //avoid division by 0
			double cosElevation = Math.cos(elevation);
			double sinElevation = Math.sin(elevation);
			double cosAzimuth = Math.cos(azimuth);
			double sinAzimuth = Math.sin(azimuth);
			double x = length * cosElevation * cosAzimuth;
			double y = length * cosElevation * sinAzimuth;
			double z = length * sinElevation;
			return new Vertex(x, y, z, length);
		}else{
			SLReporter.printInStandardOutput("SLVoxelCrownPart.getVertex() - parallel line and plane - no intersection should be considered?");
			return null;
		}
	}

	/**
	 * Inner class: a vertex with an associated length
	 */
	private static class Vertex implements java.lang.Comparable {
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
		public String toString() {return "Vertex x: " + x + " y: " + y + " z: " + z + " l: " + l;}
		@Override
		public int compareTo(Object o) {
			double score1 = ((Vertex) o).l;
			double score2 = this.l;
			if (score1 < score2) return -1;
			else if(score1 == score2)return 0;
			else return 1;
		}
	}

	@Override
	public double getVolume () {
		double volume = Math.pow(size,3) ; // full cubic volume
		return volume;
	}

	@Override
	public double getExtinctionCoefficient () {
		return this.extinctionCoefficient;
	}

	@Override
	public void setExtinctionCoefficient (double extinctionCoefficient) {
		this.extinctionCoefficient = extinctionCoefficient;

	}


	public double getDirectEnergy () {
		return directEnergy;
	} // direct beam energy in MJ

	public double getDiffuseEnergy () {
		return diffuseEnergy;
	} // diffuse beam energy in MJ

	// fc+fa-3.8.2017 added synchronized for Parallel mode
	synchronized public void addDirectEnergy (double e) {
		this.directEnergy += e;
	} // MJ
	
	// fc+fa-3.8.2017 added synchronized for Parallel mode
	synchronized public void addDiffuseEnergy (double e) {
		this.diffuseEnergy += e;
	} // MJ


	
	
//	public double getPotentialEnergy () {
//		return potentialEnergy;
//	} // in MJ, energy intercepted by this part but without neighbours
//	public void addPotentialEnergy (double e) {
//		this.potentialEnergy += e;
//	} // MJ


	


	/**check if a vertex point is included in this voxel
	 * Because of rounding problems all number were rounded to the 6th decimal
	 */
	private boolean inVoxel(Vertex3d shiftedVoxelOrigin, Vertex point) {

		int digits = 6; //because of rounding problems 

		boolean inBBox = (  round(point.x,digits) <= round((shiftedVoxelOrigin.x+size),digits) &&
				round(point.y,digits) <= round((shiftedVoxelOrigin.y+size),digits) &&
				round(point.z,digits) <= round((shiftedVoxelOrigin.z+size),digits) &&
				round(point.x,digits) >= round((shiftedVoxelOrigin.x),digits) &&
				round(point.y,digits) >= round((shiftedVoxelOrigin.y),digits) &&
				round(point.z,digits) >= round((shiftedVoxelOrigin.z),digits)
				);
		return inBBox;
	}

	/**simple round method
	 */
	public double round(double value, int places) {
		if (places < 0) throw new IllegalArgumentException();
		long factor = (long) Math.pow(10, places);
		value = value * factor;
		long tmp = Math.round(value);
		return (double) tmp / factor;
	}


	public double getLeafAreaDensity () {return leafAreaDensity;}

	public void setLeafAreaDensity (double leafAreaDensity) {
		this.leafAreaDensity = leafAreaDensity;
		this.isSetLeafAreaDensity = true;}

	/**
	 * get Leaf area m2
	 * @pre LAD must be set
	 */
	public double getLeafArea () {
		if (isSetLeafAreaDensity) {
			return leafAreaDensity * getVolume ();
		}else {
			SLReporter.printInLog(Log.WARNING, "SLEllipsoidalCrownPart.getLeafArea(): this has been called before setting crown part LAD. It has returned 0.", null);
			return 0;
		}
	}

	@Override
	public boolean isIndsideCrownPart (double xp, double yp, double zp) {
		double x0 = this.x;
		double y0 = this.y;
		double z0 = this.z;
		Vertex3d v = new Vertex3d (x0,y0,z0);
		Vertex3d min = new Vertex3d (Math.min (x0, x0+size), Math.min (y0, y0+size), Math.min (z0, z0+size)); // box
		Vertex3d max = new Vertex3d (Math.max (x0, x0+size), Math.max (y0, y0+size), Math.max (z0, z0+size));
		boolean inBBox = (min.x <= v.x && v.x <= max.x && min.y <= v.y && v.y <= max.y && min.z <= v.z && v.z <= max.z);
		return inBBox;
	}

	public double getTransmissivity() {return transmissivity;}
	public void setTransmissivity(double transmissivity) {this.transmissivity = transmissivity;}

	/**
	 * Returns true if this crown part is located in the upper part of the
	 * crown, Voxel crown parts are considered as upper, used by SamsaraLight.
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
