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
package capsis.extension.treelogger;

import java.io.Serializable;

/**
 * Contour description. For now, this class only serves to define the radius of a circle section. 
 * NOTE: The other features, i.e. the centre in x and z as well as the angle are unused.
 * @author F. Mothe + F. de Coligny - january 2006
 * @author M. Fortin - November 2011 (refactoring)
 */
@SuppressWarnings("unused")
public class GContour implements Serializable {
	// Axes: x horizontal to right, y vertical upward, z horizontal towards the front
	
	// GContour describes a contour in the (x, z) plane (horizontal plane)
	// No reference to y here
	@Deprecated
	private double centreX_mm;
	@Deprecated
	private double centreZ_mm;
	
	// angleY_deg is applied to the first radius in radius_mm array
	// Degrees rotation around the Y axis
	// Trigonometric direction (anticlockwise), 
	// angleY_deg=0 means the first radius is along the positive x axis
	@Deprecated
	private double angleY_deg;

	// radius all start from the (x, z) origin
	// n = number of radius = radius_mm.length
	// n=1 : circle (angleY_deg is unused)
	// n=2 : ellipse : majorRadius is in first position, minor radius is in second position
	// n>2 : the angle between two successive radius is 360/n
	private double[] radius_mm;


//	/**	General contour constructor.
//	*/
//	protected GContour(double centreX_mm,
//			double centreZ_mm,
//			double[] radius_mm, 
//			double angleY_deg) {
//		this.centreX_mm = centreX_mm;
//		this.centreZ_mm = centreZ_mm;
//		this.radius_mm = radius_mm;
//		this.angleY_deg = angleY_deg;
//	}

	/**	Circle constructor.
	*/
	public GContour(double centreX_mm,
			double centreZ_mm,
			double radius_mm) {
		this.centreX_mm = centreX_mm;
		this.centreZ_mm = centreZ_mm;
		this.radius_mm = new double[1];
		this.radius_mm[0] = radius_mm;
		this.angleY_deg = 0;	// unused for circle
	}

//	/**	Ellipse constructor.
//	*/
//	protected GContour(double centreX_mm,
//			double centreZ_mm,
//			double majorRadius_mm, 
//			double minorRadius_mm, 
//			double angleY_deg) {
//		this.centreX_mm = centreX_mm;
//		this.centreZ_mm = centreZ_mm;
//		this.radius_mm = new double[2];
//		this.radius_mm[0] = majorRadius_mm;	// first radius is the major radius
//		this.radius_mm[1] = minorRadius_mm;
//		this.angleY_deg = angleY_deg;
//	}

	
	/**
	 * This method returns the mean radius of the contour. If 2 radius (ellipse), check if calculation is correct (meanRadius=(r+R) / 2)
	 * @return the mean radius (mm)
	 */
	protected double getMeanRadius_mm(){
		if (radius_mm == null) {
			return -1;
		}
		int n = radius_mm.length;
		if (n == 0) {
			return -1;
		}
		
		double sum = 0;
		for (int i = 0; i < n; i++) {
			sum += radius_mm[i];
		}
		return sum / n;
	}
	
}

