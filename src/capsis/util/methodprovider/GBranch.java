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
package capsis.util.methodprovider;


/**
 * Branch description.
 *
 * @author D. Pont - dec 2005
 */
public class GBranch {

	private int id;
	private int bearerId;
	private double insertionHeight_mm;
	private double insertionAngle_deg;
	private double insertionAzimuth_deg;
	private double diameter_mm;
	private double length_mm;
	private double deadLength_mm;	// details needed...
	// status ? alive / dead / pruned

	/**	Get a crown description
	*/
	public GBranch (
			int id,			// from 1 for the first branch
			int bearerId,	// 0 for the trunk
			double insertionHeight_mm,
			double insertionAngle_deg,
			double insertionAzimuth_deg,
			double diameter_mm,
			double length_mm,
			double deadLength_mm
			) {
		this.id = id;
		this.bearerId = bearerId;
		this.insertionHeight_mm = insertionHeight_mm;
		this.insertionAngle_deg = insertionAngle_deg;
		this.insertionAzimuth_deg = insertionAzimuth_deg;
		this.diameter_mm = diameter_mm;
		this.length_mm = length_mm;
		this.deadLength_mm = deadLength_mm;
	}

	public int getId () {return this.id;}
	public int getBearerId () {return this.bearerId;}
	public double getInsertionHeight_mm () {return this.insertionHeight_mm;}
	public double getInsertionAngle_deg () {return this.insertionAngle_deg;}
	public double getInsertionAzimuth_deg () {return this.insertionAzimuth_deg;}
	public double getDiameter_mm () {return this.diameter_mm;}
	public double getLength_mm () {return this.length_mm;}
	public double getDeadLength_mm () {return this.deadLength_mm;}

}


