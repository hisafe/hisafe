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
 * Ring description.
 * Part of a Dics in a Piece
 * Rings may carry wood properties values for sectors, see GPiece for the number 
 * of sectors (numberOfRadius) and the number of wood properties 
 * (woodPropertyNames.length).
 * @author D. Pont - dec 2005
 * @author Mathieu Fortin - November 2011 (refactoring)
 */
public class GPieceRing extends GContour implements Serializable {


	private int id;			// needed, unique for the disc
	private int discId;		// needed
	
//	public double[][] woodProperties;	// optional, dimensioned by GPiece.numberOfRadius then GPiece.woodPropertyNames.length


	/**	
	 * Circle constructor
	 */
	/**
	 * General constructor.
	 * @param id the id of this ring
	 * @param discId the id of the disc
	 * @param centreX_mm the centre location in x (mm)
	 * @param centreZ_mm the centre location in z (mm)
	 * @param radius_mm the radius of the ring
	 */
	public GPieceRing(int id,
			int discId,
			double centreX_mm,
			double centreZ_mm,
			double radius_mm) {
		super (centreX_mm, centreZ_mm, radius_mm);
		this.id = id;
		this.discId = discId;
	}

	/**
	 * This method returns the id of this ring.
	 * @return an integer
	 */
	public int getRingId() {return id;}
	
	/**
	 * This method returns the id of the disc from which comes this ring.
	 * @return a integer
	 */
	public int getDiscId() {return discId;}
	
	
	
//	/**	Ellipse constructor
//	*/
//	public GPieceRing (
//			int id,
//			int discId,
//			double centreX_mm,
//			double centreZ_mm,
//			double majorRadius_mm,
//			double minorRadius_mm,
//			double angleY_deg
//			) {
//
//		super (centreX_mm, centreZ_mm, majorRadius_mm, minorRadius_mm, angleY_deg);
//
//		this.id = id;
//		this.discId = discId;
//	}
	
//	/**	
//	 * General constructor. UNUSED.
//	 */
//	public GPieceRing(int id,
//			int discId,
//			double centreX_mm,
//			double centreZ_mm,
//			double[] radius_mm,
//			double angleY_deg) {
//		super (centreX_mm, centreZ_mm, radius_mm, angleY_deg);
//		this.id = id;
//		this.discId = discId;
//	}


//	// UNUSED
//	/**
//	 * This method returns the property that is contained in the row (locationIndex) and the column (propertyIndex).
//	 * @param locationIndex
//	 * @param propertyIndex
//	 * @return a double
//	 */
//	@Deprecated
//	public double getWoodProperty (int locationIndex, int propertyIndex) {
//		return woodProperties[locationIndex][propertyIndex];
//	}

	/**
	 * A representation of the ring in a String to check its structure.
	 */
	public String toString () {
		StringBuffer b = new StringBuffer ("  GPieceRing: "+getRingId()+" radius_mm: "+getMeanRadius_mm());
		return b.toString ();
		
	}

	
}


