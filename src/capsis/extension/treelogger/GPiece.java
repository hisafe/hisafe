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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;

import capsis.util.methodprovider.GBranch;
import jeeb.lib.util.Vertex3d;
import repicea.simulation.treelogger.LogCategory;
import repicea.simulation.treelogger.LoggableTree;

/**
 * Piece description. Could be a whole tree or a part such as a log. Please
 * respect naming conventions and provide needed properties. Detailled comments
 * can be found for every properties in GPiece and related descriptions, please
 * conform to this online documentation.
 * 
 * @author D. Pont - dec 2005
 * @author modified by M. Fortin and F. Mothe - June 2010
 * @author modified by M. Fortin - November 2011
 * @author modified by M. Fortin - February 2018
 * 
 */
public class GPiece extends GPieceLight {

	private static final long serialVersionUID = 20100805L;

	private Vector<GPieceDisc> discs; // needed, at least one disc, else
										// (general case) at least 2
										// extremeties, maybe more discs
	private Collection<GBranch> branches; // optional, instances of GBranch
	private Vector<Vertex3d> pithPoints; // optional, instances of Vertex3d

	// we may not have at least 2 discs in the piece in the case
	// of small tress < 4 m OR the discs may not be at the extremities
	// so piece length is not calculated from the discs positions
	private double pieceLength_mm; // needed
	private double pieceY_mm; // needed, piece bottom height (there may be no
								// disc at the bottom of the piece)

	// see GContour, applied to all rings and contours in discs
	// all contours and rings in the GPiece have the same number od radius
	private byte numberOfRadius; // needed

	// private GPieceTreeInfo treeInfo; // needed
	private String treeStatus; // TODO check if the method
								// getTreeStatusPriorToLogging in the
								// LoggableTree interface does the same ???

	// private String pieceProduct; // optional, free typology - fc [FMothe] -
	// 30.3.2006
	private String pieceOrigin; // optional, free typology

	private int priority;

	/**
	 * Constructor.
	 * 
	 * @param tree
	 *            the tree from which comes this piece
	 * @param id
	 *            the piece id
	 * @param rankInTree
	 *            the rank of the piece in the tree from the bottom to the top
	 * @param withinTreeExpansionFactor
	 *            a within tree expansion factor, typically a crown expansion
	 *            factor
	 * @param pieceLength_mm
	 *            the length of the piece (mm)
	 * @param pieceY_mm
	 *            the height of the bottom section of the piece in the tree (mm)
	 * @param pieceWithBark
	 *            true if the piece is considered with bark or false otherwise
	 * @param pieceWithPith
	 *            true if the piece is considered with pith or false otherwise
	 * @param numberOfRadius
	 *            the number of radius in this piece
	 * @param treeStatus
	 *            the tree status
	 * @param logCategory
	 *            the log category of this piece
	 */
	public GPiece(LoggableTree tree, 
			int id, 
			int rankInTree, 
			double withinTreeExpansionFactor, 
			double pieceLength_mm, // fc+cm+tl+fm+pv
			double pieceY_mm, // fc+cm+tl+fm+pv - 20.3.2006
			boolean pieceWithBark, // fc+cm+tl+fm+pv - 20.3.2006
			boolean pieceWithPith, // fc+cm+tl+fm+pv - 20.3.2006
			byte numberOfRadius, String treeStatus, LogCategory logCategory) {
		super(logCategory, id, tree, rankInTree, pieceWithBark, pieceWithPith);
		setWithinTreeExpansionFactor(withinTreeExpansionFactor);
		this.pieceLength_mm = pieceLength_mm;
		this.pieceY_mm = pieceY_mm;
		this.numberOfRadius = numberOfRadius;
		this.treeStatus = treeStatus;
		discs = new Vector<GPieceDisc>();
		branches = new ArrayList<GBranch>();
		pithPoints = new Vector<Vertex3d>();
	}

	/**
	 * This method adds a disc to this piece of wood.
	 * 
	 * @param disc
	 *            a GPieceDisc instance
	 */
	public void addDisc(GPieceDisc disc) {
		getDiscs().add(disc);
	}

	/**
	 * This method returns the number of radius on this piece.
	 * 
	 * @return a byte instance
	 */
	public byte getNumberOfRadius() {
		return numberOfRadius;
	}

	/**
	 * This method returns the collection of discs that represents this wood
	 * piece.
	 * 
	 * @return a Collection of GPieceDisc instances
	 */
	public Vector<GPieceDisc> getDiscs() {
		return discs;
	}

	/**
	 * This method returns the number of discs that compose the wood piece.
	 * 
	 * @return a integer
	 */
	public int getNumberOfDiscs() {
		return getDiscs().size();
	}

	/**
	 * This method returns the disc at the bottom of the wood piece.
	 * 
	 * @return a GPieceDisc instance
	 */
	public GPieceDisc getBottomDisc() {
		return getDiscs().firstElement();
	}

	/**
	 * This method returns the disc at the top of the wood piece.
	 * 
	 * @return a GPieceDisc instance
	 */
	public GPieceDisc getTopDisc() {
		return getDiscs().lastElement();
	}

	// UNUSED
	/**
	 * This method adds a branch to this wood piece.
	 * 
	 * @param branch
	 *            a GBranch instance
	 */
	@Deprecated
	public void addBranch(GBranch branch) {
		branches.add(branch);
	}

	/**
	 * This method returns the collection of branches of this wood piece.
	 * 
	 * @return a Collection of GBranch instances
	 */
	public Collection<GBranch> getBranches() {
		return branches;
	}

	/**
	 * This method adds a pith point in the wood piece.
	 * 
	 * @param pithPoint
	 *            a Vertex3d instance
	 */
	public void addPithPoint(Vertex3d pithPoint) {
		pithPoints.add(pithPoint);
	}

	/**
	 * This method returns the collection of pith points that compose the wood
	 * piece.
	 * 
	 * @return a Collection of Vertex3d instances
	 */
	public Vector<Vertex3d> getPithPoints() {
		return pithPoints;
	}

	/**
	 * This method returns the length of the piece in m.
	 * 
	 * @return a double
	 */
	public double getLengthM() {
		return this.pieceLength_mm * .001;
	}

	/**
	 * This method returns the height of the bottom section in the tree in m.
	 * 
	 * @return a double
	 */
	public double getHeightOfBottomSectionM() {
		return pieceY_mm * .001;
	}

	/**
	 * This method returns the height of the top section in the tree in m.
	 * 
	 * @return a double
	 */
	private double getHeightOfTopSectionM() {
		return getHeightOfBottomSectionM() + getLengthM();
	}

	@Override
	public double getWoodVolumeM3() {

		return super.getWoodVolumeM3();
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	/**
	 * This method returns the priority of the wood product class.
	 * 
	 * @return an integer
	 */
	public int getPriority() {
		return priority;
	}

	// TODO check if this is necessary
	public String getTreeStatus() {
		return treeStatus;
	}

	public String getOrigin() {
		return pieceOrigin;
	}

	public void setOrigin(String origin) {
		this.pieceOrigin = origin;
	}

	/**
	 * This method returns the full name of the GPiece object. That is the
	 * origin, plus the log category.
	 * 
	 * @return a String
	 */
	public String getFullName() {
		String name = getLogCategory().getName();
		if (name.length() == 0) {
			if (getOrigin() != null && getOrigin().length() > 0) {
				int last = Math.min(getOrigin().length(), 2);
				name = "[" + getOrigin().substring(0, last) + "]";
			}
		}
		if (name.length() > 0) {
			name = " " + name;
		}
		name = "" + getRank() + name;
		return name;
	}

	/**
	 * This method returns true if all the discs of the piece have some
	 * heartwood or false otherwise.
	 * 
	 * @return a boolean
	 */
	public boolean hasHeartWood() {
		for (GPieceDisc disc : getDiscs()) {
			if (!disc.getContours().containsKey("heartWood")) {
				return false;
			}
		}
		return true;
	}

	/**
	 * This method returns true if all the discs of the piece have some
	 * knottycore or false otherwise.
	 * 
	 * @return a boolean
	 */
	public boolean hasKnottyCore() {
		for (GPieceDisc disc : getDiscs()) {
			if (!disc.getContours().containsKey("crownBase")) {
				return false;
			}
		}
		return true;
	}

	/**
	 * This method returns true if all the discs of the piece have a lowest dead
	 * branch or false otherwise.
	 * 
	 * @return a boolean
	 */
	public boolean hasFirstDeadBranch() {
		for (GPieceDisc disc : getDiscs()) {
			if (!disc.getContours().containsKey("firstDeadBranch")) {
				return false;
			}
		}
		return true;
	}

	/**
	 * This method returns true if all the discs of the piece have some juvenile
	 * wood or false otherwise.
	 * 
	 * @return a boolean
	 */
	public boolean hasJuvenileWood() {
		for (GPieceDisc disc : getDiscs()) {
			if (!disc.getContours().containsKey("juvenileWood")) {
				return false;
			}
		}
		return true;
	}

	/**
	 * This method returns the radius on the external side (toward bark) of the
	 * ring at a given cambial age and a given height (uses interpolation
	 * between discs).
	 * 
	 * @param height_m
	 *            the height (m)
	 * @param cambialAge
	 *            the cambial age (yr)
	 * @return the external radius (mm)
	 */
	private double getExtRadius_mm(double height_m, int cambialAge) {
		double radius = 0.0;
		GPieceDisc d0 = getBottomDisc();
		GPieceDisc d1 = getTopDisc();
		if (d0.getHeight_m() <= height_m && d1.getHeight_m() >= height_m) {
			for (GPieceDisc d : getDiscs()) {
				double z = d.getHeight_m();
				if (z < height_m) {
					d0 = d;
				}
				if (z >= height_m) {
					d1 = d;
					break;
				}
			}

			double z0 = d0.getHeight_m();
			double z1 = d1.getHeight_m();
			double r0 = d0.getExtRadius_mm(cambialAge);
			if (z1 > z0) {
				double r1 = d1.getExtRadius_mm(cambialAge);
				double ratio = (height_m - z0) / (z1 - z0);
				radius = r0 * (1.0 - ratio) + r1 * ratio;
			} else {
				radius = r0;
			}
		}
		return radius;
	}

	/**
	 * This method returns the radius on the external side (toward bark) of the
	 * ring at a given height (uses interpolation between discs).
	 * 
	 * @param height_m
	 *            the height (m)
	 * @param overBark
	 *            true to have the radius over bark or false to get the inside
	 *            bark radius.
	 * @return the external radius (mm)
	 */
	public double getRadius_mm(double height_m, boolean overBark) {
		return getExtRadius_mm(height_m, overBark ? 0 : 1);
	}

	// Returns the crown ratio (no dimension)
	// (works with any units but must be homogeneous !)
	/**
	 * This method returns the proportion of the log that is contained in the
	 * crown.
	 * botHeight: the height of the bottom section of the piece (m)
	 * topHeight: the height of the top section of the piece (m)
	 * 
	 * @param crownBaseHeight
	 *            the base of the crown (m)

	 * @return the ratio of the part that is located in the crown
	 */
	public double getCrownRatio(double crownBaseHeight) {
		double ratio;
		double topHeight = getHeightOfTopSectionM();
		double botHeight = getHeightOfBottomSectionM();
		if (topHeight <= crownBaseHeight) {
			ratio = 0;
		} else if (botHeight >= crownBaseHeight) {
			ratio = 1;
		} else {
			ratio = (topHeight - crownBaseHeight) / (topHeight - botHeight);
			// ratio = (crownBaseHeight - botHeight) / (topHeight - botHeight);
		}
		return ratio;
	}


	/**
	 * A representation of the piece in a String to check its structure.
	 */
	public String toString() {
		StringBuffer b = new StringBuffer("GPiece nDiscs: " + getDiscs().size() + "...");
		for (GPieceDisc d : getDiscs()) {
			b.append("\n");
			b.append(d.toString());

		}
		return b.toString();

	}

}
