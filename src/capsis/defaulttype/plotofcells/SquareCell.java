/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.defaulttype.plotofcells;

import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Vector;

import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Tree;
import capsis.kernel.Plot;

/**
 * SquareCell is a square part of a RectangularPlot.
 * 
 * WARNING : if subclassed and subclass holds any object instance variables (not
 * primitive types), subclass must redefine "public Object clone ()" to provides
 * clonage for these objects (see RectangularPlot.clone () for template).
 * 
 * @author F. de Coligny - July 1999
 */
public class SquareCell extends TreeListCell {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a logical SquareCell.
	 * 
	 * @see Tree
	 */
	public static class Immutable extends TreeListCell.Immutable {

		// nb-27.08.2018
		// private static final long serialVersionUID = 1L;

		public int iGrid; // line# in the cell grid
		public int jGrid; // column# in the cell grid
		public Vertex3d center;
		public int holderId; // knows the square cell matrix. If 0, holder is a
								// RectangularPlot else it's a SquareCellHolderl
	}

	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	/**
	 * Constructor 1 for new logical cell. Immutable object is created.
	 */
	public SquareCell(SquareCellHolder holder, int id, int motherId, // 0 if no
																		// cell
																		// nesting
			Vertex3d origin, int iGrid, int jGrid) {

		createImmutable(); // do not redo it in subclass, just redefine
							// createImmutable () (same body)

		this.plot = holder.getPlot();
		if (holder instanceof RectangularPlot) {
			getImmutable().holderId = 0;
		} else {
			getImmutable().holderId = ((Cell) holder).getId();
		}
		immutable.id = id;

		immutable.cellsIds = null; // i.e. empty. Created when first add done
		immutable.origin = origin;

		// vertices
		immutable.vertices = new Vector<Vertex3d>();
		double w = getWidth();
		immutable.vertices.add((Vertex3d) origin.clone());
		immutable.vertices.add(new Vertex3d(origin.x, origin.y + w, origin.z));
		immutable.vertices.add(new Vertex3d(origin.x + w, origin.y + w, origin.z));
		immutable.vertices.add(new Vertex3d(origin.x + w, origin.y, origin.z));

		getImmutable().iGrid = iGrid;
		getImmutable().jGrid = jGrid;

		double halfW = getWidth();
		halfW /= 2;
		getImmutable().center = new Vertex3d(origin.x + halfW, origin.y + halfW, origin.z);

		this.trees = null; // we let it null until first use -> null=empty

		// General cell nesting scheme
		// This will put cell id in mother cell matrix -> iGrid and jGrid must
		// be set
		setMother(plot.getCell(motherId)); // cross references mother/daughter

	}

	/**
	 * Constructor 2 for new instance of existing logical cell. Immutable object is
	 * retrieved from model. May be short circuited by clone () method used.
	 */
	public SquareCell(SquareCell modelCell, // contains immutable to retrieve
			SquareCellHolder holder) {
		this.immutable = modelCell.immutable; // that's why immutable was
												// created

		this.plot = holder.getPlot();
		this.trees = null; // we let it null until first use -> null=empty
	}

	/**
	 * Constructor 3 for compatibility with mountain module.
	 */
	public SquareCell(RectangularPlot plot, int iGrid, // immutable
			int jGrid, // immutable
			double x, // immutable
			double y) { // immutable
		this(plot, plot.getId(iGrid, jGrid), 0, new Vertex3d(x, y, 0d), iGrid, jGrid);
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the
	 * hierarchy. This is called only in constructor for new logical object in
	 * superclass. If an Immutable is declared in subclass, subclass must redefine
	 * this method (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable() {
		immutable = new Immutable();
	}

	public SquareCellHolder getHolder() {

		if (getImmutable().holderId == 0) {
			return (SquareCellHolder) getPlot();
		}
		return (SquareCellHolder) getPlot().getCell(getImmutable().holderId);
	}

	/**
	 * Clone a SquareCell: first calls super.clone (), then clone the SquareCell
	 * properties. Result -> a cell without trees and without plot.
	 */
	public Object clone() {
		SquareCell c = (SquareCell) super.clone(); // calls protected Object
													// Object.clone () {}

		c.immutable = immutable;

		c.plot = null; // fc trial on 20.10.2000
		c.trees = null;

		return c;

	}

	/**
	 * Returns true if the given point is contained within this cell's boundaries.
	 * Please not the <= for lower boundaries and < for upper boundaries: a point at
	 * the limit between two cells will be contained by only one of them.
	 */
	public boolean contains(double x, double y) {
		// fc+mm+pb-23.1.2024 Used to dispatch roots in soilLayers in the regeneration
		// lib

		return getX() <= x && x < getX() + getWidth() && getY() <= y && y < getY() + getWidth();

	}

	public Shape getShape() {
		return new Rectangle.Double(getX(), getY(), getWidth(), getWidth());
	}

	public double getArea() {
		return getWidth() * getWidth();
	}

	public void setArea(double a) {
	} // unused, see getArea ()

	public double getWidth() {
		return getHolder().getCellWidth();
	}

	public int getIGrid() {
		return getImmutable().iGrid;
	}

	public int getJGrid() {
		return getImmutable().jGrid;
	}

	public double getX() {
		return immutable.origin.x;
	}

	public double getY() {
		return immutable.origin.y;
	}

	public double getZ() {
		return immutable.origin.z;
	}

	// fc - 6.11.2009 - added the setters
	public void setX(double v) {
		immutable.origin.x = v;
	}

	public void setY(double v) {
		immutable.origin.y = v;
	}

	public void setZ(double v) {
		immutable.origin.z = v;
	}

	public void setXYZ(double x, double y, double z) {
		setX(x);
		setY(y);
		setZ(z);
	}

	public double getXCenter() {
		return getImmutable().center.x;
	}

	public double getYCenter() {
		return getImmutable().center.y;
	}

	public double getZCenter() {
		return getImmutable().center.z;
	}

	/**
	 * This method may be redefined in subclass to compute zShift in some
	 * appropriate way if the plot is not flat. In torus mode, we may like to
	 * consider this cell with a shift in x and y, this method is supposed to
	 * calculate and return the matching shift in z in case the plot has a slope.
	 */
	public double getZShift(double xShift, double yShift) {
		return 0;
	}

	public String getPosition() {
		return "[" + getImmutable().iGrid + ", " + getImmutable().jGrid + "]";
	}

	/**
	 * Distance from this cell to the given cell.
	 */
	public double distance(SquareCell other) {

		double v0 = getX() - other.getX();
		double v1 = getY() - other.getY();

		return Math.sqrt(v0 * v0 + v1 * v1);
	}

	public String toString() {
		return "SquareCell_" + getPosition();
	}

	public String bigString() {
		StringBuffer sb = new StringBuffer(toString());
		sb.append(" id: ");
		sb.append(immutable.id);
		sb.append(" motherId: ");
		sb.append(immutable.motherId);
		sb.append(" origin: ");
		sb.append(immutable.origin);
		sb.append(" iGrid: ");
		sb.append(getImmutable().iGrid);
		sb.append(" jGrid: ");
		sb.append(getImmutable().jGrid);
		sb.append(" center: ");
		sb.append(getImmutable().center);
		sb.append(" width: ");
		sb.append(getWidth());
		if (this.isEmpty()) {
			sb.append(" *** NO TREES IN CELL");
		} else {
			sb.append(" Tree list (");
			for (CellTree t : trees) {
				sb.append(t.getId());
				sb.append(" ");

			}
			sb.append(")");
		}

		return sb.toString();
	}

	// fc-24.10.2017
	@Override
	public void setPlot(Plot plot) {
		this.plot = (PlotOfCells) plot;

	}

}
