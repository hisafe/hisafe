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

import java.awt.Point;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.kernel.GScene;

/**
 * RectangularPlot is a rectangular area, an aggregate of Square cells.
 * 
 * WARNING : if subclassed and subclass holds any object instance variables (not
 * primitive types), subclass must redefine "public Object clone ()" to provides
 * clonage for these objects (see RectangularPlot.clone () for template).
 * 
 * @author F. de Coligny - July 1999, review - October 2017
 */
public class RectangularPlot extends PlotOfCells<SquareCell> implements SquareCellHolder, Cloneable, Serializable {

	/**
	 * This class contains getImmutable() instance variables for a logical
	 * RectangularPlot.
	 * 
	 * @see Tree
	 */
	public static class Immutable extends PlotOfCells.Immutable {

		public float cellWidth; // always use accessor which returns double
		public int nLin; // # of lines in cells matrix
		public int nCol; // # of columns
		public HashMap<Vertex3d, Integer> cellOrigin_cellId;
		// public Collection<Vertex3d> vertices;

		public int[][] cellIdMatrix; // [line][column], line in [0, nLin[, col
										// in [O, nCol[ - fc- 24.2.2003
	}

	/**
	 * Constructor for new logical plot. getImmutable() object is created. It calls
	 * initPlot function
	 */
	public RectangularPlot(GScene s, double cellWidth) {

		super(); // calls createImmutable()

		getImmutable().cellWidth = (float) cellWidth;
		getImmutable().nLin = 0;
		getImmutable().nCol = 0;
		getImmutable().cellOrigin_cellId = null;
		// getImmutable().vertices = new ArrayList<Vertex3d>();
		getImmutable().cellIdMatrix = null;

		setScene(s);

		initPlot();
	}

	// ----------------------------------------------------- Specific methods

	/**
	 * Create an getImmutable() object whose class is declared at one level of the
	 * hierarchy. This is called only in constructor for new logical object in
	 * superclass. If an getImmutable() is declared in subclass, subclass must
	 * redefine this method (same body) to create an getImmutable() defined in
	 * subclass.
	 */
	@Override
	protected void createImmutable() {
		setImmutable(new Immutable());
	}

	@Override
	protected Immutable getImmutable() {
		return (Immutable) super.getImmutable();
	}

	/**
	 * May be redefined in subclasses if different methods used. This method must
	 * compute nLin, nCol, declare cells matrix and set Plot bottom left point
	 * coordinates. Subclass' redefined method may do more.
	 */
	protected void initPlot() {

		// If cellWidth is 0, do not create the cells
		// fc - 26.11.2009 - FiPlot with or without cells (big files of FP)
		if (getCellWidth() <= 0) {
			return;
		}

		// 1. Compute # of lines and columns from stand size and cell width
		int nLin = 0;
		int nCol = 0;
		GScene s = getScene();
		nLin = (int) (s.getYSize() / getCellWidth());
		if (nLin * getCellWidth() < s.getYSize()) {
			nLin += 1;
		}
		nCol = (int) (s.getXSize() / getCellWidth());
		if (nCol * getCellWidth() < s.getXSize()) {
			nCol += 1;
		}

		// 2. Prepare a cell matrix - This sets getImmutable().nLin / nCol
		defineMatrix(nLin, nCol);

		// 3. Recompute plot origin (with bottom left origin) to center the
		// stand

		// fc-2.12.2010 - a DefaultPlot may have been built before this
		// RectangularPlot,
		// -> ask origin to the scene (instead of 0,0,0)
		Vertex3d origin = (Vertex3d) s.getOrigin().clone();

		double h = nLin * getCellWidth();
		double w = nCol * getCellWidth();
		double xDiff = w - s.getXSize();
		double yDiff = h - s.getYSize();
		origin.x = origin.x - xDiff / 2;
		origin.y = origin.y - yDiff / 2;

		setOrigin(origin);
		setXSize(w);
		setYSize(h);
	}

	/**
	 * This method creates GCells in the plot. It can be redefined in a subclass to
	 * create different Cells, ex: WizzCells, a subclass of Cell with special
	 * attributes (ex: boolean grass...).
	 */
	public void createCells() {
		for (int i = 0; i < getImmutable().nLin; i++) {
			for (int j = 0; j < getImmutable().nCol; j++) {
				// x of the bottom left corner of the Cell
				double a = getOrigin().x + j * getCellWidth();
				// y of the bottom left corner of the Cell
				double b = getOrigin().y + (getImmutable().nLin - i - 1) * getCellWidth();
				// Cells relative coordinates = line, column (i, j)
				addCell(new SquareCell(this, i, j, a, b)); // here, we create
															// SquareCell
															// instances
			}
		}
		createTableCellOrigin_CellId();
	}

	/**
	 * Builds a HashMap (cell origin point, cellId). Origin is the cell's bottom
	 * left corner.
	 */
	protected void createTableCellOrigin_CellId() {
		getImmutable().cellOrigin_cellId = new HashMap<Vertex3d, Integer>();

		for (Cell cell : getCells()) {
			TreeListCell c = (TreeListCell) cell;
			if (c.isTreeLevel()) { // fc - 25.6.2001
				getImmutable().cellOrigin_cellId.put(cell.getOrigin(), new Integer(cell.getId()));
			}
		}
	}

	/**
	 * Cell accessor from coordinates in matrix : getCell [i, j]. (Checked bc + fc :
	 * ok)
	 */
	// REMOVED fc+nb-30.1.2017
	// public SquareCell getCell(Point p) {
	// return (getCell(p.x, p.y));
	// }

	/**
	 * Translation cell [i, j] -> id. May help for automatic square cell matrix
	 * construction.
	 */
	public int getId(int i, int j) {
		if (i < 0 || i > getImmutable().nLin - 1 || j < 0 || j > getImmutable().nCol - 1) {
			Log.println(Log.WARNING, "RectangularPlot.getId ()",
					"Request for incorrect [i, j]=[" + i + ", " + j + "]" + ", i should be in [0, "
							+ (getImmutable().nLin - 1) + "] and j in [0, " + (getImmutable().nCol - 1)
							+ "], RectangularPlot is " + toString());
			return 1;
		}
		return i * getImmutable().nCol + j + 1;
	}

	/**
	 * From SquareCellHolder interface. This method is useful when the
	 * SquareCellHolder is a GCell.
	 */
	@Override
	public PlotOfCells<SquareCell> getPlot() {
		return this;
	}

	/**
	 * From SquareCellHolder interface.
	 */
	@Override
	public double getCellWidth() {
		return (double) getImmutable().cellWidth;
	}

	/**
	 * From SquareCellHolder interface.
	 */
	public void defineMatrix(int nLin, int nCol) {
		getImmutable().nLin = nLin;
		getImmutable().nCol = nCol;
		// ~ getImmutable().cellIdMatrix = new Integer [nLin][nCol];
		getImmutable().cellIdMatrix = new int[nLin][nCol]; // fc - 24.2.2003
	}

	/**
	 * From SquareCellHolder interface.
	 */
	@Override
	public int getNLin() {
		return getImmutable().nLin;
	}

	/**
	 * From SquareCellHolder interface.
	 */
	@Override
	public int getNCol() {
		return getImmutable().nCol;
	}

	/**
	 * From SquareCellHolder interface.
	 */
	@Override
	public void setCell(int i, int j, SquareCell c) {
		if (c == null) {
			Log.println(Log.ERROR, "RectangularPlot.setCell ()", "cell is null");
		}
		if (getImmutable() == null) {
			Log.println(Log.ERROR, "RectangularPlot.setCell ()", "getImmutable () is null");
		}

		// Added the test below. In PDG, the FmCells are SquareCells but they
		// should not be added
		// in the cellIdMatrix, their i and j may be set to -1 - fc-20.9.2011
		if (i >= 0 && j >= 0) { // Added
			getImmutable().cellIdMatrix[i][j] = c.getId();
		} // Added
	}

	/**
	 * From SquareCellHolder interface.
	 */
	@Override
	public SquareCell getCell(int i, int j) {
		// getAModuloB (a, b) is not equivalent to a % b (see TstModulo.java)
		int a = Tools.getAModuloB(i, getImmutable().nLin);
		int b = Tools.getAModuloB(j, getImmutable().nCol);

		// ~ return (SquareCell) getCell (getImmutable().cellIdMatrix
		// [a][b].intValue ());

		int cellId = getImmutable().cellIdMatrix[a][b];
		SquareCell res = (SquareCell) getCell(cellId);

		// fc-15.2.2021 In case of error, print details (working in PDGModel)
		if (res == null)
			System.out.println("Error in RectangularPlot.getCell (i, j), nLin: " + getImmutable().nLin + ", nCol: "
					+ getImmutable().nCol + ", i: " + i + ", j: " + j + ", a: " + a + ", b: " + b + ", found cellId: "
					+ cellId + ", returned null;");

		return res;

//		return (SquareCell) getCell(getImmutable().cellIdMatrix[a][b]);
	}

	/**
	 * From SquareCellHolder interface. Translation cell id -> [i, j].
	 */
	@Override
	public Point getIJ(int id) {
		if (id < 1 || id > getImmutable().nLin * getImmutable().nCol) {
			Log.println(Log.WARNING, "RectangularPlot.getIJ ()", "Request for incorrect id=" + id + ", should be in ["
					+ 1 + ", " + getImmutable().nLin * getImmutable().nCol + "], RectangularPlot is " + toString());
			return new Point(0, 0);
		}
		int l = (id - 1) / getImmutable().nCol;
		int c = (id - 1) % getImmutable().nCol;
		return new Point(l, c);
	}

	// @Override
	// public Collection<Vertex3d> getVertices() {
	// return getImmutable().vertices;
	// }
	//
	// @Override
	// public void setVertices(Collection<Vertex3d> v) {
	// getImmutable().vertices = v; // v is a Collection of Vertex3d
	// }

	/**
	 * From GPlot interface. Adds a cell in cells array.
	 */
	@Override
	public void addCell(SquareCell cell) {
		if (!(cell instanceof SquareCell)) {
			Log.println(Log.ERROR, "RectangularPlot.addCell (GCell)", "Cell " + cell + " should be a GCell");
			return;
		}

		// 1. add ref in map
		cellMap.put(new Integer(cell.getId()), cell);

		// 2. add id in matrix
		SquareCell c = (SquareCell) cell;
		int i = c.getIGrid();
		int j = c.getJGrid();
		setCell(i, j, c);
	}

	/**
	 * From GPlot interface. getImmutable() object is retrieved from model. First
	 * calls super.clone (), then clones the Object instance variables (GCells...).
	 * Result -> a plot with no stand and empty Cells (the Cells know their plot).
	 */
	@Override
	public Object clone() {
		try {
			RectangularPlot p = (RectangularPlot) super.clone(); // calls
																	// protected
																	// Object
																	// Object.clone
																	// () {}

			p.setImmutable(getImmutable());
			p.setScene(null);

			// Clone the cells inside and reference them in map
			p.cellMap = new HashMap<Integer, SquareCell>(); // contains real
															// refs
															// -> cloned
			for (SquareCell cell : getCells()) {
				SquareCell cellClone = (SquareCell) cell.clone(); // cloned cell
																	// is empty

				// Added the test below: PDG may return null cells (when trees
				// die, they are removed with their FmCell) fc-20.9.2011
				if (cellClone != null) { // Added
					cellClone.setPlot(p);
					p.addCell(cellClone);
				} // Added

			}

			return p;
		} catch (Exception e) {
			Log.println(Log.ERROR, "RectangularPlot.clone ()", "Error while cloning this RectangularPlot: " + this, e);
			return null;
		}
	}

	// /**
	// * From GPlot interface.
	// */
	// @Override
	// public Shape getShape() {
	// return new Rectangle.Double(getOrigin().x, getOrigin().y, getXSize(),
	// getYSize());
	// }

	/**
	 * Returns the cell corresponding to a given position.
	 * createTableBottomLeft_CellId () has been modified to consider only tree level
	 * cells.
	 */
	// fc-8.9.2017 CHANGED: several matching cells now possible in several
	// layers
	@Override
	public List<SquareCell> matchingCells(Spatialized pos) {

		if (getImmutable().cellOrigin_cellId == null) {
			createTableCellOrigin_CellId();
		}

		Spatialized t = (Spatialized) pos;

		double x = t.getX();
		double y = t.getY();

		// fc-8.9.2017
		List<SquareCell> cells = new ArrayList<>();

		SquareCell c = getCell(x, y);

		// fc-11.10.2017 added the test below to prevent adding nulls in the
		// returned list
		if (c != null)
			cells.add(c);

		return cells;
	}

	// fc-89.2017 ORIGINAL, see upper
	// /**
	// * From Plot interface. Returns the cell corresponding to a position.
	// * createTableBottomLeft_CellId () has been modified to consider only tree
	// * level cells.
	// */
	// @Override
	// public SquareCell matchingCell(Spatialized tree) {
	//
	// if (getImmutable().cellOrigin_cellId == null) {
	// createTableCellOrigin_CellId();
	// }
	//
	// Spatialized t = (Spatialized) tree;
	//
	// double x = t.getX();
	// double y = t.getY();
	//
	// return getCell(x, y);
	// }

	/**
	 * New Method fc+st-25.6.2004: return the cell containing a given position.
	 */
	@Override
	public SquareCell getCell(double x, double y) {

		// fc+jb-30.10.2020 Adapted for stricter appreciation, cell lower bound
		// included, cell upper bound excluded, except if upper bound of plot

		double plotXUpperBound = getOrigin().x + getXSize();
		double plotYUpperBound = getOrigin().y + getYSize();

		double w = getCellWidth();
		boolean found = false;

		// We use the table: cell origin -> cell logical id
		Iterator<Vertex3d> it = getImmutable().cellOrigin_cellId.keySet().iterator();
		Vertex3d p = null;
		while (it.hasNext() && !found) {
			p = (Vertex3d) it.next();

			// fc+jb-30.10.2020 Removed the upper bounds with a strict <
			// The cells are often not sorted, using a <= for the upper bound was assigning
			// a (x,y) to a cell or another depending on the order at cell iteration time
			// Kept only the <= if x or y equals max upper bound to include the (x,y) when
			// on the top or right border of the plot

			boolean includedOnX = x >= p.x && (x < p.x + w || (x == plotXUpperBound && x <= p.x + w));
			boolean includedOnY = y >= p.y && (y < p.y + w || (y == plotYUpperBound && y <= p.y + w));

			if (includedOnX && includedOnY) {
//			if ((x >= p.x) && (x < p.x + w) && (y >= p.y) && (y < p.y + w)) {
//			if ((x >= p.x) && (x <= p.x + w) && (y >= p.y) && (y <= p.y + w)) {

				found = true;
			}
		}

		if (found) {
			// cell origin -> cell id
			int id = ((Integer) getImmutable().cellOrigin_cellId.get(p)).intValue();

			// cell id -> cell
			SquareCell cell = (SquareCell) getCell(id);
			return cell;
		} else {
			return null;
		}

	}
	// fc+jb-30.10.2020 Backup before adaptation, see just upper
//	@Override
//	public SquareCell getCell(double x, double y) {
//
//		double w = getCellWidth();
//		boolean found = false;
//
//		// We use the table: cell origin -> cell logical id
//		Iterator<Vertex3d> it = getImmutable().cellOrigin_cellId.keySet().iterator();
//		Vertex3d p = null;
//		while (it.hasNext() && !found) {
//			p = (Vertex3d) it.next();
//			if ((x >= p.x) && (x <= p.x + w) && (y >= p.y) && (y <= p.y + w)) {
//				found = true;
//			}
//		}
//
//		if (found) {
//			// cell origin -> cell id
//			int id = ((Integer) getImmutable().cellOrigin_cellId.get(p)).intValue();
//
//			// cell id -> cell
//			SquareCell cell = (SquareCell) getCell(id);
//			return cell;
//		} else {
//			return null;
//		}
//
//	}

	/**
	 * fc+FDelerue-22.3.2012 - Returns the cell containing the given point
	 * *considering torus if requested*.
	 */
	public SquareCell getCell(double x, double y, boolean torusActivated) {
		if (torusActivated) {
			double xSize = getXSize();
			double ySize = getYSize();

			// fc+mj+ag-25.2.2022 Adapted for when the xMin, yMin != 0,0
			
			double xMin = this.getOrigin().x;
			double xMax = xMin + xSize;
			double yMin = this.getOrigin().y;
			double yMax = yMin + ySize;

			x = x - xMin;
//			xprime = xprime % xSize;

			if (x >= 0) {
				x = x % xSize;
			} else {
				x = xSize - (Math.abs(x) % xSize);
			}

			x = x + xMin;

			y = y - yMin;

			if (y >= 0) {
				y = y % ySize;
			} else {
				y = ySize - (Math.abs(y) % ySize);
			}

			y = y + yMin;

//			if (x >= 0) {
//				x = x % xSize;
//			} else {
//				x = xSize - (Math.abs(x) % xSize);
//			}
//
//			if (y >= 0) {
//				y = y % ySize;
//			} else {
//				y = ySize - (Math.abs(y) % ySize);
//			}

		}
		return getCell(x, y);

	}

	/**
	 * From GPlot interface.
	 */
	@Override
	public String bigString() {
		StringBuffer sb = new StringBuffer(toString());
		sb.append(" origin=");
		sb.append(getOrigin());
		sb.append(" width=");
		sb.append(getXSize());
		sb.append(" height=");
		sb.append(getYSize());
		sb.append(" nLin=");
		sb.append(getImmutable().nLin);
		sb.append(" nCol=");
		sb.append(getImmutable().nCol);
		sb.append(" cellWidth=");
		sb.append(getCellWidth());
		sb.append(" Plot contains ");
		sb.append(getCells().size());
		sb.append(" cell(s)\n");

		sb.append(completeString());
		return sb.toString();
	}

}
