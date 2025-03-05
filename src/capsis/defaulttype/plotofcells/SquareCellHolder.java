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

/**
 * A manager for a rectangle of SquareCells (see SquareCell).
 * The holder manages a matrix. Cells can be added with setCell (i, j, cell)
 * and accessed by getCell (i, j).
 * Matrix size is nLin * nCol. Holder can return reference to the plot.
 * Note: if a plot is a holder , it must return a reference on itself.
 * All cells have same width, getCellWidth (). 
 * 
 * @author F. de Coligny - november 2001
 */
public interface SquareCellHolder {

	/**
	 * SquareCellHolder is either a Plot or a Cell. In this last case, it can return its plot.
	 */
	public PlotOfCells getPlot ();

	/**
	 * The cell width for all SquareCells managed by this holder.
	 */
	public double getCellWidth ();
	
	/**
	 * Define matrix.
	 */
	public void defineMatrix (int nLin, int nCol);
	
	/**
	 * Number of lines of the cell matrix.
	 */
	public int getNLin ();

	/**
	 * Number of columns of the cell matrix.
	 */
	public int getNCol ();
	
	/**
	 * Set a cell in matrix.
	 */
	public void setCell (int i, int j, SquareCell c);

	/**
	 * Cell accessor from coordinates in matrix : getCell [i, j].
	 */
	public SquareCell getCell (int i, int j);

	/**
	 * Cell accessor for the cell containing the given point.
	 */
	public SquareCell getCell (double x, double y);		// fc - 28.6.2004

	/**
	 * Translation cell id -> [i, j].
	 */
	public Point getIJ (int id);
	

}