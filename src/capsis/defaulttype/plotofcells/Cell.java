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

import java.util.Collection;

import capsis.defaulttype.SceneConnected;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.PlotSubdivision;
import jeeb.lib.util.Log;

/**
 * Interface for plot cells.
 *
 * Plot and Cells are a static infrastructure: all the cells are built on the
 * first Step of the project and cells can never be removed. At each Step, the
 * cells are new instances with the same ids and may have properties changing in
 * time.
 *
 * Cells that can be nested in several cell levels. A mother Cell may be divided
 * into nested Cells at a lower level. If elements (e.g. trees) are registered
 * in the Cells, only one level is the element level and the elements are
 * registered at this unique level.
 * 
 * author F. de Coligny - March 2001 / June 2001, September 2010, October 2017
 * 
 * @see Plot
 * @see PlotOfCells
 */
public abstract class Cell implements PlotSubdivision, SceneConnected {
	
	protected PlotOfCells plot;

	// The plot this cell is part of

	public PlotOfCells getPlot() {
		return plot;
	}

	public void setPlot(PlotOfCells plot) {
		this.plot = plot;
	}

	
	// fc7.6.2018 added SceneConnected (through the plot)
	@Override
	public GScene getScene() {
		return plot == null ? null : plot.getScene();
	}
	
	
	// Nested Cells management

	/**
	 * Mother is the Cell containing this Cell
	 */
	public abstract Cell getMother();

	public abstract void setMother(Cell cell);

	/**
	 * Returns true if this cell contains nested Cells, i.e. is their mother
	 */
	public abstract boolean isMother();

	/**
	 * Returns the list of Cells nested in this Cell
	 */
	public abstract Collection<Cell> getCells();

	/**
	 * Add a nested Cell
	 */
	public abstract void addCell(Cell cell);

	/**
	 * Returns the nesting level. If one level only, this is level 1.
	 */
	public abstract int getLevel();

	/**
	 * Cells may contain elements (e.g. trees located within their shape). It is
	 * possible only at one level of nesting: the element level.
	 */
	public abstract boolean isElementLevel();

	// Cloning cells in the history (Cell 1 has different instances at date
	// 0, 1, ... n).

	/**
	 * Clone function
	 */
	public Object clone() {
		try {
			return super.clone();

		} catch (Exception e) {
			Log.println(Log.ERROR, "Cell.clone ()", "Could not clone this cell", e);
			return null;
		}
	}

}
