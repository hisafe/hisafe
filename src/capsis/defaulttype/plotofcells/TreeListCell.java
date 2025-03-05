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

import java.awt.Shape;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import capsis.defaulttype.TreeList;
import capsis.util.group.GroupableElement;
import capsis.util.group.GroupableType;
import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;

/**
 * TreListCell is a cell containing a list of trees.
 * 
 * @author S. Dufour Kowalski - February 2009
 */
public abstract class TreeListCell extends Cell implements Cloneable, Serializable, GroupableElement {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected Immutable immutable;
//	protected PlotOfCells plot;
	protected List<CellTree> trees;

	/**
	 * Fields which values do not change in time
	 */
	public static class Immutable implements Cloneable, Serializable {

		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		public int id;
		public int motherId;
		public Collection<Integer> cellsIds;
		public Vertex3d origin;
		public Collection<Vertex3d> vertices;

	}
//
//	public PlotOfCells getPlot() {
//		return plot;
//	}
//
//	public void setPlot(PlotOfCells plot) {
//		this.plot = plot;
//	}

	/**
	 * Returns the type of the groupable element.
	 */
	public GroupableType getGroupableType() {
		// fc-1.7.2019 square and polygonal cells
		return TreeList.GROUP_PLOT_CELL;
	}

	/** 
	 * Returns the cell level 
	 */
	public int getLevel() {
		Cell c = this;
		int i = 0;
		do {
			i += 1;
		} while ((c = c.getMother()) != null);
		return i;
	}

	/** 
	 * Registers a tree only if the cell is tree level 
	 */
	public void registerTree(CellTree tree) {

		if (!isTreeLevel())
			return;

		if (trees == null)
			trees = new ArrayList<CellTree>();

		// To avoid duplicate registering fc-25.6.2012 (Samsara2 in some cases:
		// tree was registered twice in the cell)
		if (trees.contains(tree))
			return;

		trees.add(tree);
		tree.setCell(this);
	}

	public void unregisterTree(CellTree tree) {
		if (trees == null) {
			return;
		}

		trees.remove(tree);
		tree.setCell(null);

	}

	public Collection<CellTree> getTrees() {
		if (trees == null)
			return new ArrayList<CellTree>();

		return trees;
	}

	/** May be redefined to return false if needed */
	public boolean isTreeLevel() {
		return true;
	}

	public boolean isElementLevel() {
		return isTreeLevel();
	}

	public int getTreeNumber() {
		if (trees == null)
			return 0;

		return trees.size();
	}

	public boolean isEmpty() {
		return (trees == null || (trees != null && trees.isEmpty()));

	}

	abstract public String bigString();

	abstract public double getArea();

	abstract public Shape getShape();

	abstract public void setArea(double a);

	/** 
	 * Clone function
	 */
	public Object clone() {
		try {
			return super.clone();
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "TreeListCell.clone ()", "Could not clone this cell", e);
			return null;
		}
	}

	public int getId() {
		return immutable.id;
	}

	public Vertex3d getOrigin() {
		return immutable.origin;
	}

	public void setOrigin(Vertex3d v) {
		immutable.origin = v;
	}

	public Collection<Vertex3d> getVertices() {
		return immutable.vertices;
	}

	public void setVertices(Collection<Vertex3d> v) {
		immutable.vertices = v;
	}

	public Collection<Cell> getCells() {
		if (immutable.cellsIds == null)
			return null;

		Collection<Cell> cells = new ArrayList<Cell>();
		for (Iterator<Integer> i = immutable.cellsIds.iterator(); i.hasNext();) {
			
			int id = (i.next()).intValue();
			Cell cell = (Cell) plot.getCell(id);
			cells.add (cell);
			
			// fc-24.10.2017 original line
//			cells.add(plot.getCell((i.next()).intValue()));
		}
		
		return cells;
	}

	public void addCell(Cell c) {
		try {
			getPlot().addCell((TreeListCell) c);
		} catch (Exception e) {
			Log.println(Log.WARNING, "TreeListCell.addCell ()",
					"Added a null cell in the cell, please check cells construction and nesting", e);
		}

		if (immutable.cellsIds == null)
			// no duplicates
			immutable.cellsIds = new HashSet<Integer>();

		immutable.cellsIds.add(new Integer(c.getId()));

		if (this instanceof SquareCellHolder) {
			SquareCell s = (SquareCell) c;

			int i = s.getIGrid();
			int j = s.getJGrid();

			((SquareCellHolder) this).setCell(i, j, s);
		}
		
	}

	public Cell getMother() {
		return (Cell) plot.getCell(immutable.motherId);
	}

	public void setMother(Cell c) {
		if (c != null) {
			immutable.motherId = c.getId(); // motherId != 0
			c.addCell(this);
		} else {
			getPlot().addCell(this); // was made in c.addCell (this); if c!=null
		}
	}

	public boolean isMother() {
		return (immutable.cellsIds != null) && (!immutable.cellsIds.isEmpty());
	}

}
