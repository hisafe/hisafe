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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;

import capsis.defaulttype.Tree;
import capsis.kernel.Plot;

/**
 * An abstract superclass for the plot subclasses. A plot is a geometrical
 * description of the terrain associated to the GScene.
 * 
 * @author F. de Coligny - March 2001, September 2010
 */
public abstract class PlotOfCells<T extends Cell> extends Plot<T> implements Serializable {

	/** The Map of cells (optional) */
	protected Map<Integer, T> cellMap;

	/**
	 * Default constructor
	 */
	public PlotOfCells() {

		cellMap = new HashMap<Integer, T>();

	}

	// Trees management: connected to Scene.addTree ()
	
	/**
	 * Add the given tree in the matching plot subdivision (optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public boolean addTree(Tree tree) {

		if (tree instanceof Spatialized) {

			Spatialized spa = (Spatialized) tree;

			adaptSize(spa);

			// fc-8.9.2017 CHANGED: several matching cells are now possible (in
			// several tree level layers)
			// Associate cell to tree
			if (this.hasCells()) { // fc-26.10.2017

				TreeListCell c = tree.getCell();
				if (c != null)
					return true; // tree already knows its cell, return ok

				// Look for the cells (several tree levels now possible)
				// covering the tree
				List cells = this.matchingCells(spa); // fc-8.9.2017

				// fc-17.12.2003 if tree belongs to no cell, remove it from
				// scene (SOM bug in Alisier)
				if (cells == null || cells.isEmpty()) {
					tree.setScene(null);
					Log.println(Log.ERROR, "PlotOfCells.addTree ()", "tree " + tree.getId() + " (" + spa.getX() + ","
							+ spa.getY() + ") belongs to no cell");

					// fc-9.11.2017 this is now done in Scene.addTree () when we
					// return false
					// removeTree(tree);

					return false; // fc-17.12.2003 false: was not added in plot
									// due to trouble
				} else {
					// Register the tree in its cells
					for (Object cell : cells) {
						TreeListCell tlc = (TreeListCell) cell;
						tlc.registerTree(tree);
					}
				}

			}

		}

		return true; // no trouble
	}
	
	/**
	 * Remove the given tree from the matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void removeTree (Tree tree) {
		if (tree.getCell() != null) {
//			tree.unregisterFromPlot();
			
			tree.getCell ().unregisterTree(tree);
			
		}
	}

	/**
	 * Remove all trees from their matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void clearTrees() {
		if (this.hasCells()) {
			for (Object o : getCells()) {
				if (o instanceof TreeListCell) {
					TreeListCell tlc = (TreeListCell) o;
//					for (CellTree ct : tlc.getTrees()) {
//					tlc.unregisterTree(ct);
					// fa-16.02.2023: to avoid ConcurrentModificationException
					List trees = new ArrayList(tlc.getTrees());
					for (Iterator it = trees.iterator(); it.hasNext();){
						CellTree ct = (CellTree) it.next();
						tlc.unregisterTree(ct);
					}
				}
			}
		}
	}

	// Cells management: not in PlotOfCells (abstract here)

	abstract public void addCell(T cell); // for cells at levels!=1, this method
											// is called by GCell.addCell
											// (GCell)

	public T getCell(int id) {
		if (id == 0) {
			return null;
		}
		return cellMap.get(id);
	}

	public Collection<T> getCells() {
		return cellMap.values();
	}

	/**
	 * Returns the cells with the given ids.
	 */
	public List<T> getCells (Collection<Integer> cellIds) {
		// fc+bc-9.3.2021
		List<T> cells = new ArrayList<> ();
		for (int id : cellIds) {
			cells.add(cellMap.get(id));
		}
		return cells;
	}

	
	public boolean hasCells() {
		return !cellMap.isEmpty();
	}

	public boolean isEmpty() {
		return cellMap.isEmpty();
	}

	public Collection<T> getCellsAtLevel(int l) {
		Collection<T> v = new ArrayList<T>();
		for (T c : getCells()) {
			if (c.getLevel() == l) {
				v.add(c);
			}
		}
		return v;
	}

	public Collection<T> getCellsAtLevel1() {
		return getCellsAtLevel(1);
	}

	public Collection<T> getCellsAtLevel2() {
		return getCellsAtLevel(2);
	}

	public Collection<T> getCellsAtLevel3() {
		return getCellsAtLevel(3);
	}

	public T getFirstCell() {
		if (cellMap.isEmpty()) {
			return null;
		}
		return getCells().iterator().next();
	}

	// fc-8.9.2017 CHANGED matchingCell into matchingCells
	abstract public List<T> matchingCells(Spatialized t);

	// abstract public T matchingCell (Spatialized t);

	protected String completeString() {
		StringBuffer sb = new StringBuffer();
		sb.append("Cells details {\n");
		for (Cell cell : getCells()) {

			sb.append(cell.bigString());
			sb.append("\n");
		}
		sb.append("}\n");
		return sb.toString();
	}

	// abstract public String bigString();

}
