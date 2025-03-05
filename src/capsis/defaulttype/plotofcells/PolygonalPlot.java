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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Tree;
import capsis.kernel.GScene;

/**
 * Interface for generic polygonal plots.
 *
 * @author F. de Coligny - march 2001
 */
public class PolygonalPlot<T extends Cell> extends PlotOfCells<T> implements Cloneable, Serializable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a logical
	 * PolygonalPlot.
	 * 
	 * @see Tree
	 */

	public static class Immutable extends PlotOfCells.Immutable {
	
		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		public Collection<Vertex3d> vertices;

	}

	@Override
	protected void createImmutable() {
		setImmutable(new Immutable());
	}

	@Override
	protected Immutable getImmutable() {
		return (Immutable) super.getImmutable();
	}

	/**
	 * Constructor for new logical plot. Immutable object is created.
	 */
	public PolygonalPlot(GScene s) {

		super(); // call createImmutable

		getImmutable().vertices = new ArrayList<Vertex3d>();

		setScene(s);
		s.setPlot(this);

	}

	/**
	 * Computes a bounding box from the nested cells.
	 */
	public void autoComputeVerticesOriginWidthAndHeight() throws Exception {
		if ((getCellsAtLevel1() == null) || (getCellsAtLevel1().isEmpty())) {
			throw new Exception("Unable to auto compute vertices for PolygonalPlot. No cells at level 1.");
		}
		Vertex3d vMin = new Vertex3d(Double.MAX_VALUE, Double.MAX_VALUE, 0d);
		Vertex3d vMax = new Vertex3d(Double.MIN_VALUE, Double.MIN_VALUE, 0d);
		for (Cell c : getCellsAtLevel1()) {

			Collection<Vertex3d> vs = c.getVertices();
			for (Vertex3d v : vs) {

				vMin.x = Math.min(vMin.x, v.x);
				vMin.y = Math.min(vMin.y, v.y);
				vMax.x = Math.max(vMax.x, v.x);
				vMax.y = Math.max(vMax.y, v.y);
			}
		}
		Vertex3d origin = new Vertex3d(vMin.x, vMin.y, 0d);
		getImmutable().vertices = new Vector<Vertex3d>();
		getImmutable().vertices.add(origin);
		getImmutable().vertices.add(new Vertex3d(vMin.x, vMax.y, 0d));
		getImmutable().vertices.add(new Vertex3d(vMax.x, vMax.y, 0d));
		getImmutable().vertices.add(new Vertex3d(vMax.x, vMin.y, 0d));
		setOrigin(origin);
		setXSize(vMax.x - vMin.x);
		setYSize(vMax.y - vMin.y);
	}

	// -------------------------------------------------------------- GPlot
	// interface
	// -------------------------------------------------------------- GPlot
	// interface
	// -------------------------------------------------------------- GPlot
	// interface
	/**
	 * From GPlot interface.
	 */
	@Override
	public void addCell(T cell) {
		cellMap.put(new Integer(cell.getId()), cell);
	}

	/**
	 * From GPlot interface.
	 */
	@Override
	public String bigString() {
		StringBuffer sb = new StringBuffer(toString());
		sb.append(" origin=");
		sb.append(getOrigin());
		sb.append(completeString());
		return sb.toString();
	}

	/**
	 * From GPlot interface. getImmutable object is retrieved from model. First
	 * calls super.clone (), then clones the Object instance variables
	 * (GCells...). Result -> a plot with no stand and empty Cells (the Cells
	 * know their plot).
	 */
	@Override
	public Object clone() {
		try {
			PolygonalPlot p = (PolygonalPlot) super.clone(); // calls protected
																// Object
																// Object.clone
																// () {}

			p.setScene(null);
			p.cellMap = new HashMap<Integer, Cell>(); // contains real refs ->
														// cloned
			p.setImmutable(getImmutable());

			for (Cell cell : getCells()) {

				Cell cellClone = (Cell) cell.clone(); // cloned cell is empty
				cellClone.setPlot(p);
				p.addCell(cellClone); // updates "cells"
			}

			return p;
		} catch (Exception exc) {
			Log.println(Log.ERROR, "PolygonalPlot.clone ()", "Error while cloning plot" + " " + exc.toString(), exc);
			return null;
		}
	}

	/**
	 * From GPlot interface.
	 */
	@Override
	public Collection<Vertex3d> getVertices() {
		return getImmutable().vertices;
	}

	/**
	 * From GPlot interface.
	 */
	@Override
	public void setVertices(Collection<Vertex3d> v) {
		getImmutable().vertices = v; // v is a Collection of Vertex3d
	}

	/**
	 * From GPlot interface. Needs origin, width & height to be set.
	 */
	@Override
	public Shape getShape() {
		GScene s = getScene();
		return new Rectangle.Double(s.getOrigin().x, s.getOrigin().y, s.getXSize(), s.getYSize());
	}

	/**
	 * From GPlot interface. Returns the cells corresponding to the given
	 * position and intended to contain the list of their trees.
	 */
	// fc-8.9.2017 CHANGED to return several cells if needed and if several
	// levels are tree level (luberon2)
	@Override
	public List<T> matchingCells(Spatialized tree) {

		// Generally one single cell
		// But several possible if several layers of cells are 'tree level'
		List<T> cells = new ArrayList<> ();
		
		java.awt.geom.Point2D.Double p2 = new java.awt.geom.Point2D.Double(tree.getX(), tree.getY());

		for (T cell : getCells()) {

			Shape sh = cell.getShape();
			if (sh.contains(p2) && cell.isElementLevel())
				cells.add (cell);

		}

		// If no cell found for the tree, Log it
		if (cells.isEmpty ())
			Log.println(Log.ERROR, "PolygonalPlot.matchingCells ()", "Error while searching cell(s) covering tree: " + tree.toString ()
					+ ", tree belongs to no Cell");
		
		return cells;
	}

	// fc-8.9.2017 ORIGINAL, was changed by version upper returning several
	// cells
	// /**
	// * From GPlot interface.
	// * Returns the cell corresponding to a position
	// */
//	 @Override
//	 public T matchingCell (Spatialized tree) {
//	
//	 java.awt.geom.Point2D.Double p2 =
//	 new java.awt.geom.Point2D.Double (tree.getX (), tree.getY ());
//	
//	 for (T cell : getCells ()) {
//	
//	 Shape sh = cell.getShape ();
//	 if (sh.contains (p2) && cell.isElementLevel()) {
//	 return cell;
//	 }
//	
//	 }
//	
//	 // We arrive here only if not found...
//	 Log.println (Log.ERROR, "PolygonalPlot.registerTree ()",
//	 "Error while registering tree "
//	 +tree.toString ()+" : tree belongs to no Cell");
//	 return null;
//	 }

}
