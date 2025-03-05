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
package capsis.defaulttype;

import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.plotofcells.Cell;
import capsis.kernel.Plot;

/**
 * Default implementation of a plot (without cells).
 * 
 * @author S. Dufour-Kowalski - February 2009, Reviewed by F. de Coligny, November 2017
 */
public class DefaultPlot<T extends Cell> extends Plot<T> implements Cloneable {

	// fc-29.11.2017 Changed superclass of DefaultPlot from PlotOfCells to Plot

	private Shape shape;

	/**
	 * Redefinition of addTree: we do not keep the trees in a DefaultPlot, but
	 * we keep the plot size adaptation when trees are added, resulting in
	 * origin, xSize and ySize update.
	 */
	// fc-29.11.2017 update plot size
	public boolean addTree(Tree tree) {

		if (tree instanceof Spatialized) {
			Spatialized spa = (Spatialized) tree;
			adaptSize(spa);

		}

		return true;
	}

	/**
	 * Returns a rectangle based on basic plot geometry
	 */
	@Override
	public Shape getShape() {
		if (shape == null) {
			double x = getOrigin().x;
			double y = getOrigin().y;
			double w = getXSize();
			double h = getYSize();
			shape = new Rectangle2D.Double(x, y, w, h);
		}
		return shape;
	}

	@Override
	public String bigString() {
		return "DefaultPlot";
	}

	// @Override
	// /** Return an empty list */
	// public Collection<Vertex3d> getVertices() {
	// return new ArrayList<Vertex3d>();
	// }
	//
	// @Override
	// /** Do Nothing */
	// public void setVertices(Collection<Vertex3d> c) {
	// }

}

// fc-29.11.2017 FORMER implementation backup
// Changed superclass of DefaultPlot from PlotOfCells to Plot
// (Errors in SVSimple, wrong type hierarchy, was not logical)...

// public class DefaultPlot<T extends Cell> extends PlotOfCells<T> implements
// Cloneable {
//
// private Shape shape;
//
// /** Unused in Default plot */
// @Override
// public void addCell(T cell) {
// }
//
// /** Unused in Default plot */
// @Override
// public List<T> matchingCells(Spatialized t) {
// return null;
// }
//
// @Override
// public String bigString() {
// return "DefaultPlot";
// }
//
// /** Return a rectangle based on basic plot geometry */
// @Override
// public Shape getShape() {
// if (shape == null) {
// double x = getOrigin().x;
// double y = getOrigin().y;
// double w = getXSize();
// double h = getYSize();
// shape = new Rectangle2D.Double(x, y, w, h);
// }
// return shape;
// }
//
// @Override
// /** Return an empty list */
// public Collection<Vertex3d> getVertices() {
// return new ArrayList<Vertex3d>();
// }
//
// @Override
// /** Do Nothing */
// public void setVertices(Collection<Vertex3d> c) {
// }
//
// }

