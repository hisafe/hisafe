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
import java.awt.geom.GeneralPath;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Tree;
import capsis.kernel.Plot;
import capsis.util.Point3D;
import capsis.util.Polygon2D;

/**
 * Interface for generic polygonal cells.
 *
 * @author F. de Coligny - march 2001
 */
public class PolygonalCell extends TreeListCell {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a logical
	 * PolygonalCell.
	 * 
	 * @see Tree
	 */
	public static class Immutable extends TreeListCell.Immutable {
	
		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;
		
		public double area; // m2
	}

	/**
	 * Constructor for new logical cell. Immutable object is created.
	 */
	public PolygonalCell(PolygonalPlot plot, int id, int motherId, Vertex3d origin, Collection<Vertex3d> vertices, // a
																													// Collection
																													// of
																													// Vertex3d
			double area) {

		createImmutable();

		this.plot = plot;
		immutable.id = id;

		// General cell nesting scheme
		setMother(plot.getCell(motherId)); // cross references mother/daughter

		immutable.cellsIds = null; // i.e. empty. Created when first add done
		immutable.origin = origin;
		immutable.vertices = vertices;
		if (area > 0d) {
			getImmutable().area = (float) area;
		} else {
			try {
				getImmutable().area = new Polygon2D(vertices).getArea();
			} catch (Exception e) {
				getImmutable().area = 0d;
			}
		}

		this.trees = null;

	}

	/**
	 * Constructor for new instance of existing logical cell. Immutable object
	 * is retrieved from model.
	 */
	public PolygonalCell(PolygonalCell modelCell, // contains immutable to
													// retrieve
			PolygonalPlot plot) {
		this.immutable = modelCell.immutable;

		this.plot = plot;
		this.trees = null;
	}

	// To be redefined in subclasses to create a subclass of this immutable.
	protected void createImmutable() {
		immutable = new Immutable();
	}

	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	/**
	 * Clone a PolygonalCell: first calls super.clone (), then clone the
	 * PolygonalCell properties. Result -> a cell without trees and without
	 * plot.
	 */
	public Object clone() {
		PolygonalCell c = (PolygonalCell) super.clone(); // calls protected
															// Object
															// Object.clone ()
															// {}

		c.immutable = immutable;
		c.plot = null;
		c.trees = null;
		return c;

	}

	/**
	 * Computes a bounding box of the nested cells.
	 */
	public void autoComputeVerticesAreaAndOrigin() throws Exception {
		if ((getCells() == null) || (getCells().isEmpty())) {
			throw new Exception("Cell with id=" + getId() + ", unable to auto compute vertices. No nested cells.");
		}
		Vertex3d vMin = new Vertex3d(Double.MAX_VALUE, Double.MAX_VALUE, 0d);
		Vertex3d vMax = new Vertex3d(Double.MIN_VALUE, Double.MIN_VALUE, 0d);
		for (Iterator i = getCells().iterator(); i.hasNext();) {
			Cell c = (Cell) i.next();
			Collection<Vertex3d> vs = c.getVertices();

			// ~ for (Iterator j = vs.iterator (); j.hasNext ();) { // fc + phd
			// - 28.11.2007
			// ~ Vertex3d v = (Vertex3d) j.next (); // fc + phd - 28.11.2007

			// fc + phd - 28.11.2007
			Vertex3d v = null;
			for (Iterator j = vs.iterator(); j.hasNext();) {
				Object o = j.next();
				if (o instanceof Vertex3d) {
					v = (Vertex3d) o;
				} else if (o instanceof Vertex2d) {
					v = Vertex3d.convert((Vertex2d) o);
				} else {
					throw new Exception("PolygonalCell.autoComputeVerticesAreaAndOrigin (), "
							+ "vertex with wrong type: " + o.getClass().getName());
				}
				// fc + phd - 28.11.2007

				vMin.x = Math.min(vMin.x, v.x);
				vMin.y = Math.min(vMin.y, v.y);
				vMax.x = Math.max(vMax.x, v.x);
				vMax.y = Math.max(vMax.y, v.y);
			}
		}
		immutable.vertices = new Vector<Vertex3d>();
		immutable.origin = new Vertex3d(vMin.x, vMin.y, 0d);
		immutable.vertices.add(immutable.origin);
		immutable.vertices.add(new Vertex3d(vMin.x, vMax.y, 0d));
		immutable.vertices.add(new Vertex3d(vMax.x, vMax.y, 0d));
		immutable.vertices.add(new Vertex3d(vMax.x, vMin.y, 0d));
		try {
			getImmutable().area = new Polygon2D(immutable.vertices).getArea();
		} catch (Exception e) {
		} // if trouble -> area is unchanged
	}

	/**
	 * From GCell interface.
	 */
	public double getArea() {
		return getImmutable().area;
	} // fc - 23.11.2001 - lacked

	/**
	 * From GCell interface.
	 */
	public void setArea(double a) {
		getImmutable().area = a;
	}

	/**
	 * From GCell interface.
	 */
	public Shape getShape() {
		Collection<Vertex3d> vs = getVertices();

		int n = vs.size();

		double[] xs = new double[n];
		double[] ys = new double[n];

		int j = 0;
		for (Iterator i = vs.iterator(); i.hasNext();) {
			Object o = i.next();
			if (o instanceof Point3D.Double) {
				Point3D.Double p3 = (Point3D.Double) o;
				xs[j] = p3.x;
				ys[j] = p3.y;
			} else if (o instanceof Vertex2d) { // fc + hd - 29.3.2006
				Vertex2d v2 = (Vertex2d) o;
				xs[j] = v2.x;
				ys[j] = v2.y;
			} else if (o instanceof Vertex3d) {
				Vertex3d v3 = (Vertex3d) o;
				xs[j] = v3.x;
				ys[j] = v3.y;
			} else {
				Log.println(Log.ERROR, "PolygonalCell.getShape ()", "Cell with id=" + getId()
						+ " has vertices with wrong type. Vertices=" + getVertices());
			}
			j++;
		}
		GeneralPath p2 = Polygon2D.getGeneralPath(xs, ys, n); // fc - 23.11.2001
																// (changed from
																// Tools.getPolygon2D
																// ())

		return p2;
	}

	/**
	 * From GCell interface.
	 */
	public String toString() {
		return "PolygonalCell_" + getId();
	}

	/**
	 * From GCell interface.
	 */
	public String bigString() {
		StringBuffer sb = new StringBuffer(toString());
		sb.append(" id=");
		sb.append(immutable.id);
		sb.append(" plot=");
		sb.append(plot);
		sb.append(" origin=");
		sb.append(immutable.origin);
		sb.append(" area=");
		sb.append(getImmutable().area);
		sb.append(" trees=");
		sb.append(trees);
		sb.append(" vertices=");
		sb.append(immutable.vertices);

		return sb.toString();
	}

	// Needed for Presage integration, unused elsewhere and not yet implemented
	// here

	public SquareCell getCell(double x, double y) {
		return null;
	}

	// fc-24.10.2017
	@Override
	public void setPlot(Plot plot) {
		this.plot = (PlotOfCells) plot;
		
	}

}
