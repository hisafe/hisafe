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

package capsis.kernel;

import java.awt.Rectangle;
import java.awt.Shape;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Tree;

/**
 * A plot is a geometric description of the terrain associated to the GScene. It
 * has an origin and a bounding rectangle and may be divided in
 * plotSubdivisions.
 *
 * @author F. de Coligny - March 2001, September 2010, October 2017
 * @see DefaultPlot, PlotOfCells, PlotOfPixels
 */
public class Plot<T extends PlotSubdivision> implements Cloneable, Serializable {

	/**
	 * This inner class describes the properties that do not change when considering
	 * several instances of PlotOfCells in the Project time history. E.g. The plot
	 * at time 0 and 10 years both have the same area. This was introduced to save
	 * memory space.
	 */
	public static class Immutable implements Cloneable, Serializable {

		public Vertex3d origin; // origin: point at the bottom left corner of
								// the bounding rectangle from top view
		public double xSize;
		public double ySize;
		public double area; // m2
		public Collection<Vertex3d> vertices;
	}

	/** The reference to the immutable part of the Plot */
	protected Immutable immutable;

	protected boolean autoSize = true;

	private GScene scene;

	/**
	 * Constructor
	 */
	public Plot() {
		createImmutable();

		getImmutable().vertices = new ArrayList<Vertex3d>();

	}

	/**
	 * Add the given tree in the matching plot subdivision(s) (optional). Returns
	 * false if the tree does not belong to any subdivision and causes trouble.
	 */
	// fc-9.11.2017 refactored trees management in plots
	public boolean addTree(Tree tree) {
		// May be redefined to register the tree in some plot subdivision
		return true; // ok
	}

	/**
	 * Remove the given tree from the matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void removeTree(Tree tree) {
		// May be redefined to register the tree in some plot subdivision
	}

	/**
	 * Remove all trees from their matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void clearTrees() {
		// May be redefined to register the tree in some plot subdivision
	}

	// Immutable part management

	/**
	 * Creates an Immutable object whose class is declared at one level of the
	 * hierarchy. This is -called- only in the constructor of the object at the top
	 * of the hierarchy. If the Immutable is overriden in a subclass, the subclass
	 * must -override- this method (same body) to create an Immutable defined in
	 * this subclass.
	 */
	protected void createImmutable() {
		immutable = new Immutable();
	}

	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	protected void setImmutable(Immutable i) {
		immutable = i;
	}

	// Link to the scene

	public GScene getScene() {
		return scene;
	}

	public void setScene(GScene scene) {
		this.scene = scene;

	}

	// Simple geometry

	public Vertex3d getOrigin() {

		if (immutable.origin != null) {
			return immutable.origin;
		} else {
			return new Vertex3d(0, 0, 0); // fc - 21.9.2006 - SVLollypop /
											// Spatializers need an origin even
											// for non spatialized models (ex:
											// PP3)
		}
	}

	public void setOrigin(Vertex3d origin) {
		immutable.origin = origin;
	}

	public void setXSize(double w) {
		immutable.xSize = (float) w;
	}

	public void setYSize(double h) {
		immutable.ySize = (float) h;
	}

	public double getXSize() {
		if (immutable.xSize >= 0) {
			return immutable.xSize;
		} else {
			double sqrt = Math.sqrt(getArea());
			if (sqrt != Double.NaN)
				immutable.xSize = sqrt;
			return sqrt;
		}
	}

	public double getYSize() {
		if (immutable.ySize >= 0) {
			return immutable.ySize;
		} else {
			double sqrt = Math.sqrt(getArea());
			if (sqrt != Double.NaN)
				immutable.ySize = sqrt;
			return sqrt;
		}
	}

	public double getArea() {
		if (immutable.area > 0) {
			return immutable.area;
		} else if (immutable.xSize > 0 && immutable.ySize > 0) {
			return immutable.xSize * immutable.ySize;
		} else {
			return -1; // not set, could not be computed
		}
	}

	public void setArea(double a) {
		immutable.area = a;
	}

	public Collection<Vertex3d> getVertices() {
		return getImmutable().vertices;
	}

	public void setVertices(Collection<Vertex3d> v) {
		getImmutable().vertices = v;
	}

	public Shape getShape() {
		return new Rectangle.Double(getOrigin().x, getOrigin().y, getXSize(), getYSize());
	}

	/**
	 * Updates the plot size to include the given (x, y) point. Only if autoSize is
	 * true.
	 */
	public void adaptSize(Spatialized t) {

		if (!autoSize) {

			// fc-10.5.2022 If not autoSize and the given tree is not in the plot
			// boundaries, tell it
			// These RuntimeExceptions are interesting during debugging and could be turned
			// into Log messages
			try {
				if (t.getX() < immutable.origin.x || t.getX() > immutable.origin.x + getXSize()
						|| t.getY() < immutable.origin.y || t.getY() > immutable.origin.y + getYSize())
					throw new RuntimeException("Plot.adaptSize () Error, autoSize: " + autoSize
							+ ", attempt to add a spatialized object out of the plot boundaries. origin: "
							+ immutable.origin + ", xSize: " + getXSize() + ", ySize: " + getYSize()
							+ ", spatialized object: " + t);
			} catch (Exception e) {
				throw new RuntimeException("Plot.adaptSize () Error, autoSize: " + autoSize
						+ ", error while checking if the spatialized object fits in the plot boundaries, spatialized object: "
						+ t);
			}

			return;
		}

		if (immutable.origin == null) {
			setOrigin(new Vertex3d(t.getX(), t.getY(), t.getZ()));
			setXSize(0);
			setYSize(0);
		}

		double x = t.getX();
		double y = t.getY();

		double upright_x = immutable.origin.x + getXSize();
		double upright_y = immutable.origin.y + getYSize();

		if (x < immutable.origin.x)
			immutable.origin.x = x;
		if (y < immutable.origin.y)
			immutable.origin.y = y;
		if (x > upright_x)
			upright_x = x;
		if (y > upright_y)
			upright_y = y;

		immutable.xSize = upright_x - immutable.origin.x;
		immutable.ySize = upright_y - immutable.origin.y;

	}

	// Cloning

	public Object clone() {
		try {
			Plot<T> s = (Plot<T>) super.clone();

			s.immutable = immutable; // same immutable part

			return s;

		} catch (Exception e) {
			Log.println(Log.ERROR, "Plot.clone ()", "Exception in clone ()", e);
			return null;
		}

	}

	// fc-10.5.2022
	public void setAutoSize(boolean autoSize) {
		this.autoSize = autoSize;
	}
	
	// fc-10.5.2022
	public boolean isAutoSize() {
		return autoSize;
	}
	
	// String descriptions

	public String toString() {
		StringBuffer b = new StringBuffer();
		b.append(this.getClass().getName());
		if (scene != null) {
			b.append(" (");
			b.append(scene.toString());
			b.append(")");
		}
		return b.toString(); // light version
	}

	public String bigString() {
		return toString(); // might be redefined to send more details
	}

}
