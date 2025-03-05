/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.defaulttype.plotofcells;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import capsis.defaulttype.ShiftItem;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex2d;

/**
 * RoundMask describes a neighbourhood around a point in a square cell. NOTE:
 * The SquareCells must contain trees instanceof Spatialized.
 * 
 * <pre>
 * 
 * RoundMask m10 = new RoundMask(squareCellHolder, 10, true); // dist=10 m.
 * Collection c1 = m10.getTreesNear(tree1);
 * Collection c2 = m10.getTreesNear(tree2);
 * </pre>
 * 
 * @author F. de Coligny - february 2003 - review 10.10.2008
 */
public class RoundMask extends NeighbourhoodMask {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	public static final byte PARTIALLY_INCLUDED = 0; // fc - 8.10.2008
	public static final byte CENTER_INCLUDED = 1; // fc - 8.10.2008
	public static final byte COMPLETELY_INCLUDED = 2; // fc - 8.10.2008

	private double radius; // meters
	private double cellWidth; // meters
	// fc - 25.11.2008 - see NeighbourhoodMask - private boolean torusEnabled;
	private SquareCellHolder cellHolder;

	private Map treeLocationsWithinTorus; // fc - 9.12.2008
	private Map neighbourDistances;

	// Hourly angle (0 at 12 o'clock, clockwise) in degrees
	// fc+ag+fc-11.2.2022, reviewed, bug fixed, doc added
	// Key: neighbour tree, value: angle_deg
	private Map neightbourAngles; // fc+mj-21.11.2013

	private Map cellDistances; // fc - 8.10.2008

	private Collection<Neighbour> neighbours; // Neighbour = a cell + a shift related to torus

	private Collection<ShiftedSpatializedTree> neighboursWithinThorus; // gl 26/05/2014

	/**
	 * Constructor. Holder is a RectangularPlot or a cell containing SquareCells.
	 * NOTE: The SquareCells must contain trees instanceof Spatialized. The given
	 * radius is used to compute distance to the target tree in getTreesNear () /
	 * getCellsNear methods. If torus is enabled, the scene is considered as a
	 * torus.
	 */
	public RoundMask(SquareCellHolder holder, double radius, boolean torusEnabled) { // holder : a
																						// RectangularPlot
																						// or a
																						// PolygonalCell
																						// containing
																						// square
																						// cells
		super();

		this.radius = radius; // meters
		this.cellWidth = holder.getCellWidth(); // meters

		setTorusEnabled(torusEnabled);

		cellHolder = holder; // fc - 28.6.2004
		treeLocationsWithinTorus = new HashMap();
		neighbourDistances = new HashMap();
		neightbourAngles = new HashMap();
		cellDistances = new HashMap();

		neighboursWithinThorus = new ArrayList<ShiftedSpatializedTree>(); // gl 26/02/2014

		// Compute width in cells around the target cell
		// according to radius and cell width
		double w = radius / cellWidth;
		int width = (int) w;
		if (width < w) {
			width++;
		}

		// Build a square site collection, getTreesNear and getCellsNear will rely on it
		for (int i = -width; i <= width; i++) {
			for (int j = -width; j <= width; j++) {
				addSite(new Point(i, j));
			}
		}

	}

	/**
	 * Returns the area of the round mask, m2
	 */
	public double getArea() {
		return Math.PI * radius * radius; // m2
	}

	/**
	 * Return a Collection of trees which are close enough to the given tree
	 * according to radius.
	 */
	public Collection getTreesNear(Spatialized tree) {
		double x0 = tree.getX();
		double y0 = tree.getY();
		return getTreesNear(x0, y0);
	}

	/**
	 * Return the trees in the neighbourhood if the mask is centered on (x, y).
	 */
	public Collection getTreesNear(double x, double y) {
		boolean includingXYTree = false; // compatibility with former version
		return getTreesNear(x, y, includingXYTree); // gl - changed from getTreesNear (x, y) - 26/02/2014
	}

	/**
	 * Return the trees in the neighbourhood if the mask is centered on (x, y). If a
	 * tree is at this accurate location and includingXYTree is false, it is not
	 * added in the neighbourhood.
	 */
	public Collection getTreesNear(double x, double y, boolean includingXYTree) {
		Collection trees = new ArrayList();
		treeLocationsWithinTorus.clear();
		neighbourDistances.clear();
		neightbourAngles.clear();

		neighboursWithinThorus.clear(); // gl 26/02/2014

		try {
			SquareCell cell = (SquareCell) cellHolder.getCell(x, y); // ok in RectangularPlot

			boolean onlyCellsWithTreesInside = true;
			neighbours = getNeighbours(cell, onlyCellsWithTreesInside);

			for (Neighbour n : neighbours) {
				SquareCell ccell = (SquareCell) n.cell;

				for (Iterator k = ccell.getTrees().iterator(); k.hasNext();) {
					Spatialized t = (Spatialized) k.next();

					// fc-26.2.2014 added '&& !includingXYTree'
					if (t.getX() == x && t.getY() == y && !includingXYTree) {
						continue; // ignore a tree accurately located in (x, y)
					}

					double x1 = t.getX() + n.shift.x; // x & y shift are related to torus
					double y1 = t.getY() + n.shift.y;

					double distance = Math.sqrt(Math.pow(x1 - x, 2) + Math.pow(y1 - y, 2));
					if (distance <= radius) {
						trees.add(t);
						treeLocationsWithinTorus.put(t, new Vertex2d(x1, y1));
						neighbourDistances.put(t, new Double(distance));

						// store all Neighbours including those added by the use of the torus with
						// shifted coordinates (gl 27/02/2014)
						ShiftedSpatializedTree shiftedNeighbour = new ShiftedSpatializedTree(t, new Vertex2d(x1, y1));
						neighboursWithinThorus.add(shiftedNeighbour);

						// fc+ag+fa-11.2.2022
						double angle_deg = calcAngle_deg(x, y, x1, y1);

						neightbourAngles.put(t, angle_deg);

					}
				}
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "RoundMask.getTreesNear ()", "Exception, returned no trees", e);
		}

		return trees;
	}

	/**
	 * Hourly angle (0 at 12 o'clock, clockwise) in degrees
	 */
	static private double calcAngle_deg(double x, double y, double x1, double y1) {

		double diffX = x1 - x;
		double diffY = y1 - y;

		// The returned angle is in [0,360]
//		double angle = Math.atan2(diffX, diffY); // params in wrong order !
		double angle = Math.atan2(diffY, diffX); // fc+ag+fa-11.2.2022
		double angle_deg = Math.toDegrees(angle);
		if (angle_deg < 0)
			angle_deg += 360;

		double a = 360 - angle_deg;
		a += 90;
		a = a % 360;

		angle_deg = a;

//		System.out.println(
//				"RoundMask calcAngle_deg x,y: " + x + "," + y + ", x1,y1: " + x1 + "," + y1 + " angle: " + angle_deg);

		return angle_deg;
	}

	public static void main(String[] args) {
		calcAngle_deg(0, 0, 1, 0); // 90
		calcAngle_deg(0, 0, 1, 1); // 45
		calcAngle_deg(0, 0, 0, 1); // 0
		calcAngle_deg(0, 0, 2, 1); // 60 env.

		calcAngle_deg(0, 0, -1, 0); // 270
		calcAngle_deg(0, 0, 0, -1); // 180
//		calcAngle_deg(0, 0, 0, 0); // 90

		System.out.println("RoundMask 1 % 4: "+(1 % 4)); // 1
		System.out.println("RoundMask 6 % 4: "+(6 % 4)); // 2
		System.out.println("RoundMask -1 % 4: "+(-1 % 4)); // 1
		System.out.println("RoundMask -5 % 4: "+(-5 % 4)); // 3
		
		
		
		
//		calcAngle_deg(0, 0, 1, 0); // 0
//		calcAngle_deg(0, 0, 1, 1); // 45
//		calcAngle_deg(0, 0, 0, 1); // 90
//		calcAngle_deg(0, 0, 2, 1); // 26.56
//
//		calcAngle_deg(0, 0, -1, 0); // 180
//		calcAngle_deg(0, 0, 0, -1); // 270
//		calcAngle_deg(0, 0, 0, 0); // 90

	}

	/**
	 * Return a Collection of cells which are "close enough" (see criterion) to the
	 * given cell according to the mask radius.
	 */
	public Collection<SquareCell> getCellsNear(SquareCell cell, byte criterion) {
		double x0 = cell.getXCenter();
		double y0 = cell.getYCenter();
		return getCellsNear(x0, y0, criterion);
	}

	/**
	 * Returns the cells "near" the given position. Criterion is either
	 * PARTIALLY_INCLUDED, CENTER_INCLUDED, or COMPLETELY_INCLUDED. If
	 * PARTIALLY_INCLUDED, the cells are returned if they intersect the mask circle
	 * (approximatively: few extra cells may be returned with this option), if
	 * CENTER_INCLUDED, the cells are returned if their center is included in the
	 * mask circle (accurate), if COMPLETELY_INCLUDED, the cells are returned if
	 * they are completely in the mask circle (accurate).
	 */
	public Collection<SquareCell> getCellsNear(double x, double y, byte criterion) { // fc -
																						// 8.10.2008
		Collection<SquareCell> ccells = new ArrayList<SquareCell>();
		cellDistances.clear();
		try {
			SquareCell cell = (SquareCell) cellHolder.getCell(x, y); // ok in RectangularPlot

			boolean onlyCellsWithTreesInside = false;
			neighbours = getNeighbours(cell, onlyCellsWithTreesInside);

			for (Neighbour n : neighbours) {
				SquareCell ccell = (SquareCell) n.cell;

				// x & y shift are related to torus
				double shiftX = 0d;
				double shiftY = 0d;
				if (n.shift != null) {
					shiftX = n.shift.x;
					shiftY = n.shift.y;
				}

				// ~ double x1 = ccell.getXCenter ()+n.shift.x;
				// ~ double y1 = ccell.getYCenter ()+n.shift.y;
				double x1 = ccell.getXCenter() + shiftX;
				double y1 = ccell.getYCenter() + shiftY;

				double distanceToCenter = Math.sqrt(Math.pow(x1 - x, 2) + Math.pow(y1 - y, 2));
				double halfDiagonal = Math.sqrt(2) * ccell.getWidth() / 2;

				// For PARTIALLY_INCLUDED, this distance works better (few extra cells may be
				// included)
				double distanceMin = distanceToCenter - halfDiagonal;

				// For COMPLETELY_INCLUDED, test that all the vertices of the square are in the
				// circle
				double distanceMax = distanceMax(x, y, ccell, shiftX, shiftY);

				double distance = 0;
				if (criterion == RoundMask.PARTIALLY_INCLUDED) {
					distance = distanceMin;
				} else if (criterion == RoundMask.CENTER_INCLUDED) {
					distance = distanceToCenter;
				} else if (criterion == RoundMask.COMPLETELY_INCLUDED) {
					distance = distanceMax;
				}

				if (distance <= radius) {
					ccells.add(ccell);
					cellDistances.put(ccell, distance);
				}
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "RoundMask.getCellsNear ()", "Exception, returned no concurrent cells", e);
		}

		return ccells;
	}

	// 10.10.2008
	private double distanceMax(double x, double y, SquareCell cell, double xShift, double yShift) {
		double d = -Double.MAX_VALUE;
		for (Object o : cell.getVertices()) {
			Vertex2d v2 = new Vertex2d((Vertex2d) o);
			v2.x += xShift;
			v2.y += yShift;
			double distanceToThisVertex = Math.sqrt(Math.pow(v2.x - x, 2) + Math.pow(v2.y - y, 2));
			d = Math.max(d, distanceToThisVertex);
		}
		return d;
	}

	/**
	 * Return the distance of this tree to the location of the last getTreesNear ()
	 * call.
	 */
	// SHOULD BE RETESTED - fc - 10.10.2008
	// SHOULD BE RETESTED - fc - 10.10.2008
	// SHOULD BE RETESTED - fc - 10.10.2008
	// fc - 9.12.2008 - seems ok (stretch)
	// public double getDistance (SpatializedTree t) { // fc - 28.6.2004
	public double getDistance(Spatialized t) {
		Double d = (Double) neighbourDistances.get(t);
		if (d == null) {
			return Double.MAX_VALUE;
		}
		return d.doubleValue();
	}

	/**
	 * Return the map cell -> coordinate shifts calculated during the last
	 * getTreesNear () [EDIT fc+bc-19.1.2021 or getCellsNear ()] call.
	 */
	// SHOULD BE RETESTED - fc-10.10.2008
	public Map<SquareCell, ShiftItem> getShiftMap() { // fc-21.6.2006

		// fc+bc-19.1.2021 Added <SquareCell, ShiftItem>

		Map<SquareCell, ShiftItem> m = new HashMap<>();

		for (Iterator i = neighbours.iterator(); i.hasNext();) {
			Neighbour n = (Neighbour) i.next();
			SquareCell c = (SquareCell) n.cell;

			m.put(c, n.shift);
		}
		return m;
	}

	public double getRadius() {
		return radius;
	}

	// fc - 25.11.2008 - see GNeighbourhoodMask - public void setTorusEnabled
	// (boolean v)
	// {torusEnabled = v;}

	// fc - 25.11.2008 - see GNeighbourhoodMask - public boolean isTorusEnabled ()
	// {return
	// torusEnabled;}

	public Map getNeighbourDistances() {
		return neighbourDistances;
	} // fc - 13.11.2008

	public Map getNeighbourAngles() {
		return neightbourAngles;
	}

	public Map getTreeLocationsWithinTorus() {
		return treeLocationsWithinTorus;
	} // fc - 9.12.2008

	public String toString() {
		StringBuffer b = new StringBuffer("RoundMask radius=" + radius + " torusEnabled=" + isTorusEnabled() + " ");
		for (Iterator i = getSites().iterator(); i.hasNext();) {
			Point p = (Point) i.next();
			b.append("(" + p.x + " " + p.y + ") ");
		}
		return b.toString();
	}

	public Collection<ShiftedSpatializedTree> getNeighboursWithinThorus() { // gl 26/02/2014
		return neighboursWithinThorus;
	}

	/**
	 * An inner class to store in a collection duplicated tree and their coordinates
	 * within the torus.
	 * 
	 * @author gl (26/02/2014)
	 *
	 */
	public static class ShiftedSpatializedTree {
		private Spatialized tree;
		private Vertex2d shiftedCoordinates;

		ShiftedSpatializedTree(Spatialized t, Vertex2d v) {
			this.tree = t;
			this.shiftedCoordinates = v;
		}

		public Spatialized getTree() {
			return tree;
		}

		public Vertex2d getShiftedCoordinates() {
			return shiftedCoordinates;
		}
	}

}
