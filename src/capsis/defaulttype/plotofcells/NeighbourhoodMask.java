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
import java.util.Collection;

import capsis.defaulttype.ShiftItem;
import jeeb.lib.util.Log;

/**
 * A mask describing a neighbourhood shape around a SquareCell. It is
 * represented by a list of sites, i.e. neighbour SquareCell in terms of i-shift
 * and j-shift relatively to a target SquareCell in the SquareCell matrix. I is
 * the line number (top -> bottom) and J the column number (left -> right).
 * 
 * @author F. de Coligny - march 2000, review october 2008
 */
public class NeighbourhoodMask implements Serializable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected Collection<Point> sites; // relative coordinates (Points)
	// fc - 25.11.2008 - bug correction (thanks to bc) - torusEnabled field is
	// now here
	private boolean torusEnabled;

	/**
	 * Constructor
	 */
	public NeighbourhoodMask() {
		sites = new ArrayList<Point>();
		torusEnabled = true; // fc - 25.11.2008 - default value: torus activated
	}

	/**
	 * Copy constructor
	 */
	public NeighbourhoodMask(NeighbourhoodMask mask) {

		// fc+fa-3.8.2017 for SamsaraLight Parallel mode

		// Sites will not change
		this.sites = mask.sites;
		this.torusEnabled = mask.torusEnabled;

	}

	public void addSite(Point p) {
		sites.add(p);
	}

	public Collection<Point> getSites() {
		return sites;
	}

	public void removeSite(Point p) {
		sites.remove(p);
	}

	// fc - 25.11.2008 - all masks can be torus enabled / disabled (see
	// RoundMask)
	public boolean isTorusEnabled() {
		return torusEnabled;
	}

	public void setTorusEnabled(boolean v) {
		torusEnabled = v;
	}

	// fc - 25.11.2008

	/**
	 * Returns a list of concurrent cells according to the given mask. The mask
	 * owns a vector of cell indices relative to this cell. These indices are
	 * relative to a cell grid (see iGrid and jGrid). This method computes the
	 * shifts to apply on tree coordinates to simulate a virtual torus on the
	 * plot. The torus means that the neighbours of the edge cells are those
	 * which are located at the corresponding opposite edge.
	 */
	public Collection<Neighbour> getNeighbours(SquareCell cell) {
		return getNeighbours(cell, false);
	}

	public Collection<Neighbour> getNeighbours(SquareCell cell, boolean onlyCellsWithTreesInside) {
		Collection<Neighbour> neighbours = new ArrayList<Neighbour>();

		// ~ SquareCellHolder holder = cell.getHolder ();
		SquareCellHolder holder = null;
		if (cell.getPlot() instanceof SquareCellHolder) {
			holder = (SquareCellHolder) cell.getPlot();
		} else if (cell.getMother() != null && cell.getMother() instanceof SquareCellHolder) {
			holder = (SquareCellHolder) cell.getMother();
		}

		int nCol = holder.getNCol();
		int nLin = holder.getNLin();

		double holderWidth = nCol * holder.getCellWidth();
		double holderHeight = nLin * holder.getCellWidth();

		double xShift = 0;
		double yShift = 0;
		double zShift = 0;

		// ~ Vector relCoord = getNeighbourCells ();

		// Search all the concurrent cells neighbouring this cell according to
		// the mask
		// fc - 9.10.2008 - with or without the torus in the mask
		for (Point p : getSites()) {

			int iDec = p.x;
			int jDec = p.y;

			xShift = 0;
			yShift = 0;
			zShift = 0;

			int iGrid = cell.getIGrid();
			int jGrid = cell.getJGrid();

			// fc - 9.10.2008 - better torus consideration
			// check if torus is enabled, if not, consider only the cells in the
			// grid
			// without modulo on iGrid and jGrid
			if (!isTorusEnabled()) {
				if (iGrid + iDec < 0 || iGrid + iDec >= nLin || jGrid + jDec < 0 || jGrid + jDec >= nCol) {
					continue;
				}

			}

			SquareCell cc = holder.getCell(iGrid + iDec, jGrid + jDec); // uses
																		// modulo
			if (cc == null) {
				Log.println(Log.ERROR, "GNeighbourhoodMask.getNeighbours ()",
						"found a null concurrent cell, looked in [" + (iGrid + iDec) + "," + (jGrid + jDec) + "]");
			}
			// if we're asked to avoid empty cells... check
			if (!(onlyCellsWithTreesInside && cc.isEmpty())) {
				if (iGrid + iDec < 0) {
					int dep = Math.abs(iGrid + iDec) - 1; // overflow
					int n = dep / nLin; // integer division
					// ~ yShift = (n+1) * plot.getHeight ();
					yShift = (n + 1) * holderHeight;
				}

				if (iGrid + iDec > nLin - 1) {
					int dep = Math.abs((nLin - 1) - iGrid - iDec) - 1; // overflow
					int n = dep / nLin; // integer division
					// ~ yShift = - (n+1) * plot.getHeight ();
					yShift = -(n + 1) * holderHeight;
				}

				if (jGrid + jDec < 0) {
					int dep = Math.abs(jGrid + jDec) - 1; // overflow
					int n = dep / nCol; // integer division
					// ~ xShift = - (n+1) * plot.getWidth ();
					xShift = -(n + 1) * holderWidth;
				}

				if (jGrid + jDec > nCol - 1) {
					int dep = Math.abs((nCol - 1) - jGrid - jDec) - 1; // overflow
					int n = dep / nCol; // integer division
					// ~ xShift = (n+1) * plot.getWidth ();
					xShift = (n + 1) * holderWidth;
				}

				// zShift computation is delegated to subclasses which redefine
				// getZShift ()
				zShift = cell.getZShift(xShift, yShift);

				Neighbour n = new Neighbour(cc, new ShiftItem(xShift, yShift, zShift));
				neighbours.add(n);
			}
		}
		return neighbours;
	}

}