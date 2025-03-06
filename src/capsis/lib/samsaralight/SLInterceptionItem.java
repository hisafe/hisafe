/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.lib.samsaralight;

import java.text.NumberFormat;

import capsis.defaulttype.Tree;
import jeeb.lib.util.DefaultNumberFormat;

/**
 * SLInterceptionItem: interception characteristics for a target cell, a given
 * beam and a given tree part (i.e. crown part, trunk) of a tree.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLInterceptionItem implements Comparable {

	private double distance;
	private double pathLength;
	private SLTreePart treePart;
	private Tree tree;

	/**
	 * Constructor.
	 */
	public SLInterceptionItem(double distance, double pathLength,
			SLTreePart treePart, Tree tree) {
		this.distance = distance;
		this.pathLength = pathLength;
		this.treePart = treePart;
		this.tree = tree;
	}

	/**
	 * From Comparable interface.
	 */
	public int compareTo(Object b) {
		SLInterceptionItem tb = (SLInterceptionItem) b;
		double la = distance;
		double lb = tb.getDistance();
		if (la == lb) {
			return 0;
		} else if (la < lb) {
			return 1;
		} else {
			return -1;
		}
	}

	public double getDistance() {
		return distance;
	}

	public double getPathLength() {
		return pathLength;
	}

	public SLTreePart getTreePart() {
		return treePart;
	}

	public Tree getTree() {
		return tree;
	}

	/**
	 * Return a String representation of this object.
	 */
	public String toString() {
		NumberFormat nf = DefaultNumberFormat.getInstance();
		StringBuffer b = new StringBuffer("SLInterceptionItem");
		b.append(" distance: ");
		b.append(nf.format(distance));
		b.append(" pathLength: ");
		b.append(nf.format(pathLength));
		b.append(" treePart: ");
		b.append(treePart);
		b.append(" tree: ");
		b.append(tree);
		return b.toString();
	}

}