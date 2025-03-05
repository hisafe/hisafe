/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.modeltool.rivermaker;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Vertex3d;
import jeeb.lib.util.Node;
import capsis.util.WatershedNode;

/**	Reach : "tronçon" - portion of river.
*
*	@author B. Parisi - march 2006
*/
public class Reach extends Node implements WatershedNode {

	private final static Collection EMPTY_COLLECTION = new ArrayList ();

	private int id;
	private String address;			// ex: "rdg"
	private int order;				// reach order
	private short length;			// m.
	private float meanWidth;		// mean width - m..
	private Collection fishes;
	private Vertex3d origin;
	private Vertex3d end;


	// constructor with vertices j.l. 01.10.2004
	public Reach (int id,
			String address,
			int order,
			short length,
			float meanWidth,
			// fishes: unused here, "{}" in the files
			Vertex3d origin,
			Vertex3d end
			) {
		this.id = id;
		this.address = address;
		this.order = order;
		this.length = length;
		this.meanWidth = meanWidth;
		this.fishes = null;
		this.origin = origin;
		this.end = end;
	}

	// accessors
	public int getId () {return id;}
	public String getAddress () {return address;}
	public int getOrder () {return order;}
	public short getLength () {return length;}
	public float getMeanWidth () {return meanWidth;}
	public Collection getFishes () {return fishes == null ? EMPTY_COLLECTION : fishes;} // always returns a collection
	public Vertex3d getOrigin () {return origin;}
	public Vertex3d getEnd () {return end;}

	public void setOrigin (Vertex3d v) {origin = v;}
	public void setEnd (Vertex3d v) {end = v;}

	public void setOrder (int o) {order = o;}
	public void setLength (short l) {length = l;}
	public void setMeanWidth (float mw) {meanWidth = mw;}



	/**	True if there is a weir just above the reach
	*/
	public boolean hasWeirDownstream () {
		Node n = getFather ();
		return (n != null && n instanceof Weir);
	}

	/**	True if there is a weir just below the reach
	*/
	public boolean hasWeirUpstream () {
		Node n = getLeftSon ();
		return (n != null && n instanceof Weir);
	}

	/**	Return the weir just above the reach (null if no weir)
	*/
	public Weir getWeirDownstream () {
		Node n = getFather ();
		return (n != null && n instanceof Weir) ? (Weir) n : null;
	}

	/**	Return the weir just below the reach (null if no weir)
	*/
	public Weir getWeirUpstream () {
		Node n = getLeftSon ();
		return (n != null && n instanceof Weir) ? (Weir) n : null;
	}

	/**	Return a Collection of downstream reaches
	*	Consider an order = how many levels upper
	*/
	public Collection getDownstream (int unit) {   // changed "order" for "unit" . Labonne.
		Collection down = new ArrayList ();
		Node n = this;
		for (int i = 0; i < unit; i++) {
			n = n.getFather ();
			while (n != null && !(n instanceof Reach)) {
				n = n.getFather ();
			}
			if (n == null) {return down;}
			down.add (n);
		}
		return down;
	}

	/**	Return a Collection of downstream reaches
	*	Consider an order = how many levels upper
	*/
	public Collection getDownstreamOrder (int order) {
		Collection down = new ArrayList ();
		Node n = this;
		while (n.getFather() != null) {
			n = n.getFather ();
			while (n != null && !(n instanceof Reach)) {
				n = n.getFather ();
			}
			if (n == null) {return down;}
			if (n instanceof Reach && ((Reach)n).getOrder() != order) {
				return down;}
			down.add (n);
		}
		return down;
	}



	/**	Return a Collection of downstream reaches
	*	until the considered node.
	*/
	public Collection getDownstream (Node n ) {   // changed "order" for "unit" . Labonne.
		Collection down = new ArrayList ();

		while (n.getFather() != null) {
			//down.add(n)
			//n = n.getFather ();
			while (n != null && !(n instanceof Reach)) {
				n = n.getFather ();
			}
			if (n == null) {return down;}
			down.add (n);
		}



		return down;
	}


	/**	Return a Collection of upstream reaches
	*	Consider an order = how many levels below
	*/
	public Collection getUpstream (int unit) {
		Collection up = new ArrayList ();
		Collection nodes = new ArrayList ();
		nodes.add (this);
		for (int i = 0; i < unit; i++) {
			Collection levelAbove = new ArrayList ();
			for (Iterator j = nodes.iterator (); j.hasNext ();) {
				Node n = (Node) j.next ();
				Collection sons = getUpstream (n);
				up.addAll (sons);
				levelAbove.addAll (sons);
			}
			nodes = levelAbove;
		}
		return up;
	}

	// Tool method for getUpstream (int order)
	//
	private Collection getUpstream (Node n) {
		Collection up = new ArrayList ();
		n = n.getLeftSon ();
		if (n != null && !(n instanceof Reach)) {
			n = n.getLeftSon ();
		}
		if (n == null) {return up;}
		up.add (n);
		while (n.getRightBrother () != null) {
			up.add (n.getRightBrother ());
			n = n.getRightBrother ();
		}
		return up;
	}

	// Tool method to retrieve the brothers of a reach
	// i.e. the reaches with same father than this
	//
	public Collection getBrothers () {
		Collection brothers = new ArrayList ();
		if (getFather () != null) {
			brothers = getFather ().getSons ();
			if (brothers != null && !brothers.isEmpty ()) {
				brothers.remove (this);
			}
		}
		return brothers;
	}

	public String toString () {return "reach "+id;}

}



