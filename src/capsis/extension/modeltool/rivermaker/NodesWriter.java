/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.rivermaker;

import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import jeeb.lib.util.Vertex3d;

/**	A tool to write trees of watershed nodes to file
*	usage: new NodesWriter (tree).save (fileName);
*
*	@author B. Parisi - april 2006
*/
public class NodesWriter extends RecordSet {

	@Import
	static public class ReachRecord extends Record {
		public int id;
		public String address;
		public int order;
		public short length;
		public float meanWidth;
		public Collection fishes;
		public Vertex3d origin;
		public Vertex3d end;

		public ReachRecord (Reach r) throws Exception {
			super ();
			id = r.getId ();
			address = r.getAddress ();
			order = r.getOrder ();
			length = r.getLength ();
			meanWidth = r.getMeanWidth ();
			fishes = r.getFishes ();
			origin = r.getOrigin ();
			end = r.getEnd ();
		}
	}

	@Import
	static public class WeirRecord extends Record {
		public int id;
		public byte upPass;
		public byte downPass;
		public Vertex3d origin;
		public String address;

		public WeirRecord (Weir r) throws Exception {
			super ();
			id = r.getId ();
			upPass = r.getUpPass ();
			downPass = r.getDownPass ();
			origin = r.getOrigin ();
			address = r.getAddress ();
		}
	}

	@Import
	static public class NetworksRatiosRecord extends Record {
		public float Rn_1;
		public float Rl_1;
		// only recording the averaged ratios deduced from regressiosn, jl - 04.09.2006
		//public float Rn_2;
		//public float Rl_2;
		//public float Rn_3;
		//public float Rl_3;

		public NetworksRatiosRecord (float [] ratios) throws Exception {
			super ();
			Rn_1 = ratios [0];
			Rl_1 = ratios [1];

		}
	}


	/**	Constructor.
	*/
	public NodesWriter (Collection nodes, float [] ratios) throws Exception {
		super ();

		for (Iterator i = nodes.iterator (); i.hasNext ();) {
			Object o = i.next ();
			if (o instanceof Reach) {
				add (new ReachRecord ((Reach) o));
			} else {	// Weir
				add (new WeirRecord ((Weir) o));
			}
		}
		add(new NetworksRatiosRecord (ratios));

	}

	public void save (String fileName) throws Exception {
		setHeaderEnabled (false);
		RecordSet.commentMark = "//";
		super.save (fileName);
	}

}


