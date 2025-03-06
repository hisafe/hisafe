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
package capsis.util.group;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Log;
import capsis.commongui.util.Tools;
import capsis.kernel.GScene;

/**	Identifier is a grouper. It can make groups at runtime
 *	in collections containing Identifiable Objects.
 * 
 *	@author F. de Coligny - march 2004
 */
public class Identifier<T> extends HashSet<T> implements Grouper {

	private String name;
	private GroupableType type;

	private boolean not;	// fc - 16.4.2007
	public boolean isNot () {return not;}	// fc - 16.4.2007
	public void setNot (boolean not) {this.not = not;}	// fc - 16.4.2007

	/**	Constructor.
	 */
	public Identifier (String name, GroupableType type, Collection ids) {
		super ();
		this.name = name;
		this.type = type;
		addAll (ids);		// HashSet is fast on contains (), ids are instances of Integer

		not = false;	// fc - 16.4.2007
	}

	/**
	 * Returns a copy of this grouper. Groupers may be used in several threads,
	 * copies are needed to avoid concurrence problems.
	 */
	public Grouper getCopy () {
		try {
			return new Identifier (name, type, this);
		} catch (Exception e) {
			Log.println (Log.ERROR, "Identifier.getCopy ()", "Could not create a copy of this grouper", e);
			return null;
		}
	}

	/**	Name of the Identifier.
	 */
	public String getName () {return name;}

	/**	Tell if the Identifier can be used on this referent.
	 *	Referent must be a composite or a collection inside a composite 
	 *	(with all its elements Identifiable).
	 */
	public boolean matchWith (Object referent) {

		// DISCARDED - fc - 16.9.2004
		//~ if (Group.isComposite (referent)) {
		//~ referent = Group.whichCollection (referent);
		//~ }
		// DISCARDED - fc - 16.9.2004

		if (referent instanceof Collection) {		// Test the collection
			Collection c = (Collection) referent;
			if (c.isEmpty ()) {return false;}
			Collection reps = Tools.getRepresentatives (c);	// one instance of each class
			//
			// Possibly several subclasses of Identifiable
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				if (!(i.next () instanceof Identifiable)) {return false;}
			}
			return true;
			//~ } else {		// Can also work with an object which is known by the capsis grouping system
			//~ if (!(Group.whichType (referent).equals (Group.UNKNOWN))) {return true;}
		}
		return false;
	}

	/**	Apply the Identifier on this collection and return the resulting sub collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	//~ public GroupCollection apply (Collection c) {return apply (c, false);}
	public GroupCollection apply (Collection c) {return apply (c, not);}	// fc - 16.4.2007

	/**	Apply the Identifier in complementary mode on this collection and return the resulting sub collection.
	 *	Never return null.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply (Collection c, boolean complementary) {
		//~ Collection inside = new ArrayList ();
		//~ Collection outside = new ArrayList ();
		GroupCollection inside = new GroupCollection ();
		GroupCollection outside = new GroupCollection ();
		for (Iterator i = c.iterator (); i.hasNext ();) {
			Identifiable indiv = (Identifiable) i.next ();
			if (contains (new Integer (indiv.getId ()))) {
				inside.add (indiv);
			} else {
				outside.add (indiv);
			}
		}
		return complementary ? outside : inside;
	}

	/**	Return the type of the individuals this Identifier can handle.
	 */
	public GroupableType getType () {return type;}

	/**	Return ids.
	 */
	public Collection<T> getIds () {return this;}


	/**
	 * Returns the area (m2) of the given scene. Default is scene.getArea () but
	 * the grouper may have a different answer, see Samsa2CoreTreeGrouper.
	 */
	public double getArea(GScene scene) { // fc-2.6.2020
		
		return scene.getArea ();
		
	}

	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD 
	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD 
	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD 
	//~ public static void main (String[] args) {
	//~ Collection co = new ArrayList ();
	//~ for (int i = 0; i < 10; i++) {
	//~ sapin.model.SapTree t = new sapin.model.SapTree (i, null, 2.0, 5.5, i, 0, 0, null) {
	//~ public String toString () {
	//~ return "SapTree_"+getId ();
	//~ }
	//~ };
	//~ co.add (t);
	//~ }

	//~ Collection ids = new ArrayList ();
	//~ ids.add (new Integer (4));
	//~ ids.add (new Integer (5));
	//~ ids.add (new Integer (6));
	//~ ids.add (new Integer (99));

	//~ Grouper g = new Identifier ("groupe A", TreeList.GROUP_ALIVE_TREE, ids);

	//~ Collection groupeA = g.apply (co);
	//~ Collection groupeB = g.apply (co, true);

	//~ System.out.println ("co: "+Tools.toString (co));
	//~ System.out.println ("ids: "+Tools.toString (ids));
	//~ System.out.println ("groupeA: "+Tools.toString (groupeA));
	//~ System.out.println ("groupeB: "+Tools.toString (groupeB));
	//~ }

}
