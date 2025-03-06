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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.util.Tools;
import capsis.extensiontype.Filter;
import capsis.kernel.GScene;
import capsis.util.RelativeFilter;

/**
 * Filtrer is a grouper. It can make groups at runtime by filtering a Collection
 * with filters.
 * 
 * @author F. de Coligny - march 2004
 */
public class Filtrer extends ArrayList implements Grouper {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1590619719536257765L;

	private String name;
	private GroupableType type;

	private boolean not; // fc - 16.4.2007

	public boolean isNot () {
		return not;
	} // fc - 16.4.2007

	public void setNot (boolean not) {
		this.not = not;
	} // fc - 16.4.2007

	/**
	 * Constructor.
	 */
	public Filtrer (String name, GroupableType type, Collection filters) {
		super ();
		this.name = name;
		this.type = type;

		// fc-16.11.2011 we must clone the filter (several filtrer instances in
		// several threads may call them concurently).
		// addAll (filters); // instances of Filter
		for (Object o : filters) {
			Filter f = (Filter) o;
			add (f.clone ());
		}

		not = false; // fc - 16.4.2007
	}

	/**
	 * Constructor 2, for one single filter.
	 */
	public Filtrer (String name, GroupableType type, Filter filter) {
		this (name, type, new ArrayList ());

		// fc-16.11.2011 we must clone the filter (several filtrer instances in
		// several threads may call them concurently).
		// add (filter);
		add (filter.clone ());
	}

	/**
	 * Returns a copy of this grouper. Groupers may be used in several threads,
	 * copies are needed to avoid concurrence problems.
	 */
	public Grouper getCopy () {
		try {
			return new Filtrer (name, type, this);
		} catch (Exception e) {
			Log.println (Log.ERROR, "Filtrer.getCopy ()", "Could not create a copy of this grouper", e);
			return null;
		}
	}

	/**
	 * Name of the Filtrer.
	 */
	public String getName () {
		return name;
	}

	/**
	 * Tell if the Filtrer can be used on this referent. Referent must be a
	 * composite or a collection inside a composite (with all its elements
	 * Identifiable).
	 */
	public boolean matchWith (Object referent) {

		// DISCARDED - fc - 16.9.2004
		// ~ if (Group.isComposite (referent)) {
		// ~ referent = Group.whichCollection (referent);
		// ~ }
		// DISCARDED - fc - 16.9.2004

		if (referent instanceof Collection) { // Test the Collection
			Collection c = (Collection) referent;
			if (c.isEmpty ()) { return false; }
			Collection reps = Tools.getRepresentatives (c); // one instance of
			// each class
			//
			// Iterate on filters and check if they can deal with reps (shorter
			// than with c)
			for (Iterator i = iterator (); i.hasNext ();) {
				Filter f = (Filter) i.next ();
				// One filter can not deal with one of the representatives ->
				// false
				if (!ExtensionManager.matchWith (f.getClass ().getName (), reps)) { return false; }
			}
			return true;
			// ~ } else { // Can also work with an object which is known by the
			// capsis grouping system
			// ~ if (!(Group.whichType (referent).equals (Group.UNKNOWN)))
			// {return true;}
		}
		return false;
	}

	/**
	 * Apply the Filtrer on this collection and return the resulting sub
	 * collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	// ~ public GroupCollection apply (Collection c) {return apply (c, false);}
	// // fc - 16.4.2007
	public GroupCollection apply (Collection c) {
		return apply (c, not);
	} // fc - 16.4.2007

	/**
	 * Apply the Filtrer (optionally in complementary mode) on this collection
	 * and return the resulting sub collection. Never return null.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply (Collection c, boolean complementary) {
		// ~ Collection inside = new ArrayList ();
		// ~ Collection outside = new ArrayList ();
		GroupCollection inside = new GroupCollection ();
		GroupCollection outside = new GroupCollection ();

		Filter[] filters = (Filter[]) this.toArray (new Filter[this.size ()]);

		// 0. Detect some possible RelativeFilter -> shunting
		// If at least one is detected, we must preset and apply the filters one
		// after the other
		// fc - 27.9.2004
		if (filters.length > 1) { // one single filter can go through normal
			// case even if relative
			for (int i = 0; i < filters.length; i++) {
				if (filters[i] instanceof RelativeFilter) { return applyRelativeFilters (c, complementary); }
			}
		}
		// ~ System.out.print ("Filtrer, apply... ");
		// Normal case
		// 1. Preset filters
		for (int i = 0; i < filters.length; i++) {
			try {
				filters[i].preset (c); // fc - 5.4.2004
			} catch (Exception e) {
				Log.println (Log.WARNING, "Filtrer.apply ()", "Exception in preset () for filter " + filters[i], e);
			}
		}

		// 2. Effective filtering
		for (Iterator i = c.iterator (); i.hasNext ();) {
			Object indiv = i.next ();
			if (isRetained (indiv, filters)) {
				inside.add (indiv);
			} else {
				outside.add (indiv);
			}
		}
		// ~ System.out.println ("done");
		return complementary ? outside : inside;
	}

	// Checks if each filter retains the individual.
	// If one filter do not retain it, do not consider the others.
	//
	private boolean isRetained (Object indiv, Filter[] filters) {
		int i = 0;
		while (i < filters.length) {
			try {
				if (!filters[i].retain (indiv)) { return false; }
			} catch (Exception e) {
				Log.println (Log.WARNING, "Filtrer.isRetained ()", "Exception in retain (Object) for filter " + i
						+ " and individual " + indiv, e);
				return false;
			}
			i++;
		}
		return true;
	}

	/**
	 * Return the type of the individuals this Filtrer can handle.
	 */
	public GroupableType getType () {
		return type;
	}

	/**
	 * Return filters.
	 */
	public Collection getFilters () {
		return (Collection) this.clone ();
	}

	// This method is only called if at least one of the filters is instance of
	// RelativeFilter.
	// In that case, filters must be preset and run the one after the other
	// (more
	// collection traversing, less efficient than apply ()).
	// Apply the Filtrer (optionally in complementary mode) on this collection
	// and return the resulting sub collection.
	// Never return null.
	// fc - 27.9.2004
	//
	private GroupCollection applyRelativeFilters (Collection c, boolean complementary) {
		// ~ System.out.println ("Filtrer, applyRelativeFilters... ");
		// ~ Collection inside = new ArrayList (c);
		// ~ Collection outside = new ArrayList ();
		GroupCollection inside = new GroupCollection (c);
		GroupCollection outside = new GroupCollection ();

		Filter[] filters = (Filter[]) this.toArray (new Filter[this.size ()]);

		// For each filter
		for (int i = 0; i < filters.length; i++) {
			// ~ System.out.println ("  filter "+filters[i].getClass ().getName
			// ());
			// 1. Preset filters
			try {
				filters[i].preset (inside);
			} catch (Exception e) {
				Log.println (Log.WARNING, "Filtrer.applyRelativeFilters ()", "Exception in preset () for filter "
						+ filters[i], e);
			}

			// 2. Effective filtering
			try {
				Object[] indivs = inside.toArray (new Object[inside.size ()]);
				inside.clear ();
				// ~ System.out.println ("  #indivs "+indivs.length+"...");
				for (int j = 0; j < indivs.length; j++) {
					if (filters[i].retain (indivs[j])) {
						inside.add (indivs[j]);
					} else {
						outside.add (indivs[j]);
					}
				}
				// ~ System.out.println ("  #inside "+inside.size
				// ()+"  #outside "+outside.size ());
			} catch (Exception e) {
				Log.println (Log.WARNING, "Filtrer.applyRelativeFilters ()", "Exception in retain () for filter "
						+ filters[i], e);
			}
		}
		// ~ System.out.println ("done");
		return complementary ? outside : inside;
	}

	/**
	 * Returns the area (m2) of the given scene. Default is scene.getArea () but
	 * the grouper may have a different answer, see Samsa2CoreTreeGrouper.
	 */
	public double getArea(GScene scene) { // fc-2.6.2020
		
		return scene.getArea ();
		
	}

	public String toString () {
		return "Filtrer name=" + getName () + super.toString ();
	}

	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD
	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD
	// TEST METHOD TEST METHOD TEST METHOD TEST METHOD TEST METHOD
	// ~ public static void main (String[] args) {
	// ~ Collection co = new ArrayList ();
	// ~ for (int i = 0; i < 10; i++) {
	// ~ sapin.model.SapTree t = new sapin.model.SapTree (i, null, 2.0, 5.5, i,
	// 0, 0, null) {
	// ~ public String toString () {
	// ~ return "SapTree_"+getId ();
	// ~ }
	// ~ };
	// ~ co.add (t);
	// ~ }

	// ~ Collection ids = new ArrayList ();
	// ~ ids.add (new Integer (4));
	// ~ ids.add (new Integer (5));
	// ~ ids.add (new Integer (6));
	// ~ ids.add (new Integer (99));

	// ~ Grouper g = new Filtrer ("groupe A", TreeList.GROUP_ALIVE_TREE, ids);

	// ~ Collection groupeA = g.apply (co);
	// ~ Collection groupeB = g.apply (co, true);

	// ~ System.out.println ("co: "+Tools.toString (co));
	// ~ System.out.println ("ids: "+Tools.toString (ids));
	// ~ System.out.println ("groupeA: "+Tools.toString (groupeA));
	// ~ System.out.println ("groupeB: "+Tools.toString (groupeB));
	// ~ }

}
