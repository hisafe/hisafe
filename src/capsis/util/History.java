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
package capsis.util;

import java.util.LinkedList;

/**	Memorizes some events, can go back and forward.
*	Can be used for undo / redo or back / forward in a browser.
*	@author F. de Coligny - march 2005
*/
public class History extends LinkedList {

	private int i;	// the index of the current element in the history
	
	/**	Constructor
	*/
	public History () {
		super ();
		i = -1;
	}

	/**	Add the object in the history just after the index. 
	*	Previously discard all the elements after the index (lost).
	*	Increment the index.
	*	CAUTION: the given object may be cloned before to be passed to this method.
	*/
	public boolean add (Object o) {
		try {
			int from = i+1;
			int to = size ();
			//~ System.out.println ("add - discarding history end:");
			//~ System.out.println	("   history was: "+toString ());
			//~ System.out.println	("   from="+from);
			//~ System.out.println	("   to="+to);
			//~ System.out.println	("   range="+subList (from, to));
			for (int k = from; k < to; k++) {
				remove (from);
			}
			//~ System.out.println	("   discarded correctly");
			//~ System.out.println	("   history is now: "+toString ());
		} catch (Exception e) {
			//~ System.out.println	("   *** discard aborted ***");
		}	// may be normal
		
		boolean b = super.add (o);
		i++;
		//~ System.out.println ("added: "+o+" - "+toString ());
		return b;
	}
	
	/**	Decrement the index and return the previous object.
	*	If empty history, return null.
	*/
	public Object back () {
		if (!canBack ()) {return null;}
		i--;
		Object o = get (i);
		//~ System.out.println ("back returned: "+o+" - "+toString ());
		return o;
	}
	
	/**	Increment the index and return the next object.
	*	If end of history is reached, return null.
	*/
	public Object next () {
		if (!canNext ()) {return null;}
		i++;
		Object o = get (i);
		//~ System.out.println ("next returned: "+o+" - "+toString ());
		return o;
	}
	
	/**	Just return the object at the current index.
	*	No change of the index.
	*/
	public Object current () {
		if (isEmpty ()) {return null;}
		return get (i);
	}
	
	/**	Chack if back is possible.
	*/
	public boolean canBack () {return !isEmpty () && i > 0;}
	
	/**	Chack if next is possible.
	*/
	public boolean canNext () {return (!isEmpty () && i < size () - 1);}
	
	public String toString () {
		return "History i="+i+": "+super.toString ();
	}
	
}
