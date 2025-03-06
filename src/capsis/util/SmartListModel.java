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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import javax.swing.ListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

/**
 * SmartListModel is a ListModel (ex: for JList) proposing 
 * add, remove, addAll and removeAll.
 * 
 * @author F. de Coligny - september 2004
 */
public class SmartListModel implements ListModel {
	private Collection listeners;
	private List contents;
	private boolean mute;	// if true, do not tell the listeners
	
	/**	Constructor
	*/
	public SmartListModel () {
		listeners = new ArrayList ();
		contents = new ArrayList ();
	}
	
	/**	Clear the contents
	*/
	public void clear () {
		contents.clear ();
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.INTERVAL_REMOVED, 0, contents.size ()));
	}
	
	/**	Add an item
	*/
	public void add (Object item) {
		contents.add (item);
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.INTERVAL_ADDED, 0, contents.size ()));
	}
	
	/**	Remove an item
	*/
	public void remove (Object item) {
		contents.remove (item);
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.INTERVAL_REMOVED, 0, contents.size ()));
	}
	
	/**	Add some items
	*/
	public void addAll (Collection items) {
		contents.addAll (items);
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.INTERVAL_ADDED, 0, contents.size ()));
	}
	
	/**	Remove some items
	*/
	public void removeAll (Collection items) {
		contents.removeAll (items);
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.INTERVAL_REMOVED, 0, contents.size ()));
	}
	
	/**
	 * Sorts the model with the given comparator.
	 */
	public void sort (Comparator comparator) {
		
		// fc-17.6.2019 see DlistSelector
		Collections.sort(contents, comparator);
		fireListDataEvent (new ListDataEvent (this, ListDataEvent.CONTENTS_CHANGED, 0, contents.size ()));
		
	}
	
	/**	Is the list model empty ?
	*/
	public boolean isEmpty () {
		return contents.isEmpty ();
	}

	/**	Does the model contain the given object ?
	*/
	public boolean contains (Object o) {
		return contents.contains (o);
	}

	/**	Retrieve a given element
	*/
	public Object getElementAt (int index) {
		
		// fc-14.9.2021 Additional check
		int elementCount = getSize ();
		if (index >= elementCount)
			return null;
		
		return contents.get (index);
	}

	/**	Return list model size
	*/
	public int getSize () {
		return contents.size ();
	}

	/**	Return a clone of the contents
	*/
	public List getContents () {
		return new ArrayList (contents);
	}

	/**	Return wether the model is mute or not.
	*/
	public boolean isMute () {return mute;}

	/**	If the model is mute, it does not tell the listeners.
	*	When set verbose again, tells the listeners a change has occured.
	*/
	public void setMute (boolean b) {
		mute = b;
		if (!mute) {
			fireListDataEvent (new ListDataEvent (this, ListDataEvent.CONTENTS_CHANGED, 0, contents.size ()));
		}
	}

	/**	Add a listener
	*/
	public void addListDataListener (ListDataListener l) {
		listeners.add (l);
	}

	/**	Remove a listener
	*/
	public void removeListDataListener (ListDataListener l) {
		listeners.remove (l);
	}

	/**	Tell listeners
	*/
	private void fireListDataEvent (ListDataEvent evt) {
		if (mute) {return;}
		for (Iterator i = listeners.iterator (); i.hasNext ();) {
			ListDataListener l = (ListDataListener) i.next ();
			if (evt.getType () == ListDataEvent.INTERVAL_ADDED) {
				l.intervalAdded (evt);
			} else if (evt.getType () == ListDataEvent.INTERVAL_REMOVED) {
				l.intervalRemoved (evt);
			} else if (evt.getType () == ListDataEvent.CONTENTS_CHANGED) {
				l.contentsChanged (evt);
			}
		}
	}

}
