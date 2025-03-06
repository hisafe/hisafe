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

/**	SelectionEvent
*	@author F. de Coligny - december 2007
*/
public class SelectionEvent extends java.util.EventObject {
	private Object param;	// optionnal
	// selectionActuallyChanged is true whan the user actually made a new selection, 
	// it is false when the selection is "the same" : update needed because some
	// visual parameter changed (color, see labels...) or because we want to see
	// the "same trees" on another step
	// when selectionActuallyChanged, listeners can reset some states (zooms...)
	private boolean selectionActuallyChanged;
	private Collection listenerEffectiveSelection;	// optionnal, the source can know what was effectively selected
	
	public SelectionEvent (Object source, boolean selectionActuallyChanged) {
		super (source);
		this.selectionActuallyChanged = selectionActuallyChanged;
		listenerEffectiveSelection = new ArrayList ();
	}
	
	public void setListenerEffectiveSelection (Collection v) {listenerEffectiveSelection = v;}
	public void setParam (Object v) {param = v;}
	
	public SelectionSource getSource () {return (SelectionSource) source;}
	public Collection getListenerEffectiveSelection () {return listenerEffectiveSelection;}
	public Object getParam () {return param;}
	public boolean hasSelectionActuallyChanged () {return selectionActuallyChanged;}
	
	public String toString () {
		return "SelectionEvent: source="+source
				+" selectionActuallyChanged="+selectionActuallyChanged
				+" #listenerEffectiveSelection="+listenerEffectiveSelection==null?"0":""+listenerEffectiveSelection.size ()
				+" param="+param;
	}
}
