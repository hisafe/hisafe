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

package capsis.extension;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import jeeb.lib.defaulttype.PaleoExtension;
import jeeb.lib.util.InstantConfigurable;
import jeeb.lib.util.Log;
import capsis.app.CapsisExtensionManager;
import capsis.defaulttype.Tree;
import capsis.extensiontype.Spatializer;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.SpatializedObject;

/**	Spatializer: Transforms a collection of GTree subclasses into a 
*	Collection of SpatializedObjects
*
*	@author F. de Coligny - july 2006
*/
public abstract class PaleoSpatializer implements PaleoExtension, Spatializer, InstantConfigurable {

	private Collection<ActionListener> listeners;	// fc - 10.7.2006
	
	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public PaleoSpatializer () {}

	/**	Official constructor. It uses the standard Extension starter.
	*/
	public PaleoSpatializer (GenericExtensionStarter s) {}	// in GUI mode, settings are retrieved from gui components.

	/**	This is Extension dynamic compatibility mechanism.
	*
	*	This matchwith method must be redefined for each extension subclass.
	*	It must check if the extension can deal (i.e. is compatible) with the referent.
	*	Here, referent must be at least a GModel instance.
	*
	*	For Spatializer, referent should be a subject to be drawn or a collection of them.
	*/
	public boolean matchWith (Object referent) {
		Log.println (Log.ERROR, "Spatializer.matchWith ()",
				"This method was called because a subclass did not implement "+
				"public boolean matchWith (Object) method. "+
				"Subclass: "+this.getClass ().getName ()+
				". Referent was : "+referent);
		return false;
	}

	/**	From Extension interface.
	*/
	abstract public String getName ();

	/**	From Extension interface.
	*/
	public String getType () {
		return CapsisExtensionManager.SPATIALIZER;
	}

	/**
	 * From Extension interface.
	 */
	public String getClassName () {
		return this.getClass ().getName ();
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	/**	Return a collection of spatialized objects.
	*	If the given trees implement Numberable, there may be more Spatialized 
	*	objects returned than trees in the given collection (ex: if number = 2, 
	*	there will be 2 SpatializedObjects).
	*/
	abstract public Collection<SpatializedObject> getSpatializedObjects (Collection<Tree> trees);

	// Add a listener - fc - 10.7.2006
	public void addActionListener (ActionListener l) {
		if (listeners == null) {listeners = new ArrayList<ActionListener> ();}
		listeners.add (l);
	}
	
	// Remove a listener - fc - 10.7.2006
	public void removeActionListener (ActionListener l) {
		if (listeners == null) {return;}
		listeners.remove (l);
	}		
	
	// Notify the listeners that some event occurred - fc - 10.7.2006
	public void notifyActionListeners (ActionEvent event) {
		if (listeners == null) {return;}
		for (ActionListener l : listeners) {l.actionPerformed (event);}
	}	
	

}




