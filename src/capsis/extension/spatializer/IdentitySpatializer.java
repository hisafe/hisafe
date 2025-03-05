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

package capsis.extension.spatializer;

import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JPanel;

import jeeb.lib.util.InstantConfigurable;
import jeeb.lib.util.InstantPanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.extension.PaleoSpatializer;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.Action4Event;
import capsis.util.SpatializedObject;

/**	IdentitySpatializer: to spatialize trees with their own locations, in case 
*	of an already spatialized model (MADD...)
*
*	@author F. de Coligny - july 2006
*/
public class IdentitySpatializer extends PaleoSpatializer
		implements InstantConfigurable {

	static {
		Translator.addBundle("capsis.extension.spatializer.IdentitySpatializer");
	}
		// ExtensionManager memorizes GSettings subclasses
		//~ private static class ISSettings extends GSettings {
			//~ public IdentitySpatializerStarter starter;
		//~ }

	private IdentitySpatializerStarter starter;
	private InstantPanel instantPanel;
	private NumberFormat formater;


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public IdentitySpatializer () {}

	/**	Official constructor. It uses the standard Extension starter.
	*/
	public IdentitySpatializer (GenericExtensionStarter s) throws Exception {

		// Temporary
		if (s == null) {
				throw new Exception ("IdentitySpatializer needs a non null IdentitySpatializerStarter");}

		// starter may be: 
		// - an ExtensionStarter -> contains a GStand reference with origin, width and height
		// - an IdentitySpatializerStarter -> contains specific params
		// in both cases, get an IdentitySpatializerStarter
		if (s instanceof IdentitySpatializerStarter) {
			this.starter = (IdentitySpatializerStarter) s;
		} else {	// simple ExtensionStarter
			this.starter = new IdentitySpatializerStarter ();
		}


		// Try to get a previously configured starter
		//~ retrieveSettings ();	// try to get previously saved starter

		formater = NumberFormat.getNumberInstance();
		formater.setMaximumFractionDigits(2);
		formater.setGroupingUsed (false);

	}

	/**	This is Extension dynamic compatibility mechanism.
	*
	*	This matchwith method must be redefined for each extension subclass.
	*	It must check if the extension can deal (i.e. is compatible) with the referent.
	*
	*	Compatible with a (Spatialized and GTree and not Numberable) or a collection of some.
	*/
	public boolean matchWith (Object referent) {
		try {
			if (referent instanceof Tree 
					&& referent instanceof Spatialized
					&& !(referent instanceof Numberable)) {return true;}
			if (referent instanceof Collection) {
				Collection c = (Collection) referent;
				Collection reps = Tools.getRepresentatives (c);
				for (Iterator i = reps.iterator (); i.hasNext ();) {
					Object o = i.next ();
					if (!(o instanceof Tree 
							&& o instanceof Spatialized) 
							&& !(referent instanceof Numberable)) {
						return false;
					}
				}
			}
			return true;

		} catch (Exception e) {
			Log.println (Log.ERROR, "IdentitySpatializer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("IdentitySpatializer.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}
	
	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("IdentitySpatializer.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	public Collection<SpatializedObject> getSpatializedObjects (Collection<Tree> trees) {
		Collection sos = new ArrayList<SpatializedObject> ();
		
		for (Tree t : trees) {
			if (t.isMarked ()) {continue;}	// marked trees are dead
			Spatialized s = (Spatialized) t;
			
			SpatializedObject so = new SpatializedObject (t, s.getX (), s.getY (), s.getZ ());
			sos.add (so);
		}
		
		return sos;
	}
	
	/**	Called when config changes, save it for next time
	*/
	//~ protected void memoSettings () {
		//~ ISSettings s = new ISSettings ();
		//~ s.starter = (IdentitySpatializerStarter) this.starter.clone ();
		//~ ExtensionManager.memoNewSettings (createMemoKey (), s);
	//~ }

	// Create a key from class name and listener class name
	// This key is used to save config for the couple (this object, its listener)
	// Listener can for instance be SVLollypopDrawer of Vier2DHalf: they may be
	// configured differently
	//~ private String createMemoKey () {
		//~ String key = ""+this.getClass ().getName ()+"+"+listener.getClass ().getName ();
		//~ return key;
	//~ }

	/**	Retrieve the settings for this extension and the current listener as saved
	*	the last time they were changed (if found).
	*/
	//~ protected void retrieveSettings () {
		//~ GSettings memoSettings = ExtensionManager.getSettings (createMemoKey ());
		//~ if (memoSettings != null) {
			//~ ISSettings s = (ISSettings) memoSettings;
			
			//~ starter = s.starter;	// Note: listener is unchanged (not memorized)
		//~ }
	//~ }

	/**	Accessor for starter
	*/
	public IdentitySpatializerStarter getStarter () {return starter;}

	/**	Should return a JPanel if selection done, null if nothing selected
	*	True value for more means add selection to the previous selection
	*/
	public JPanel select (Rectangle.Double r, boolean more) {
		return null;
	}

	/**	InstantPanel notifies config changes by calling this actionPerformed method
	*/
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (instantPanel)) {
			// our config changed
			// notify the tool using this Spatializer
			// (it may redraw the scene by calling us again)
			Object source = this;
			String type = "configChanged";
			Action4Event e2 = new Action4Event (source, type);

			notifyActionListeners (e2);

			//~ memoSettings ();	// config changed, save it
		}
	}

	/**	Get the config panel for the subject
	*/
	public InstantPanel getInstantPanel () {
		if (instantPanel == null) {
				instantPanel = new IdentitySpatializerPanel (this);}
		return instantPanel;
	}

}




