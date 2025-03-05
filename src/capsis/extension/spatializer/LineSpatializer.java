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
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.extension.PaleoSpatializer;
import capsis.kernel.GScene;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.Action4Event;
import capsis.util.SpatializedObject;

/**	LineSpatializer: to Spatialize trees in lines
*
*	@author F. de Coligny - july 2006
*/
public class LineSpatializer extends PaleoSpatializer
		implements InstantConfigurable {

	static {
		Translator.addBundle("capsis.extension.spatializer.LineSpatializer");
	}
		// ExtensionManager memorizes GSettings subclasses
		//~ private static class LSSettings extends GSettings {
			//~ public LineSpatializerStarter starter;
		//~ }

	private LineSpatializerStarter starter;
	private InstantPanel instantPanel;
	private NumberFormat formater;


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public LineSpatializer () {}

	/**	Official constructor. It uses the standard Extension starter.
	*/
	public LineSpatializer (GenericExtensionStarter s) throws Exception {
		try {
			// Temporary
			if (s == null) {
					throw new Exception ("LineSpatializer needs a non null LineSpatializerStarter");}
	
			// starter may be: 
			// - an ExtensionStarter -> contains a GStand reference with origin, width and height
			// - a LineSpatializerStarter -> contains directly x0, x1, y0, y1 and other params
			// in both cases, get a LineSpatializerStarter
					
			if (s instanceof LineSpatializerStarter) {
				this.starter = (LineSpatializerStarter) s;
				
			} else {	// simple ExtensionStarter
				this.starter = new LineSpatializerStarter ();
				GScene stand = s.getScene ();	// needed
				starter.x0 = stand.getOrigin ().x;
				starter.x1 = stand.getOrigin ().x + stand.getXSize ();
				starter.y0 = stand.getOrigin ().y;
				starter.y1 = stand.getOrigin ().x + stand.getYSize ();
				starter.x0Border = 0;
				starter.x1Border = 0;
				starter.y0Border = 0;
				starter.y1Border = 0;
				starter.interXEnabled = true;
				starter.interX = 10;
				starter.interYEnabled = false;
				starter.interY = 0;
				
			}
			
			
	
			// Try to get a previously configured starter
			//~ retrieveSettings ();	// try to get previously saved starter
	
			formater = NumberFormat.getNumberInstance();
			formater.setMaximumFractionDigits(2);
			formater.setGroupingUsed (false);
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "LineSpatializer.LineSpatializer ()", 
					"Exception in constructor, starter="+s, e);
			throw e;	// tell caller
		}
	}

	/**	This is Extension dynamic compatibility mechanism.
	*
	*	This matchwith method must be redefined for each extension subclass.
	*	It must check if the extension can deal (i.e. is compatible) with the referent.
	*	Here, referent must be at least a GModel instance.
	*
	*	Compatible with GTree, GTree+Numberable or collections of them (GMaddTree, GMaidTree...)
	*/
	public boolean matchWith (Object referent) {
		try {
			if (referent instanceof Tree) {return true;}
			if (referent instanceof Collection) {
				Collection c = (Collection) referent;
				Collection reps = Tools.getRepresentatives (c);
				for (Iterator i = reps.iterator (); i .hasNext ();) {
					if (!(i.next () instanceof Tree)) {
						return false;
					}
				}
			}
			return true;

		} catch (Exception e) {
			Log.println (Log.ERROR, "LineSpatializer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("LineSpatializer.name");
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
		return Translator.swap ("LineSpatializer.description");
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
		Collection so = new ArrayList<SpatializedObject> ();
		
		// Scene bounds
		double x0 = starter.x0 + starter.x0Border;
		double w = starter.x1 - starter.x1Border - x0;
		double y0 = starter.y0 + starter.y0Border;
		double h = starter.y1 - starter.y1Border - y0;
//~ System.out.println ("x0="+x0+" w="+w+" y0="+y0+" h="+h);
		
		// Number of trees
		int n = 0;
		for (Tree t : trees) {
			//~ GTree t = (GTree) trees[i];
			if (t.isMarked ()) {continue;}	// marked trees are dead
			if (t instanceof Numberable && ((Numberable) t).getNumber () <= 0) {continue;}
			
			if (t instanceof Numberable) {n += ((Numberable) t).getNumber ();
			} else {n++;}
		}
//~ System.out.println ("n="+n);
		
		// Line / column numbers and intervals
		int c = 1;		// number of columns
		int l = 1;		// number of lines
		double interLine = 1;	// inter line distance (vertical lines)
		double interTree = 1;	// inter tree distance in a single line
		
		if (starter.interXEnabled) {
			c = (int) Math.floor (w/starter.interX) + 1;
			l = (int) Math.ceil (((double)n)/c);
			interLine = starter.interX;
			
			
			
			interTree = h/(l-1);
			
			
			
System.out.println ("w="+w+" h="+h+" n="+n);
System.out.println ("c="+c+" l="+l+"interLine="+interLine+" interTree="+interTree);
			
		} else {	// s.interYEnabled (radio buttons)
			l = (int) Math.floor (h/starter.interY) + 1;
			c = (int) Math.ceil (((double)n)/l);	
			interLine = w/(c-1);
			interTree = starter.interY;
			
		}


		
		// Calculate lines / columns numbers
		//~ int l = (int) Math.ceil (Math.sqrt (n * h / w));	// line number
		//~ int c = (int) Math.ceil (w / h * l);				// column number
//~ System.out.println ("l="+l);
//~ System.out.println ("c="+c);
		
		// Relocate trees
		//~ int ind = 0;
		int lineNumber = 0;
		int columnNumber = 0;
//~ System.out.println ("interLine="+interLine);
//~ System.out.println ("interColumn="+interColumn);
		
		//~ for (int i = 0; i < trees.length; i++) {
		for (Tree t : trees) {
			//~ GTree t = (GTree) trees[i];
			
			if (t.isMarked ()) {continue;}	// fc - 5.1.2004 - marked trees are dead
			if (t instanceof Numberable && ((Numberable) t).getNumber () <= 0) {continue;}	// fc - 18.11.2004
			
				if (t instanceof Numberable) {
					Numberable nu = (Numberable) t;
					
					for (int k = 0; k < nu.getNumber (); k++) {
						SpatializedObject o = new SpatializedObject (t, 
								x0 + interLine * columnNumber, 	// x
								y0 + interTree * lineNumber, 	// y
								0);				// z
						
						//~ for (SpatializedObject aux : selectedSubjects) {
							//~ if (aux.equals (o)) {o.setSelected (true);}
						//~ }
						
						so.add (o);
						//~ ind++;	// tree index
						
						// line / column management
						lineNumber++;
						if (lineNumber >= l) {
						//~ if (lineNumber > l) {
							lineNumber = 0;
							columnNumber++;
						}
					}
				} else {
					
					SpatializedObject o = new SpatializedObject (t, 
							x0 + interLine * columnNumber, 	// x
							y0 + interTree * lineNumber, 	// y
							0);				// z
					
					//~ for (SpatializedObject aux : selectedSubjects) {
						//~ if (aux.equals (o)) {o.setSelected (true);}
					//~ }
					
					so.add (o);
					//~ ind++;	// tree index
					
					// line / column management
					lineNumber++;
					if (lineNumber >= l) {
					//~ if (lineNumber > l) {
						lineNumber = 0;
						columnNumber++;
					}
				}
				
		}
		
		
		
		return so;
	}
	
	/**	Called when config changes, save it for next time
	*/
	//~ protected void memoSettings () {
		//~ LSSettings s = new LSSettings ();
		//~ s.starter = (LineSpatializerStarter) this.starter.clone ();
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
			//~ LSSettings s = (LSSettings) memoSettings;
			
			//~ // When do not want the memorized stand size -> keep the current one
			//~ s.starter.x0 = starter.x0;
			//~ s.starter.x1 = starter.x1;
			//~ s.starter.y0 = starter.y0;
			//~ s.starter.y1 = starter.y1;
			
			//~ starter = s.starter;	// Note: listener is unchanged (not memorized)
		//~ }
	//~ }

	/**	Accessor for starter
	*/
	public LineSpatializerStarter getStarter () {return starter;}

	/**	Should return a JPanel if selection done, null if nothing selected
	*	True value for more means add selection to the previous selection
	*/
	public JPanel select (Rectangle.Double r, boolean more) {
		return null;
	}

	/**	InstantPanel notifies config changes by calling this actionPerformed method
	*/
	public void actionPerformed (ActionEvent e) {
System.out.println ("LineSpatializer.actionPerformed ()...");
		if (e.getSource ().equals (instantPanel)) {
			// our config changed
			// notify the tool using this Spatializer
			// (it may redraw the scene by calling us again)
			Object source = this;
			String type = "configChanged";
			ActionEvent e2 = new Action4Event (source, type);

			notifyActionListeners (e2);
			
			//~ memoSettings ();	// config changed, save it
		}
	}

	/**	Get the config panel for the subject
	*/
	public InstantPanel getInstantPanel () {
		if (instantPanel == null) {
				instantPanel = new LineSpatializerPanel (this);}
		return instantPanel;
	}

}




