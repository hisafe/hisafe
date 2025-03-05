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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.geom.Rectangle2D;
import java.util.Collection;

import javax.swing.JPanel;

import jeeb.lib.defaulttype.PaleoExtension;
import jeeb.lib.util.Log;
import capsis.app.CapsisExtensionManager;
import capsis.extensiontype.Lollypop;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.ConfigPanelable;
import capsis.util.SpatializedObject;

// These imports may be removed when in java 1.6
												//~ import javax.media.opengl.*;
												//~ import com.sun.opengl.util.*;
// These imports may be removed when in java 1.6

//terragen
//arboro

/**	Lollypop: draw simple lollypop trees from top or in 2D / 3D.
*	The resulting scenes shows tree structure at the stand / group level.
*
*	@author F. de Coligny - march 2006
*/
public abstract class PaleoLollypop implements PaleoExtension, Lollypop, ConfigPanelable {

	public static final String MODE_TOP = "modeTop";	// maybe not a String
	public static final String MODE_2D = "mode2D";
	public static final String MODE_3D = "mode3D";


	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public PaleoLollypop () {}

	/**	Official constructor. It uses the standard Extension starter.
	*/
	public PaleoLollypop (GenericExtensionStarter s) {}	// in GUI mode, settings are retrieved from gui components.

	/**	This is Extension dynamic compatibility mechanism.
	*
	*	This matchwith method must be redefined for each extension subclass.
	*	It must check if the extension can deal (i.e. is compatible) with the referent.
	*	Here, referent must be at least a GModel instance.
	*
	*	For Lollypop, referent should be a subject to be drawn or a collection of them.
	*/
	public boolean matchWith (Object referent) {
		Log.println (Log.ERROR, "Lollypop.matchWith ()",
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
		return CapsisExtensionManager.LOLLYPOP;
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

	/**	MODE_TOP and MODE_2D. 
	*	Convenient method to draw spatialized subjects.
	*	If trouble, stop drawing and report exception.
	*/
	static public void draw (Graphics g, Rectangle.Double r, PaleoLollypop l, String mode,	// add visible Rectangle ?
			Collection<SpatializedObject> subjects) throws Exception {

		for (SpatializedObject s : subjects) {
			if (mode == MODE_TOP) {
				l.drawTopView (g, r, s);
			} else if (mode == MODE_2D) {
				l.draw2DView (g, r, s);
			//~ } else if (mode == MODE_3D) {
				//~ l.draw3DView (g, r, s);
			} else {
				throw new Exception ("Lollypop.draw (): Unknown mode: "+mode
						+" should be Lollypop.MODE_TOP, MODE_2D or MODE_3D");
			}
		}
	}

	/**	MODE_3D. fc - 25.9.2006
	*	Convenient method to draw spatialized subjects.
	*	If trouble, stop drawing and report exception.
	*/
								//~ static public void draw (GLAutoDrawable drawable, Lollypop l, String mode,
										//~ Collection<SpatializedObject> subjects) throws Exception {
										//~ if (!mode.equals (MODE_3D)) {
												//~ throw new Exception ("invalid mode, must be MODE_3D: "+mode);}
											
									//~ for (SpatializedObject s : subjects) {
										//~ l.draw3DView (drawable, s);
									//~ }
								//~ }

	/**	Top view.
	*	Draw the given object at the given location in the Graphics.
	*	The Rectangle is the visible zone (in case of zoom...)
	*	Return a bounding box of the drawing.
	*/
	abstract public Rectangle2D drawTopView (Graphics g, Rectangle.Double r, SpatializedObject subject)
			throws Exception;

	/**	2D view.
	*	Draw the given object at the given location in the Graphics.
	*	The Rectangle is the visible zone (in case of zoom...)
	*	Return a bounding box of the drawing.
	*/
	abstract public Rectangle2D draw2DView (Graphics g, Rectangle.Double r, SpatializedObject subject)
			throws Exception;

	
	
	
	
	
	/**	3D view.
	*	Prepare draw3DView in case of settings changes (update GL display lists...). fc - 25.9.2006
	*	Optional: default empty implementation.
	*/
									//~ public void init (GLAutoDrawable drawable)
											//~ throws Exception {}

	/**	3D view.
	*	Draw the given object in the GLAutoDrawable. fc - 25.9.2006
	*/
									//~ abstract public void draw3DView (GLAutoDrawable drawable, SpatializedObject subject)
											//~ throws Exception;

	
	
	
	
	
	/**	Should return a JPanel if selection done, null if nothing selected
	*	True value for more means add selection
	*/
	abstract public JPanel select (Rectangle.Double r, boolean more);


}

/*
1. spatialise trees
tree -> loc OR tree -> locs (MAID trees)

2. choose a custom drawer
ex: Lollypop d = new DefaultLollypop ();
the drawer may be parameterised
d.getConfigPanel (), d.getConfigLabel (), d.configure ();
-> with/wo label, colors, detail level...

3. choose a mode
String mode = MODE_TOP;

3. draw
MainDrawer.draw (g, d, mode, tree, loc);
MainDrawer.draw (g, d, mode, tree, locs);

*/



