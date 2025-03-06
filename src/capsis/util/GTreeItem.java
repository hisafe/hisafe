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

import java.awt.Color;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.sketch.kernel.BuiltinType;
import jeeb.lib.sketch.scene.extension.sketcher.TreeSketcher;
import jeeb.lib.sketch.scene.item.Plant;
import jeeb.lib.sketch.scene.item.VertexItem;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Species;
import capsis.defaulttype.Tree;

/**	A Sketch Item based on a GTree. To draw capsis trees inside sketch viewers.
*	@author F. de Coligny - september 2008
*/
//~ public class GTreeItem extends AbstractItem implements Plant, Speciable, SimpleCrownDescription {
public class GTreeItem extends VertexItem implements Plant, Speciable, SimpleCrownDescription {	 // fc - 5.11.2008
	private Tree tree;

	// All this is in AbstractItem
		//~ protected ItemType type = ItemType.GENERIC;	// fc - 11.6.2007 - must be set correctly in subclasses
		//~ private Key key;		// an item knows its key
		//~ private int itemId;		// fc - 12.3.2007
		//~ private Object externalRef;	// may be a File or an Object (ex: a Capsis GTree)
		//~ // 3D anchor
		//~ private double x;
		//~ private double y;
		//~ private double z;
		//~ // 3D twist
		//~ private double xTwist;
		//~ private double yTwist;
		//~ private double zTwist;
		//~ // Bounding box
		//~ private Vertex3d min;
		//~ private Vertex3d max;
	
	// Speciable
	private Species species;

	// SimpleCrownDescription
	private int crownType;			// CONIC, SPHERIC
	private double crownBaseHeight;	// in m.
	private double crownRadius;		// in m.
	private Color crownColor;		// ex : Color.GREEN, RED
	private float transparency;		// 0.0 (opaque) to 1.0 (transparent)


	public GTreeItem (Tree tree) {
		this.tree = tree;

		//~ setType (BuiltinType.CAPSIS_TREE);
		
		setName (Translator.swap ("BuiltinType.CAPSIS_TREE"));
		type = new BuiltinType (
				"BuiltinType.CAPSIS_TREE",  // name
				TreeSketcher.class);  // preferred sketcher
		
		// fc - 5.11.2008 - no key any more
		//~ setKey (getType ());	// type is also the item key
		
		setItemId (tree.getId ());
		setExternalRef (null);
		if (tree instanceof Spatialized) {
			Spatialized s = (Spatialized) tree;
			setX (s.getX ());
			setY (s.getY ());
			setZ (s.getZ ());
		}	// else x, y, z = 0...
		setXTwist (0d);
		setYTwist (0d);
		setZTwist (0d);
		
		// for bounding box : do we have a crown ?
		Vertex3d min = null;
		Vertex3d max = null;
		double radius = 0;
		double height = 0;
		if (tree instanceof SimpleCrownDescription) {
			SimpleCrownDescription c = (SimpleCrownDescription) tree;
			
			crownType = c.getCrownType ();
			crownBaseHeight = c.getCrownBaseHeight ();
			crownRadius = c.getCrownRadius ();
			crownColor = c.getCrownColor ();
			transparency = 0;  // removed from SimpleCrownProvider, 0: opaque
			
			radius = crownRadius;
			height = tree.getHeight ();
		} else {
			
			crownType = SimpleCrownDescription.SPHERIC;
			crownBaseHeight = tree.getHeight ();
			crownRadius = 0;
			crownColor = Color.GREEN;
			transparency = 0;
			
			double rbh = tree.getDbh ()/2;
			radius = rbh;
			height = tree.getHeight ();
		}

		min = new Vertex3d (getX () - radius, 
				getY () - radius, 
				getZ ());
		max = new Vertex3d (getX () + radius, 
				getY () + radius, 
				getZ () + height);
		
		setRelativeMin (min);
		setRelativeMax (max);
		
		if (tree instanceof Speciable) {
			species = ((Speciable) tree).getSpecies ();
		} else {
			species = null;
		}
		
	}

	// Plant
	public double getDbh () {return tree.getDbh ();}		// cm
	public double getHeight () {return tree.getHeight ();}	// m

	// Speciable
	public Species getSpecies () {return species;}

	// SimpleCrownDescription
	public int getCrownType () {return crownType;}
	public double getCrownBaseHeight () {return crownBaseHeight;}
	public double getCrownRadius () {return crownRadius;}
	public Color getCrownColor () {return crownColor;}
//	public float getTransparency () {return transparency;}

}