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
import java.util.Iterator;

import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;

/**	SpatializedObject: a subject (ex: a tree instance) with a Vertex3d location.
*
*	@author F. de Coligny - march 2006
*/
public class SpatializedObject extends Vertex3d implements Spatialized {
	private Object object;
	private boolean selected;

	public SpatializedObject (Object object, double x, double y, double z) {
		super (x, y, z);
		this.object = object;
		this.selected = false;	// fc - 5.12.2006
	}

	/**	Create a new location, pointing to the given Spatialized (ex: some tree)
	*/
	public SpatializedObject (Spatialized object) {	// ex: a GMaddTree
		super (object.getX (), object.getY (), object.getZ ());
		this.object = object;
	}
	
	/**	Make a copy of the location, pointing to the same Object (ex: same tree)
	*/
	public SpatializedObject (SpatializedObject so) {	// fc - 5.12.2006
		super (so.getX (), so.getY (), so.getZ ());
		this.object = so.getObject ();
		this.selected = so.isSelected ();	// fc - 5.12.2006
	}

	public void setSelected (boolean v) {selected = v;}
	public boolean isSelected () {return selected;}

	public Object getObject () {return object;}
	
	// fc - 19.6.2006 - Spatialized interface
	public double getX () {return x;}
	public double getY () {return y;}
	public double getZ () {return z;}
	// fc - 6.11.2009 - added the setters
	public void setX (double v) {x = v;}
	public void setY (double v) {y = v;}
	public void setZ (double v) {z = v;}
	public void setXYZ (double x, double y, double z) {
		setX (x);
		setY (y);
		setZ (z);
	}

	/**	Can be used to re-spatialize an object when needed.
	*	Possibly to center a scene. fc - 25.9.2006
	*/
	public void set (Object o, double x, double y, double z, boolean selected) {
		this.object = o;
		this.x = x;
		this.y = y;
		this.z = z;
		this.selected = selected;
	}
	
	// equals redefinition for SpatializedObject
	public boolean equals (Object o) {
		try {
//~ System.out.println ("SpatializedObject.equals (): this="+this
		//~ +"\n"+"other="+o);
			SpatializedObject so = (SpatializedObject) o;
//~ System.out.println ("  returned "+((x == so.x) && (y == so.y) && (z == so.z)));
			//~ return x == so.x && y == so.y && z == so.z && object == so.getObject ();
			return (x == so.x) && (y == so.y) && (z == so.z);
		} catch (Exception e) {
//~ System.out.println ("SpatializedObject.equals (): exception "+e);
			return false;
		}
	}
	
	/**	Return a collection of spatialized objects. 
	*	If the given collection is already so, return it directly 
	*	else create a collection with one spatialized object for each
	*	element in the given collection.
	*	The parameter must contain objects instanceof SpatializedObject 
	*	or Spatialized.
	*/
	static public Collection<SpatializedObject> 
			checkCollection (Object c) throws Exception{	// fc - 3.10.2006
		
		Collection <SpatializedObject> res = new ArrayList<SpatializedObject> ();
		if (c == null) {return res;}
		if (!(c instanceof Collection)) {
			Collection aux = new ArrayList ();
			aux.add (c);
			c = aux;
		}
		
		Collection source = (Collection) c;
		
		// Possibly a ClassCastException -> Exception will be thrown to caller
		for (Iterator i = source.iterator (); i.hasNext ();) {
			Object o = i.next ();
			if (o instanceof SpatializedObject 
					|| o instanceof Spatialized) {
				res.add ((o instanceof SpatializedObject) 
						? (SpatializedObject) o 
						: new SpatializedObject ((Spatialized) o));
			} else {
				// "remove" o
			}
		}
		return res;
	}

	/**	Return a collection of spatialized objects. 
	*/
	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("SpatializedObject x=");
		b.append (getX ());
		b.append (" y=");
		b.append (getY ());
		b.append (" z=");
		b.append (getZ ());
		b.append (" selected=");
		b.append (isSelected ());
		b.append (" object=");
		b.append (getObject ());
		return b.toString ();
		
	}
	
}
