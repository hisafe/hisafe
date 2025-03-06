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


//~ import capsis.lib.amapsim.*;
//~ import capsis.extension.*;
//~ import capsis.kernel.*;
//~ import capsis.gui.*;
//~ import  capsis.util.*; import jeeb.lib.util.*;
//~ import java.util.*;
//~ import java.io.*;
//~ import java.awt.*;
//~ import java.awt.event.*;
//~ import java.awt.geom.*;
//~ import javax.swing.*;
//~ import javax.swing.event.*;
//~ import javax.swing.border.*;

/**	Location for a subject.
*	Ex of use : new Location (x, y, pp3Tree);
*	@author F. de Coligny - june 2005
*/
public class Location {

	public double x;
	public double y;
	public Object subject;
	
	
	/**	Register a lollipop for the given class of subject (tree)
	*/
	public Location (double x, double y, Object subject) {
		this.x = x;
		this.y = y;
		this.subject = subject;
	}
	
	
}




