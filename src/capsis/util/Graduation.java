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


/**
 * A graduation : an anchor and a label.
 * 
 * @author F. de Coligny - august 2001
 */
public class Graduation {
	public double anchor;
	public String label;
	public Graduation (double anchor, String label) {
		this.anchor = anchor;
		this.label = label;
	}
	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("x=");
		b.append (anchor);
		b.append (" label=");
		b.append (label);
		return b.toString ();
	}
}

