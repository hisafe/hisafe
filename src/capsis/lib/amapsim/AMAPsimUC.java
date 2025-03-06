/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2001  Francois de Coligny
 * 
 * This program is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or 
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation, 
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
 */

package capsis.lib.amapsim;

import java.io.Serializable;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.Locale;

/**
 * AMAPsimUC defines an AMAPsim growth unit.
 * 
 * @author F. de Coligny - december 2002 / january 2004
 */
public class AMAPsimUC implements Serializable {

	public float topHeight;
	public int numberOfRings;
	public Collection rings;					// 19.1.2004


	public String toString () {	// AMAPsimUC height= 12.3 (rings=5)
		StringBuffer b = new StringBuffer ();
		b.append ("AMAPsimUC height=");
		b.append (style.format (topHeight));
		b.append (" (rings=");
		b.append (style.format (numberOfRings));
		b.append (")");
		return b.toString ();
	}

	private static NumberFormat style = NumberFormat.getNumberInstance (Locale.ENGLISH);
	static {
		style.setMaximumFractionDigits (2);
		style.setGroupingUsed (false);
	}

}


