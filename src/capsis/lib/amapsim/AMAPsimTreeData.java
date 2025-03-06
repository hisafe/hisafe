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
import java.util.Locale;

/**
 * AMAPsimTreeData defines some additive tree data calculated 
 * by AMAPsim during connexion with the AMAPsim modeltool 
 * extension. It is an opptional complement to trees description for 
 * modules. Such trees can be tartinated after "mode 2" connexion 
 * for further analysis.
 * 
 * @author F. de Coligny - december 2002 / january 2004
 */
public class AMAPsimTreeData implements Serializable {

	public String fileName;				// 21.1.2004 - from AMAPsimTree
	public AMAPsimTreeStep treeStep;	// 21.1.2004


	public String toString () {	// AMAPsimTreeData age=5
		StringBuffer b = new StringBuffer ();
		b.append ("AMAPsimTreeData ");
		if (treeStep != null) {
			b.append ("age=");
			b.append (treeStep.age);
		} else {
			b.append ("age=? (treeStep=null)");
		}
		return b.toString ();
	}

	private static NumberFormat style = NumberFormat.getNumberInstance (Locale.ENGLISH);
	static {
		style.setMaximumFractionDigits (2);
		style.setGroupingUsed (false);
	}

}


