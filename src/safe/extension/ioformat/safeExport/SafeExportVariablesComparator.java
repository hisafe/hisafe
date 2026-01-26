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

package safe.extension.ioformat.safeExport;

import java.util.Comparator;

/**
 * SafeExportVariablesComparator : compare 2 SafeExportVariables
 *
 * @author R.TUQUET LABURRE - august 2003
 */

public class SafeExportVariablesComparator implements Comparator {

	public int compare (Object variable1, Object variable2) throws ClassCastException {
		if (!(variable1 instanceof SafeExportVariable)) {
				throw new ClassCastException ("variable1 is not a SafeExportVariable : "+variable1);}
		if (!(variable2 instanceof SafeExportVariable)) {
				throw new ClassCastException ("variable2 is not a SafeExportVariable : "+variable2);}

		SafeExportVariable v1 = (SafeExportVariable) variable1;
		SafeExportVariable v2 = (SafeExportVariable) variable2;

		String s1 = v1.getDescription ().toLowerCase();
		String s2 = v2.getDescription ().toLowerCase();

		return s1.compareToIgnoreCase (s2);

	}

	public boolean equals (Object o) {return this.equals (o);}

}
