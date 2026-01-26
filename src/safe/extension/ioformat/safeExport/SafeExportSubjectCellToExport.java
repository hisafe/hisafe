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


import java.io.Serializable;
import java.util.Collection;

/**
  * SafeExportSubjectCellToExport defines the structure of a cell subject
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExportSubjectCellToExport extends SafeExportSubjectToExport implements Serializable {
	public boolean exportAll;		//all cells selection
	public Collection ids;			//id cells selection

	public SafeExportSubjectCellToExport (String name,SafeExportSubject subject) {
		super (name,subject); }

	public void reset() { exportAll=true;}

	public String toString() {
		StringBuffer sb= new StringBuffer ();
		sb.append(super.toString());
		return sb.toString();
	}

}
