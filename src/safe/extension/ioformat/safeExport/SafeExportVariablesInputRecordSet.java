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

import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * SafeExportVariablesInputRecordSet : load the file variables
 *
 * @author R.TUQUET LABURRE - august 2003
 */
public class SafeExportVariablesInputRecordSet extends RecordSet {

	private TreeSet variables;

	@Import
	static public class VariableRecord extends Record {
		public VariableRecord () {super ();}
		public VariableRecord (String line) throws Exception {super (line);}

		public String subject;
		public int order;
		public String variable;
		public String frenchUnity;
		public String englishUnity;
		public String frenchDescription;
		public String englishDescription;
	
	}

	/**
	 * Read a variable file and extract variable records. Records are sorted.
	 * <pre>
	 * Use
	 * new SafeExportVariablesInputRecordSet (fileName).getVariables ();
	 * </pre>
	 */
	public SafeExportVariablesInputRecordSet (String fileName) throws Exception{
		prepareImport (fileName);
		load();
	}

	public Collection getVariables () { return variables; }
	//
	/**
	 * RecordSet -> VariableRecord
	 * Implementation here.
	 */
	private void load () throws Exception		{
		variables = new TreeSet (new SafeExportVariablesComparator ());
		for (Iterator i = this.iterator (); i.hasNext ();) {
			VariableRecord r = (VariableRecord) i.next ();
			SafeExportVariable p = new SafeExportVariable(r.subject, r.order, r.variable,
					r.frenchUnity,r.englishUnity,r.frenchDescription, r.englishDescription);
			variables.add (p);
		}
    }
}
