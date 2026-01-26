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


import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * SafeExportSubjectsInputRecordSet : load the file subjects
 *
 * @author R.TUQUET LABURRE - august 2003
 */
public class SafeExportSubjectsInputRecordSet extends RecordSet {

	private Vector subjects;

	@Import
	static public class SubjectRecord extends Record {
		public SubjectRecord () {super ();}
		public SubjectRecord (String line) throws Exception {super (line);}

		public String name;
		public String frenchName;
		public String englishName;
	}

	/**
	 * Read a subject file and extract subjects records. Records are NOT sorted.
	 * <pre>
	 * Use
	 * new SafeExportSubjectsInputRecordSet (fileName).getSubjects (variables);
	 * </pre>
	 */
	public SafeExportSubjectsInputRecordSet (String fileName,Collection variables) throws Exception {
		prepareImport (fileName);
		load(variables);
	}

	public Vector getSubjects () { return subjects; }

	//
	/**
	 * RecordSet -> SubjectRecord
	 * put in map subjects, only the subjects which have variables
	 */
	private void load (Collection variables) throws Exception {
		this.subjects = new Vector();
		ArrayList subjectVariables = new ArrayList ();
		int cpt=0;
		for (Iterator it = this.iterator (); it.hasNext ();) {
			SubjectRecord r = (SubjectRecord) it.next ();
			subjectVariables = new ArrayList ();
			for (Iterator it2=variables.iterator (); it2.hasNext ();) {
				SafeExportVariable variable = (SafeExportVariable) it2.next ();
				if (variable.getSubject().equals (r.name)) {
					subjectVariables.add (variable);
				}
			}
			if (subjectVariables.size()>0) {
				SafeExportSubject subject = new SafeExportSubject(r.name,
				r.frenchName, r.englishName, subjectVariables);
				subjects.add (cpt++,subject);
			}
		}
    }

}
