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
import java.util.Vector;

import jeeb.lib.util.Translator;

/**
  * SafeExportProfile contains structure of th export profile file
  *
  * @author R. TUQUET LABURRE - august 2003
  */

public class SafeExportProfile implements Serializable {

	public boolean	extendAllSteps;
	public boolean	extendCurrentStep;
	public boolean	extendFrequency;
	public int 		extendFrequencyBegin;
	public int 		extendFrequencyValue;
	public boolean 	extendPeriod;
	public int 		extendPeriodTo;
	public int 		extendPeriodFrom;
	public boolean	extendRootStepToExport;

	public Vector subjectsToExport;

	public SafeExportSubjectToExport getSubjectToExport(String name) {
		if (subjectsToExport!=null) {
			for (int i=0;i<subjectsToExport.size();i++) {
				SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) subjectsToExport.get(i);
				if (subjectToExport.getName().equals(name)) {
					return subjectToExport;
				}
			}
		}
		return null;
	}

	public SafeExportProfile () {
		reset();
	}

	public void setSubjectsOfSubjectsToExport(Vector subjects) {


		if (subjectsToExport!=null) {
			for (int s=0;s<subjectsToExport.size();s++) {
				SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) subjectsToExport.get(s);
				String subjectName = subjectToExport.getName();
				for (int i=0;i< subjects.size();i++) {
					String subjectName2 = ((SafeExportSubject) subjects.get(i)).getName();
					if (subjectName2.equals(subjectName)) {
							subjectToExport.setSubject((SafeExportSubject) subjects.get(i));
							subjectsToExport.set(s, subjectToExport);
							i=subjects.size()+1;
					}
				}
			}
		}
	}

	private void reset () {
		extendAllSteps=true;
		extendCurrentStep=false;
		extendFrequency=false;
		extendPeriod=false;
		extendRootStepToExport=true;
	}

	public int getCountOfSubjectsToExport() {
		if (subjectsToExport==null) {
			return 0;
		}
		int count=0;
		for (int i=0;i<subjectsToExport.size();i++) {
			SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) subjectsToExport.get(i);
			if (subjectToExport!=null && subjectToExport.getSafeExportVariables()!=null) {
				count++;
			}
		}
		return count;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer ("");
		sb.append (Translator.swap ("SafeExport.extend") + " : ");
		if (extendAllSteps) {
			sb.append (Translator.swap ("SafeExport.extendAllSteps"));
		} else  if (extendCurrentStep) {
			sb.append (Translator.swap ("SafeExport.extendCurrentStep"));
		} else if (extendFrequency) {
			sb.append (Translator.swap ("SafeExport.extendFrequency")+" : ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyBegin")+" = ");
			sb.append (extendFrequency+ " ; ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyValue") + " = ");
			sb.append (extendFrequencyValue + " ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyUnity"));
		} else if (extendPeriod) {
			sb.append (Translator.swap ("SafeExport.extendPeriod")+" : ");
			sb.append (Translator.swap ("SafeExport.extendPeriodFrom")+" ");
			sb.append (extendPeriodFrom+ " ");
			sb.append (Translator.swap ("SafeExport.extendPeriodTo")+" ");
			sb.append (extendPeriodTo);
		}
		sb.append ("\n");
		sb.append ("> " + Translator.swap ("SafeExport.extendRootStepToExport")+ " : ");
		if (extendRootStepToExport)
			sb.append (Translator.swap("Shared.yes"));
		else
			sb.append (Translator.swap("Shared.no"));
		sb.append ("\n");
		for (int i=0; i<subjectsToExport.size();i++) {
			SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) subjectsToExport.get(i);
			if (subjectToExport!=null) {
					sb.append(subjectsToExport.toString());
				}
		}
		return sb.toString ();
	}

}
