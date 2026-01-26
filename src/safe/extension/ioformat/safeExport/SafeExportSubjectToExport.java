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
  * SafeExportSubjectToExport defines the structure of a subject
  * to export (stored in a profile's file)
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExportSubjectToExport implements Serializable  {

	private String name;
	private String[] variables;
	private transient SafeExportSubject subject;
	// Constructor

	public SafeExportSubjectToExport (String name,SafeExportSubject subject) {
	this.name=name;
	this.subject=subject;
	}

	public void reset() {;}

	// set accessors
	public void setName(String name) { this.name=name; }
	public void setSubject(SafeExportSubject subject) { this.subject=subject; }
	public void setVariables (String[] variables) { this.variables=variables; }

	// get accessors
	public String getName() {return name;}
	public SafeExportSubject getSubject () {return subject;}
	public String[] getVariables () { return variables; }
	public SafeExportVariable[] getSafeExportVariables() {
		Vector vVariables=new Vector();
		for (int i=0;i< this.variables.length;i++) {
			String savedVariable = variables[i];
			SafeExportVariable variable = SafeExportVariable.getVariableWithSavedString(savedVariable,subject.getVariables());
			if (variable!=null) {
				vVariables.add(variable);
			}
		}
		if (vVariables.size()>0) {
			SafeExportVariable[] variables=new SafeExportVariable[vVariables.size()];
			for (int i=0;i< vVariables.size();i++) {
				variables[i] = (SafeExportVariable) vVariables.get(i);
			}
			return variables;
		}
		return null;
	}

	public String toString () {
		StringBuffer sb=new StringBuffer();
		sb.append (Translator.swap ("SafeExport.subject") + " : ");
		Object[] variables;
		if (subject!=null) {
			sb.append(subject.getLocalName()+"\n");
			variables = getSafeExportVariables();
		}
		else {
			sb.append("subject is not defined !\n");
			variables = this.variables;
		}
		sb.append (Translator.swap ("SafeExport.variables")
			+ " ("+variables.length+")\n");
		for (int i=0 ; i < variables.length ; i++) {
			sb.append ((i+1) + ". " + variables[i].toString ()+"\n");
		}
		return sb.toString();
	}

	public void addVariable(SafeExportVariable variable,int position) {
		subject.addVariable(variable,position);
		Vector vVariables = new Vector();
		if (position>= variables.length) {
			position=variables.length-1;
		}
		for (int i =0;i<variables.length;i++) {
			if (i==position) {
				vVariables.add(variable.getStringToSave());
			}
			vVariables.add(variables[i]);
		}
		variables = new String[variables.length+1];
		for (int i=0 ; i < variables.length ; i++) {
			variables[i] = (String) vVariables.get(i);
		}
	}
}
