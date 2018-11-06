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
import java.util.Locale;
import java.util.Vector;

import jeeb.lib.util.Translator;


/**
  * SafeExportSubject defines the structure of a subject
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExportSubject {

	private String name;
	private String frenchName;
	private String englishName;
	private SafeExportVariable[] variables;

	// Constructor
	public SafeExportSubject (String name,
		String frenchName,String englishName,
		Collection variables)	{
		this.name=name;
		this.frenchName=frenchName;
		this.englishName=englishName;
		setVariables(variables);
	}

	// Get accessors
	public String getName () {return name;}
	public String getLocalName () {
		if (Locale.getDefault ().equals (Locale.FRENCH)) {
			return frenchName;
		} else {
			return englishName;
		}
	}
	public SafeExportVariable[] getVariables () { return variables; }

	public String[] getVariableItemsForList () {
		String ret[]= new String[variables.length];
		for (int i=0 ; i < variables.length ; i++) {
			ret[i] = variables[i].getStringForList();
		}
		return ret;
	}

	public String[] getVariableItemsToSave () {
		String ret[]= new String[variables.length];
		for (int i=0 ; i < variables.length ; i++) {
			ret[i] = variables[i].getStringToSave();
		}
		return ret;
	}

	public String getVariableItemForList (String savedItem) {
		for (int i=0 ; i < variables.length ; i++) {
			if (variables[i].getStringToSave().equals(savedItem)) {
				return variables[i].getStringForList();
			}
		}
		return null;
	}

	public String getVariableItemToSave (String listItem) {
		for (int i=0 ; i < variables.length ; i++) {
			if (variables[i].getStringForList().equals(listItem)) {
				return variables[i].getStringToSave();
			}
		}
		return null;
	}

	public String toString () {
		StringBuffer sb = new StringBuffer ();
		sb.append (Translator.swap ("SafeExport.subject")
			+ " : " + getName ()+ " (" + getLocalName() + ")\n");
		sb.append (Translator.swap ("SafeExport.variables")
			+ " ("+variables.length+")\n");
		for (int i=0 ; i < variables.length ; i++) {
			sb.append ((i+1) + ". " + variables[i].toString ());
		}
		return sb.toString ();
	}

	private void setVariables(Collection colVariables) {
		variables = new SafeExportVariable[0];
		variables= (SafeExportVariable[]) colVariables.toArray (variables);
	}

	public void addVariable(SafeExportVariable variable,int position) {
		Vector vVariables = new Vector();
		if (position>= variables.length) {
			position=variables.length-1;
		}
		for (int i =0;i<variables.length;i++) {
			if (i==position) {
				vVariables.add(variable);
			}
			vVariables.add(variables[i]);
		}
		variables = new SafeExportVariable[variables.length+1];
		for (int i=0 ; i < variables.length ; i++) {
			variables[i] = (SafeExportVariable) vVariables.get(i);
		}
	}


}
