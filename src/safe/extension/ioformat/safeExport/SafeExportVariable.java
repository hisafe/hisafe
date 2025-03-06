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

import java.util.Locale;

/**
  * SafeExportVariable defines the structure of a variable which could be exported
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExportVariable {

	private String subject;
	private String variable;
	private String frenchUnity;
	private String englishUnity;
	private String frenchDescription;
	private String englishDescription;
	private int order;

	public SafeExportVariable(String subject,String variable,String frenchUnity,
			String englishUnity,String frenchDescription, String englishDescription)	{
		this.subject=subject;
		this.variable=variable;
		this.frenchUnity=frenchUnity;
		this.englishUnity=englishUnity;
		this.frenchDescription=frenchDescription;
		this.englishDescription=englishDescription;
		this.order = 0;
	}
	
	public SafeExportVariable(String subject, int order, String variable,String frenchUnity,
			String englishUnity,String frenchDescription, String englishDescription
			)	{
		this.subject=subject;
		this.variable=variable;
		this.frenchUnity=frenchUnity;
		this.englishUnity=englishUnity;
		this.frenchDescription=frenchDescription;
		this.englishDescription=englishDescription;
		this.order = order;
	}

	public int getOrder () {return order;}
	public String getSubject () {return subject;}
	public String getVariable () {return variable;}
	public String getUnity () {
		if (Locale.getDefault ().equals (Locale.FRENCH)) {
			return frenchUnity;
		} else {
			return englishUnity;
		}
	}
	public String getDescription () {
		if (Locale.getDefault ().equals (Locale.FRENCH)) {
			return frenchDescription;
		} else {
			return englishDescription;
		}
	}

	public String toString () {
		return  getVariable () + " [" +getUnity () + "] - " + getDescription ();
		}
	
	public String getStringToSave () {
		return getSubject () + "-" + getVariable () ;
	}
	
	public String getStringForList () {
		StringBuffer sb =new StringBuffer ("");
		sb.append( getDescription ());
		if (getUnity ()!=null && getUnity ().length()!=0 && !getUnity ().equals ("-")) {
			sb.append (" ("+getUnity ()+")");
		}
		return sb.toString();
	}

	public boolean equals (SafeExportVariable variable) {
		if ( this.subject.equals (variable.getSubject ()) &&
			 this.variable.equals (variable.getVariable ()) ) {
				return true;
		}
		return false;			
	}
	
	public static String getStringToSave(SafeExportVariable[] variables,String listItem) {
		for (int i=0; i < variables.length ; i++) {
			if (variables[i].getStringForList()==listItem) {
				return variables[i].getStringToSave();
			}
		}
		return null;
	}
	
	public static SafeExportVariable getVariableWithSavedString(String savedVariable, SafeExportVariable[] variables) {
		if (variables!=null) {
			for (int i=0;i<variables.length;i++) {
				if (variables[i].getStringToSave().equals(savedVariable)) {
					return variables[i];
				}
			}
		}
		return null;	
	}
}
