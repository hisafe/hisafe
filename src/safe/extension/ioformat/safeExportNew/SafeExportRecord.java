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

package safe.extension.ioformat.safeExportNew;



/**
  * SafeExportRecord contains 3 keys using sort the string record
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExportRecord {

	private int key1;
	private int key2;
	private int key3;
	private String record;


	public SafeExportRecord(int key1,String record) {
		this.key1=key1;
		this.record=record;
	}

	public SafeExportRecord(int key1, int key2,String record) {
		this.key1=key1;
		this.key2=key2;
		this.record=record;
	}

	public SafeExportRecord(int key1, int key2, int key3 ,String record) {
		this.key1=key1;
		this.key2=key2;
		this.key3=key3;
		this.record=record;
	}

	public int getKey1 () {return key1;}
	public int getKey2 () {return key2;}
	public int getKey3 () {return key3;}
	public String getRecord () {return record;}
	public boolean equals(SafeExportRecord record) {
		if ( this.getKey1 () == record.getKey1 ()  &&
			 this.getKey2 () == record.getKey2 ()  &&
			 this.getKey3 () == record.getKey3 ()  &&
			 this.getRecord ().equals(record.getRecord ()) ) {
				return true;
		}
		return false;
	}

}
