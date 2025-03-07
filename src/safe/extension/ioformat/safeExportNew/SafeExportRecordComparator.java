/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.extension.ioformat.safeExportNew;

import java.util.Comparator;

/**
 * Comparator on module, class and description.
 * The compare method deals with two instances of SafeExportParam.
 *
 * @author R. Tuquet Laburre - july 2003
 */
public class SafeExportRecordComparator implements Comparator {

	public int compare (Object param1, Object param2) throws ClassCastException {
		if (!(param1 instanceof SafeExportRecord)) {
				throw new ClassCastException ("param1 is not a SafeExportRecord : "+param1);}
		if (!(param2 instanceof SafeExportRecord)) {
				throw new ClassCastException ("param2 is not a SafeExportRecord : "+param2);}

		SafeExportRecord p1 = (SafeExportRecord) param1;
		SafeExportRecord p2 = (SafeExportRecord) param2;

		if (p1.getKey1() < p2.getKey1()) { return -1; }
		if (p1.getKey1() > p2.getKey1()) { return 1; }
		if (p1.getKey2() < p2.getKey2()) { return -1; }
		if (p1.getKey2() > p2.getKey2()) { return 1; }
		if (p1.getKey3() < p2.getKey3()) { return -1; }
		if (p1.getKey3() > p2.getKey3()) { return 1; }

		return - p1.getRecord().compareToIgnoreCase(p2.getRecord());
	}

}
