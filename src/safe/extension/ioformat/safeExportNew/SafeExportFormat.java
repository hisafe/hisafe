/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Gr�goire Talbot 1, Nicolas Barbault 1, 
 * Gr�goire Vincent 2 , Rachmat Mulia 3, Fran�ois Bussi�re 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Herv� Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Universit� Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d��tude de la for�t, Universit� du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d�H�res, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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

import java.util.ArrayList;
import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;


/**
 * Records description for  EXPORT
 *
 * @author Isabelle Lecomte - September 2020
 */
public class SafeExportFormat extends RecordSet {

	private static final long serialVersionUID = 1L;
	@Import
	static public class ExportRecord extends Record {
		private static final long serialVersionUID = 1L;
		public ExportRecord () {super ();}
		public ExportRecord (String line) throws Exception {super (line);}
		public String subject;			//subject : annualCell/cell/plot 
		public String object;			//SafeTree, SafeCell 
		public String name;				//name of the variable
		public String unit;				//unit
		public String description;		//description
		public int order;				//order in output
		public int export;				//export ? 0=no 1=yes
	}

	@Import
	static public class Subject1Record extends Record {
		private static final long serialVersionUID = 1L;
		public Subject1Record () {super ();}
		public Subject1Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
	}
	
	@Import
	static public class Subject2Record extends Record {
		private static final long serialVersionUID = 1L;
		public Subject2Record () {super ();}
		public Subject2Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
		public String ids;				//list of ids
	}
	
	@Import
	static public class Subject3Record extends Record {
		private static final long serialVersionUID = 1L;
		public Subject3Record () {super ();}
		public Subject3Record (String line) throws Exception {super (line);}
		public String keyWord;			//keyword ProfileDef
		public String subject;			//subject to export
		public String object;			//SafeTree, SafeCell 
		public int frequency;			//export frequency : 1 30 365
		public String ids;				//list of ids
		public String depth;			//list of depth
	}
	
	// fc-12.10.2020 createRecordSet(fileName) was renamed prepareImport (fileName) for clarity
	public SafeExportFormat (String exportFileName) throws Exception {prepareImport (exportFileName);}
//	public SafeExportFormat (String exportFileName) throws Exception {createRecordSet (exportFileName);}

	/**
	 * Load RecordSet -> SafeExportFormat
	 */
	public void load(ArrayList<SafeExportProfile> exports, String outputDir, String projectName) throws Exception {


		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			//SUBJECTS : tree, cells, voxels...
			if (record instanceof SafeExportFormat.Subject1Record) {
				
				SafeExportFormat.Subject1Record cr =
							(SafeExportFormat.Subject1Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency);
				exports.add(s);
			}
			else if (record instanceof SafeExportFormat.Subject2Record) {
				
				SafeExportFormat.Subject2Record cr =
							(SafeExportFormat.Subject2Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency, cr.ids);
				exports.add(s);
			}
			else if (record instanceof SafeExportFormat.Subject3Record) {
				
				SafeExportFormat.Subject3Record cr =
							(SafeExportFormat.Subject3Record) record;	
				SafeExportProfile s = new SafeExportProfile(outputDir, projectName, cr.subject, cr.object, cr.frequency, cr.ids, cr.depth);
				exports.add(s);							
			}
			//VARIABLES : name, description, unit
			else if (record instanceof SafeExportFormat.ExportRecord) {
		
				SafeExportFormat.ExportRecord cr =
							(SafeExportFormat.ExportRecord) record;
		
				if (cr.export==1){		//if variable is selected to be exported

				   for (SafeExportProfile e : exports) {
				        if (e.getSubject().equals(cr.subject)) {
				            e.addVariable(cr.name);
				        }
				    }					   
				}
			}
			else {
				throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
			}		
		}
	}
}
