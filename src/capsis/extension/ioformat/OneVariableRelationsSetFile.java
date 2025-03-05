/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.ioformat;

// This extension is for regelight model.
// It may become usable by other models: implement other constructors
// ex: public OneVariableRelationsSetFile (MobyDickStand stand) throws Exception {
// with: import mobydick.model.*;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.lib.math.OneVariableRelationsSet;
import capsis.util.StandRecordSet;

/**
 * OneVariableRelationsSetFile is an io extension to :
 * import following One Variable relations sets:
 * ex : Crown Area = f(dbh) (by species)
 *
 * @author A. Piboule - March 2004
 */
public class OneVariableRelationsSetFile extends StandRecordSet {
	// Here This (us) is a Vector !!!

	// fc-4.12.2020 Checked, not an export, no need for OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.OneVariableRelationsSetFile");
	}

	// Generic keyword record is described in superclass: key = value


	// 4 Import relation module record is described here
	@Import
	static public class RegModuleRecord extends Record {
		public RegModuleRecord () {super ();}
		public RegModuleRecord (String line) throws Exception {super (line);}
		//public String getSeparator () {return ";";}	// to change default "\t" separator
		public String li; // line number
		public String be; // before variable
		public String op; // operator
		public String af; // after variable

	}

	// 2 Import parameter settings is described here
	@Import
	static public class RegParamRecord extends Record {
		public RegParamRecord () {super ();}
		public RegParamRecord (String line) throws Exception {super (line);}
		//public String getSeparator () {return ";";}	// to change default "\t" separator
		public String name; // parameter name
		public double value; // parameter value

	}



	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();

		} catch (Exception e) {
			Log.println (Log.ERROR, "OneVariableRelationsSetFile.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	// fc-8.12.2020 Not an OFormat any more
//	/**
//	 * From Extension interface.
//	 */
//	@Override
//	public String getName () {
//		return Translator.swap ("OneVariableRelationsSetFile.name");
//	}
//
//	/**
//	 * From Extension interface.
//	 */
//	@Override
//	public String getAuthor () {
//		return "A. Piboule";
//	}
//
//	/**
//	 * From Extension interface.
//	 */
//	@Override
//	public String getDescription () {
//		return Translator.swap ("OneVariableRelationsSetFile.description");
//	}
//
//	/**
//	 * From Extension interface.
//	 */
//	@Override
//	public String getVersion () {
//		return "1.0";
//	}
	
	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	//
	// RecordSet -> File
	// is described in superclass (save (fileName)).
	//

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public OneVariableRelationsSetFile () {}

	/**
	 * File -> RecordSet
	 * is delegated to superclass.
	 */
	//public OneVariableRelationsSetFile (String fileName) throws Exception {super (fileName);}
	public OneVariableRelationsSetFile (String fileName) throws Exception {prepareImport (fileName);}	// for direct use for Import
	public void createRecordSet (String fileName) throws Exception {super.prepareImport (fileName);}

	/**
	 * RegStand -> RecordSet.
	 * Implementation here.
	 * To create a RecordSet for another model: implement an other construtor
	 * ex: public OneVariableRelationsSetFile (MobyDickStand stand) throws Exception ...
	 */
	@Override
	public void prepareExport (GScene stand) throws Exception {
		// disable Header for exportation
		setHeaderEnabled (false);

	}

	/**
	 * RecordSet -> RegStand
	 * Implementation here.
	 * Was initialy described in RegModel.loadInitStand ()
	 * To load a stand for another model, recognize real type of model :
	 * if (model instanceof RegModel) -> this code
	 * if (model instanceof MobyDickModel) -> other code...
	 */
	public GScene load (GModel model) throws Exception {return null;}
	// Unused, replaced by following

	public OneVariableRelationsSet load () throws Exception {

		// Initializations
		OneVariableRelationsSet alloRel = new OneVariableRelationsSet ();

		Integer modality = null;
		OneVariableRelationsSet.Relation relation = null;


		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();


			// keys
			if (record instanceof OneVariableRelationsSetFile.KeyRecord) {
				OneVariableRelationsSetFile.KeyRecord r = (OneVariableRelationsSetFile.KeyRecord) record;	// cast to precise type
				String key = r.key;

				if (key.toLowerCase ().equals ("relation") || key.toLowerCase ().equals ("rel")) {
					String relName = r.value;
					if (relName.length ()==0) {
						System.err.println ("No relation specified");
						relation = null;
					} else {
						relation = alloRel.addRelation (relName);
					}

				} else if (key.toLowerCase ().equals ("modality") || key.toLowerCase ().equals ("mod")) {
					try {
						modality = new Integer (r.value);
						relation.addModality (modality);
					} catch (Exception ex) {
						modality = null;
						System.err.println ("ERROR with key: "+r.value+" (Not valid Modality: must be an integer value)");
					}
				}

			} else if (record instanceof OneVariableRelationsSetFile.RegModuleRecord) {
				OneVariableRelationsSetFile.RegModuleRecord r = (OneVariableRelationsSetFile.RegModuleRecord) record;	// cast to precise type

				if ((relation!=null)) {
					relation.addModule (r.li, r.be, r.op, r.af);
				} else {
					System.err.println ("ERROR with record: "+r+" (No Relation Specified)");
				}

			} else if (record instanceof OneVariableRelationsSetFile.RegParamRecord) {
				OneVariableRelationsSetFile.RegParamRecord r = (OneVariableRelationsSetFile.RegParamRecord) record;	// cast to precise type

				if ((relation!=null) && (modality!=null)) {
					relation.addVariable (modality, r.name, r.value);
				} else {
					System.err.println ("ERROR with record: "+r+" (No Relation or Modality Specified)");
				}

			} else {
				throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
			}
		}

		return alloRel;
	}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return true;}
//	public boolean isExport () {return false;}

}
