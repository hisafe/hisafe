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

package capsis.extension.ioformat;
//~ import mountain.model.*;

import java.util.Collection;
import java.util.Iterator;

import capsis.app.CapsisExtensionManager;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.IdCard;
import capsis.kernel.Project;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * CompatibleExtensions : a file with lists of compatible extensions per extension type.
 *
 * @author F. de Coligny - January 2003
 */
public class CompatibleExtensions extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat
 
	static {
		Translator.addBundle("capsis.extension.ioformat.CompatibleExtensions");
	} 
	
	// Generic keyword record is described in superclass: key = value
	
	// Custom tree record is described here
	@Import
	static public class FamilyLine extends Record {
		public FamilyLine () {super ();}
		public FamilyLine (String line) throws Exception {super (line);}
		public String familyName;
		public String familyPackage;
	}

	// Custom cell record is described here
	@Import
	static public class ExtensionLine extends Record {
		public ExtensionLine () {super ();}
		public ExtensionLine (String line) throws Exception {super (line);}
		public String littleClassName;
		public String version;
		public String author;
		public String translatedName;
	}


	/**
	 * Phantom constructor. 
	 * Only to ask for extension properties (authorName, version...).
	 */
	public CompatibleExtensions () {
		setHeaderEnabled (false);
	}

	

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "CompatibleExtensions.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/** 
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("CompatibleExtensions.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("CompatibleExtensions.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";
	
	//
	private void createLines (ExtensionManager extMan, GModel model, String extensionType) throws Exception {
		// to get family package name, look for one representative extension
		Collection classNames = extMan.getExtensionClassNames (extensionType);
		String className = null;
		
		Iterator z = classNames.iterator ();
		if (z.hasNext ()) {
			className = (String) z.next ();
		}
		
//~ System.out.println ("familyName="+extensionType);
		FamilyLine r0 = new FamilyLine ();
		r0.familyName = extensionType;
		if (className != null) {
			r0.familyPackage = AmapTools.getPackageName (className);
		}
		add (r0);	// family line
		add (new EmptyRecord ());	// a blank line
		
		boolean none = true;
		classNames = extMan.getExtensionClassNames (extensionType);
		for (Iterator i = classNames.iterator (); i.hasNext ();) {	// extensions lines
			className = (String) i.next ();
			try {
				if (extMan.isCompatible (className, model)) {
					none = false;
//~ System.out.println ("className="+className);
					ExtensionLine r = new ExtensionLine ();
					r.littleClassName = AmapTools.getClassSimpleName (className);
					r.version = ExtensionManager.getVersion (className);
					r.author = ExtensionManager.getAuthor (className);
					r.translatedName = ExtensionManager.getName (className);
					add (r);
				}
			} catch (Exception e) {
//~ System.out.println ("-> exception:"+e);
			}
		}
		if (none) {
			add (new FreeRecord ("no compatible extensions"));
		}
			
		add (new EmptyRecord ());	// a blank line
		
	}

	/**
	 * Export: Stand -> RecordSet - Implementation here. 
	 * (RecorSet -> File in superclass)
	 */
	@Override
	public void prepareExport (GScene stand) throws Exception {
		//~ super.createRecordSet (stand);		// deals with RecordSet's source
		
		Project scenario = stand.getStep ().getProject ();
		GModel model = scenario.getModel ();
		
		ExtensionManager extMan = CapsisExtensionManager.getInstance ();
		
		IdCard idCard = model.getIdCard ();
		//~ add (new CommentRecord ("Capsis "+Engine.getVersion ()+" "+new Date ()));
		add (new CommentRecord ("Compatible extensions for module "+idCard.getModelPackageName ()
				+" ("+idCard.getModelName ()+" by "+idCard.getModelAuthor ()+")"));
		add (new EmptyRecord ());	// a blank line
		
		
		createLines (extMan, model, CapsisExtensionManager.STAND_VIEWER);
		createLines (extMan, model, CapsisExtensionManager.DATA_EXTRACTOR);
		//~ createLines (extMan, model, Extension.DATA_RENDERER);
		createLines (extMan, model, CapsisExtensionManager.GENERIC_TOOL);
		createLines (extMan, model, CapsisExtensionManager.MODEL_TOOL);
		//~ createLines (extMan, model, Extension.FILTER);
		createLines (extMan, model, CapsisExtensionManager.INTERVENER);
		createLines (extMan, model, CapsisExtensionManager.IO_FORMAT);
		//~ createLines (extMan, model, Extension.OBJECT_VIEWER);

		
	}

	/**
	 * Import: RecordSet -> Stand - Implementation here.
	 * (File -> RecordSet in superclass).
	 */
	public GScene load (GModel model) throws Exception {
		return null;	
	}	

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}




}
