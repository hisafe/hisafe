/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.extension;

import java.util.StringTokenizer;

/**
 * At start, engine creates an ExtensionManager which reads the file
 * <capsisRoot>/etc/capsis.extensions and creates an ExtensionItem per
 * extension read.
 * 
 * @author F. de Coligny - july 2001
 */
public class ExtensionItem {

	/** Extension full class name for instanciation. */
	protected String extension;		// extension's full class name (= id)

	/** What allows the extension. */
	protected String type;			// StandViewer, DataRenderer, DataExtractor, Intervener, ...

	/** Optional: type can be "type.subType". ex: Intevener.SelectiveThinner */
	protected String subType;			
	
	/** Modules or module types that can use this extension. */
	//~ protected Vector validModules;	// All, MADD, MAID, mountain, ...

	/** Free settings allowed, to be passed to extension constructor. */
	protected String settings;

	public ExtensionItem () {
		extension = "";
		type = "";
		subType = "";
		//~ validModules = new Vector ();
		settings = "";
	}

	public void setExtension (String className) {extension = className;}
	public void setType (String typ) {
		StringTokenizer st = new StringTokenizer (typ, " .");
		int n = st.countTokens ();
		if (n == 0) {return;}
		
		// type is needed (ex: StandViewer)
		if (n > 0) {type = st.nextToken ();}
		
		// subType is optional (ex: "Intervener.SelectiveThinner")
		if (n > 1) {subType = st.nextToken ();}
	}
	//~ public void addValidModule (String moduleClassName) {
		//~ validModules.add (moduleClassName);
	//~ }
	public void setSettings (String set) {settings = set;}

	public String getExtension () {return extension;}
	public String getType () {return type;}
	public String getSubType () {return subType;}
	//~ public Vector getValidModules () {return validModules;}
	public String getSettings () {return settings;}

	//~ public boolean isValidForModule (GModel xModel) {
		//~ boolean valid = false;
		//~ ExtensionManager em = ExtensionManager.getInstance ();
		//~ return em.isCompatible (getExtension (), xModel);	// fc - 2001.7.24
	//~ }

	public boolean isValid () {
		boolean valid = false;
		if (isExtensionSet () && isTypeSet () 
				//~ && isValidModulesSet ()
				) {
			valid = true;
		}
		// settings are optional
		return valid;
	}
	public boolean isExtensionSet () {
		boolean set = true;
		if (extension == null || extension.length () == 0) {
			set = false;
		}
		return set;
	}
	public boolean isTypeSet () {
		boolean set = true;
		if (type == null || type.length () == 0) {
			set = false;
		}
		return set;
	}
	public boolean isSubTypeSet () {
		boolean set = true;
		if (subType == null || subType.length () == 0) {
			set = false;
		}
		return set;
	}
	//~ public boolean isValidModulesSet () {return true;}

	public boolean isSettingsSet () {
		boolean set = true;
		if (settings == null || settings.length () == 0) {
			set = false;
		}
		return set;
	}

	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("extension=");
		b.append (extension);
		b.append (", type=");
		b.append (type);
		b.append (", subType=");
		b.append (subType);
		//~ b.append (", validModules=");
		//~ b.append (Tools.traceVector (validModules));
		b.append (", settings=");
		b.append (settings.toString ());
		
		return b.toString ();
	}

}
