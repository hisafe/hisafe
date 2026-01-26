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

package safe.extension.ioformat.safeExport;


import java.io.File;
import java.util.HashMap;
import java.util.Map;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;



/**
 * SafeExportProfileCatalog is a structure to store profiles catalog.
 *
 * @author I.LECOMTE June 2016
 */
public class SafeExportProfileCatalog  {

	private String profilesDirectory;
	private Map profiles;


	//INTERACTIVE MODE
	public SafeExportProfileCatalog () {
		
		profilesDirectory = PathManager.getDir("data")
				+File.separator+"safe"
				+File.separator+"exportParameters";
		
		profiles = loadProfiles ();	// create some Maps...
	}

	//BATCH MODE
	public SafeExportProfileCatalog (String folder) {
		
		profilesDirectory = folder;
		profiles = loadProfiles ();	// create some Maps...
	}	


	/**
	 * Load profile list
	 */	
	private  Map loadProfiles () {
		Map profilesRet = new HashMap ();
		try {
			File profilesDirectory = new File (getProfileDirectory());
			File[] files = profilesDirectory.listFiles ();
			for (int i = 0; i < files.length; i++) {
				File f = files[i];
				
				String profileName = f.getName();	// gt - 3.02.2009
				int fileNameLength = profileName.length();	// gt - 3.02.2009

				boolean isExportProfileExtension = false; 

				if (profileName.contains(".")) {
					isExportProfileExtension = (profileName.substring(fileNameLength-4,fileNameLength)).equals(".pro");	// gt - 3.02.2009
				}

				
				if(isExportProfileExtension){	// gt - 3.02.2009
					try {

				
						SafeExportProfile p = new SafeProfileLoader ().load (f.getAbsolutePath ());
				
						profilesRet.put (profileName.substring(0,fileNameLength-4), p);	// gt - 3.02.2009



					} catch (java.lang.Exception exc) {
						Log.println (Log.ERROR, "SafeExportProfileCatalog.loadProfiles ()",
								"Could not load profile (Exception) "+f.getName ()
								, exc);
					}
				}


			}
		} catch (Exception e) {
			Log.println (Log.WARNING, "SafeExportProfileCatalog.loadProfiles ()",
					"No profiles were loaded due to"
					+" "+e.toString (), e);
		}
		return profilesRet;
	}


	/**
	 * Save profile 
	 */	
	public  boolean saveProfile (SafeExportProfile profile, String fileName) {

		fileName = getProfileDirectory()+File.separator+fileName+".pro";
		
		try {
			new SafeProfileLoader (profile).save (fileName);
			return true;
		} catch (Exception e) {
			Log.println (Log.ERROR, "SafeExportProfileCatalog.saveProfile ()",
					"Unable to save a profile in "+fileName, e);
			return false;
		}
	}
		
		

	/**
	 * Delete profile 
	 */
	public  void deleteProfile (String fileName) {
		String filePath = getProfileDirectory()
				+File.separator+fileName+".pro";

		File f = new File (filePath);
		f.delete ();
		SafeExportProfile p = loadProfile (fileName);
		profiles.remove(p); 
	}



	/**
	 * Return the directory name
	 */
	public  String getProfileDirectory() {
		return profilesDirectory;
	}

	/**
	 * Loads  the profile with the given name.
	 */	
	public  SafeExportProfile loadProfile (String profileName) {
		// fc-qm-20.2.2014
		return (SafeExportProfile) profiles.get (profileName);
	}
	
	/**
	 * Returns the profile with the given name.
	 */
	public SafeExportProfile getProfileFromName (String name) {
		return (SafeExportProfile) profiles.get (name);
	}

	public  Map getProfiles () {
		return profiles;
	}


	



}

