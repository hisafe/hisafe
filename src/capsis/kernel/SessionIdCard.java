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

package capsis.kernel;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.text.DateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Locale;


import jeeb.lib.util.RecordSet;
import jeeb.lib.util.Translator;

/**	Used to mark the session files when they are saved. Designed to
 *	appear nicely in an IntrospectionPanel (e.g. in fileChooser accessory).
 * 
 *	@author F. de Coligny - march 2001, september 2010
 */
public class SessionIdCard implements Serializable {
	
	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;
	
	private static String CAPSIS_SESSION = "SessionIdCard.capsisSession";
	private String compactString;
	
	private String sessionName;
	private String fileType;
	private String version;
	private String sessionDate;
	private String numberOfProjects;
	private String contents;
	
	
	/**	Constructor 1: for export.
	 */
	public SessionIdCard (Session session) {

		sessionName = session.getName ();
		fileType = CAPSIS_SESSION;  // to be translated at read time
		version = Engine.getVersionAndRevision();
		
		Date now = new Date ();
		DateFormat fmt = DateFormat.getDateTimeInstance (DateFormat.MEDIUM, DateFormat.SHORT, Locale.getDefault ());
		sessionDate = fmt.format (now);  // formated at save time
		
		Collection<Project> projects = session.getProjects ();
		numberOfProjects = ""+projects.size ();

		StringBuffer names = new StringBuffer ();
		for (Iterator<Project> i = projects.iterator (); i.hasNext ();) {
			Project p = i.next ();
			names.append (p.getName ());
			if (i.hasNext ()) {names.append (", ");}
		}
		contents = names.toString ();

	}
	
	/**	Constructor 2: for import.
	 */
	public SessionIdCard (File f) throws Exception {

		ObjectInputStream in = new ObjectInputStream (
				new BufferedInputStream (
				new FileInputStream (f)));
		
		// We do not de/serialize the SessionIdCard any more, but the compactString 
		// like for ProjectIdCard
		String cs = (String) in.readObject ();
//		SessionIdCard idc = (SessionIdCard) in.readObject ();
		
		// We do not read the whole session
		in.close ();
		
		init (cs);
	}
	
	/**	Constructor 3.
	 */
	public SessionIdCard (String compactString) throws Exception {
		init (compactString);
	}
	
	/** Creates a SessionIdCard with the given CompactString.
	 */
	private void init (String compactString) throws Exception {
		
		String separator = compactString.substring (0, 1);
		String[] entries = compactString.split (separator);
		
		if (!new RecordSet.KeyRecord (entries[2]).value.equals (CAPSIS_SESSION)) {
				throw new Exception ("Not a Capsis Session");}
		
		sessionName = new RecordSet.KeyRecord (entries[1]).value;
		fileType = Translator.swap (new RecordSet.KeyRecord (entries[2]).value);
		version = new RecordSet.KeyRecord (entries[3]).value;;
		sessionDate = new RecordSet.KeyRecord (entries[4]).value;;
		numberOfProjects = new RecordSet.KeyRecord (entries[5]).value;
		contents = new RecordSet.KeyRecord (entries[6]).value;
		
	}
	
	public String getSessionName () {return sessionName;}
	
	public String getFileType() {
		return fileType;
	}
	
	/**	This is the versionAndRevision of the app when the Session was 
	 * 	saved. If a Serialization error occurs when reopening the file with 
	 * 	a more recent version of the app, will tell that the session may be 
	 * 	reopened with the app with this versionAndRevision.
	 * 	Note: the version does not really matter but the revision is the svn 
	 * 	revision and should be enough to recreate the app if needed.
	 */
	public String getVersion () {return version;}

	public String getSessionDate() {
		return sessionDate;
	}
	
	public String getNumberOfProjects() {
		return numberOfProjects;
	}
	
	public String getContents() {
		return contents;
	}
	
	/**	This compact String is to be written in the files, it is less 
	 * 	sensitive to Serialization trouble when changing: if fields are 
	 * 	added / changed or removed in the String, it will still be a String.	
	 */
	public String getCompactString () {
		if (compactString == null) {
			
			StringBuffer b = new StringBuffer ();
			String separator = Engine.getStringSeparator();
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("sessionName", sessionName));
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("fileType", fileType));  // to be translated at read time
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("version", version));	
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("sessionDate", sessionDate));  // formated at save time
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("numberOfProjects", numberOfProjects));	
			b.append (separator);
			b.append (new RecordSet.KeyRecord ("contents", contents));	
			compactString = b.toString ();
			
		}
		return compactString;
		
	}



}