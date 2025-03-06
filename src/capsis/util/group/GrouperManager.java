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

package capsis.util.group;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import capsis.extension.filter.general.FQualitativeProperty;
import capsis.util.EnumProperty;
import capsis.util.QualitativeProperty;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.TicketDispenser;

/**
 * Manages groupers during the capsis session. Loads last session groupers and
 * save them at the end of the capsis session for next session.
 * 
 * Modified on 30.3.2004 to manage Groupers (new Groupers and filters
 * framework).
 * 
 * @author F. de Coligny - march 2004
 */
public class GrouperManager extends HashSet<Grouper> {

	static private GrouperManager instance; // Singleton pattern

	static private Map<String, String> grouperName_fileName;
	static private TicketDispenser ticketDispenser;

	/**
	 * Used to get an instance of GrouperManager.
	 */
	static public GrouperManager getInstance() { // Singleton pattern
		if (instance == null) {
			instance = new GrouperManager();
		}
		return instance;
	}

	// Private because of singleton pattern
	//
	private GrouperManager() {
		super();
		ticketDispenser = new TicketDispenser();
		loadGroupers();
	}

	/**
	 * Add a new grouper. Deals with file saving.
	 */
	public boolean add(Grouper o) {
		return add(o, true);
	} // add (grouper) -> saves on disk

	public boolean add(Grouper o, boolean saveGrouperOnDisk) { // add (grouper,
																// false) ->
																// does not save
		super.add(o);
		if (saveGrouperOnDisk) {
			saveGrouper((Grouper) o);
		}

		return true;
	}

	/**
	 * Remove a grouper. Deals with file deletion.
	 */
	public void remove(String name) {
		name = removeNot(name); // fc - 21.4.2004
		Grouper g = getGrouper(name);
		try {
			remove(g); // from HashSet
			deleteGrouper(g); // from file
		} catch (Exception e) {
			Log.println(Log.WARNING, "GrouperManager.remove ()", "Grouper name: " + name + " Exception: " + e);
		}
	}

	/**
	 * For info reasons only.
	 */
	public String getFileName(String grouperName) {
		grouperName = removeNot(grouperName); // fc - 21.4.2004
		return (String) grouperName_fileName.get(grouperName);
	}

	/**
	 * Retrieve the grouper names.
	 */
	public Collection<String> getGrouperNames() {
		return getNames(getGroupers());
	}

	// fc-18.1.2021 Was forgotten when GroupableType arrived in July 2019 (fish error)
	public Collection<String> getGrouperNames(GroupableType type) {
//	public Collection<String> getGrouperNames(String type) {
		
		return getNames(getGroupers(type));
	}

	public Collection<String> getGrouperNames(Object referent) {
		return getNames(getGroupers(referent));
	}

	public Collection<String> getNames(Collection<Grouper> groupers) {
		Collection<String> c = new ArrayList<String>();
		for (Grouper g : groupers) {
			c.add(g.getName());
		}
		return c;
	}

	/**
	 * Retrieve a grouper given its name.
	 */
	public Grouper getGrouper(String name) {

		boolean not = false; // fc - 16.4.2007
		if (name != null && name.toLowerCase().startsWith("not ")) {
			not = true;
		} // fc - 16.4.2007

		name = removeNot(name); // fc - 21.4.2004
		Grouper result = null;
		for (Iterator<Grouper> i = iterator(); i.hasNext();) {
			Grouper g = (Grouper) i.next();
			if (g.getName().equals(name)) {
				result = g;
				result.setNot(not); // fc - 16.4.2007
				break;
			}
		}
		return (result == null) ? new DummyGrouper() : result; // fc - 5.4.2004
	}

	/**
	 * Retrieve all groupers : this object is returned (an HashSet, see
	 * superclass).
	 */
	public Collection<Grouper> getGroupers() {
		return this;
	}

	/**
	 * Retrieve available groupers matching with given referent (should be a
	 * Collection of individuals).
	 */
	public Collection<Grouper> getGroupers(Object referent) {

		Collection<Grouper> result = new ArrayList<Grouper>();

		if (referent == null) {
			Log.println(Log.WARNING, "GrouperManager.getGroupers (Object)",
					"Referent is null. Empty Collection returned");
			return result;
		}
		if (!(referent instanceof Collection)) {
			Log.println(Log.WARNING, "GrouperManager.getGroupers (Object)", "Referent is not a Collection: " + referent
					+ ". Empty Collection returned");
			return result;
		}

		// fc - 30.3.2004
		Collection collection = ((Collection) referent);
		if (collection.isEmpty()) {
			return result;
		} // fc - security - 14.9.2004

		Object indiv = collection.iterator().next();

		GroupableType type = Group.whichType(indiv);

		// Extract the concerned groupers
		//
		for (Iterator<Grouper> i = iterator(); i.hasNext();) {
			Grouper g = (Grouper) i.next();
			if (g.getType().equals(type) && g.matchWith(referent)) {
				result.add(g);
			}
		}
		return result;
	}

	/**
	 * Retrieve available groupers matching the given individual type.
	 */
	// fc-18.1.2021 Was forgotten when GroupableType arrived in July 2019 (fish error)
	public Collection<Grouper> getGroupers(GroupableType type) {
//	public Collection<Grouper> getGroupers(String type) {
	
		Collection<Grouper> result = new ArrayList<Grouper>();

		// fc-1.7.2019 removed Group.possibleTypes. No more global list for groupable types
		
//		if (!Group.getPossibleTypes().contains(type)) {
//			Log.println(Log.WARNING, "GrouperManager.getGroupers (type)", "Wrong type : " + type + ". Should be in "
//					+ AmapTools.toString(Group.getPossibleTypes()) + ".Empty Collection returned");
//			return result;
//		}

		// Extract the concerned groupers
		//
		for (Iterator<Grouper> i = iterator(); i.hasNext();) {
			Grouper g = (Grouper) i.next();
			if (g.getType().equals(type)) {
				result.add(g);
			} // fc - 19.5.2005 - changed "=="
		}
		return result;
	}

	// ----------------------------------------------- private methods

	// Extract grouper id from fileName
	// ex : "grouper_12" -> 12
	//
	private int getId(String fileName) {
		try {
			String s = fileName.substring(fileName.indexOf("_") + 1);
			return new Integer(s).intValue();
		} catch (Exception e) {
			Log.println(Log.ERROR, "GrouperManager.getId()", "Could not get id in fileName " + fileName + " " + e);
			return 0; // maybe user file name without _automaticNumber suffix
		}
	}

	// Retrieve last session groupers from disk.
	// If trouble (not found...) loads nothing.
	//
	private void loadGroupers() {
		StringBuffer loadedGroupers = new StringBuffer();
		try {
			int maxId = 0;
			grouperName_fileName = new HashMap<String, String>();

			File groupersDirectory = new File(PathManager.getDir("etc") + File.separator + "groupers");
			File[] files = groupersDirectory.listFiles();
			if (files != null) {
				for (int i = 0; i < files.length; i++) {
					File f = files[i];
					ObjectInputStream in = null;
					
					try {
						in = new ObjectInputStream(new BufferedInputStream(new FileInputStream(f)));

						String grouperName = (String) in.readObject();

						grouperName_fileName.put(grouperName, f.getName());
						int id = getId(f.getName());
						if (id > maxId) {
							maxId = id;
						}

						Grouper g = (Grouper) in.readObject();
						this.add(g);

						if (loadedGroupers.length() != 0) {
							loadedGroupers.append(", ");
						}
						loadedGroupers.append(g.getName());

						in.close();
						
					} catch (EOFException e) {
						// fc-29.8.2019 trying to fix a bug at groupers reloading time
						if (in != null)
							in.close ();

					} catch (java.io.IOException exc) {
						// fc-18.1.2021 Removed exc to have a shorter log
						Log.println(Log.ERROR, "GrouperManager.loadGroupers ()",
								"Could not load grouper (IOException) " + f.getName() + " " + exc.toString() /*, exc*/ );

					} catch (java.lang.ClassNotFoundException exc) {
						// fc-18.1.2021 Removed exc to have a shorter log
						Log.println(
								Log.ERROR,
								"GrouperManager.loadGroupers ()",
								"Could not load grouper (ClassNotFoundException) " + f.getName() + " " + exc.toString()
								/*, exc*/);

					} catch (java.lang.Exception exc) {
						// fc-18.1.2021 Removed exc to have a shorter log
						Log.println(Log.ERROR, "GrouperManager.loadGroupers ()", "Could not load grouper (Exception) "
								+ f.getName() + " " + exc.toString() /*, exc*/);

					}

				}
			}
			ticketDispenser.setCurrentValue(maxId);
			
		} catch (Exception e) {
			Log.println(Log.WARNING, "GrouperManager.loadGroupers ()",
					"No groupers were loaded due to" + " " + e.toString(), e);
		}
		Log.println("Loaded groupers: "
				+ ((loadedGroupers.toString().trim().length() != 0) ? loadedGroupers.toString() : " none"));
	}

	// Save given grouper to file
	//
	private void saveGrouper(Grouper grouper) {

		// Ensure groupers directory exists
		// fc - 6.10.2003
		//
		File etcGroupers = new File(PathManager.getDir("etc") + File.separator + "groupers");
		if (!etcGroupers.exists())
			etcGroupers.mkdir();

		deleteGrouper(grouper); // no duplicate files

		String suffix = (String) grouperName_fileName.get(grouper.getName());
		if (suffix == null)
			suffix = "grouper_" + ticketDispenser.getNext();
		
		String fileName = PathManager.getDir("etc") + File.separator + "groupers" + File.separator + suffix;
		File f = new File(fileName);

		try {
			ObjectOutputStream out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(f)));

			out.writeObject(grouper.getName());
			out.writeObject(grouper);
			out.close(); // also flushes output
			grouperName_fileName.put(grouper.getName(), f.getName());

		} catch (Exception exc) {
			Log.println(Log.ERROR, "GrouperManager.saveGrouper ()", "Unable to write " + fileName + " to disk." + " "
					+ exc.toString(), exc);
		}
	}

	// Delete grouper file
	//
	private void deleteGrouper(Grouper grouper) {
		String suffix = (String) grouperName_fileName.get(grouper.getName());
		if (suffix == null)
			return;

		String fileName = PathManager.getDir("etc") + File.separator + "groupers" + File.separator + suffix;
		File f = new File(fileName);
		f.delete();
	}

	/**
	 * Given a QualitativeProperty p, create a grouper for each possible value.
	 * Memorize them but do not save them on disk : they must be recreated at
	 * module loading time (or whenever). For each value, create a filter and a
	 * matching filtrer. Type is a value in Group.possibleTypes (TREE, CELL...).
	 */
	public void buildGroupers(QualitativeProperty p, GroupableType type) { // fc -
																	// 4.9.2003
		Class<?> klass = p.getClass();
		String propertyName = p.getPropertyName();
		Map possibleValues = p.getValues(); // ex: 1-"Beech", 2-"Oak"...

		Iterator values = possibleValues.keySet().iterator();
		Iterator names = possibleValues.values().iterator();

		while (values.hasNext() && names.hasNext()) { // for each possible
														// value
			Integer value = (Integer) values.next();
			String name = (String) names.next();

			// Map for className of the property -> the (only) value for the
			// grouper under construction
			Map class_validValues = new HashMap();
			Collection validValues = new ArrayList();
			validValues.add(value);
			class_validValues.put(klass, validValues); // ex: MaddSpecies-{1}

			// New filter
			FQualitativeProperty filter = new FQualitativeProperty(class_validValues);

			// New grouper
			// fc-11.1.2017 CAUTION: removed Translator.swap () in grouperName
			// this translation was obfuscating the grouper names when trying to
			// find them programmatically
//			StringBuffer grouperName = new StringBuffer(propertyName);
//			grouperName.append('.');
//			grouperName.append(label); // ex: Species.Beech
			// fc-11.1.2017
			
			// fc+bc-9.1.2019 changed grouperName for a better option
			// e.g. Beech (Samsara2)	
			String grouperName = EnumProperty.getUniqueName(propertyName, name);
			// fc+bc-9.1.2019
			
			Grouper grouper = new Filtrer(grouperName, type, filter);

			add(grouper, false); // false : do not save this grouper on disk
									// (recreated at module load time)
		}
	}

	/**
	 * GrouperName may begin with "NOT " if complementary is required. For
	 * grouper management, "NOT " must be ignored. NOTE: it will be used in
	 * Filtrer.apply (). fc - 21.4.2004
	 */
	public String removeNot(String grouperName) {
		if (grouperName != null && grouperName.toLowerCase().startsWith("not ")) {
			grouperName = grouperName.substring(4);
		}
		return grouperName;
	}

	public String trace() {
		StringBuffer b = new StringBuffer();
		b.append("GrouperManager: contains " + size() + " groupers");
		b.append("\n");
		int k = 0;
		for (Iterator<Grouper> i = iterator(); i.hasNext();) {
			Grouper g = (Grouper) i.next();
			b.append("" + (k++) + ": " + g.getName() + " type " + g.getType());
			b.append("\n");
		}
		b.append("GrouperManager: end-of-trace");
		return b.toString();
	}

}
