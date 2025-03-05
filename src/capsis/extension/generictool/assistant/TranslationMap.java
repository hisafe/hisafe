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

package capsis.extension.generictool.assistant;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.table.AbstractTableModel;

import jeeb.lib.util.Log;
import jeeb.lib.util.OrderedProperties;
import jeeb.lib.util.Translator;
import capsis.kernel.IdCard;
import capsis.kernel.ModelManager;
import jeeb.lib.util.PathManager;

/**
 * Map key with its different translation and its different source lexicons
 */

public class TranslationMap extends AbstractTableModel {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	private Map<String, String> db; // map key -> basename
	private Map<String, EditableLexicon> lexiconMap; // filename -> lexicon
	private Set<String> baseNameSet;
	private Map<String, Set<String>> baseNamesForKey;
	private List<Locale> langs;

	private String defaultBaseName;

	private String[] columnNames;
	private String[][] rowData;

	public boolean editStytemBundle = false; // allow to edit system bundle

	/** Constructor */
	public TranslationMap(Locale lang2) {

		langs = new ArrayList<Locale>();
		langs.add(Locale.ENGLISH);
		langs.add(lang2);

		columnNames = new String[] { "Key", "en", lang2.getLanguage(), "Source" };

		baseNamesForKey = new HashMap<String, Set<String>>();
		db = new HashMap<String, String>();
		lexiconMap = new HashMap<String, EditableLexicon>();
		baseNameSet = new HashSet<String>();

	}

	/**
	 * Load all key in a particular class
	 * 
	 * @throws ClassNotFoundException
	 */
	public void loadFromClass(String className) throws ClassNotFoundException {

		clear();

		Collection<String> keys = new ArrayList<String>();
		Collection<String> classNames = this.getRelatedClasseNames(className);
		for (String name : classNames) {

			String filename = name.replace('.', File.separatorChar);
			filename += ".java";
			URL url = getClass().getClassLoader().getResource(filename);

			if (url == null) {
				continue;
			}
			// Extract "swap" keys from .java
			keys.addAll(extractSwapKeys(url));

			if (name.equals(className)) {
				// Find default basename
				getDefaultBaseName(url, keys, className);
				baseNameSet.add(defaultBaseName);

			}
		}

		assert defaultBaseName != null;

		loadFromKeys(keys);
	}

	/** Clear data */
	private void clear() {

		baseNamesForKey.clear();
		db.clear();
		lexiconMap.clear();
		baseNameSet.clear();
		defaultBaseName = null;

	}

	/** Load keys from a particular bundle */
	public void loadFromBundle(String bundleName) {

		clear();
		String baseName = bundleName;
		defaultBaseName = bundleName;
		baseNameSet.add(baseName);

		// For each language
		for (Locale loc : langs) {

			EditableLexicon lexicon = getLexicon(baseName, loc);
			for (Object o : lexicon.keySet()) {
				String key = (String) o;

				// key -> baseName
				// System bundle are preferred
				if (!db.containsKey(key) || Translator.isSystem(baseName)) {
					db.put(key, baseName);
				}

				// Save all baseName to compute duplicates
				if (!baseNamesForKey.containsKey(key)) {
					baseNamesForKey.put(key, new HashSet<String>());
				}
				baseNamesForKey.get(key).add(baseName);
			}
		}
		buildRowData();
	}

	/**
	 * initialize db map for a collection of keys If several lexicon are found
	 * for a particular key, system bundle are preferred
	 * */
	public void loadFromKeys(Collection<String> keys) {

		// Search for existing key in lexicon

		// For each language
		for (Locale loc : langs) {
			// For each key
			for (String key : keys) {

				boolean found = false;
				// For each loaded bundle
				for (String baseName : Translator.getLoadedBundles()) {
					EditableLexicon lexicon = getLexicon(baseName, loc);

					// Keys already in lexicon
					if (lexicon.containsKey(key)) {

						baseNameSet.add(baseName);

						// key -> baseName
						// System bundle are preferred
						if (!db.containsKey(key)
								|| Translator.isSystem(baseName)) {
							db.put(key, baseName);
						}

						// Save all baseName to compute duplicates
						if (!baseNamesForKey.containsKey(key)) {
							baseNamesForKey.put(key, new HashSet<String>());
						}
						baseNamesForKey.get(key).add(baseName);
						found = true;
					}
				}

				// Key is not found in any lexicon
				if (!found && !db.containsKey(key)) {
					// key -> baseName
					db.put(key, defaultBaseName);

					// Save all baseName to compute duplicates
					if (!baseNamesForKey.containsKey(key)) {
						baseNamesForKey.put(key, new HashSet<String>());
					}
					baseNamesForKey.get(key).add(defaultBaseName);
				}
			}
		}
		buildRowData();
	}

	/** Add new key */
	public void addKey(String key) {

		if (db.containsKey(key) || defaultBaseName == null) {
			return;
		}
		db.put(key, defaultBaseName);
		baseNamesForKey.put(key, new HashSet<String>());
		baseNamesForKey.get(key).add(defaultBaseName);

		buildRowData();
		fireTableDataChanged();
	}

	public void removeKey(String key, String baseName) {

		if (!db.containsKey(key)) {
			return;
		}
		db.remove(key);
		baseNamesForKey.remove(key);

		buildRowData();
		fireTableDataChanged();

		for (Locale lg : langs) {
			EditableLexicon lex = getLexicon(baseName, lg);
			lex.remove(key);
			lex.setModified();
		}

	}

	/**
	 * Return duplicate keys Ignore system bundle
	 * */
	public Map<String, Set<String>> getDuplicates() {

		Map<String, Set<String>> duplicateCopy = new HashMap<String, Set<String>>();

		// Research for real duplicate
		for (String key : baseNamesForKey.keySet()) {

			Set<String> dup = baseNamesForKey.get(key);
			if (dup.size() <= 1) {
				continue;
			} // if size is one there is no duplicate

			Set<String> newdup = new HashSet<String>();
			duplicateCopy.put(key, newdup);

			for (String s : dup) {
				// Do not store actual baseName or System bundle
				if ((!editStytemBundle && Translator.isSystem(s))
						|| s.equals(db.get(key))) {
					continue;
				}
				newdup.add(s);
			}
			if (newdup.size() == 0) {
				duplicateCopy.remove(key);
			}
		}
		return duplicateCopy;

	}

	/** Remove duplicated keys */
	public void removeDuplicates() {

		Map<String, Set<String>> duplicates = getDuplicates();

		// For each key with duplicates
		for (String key : duplicates.keySet()) {

			Set<String> duplicateBaseNames = duplicates.get(key);

			// Get corresponding lexicon
			for (EditableLexicon lex : lexiconMap.values()) {
				String baseName = lex.getBaseName();

				if (duplicateBaseNames.contains(baseName)) {
					lex.remove(key);
					lex.setModified();
				}
			}
		}
	}

	/** Save data : save all modified editable lexicon */
	public void saveData() {

		ResourceBundle.clearCache();

		for (EditableLexicon l : lexiconMap.values()) {
			if (l.isModified()) {
				l.removeEmptyEntry();
				try {
					l.store();
				} catch (Exception e) {
					Log.println("Cannot store Lexicon: " + e);
				}

				Translator.addBundle(l.getBaseName());
				if (l.getLocale()
						.getLanguage()
						.equals(Translator.getMainLexicon().getLocale()
								.getLanguage())) {
					Translator.getMainLexicon().loadBundle(l);
				}
			}
		}
	}

	/** Return a lexicon given a basename and a language */
	public EditableLexicon getLexicon(String basename, Locale lang) {

		String filename = EditableLexicon.getFileName(basename, lang);
		EditableLexicon lex = this.lexiconMap.get(filename);
		if (lex == null) {
			lex = new EditableLexicon(basename, lang);
			this.lexiconMap.put(filename, lex);
		}

		return lex;
	}

	/** BaseName Accessors */
	public void setBaseName(String key, String basename) {
		this.db.put(key, basename);
	}

	public String getBaseName(String key) {
		return this.db.get(key);
	}

	public Collection<String> getBaseNames() {
		return new TreeSet<String>(baseNameSet);
	}

	/**
	 * Extraction of the keys from the className if not found, returns an empty
	 * TreeSet<String> if error, writes in the Log and returns null
	 */
	private Collection<String> extractSwapKeys(URL url) {

		try {
			InputStream stream = url.openStream();
			Collection<String> classKeys = new TreeSet<String>(); // no
																	// duplicates
			String mark = ".swap";

			BufferedReader in = new BufferedReader(
					new InputStreamReader(stream));

			StringBuffer buffer = new StringBuffer();
			String line = null;
			while ((line = in.readLine()) != null) {
				buffer.append(line.replaceAll("\n", ""));
			}
			String buf = buffer.toString();

			int posMark = buf.indexOf(mark);
			while (posMark != -1) {
				buf = buf.substring(posMark + mark.length()).trim();
				if (buf.startsWith("(")) {
					buf = buf.substring(1).trim();
					if (buf.startsWith("\"")) {
						buf = buf.substring(1).trim();
						int end = buf.indexOf("\"");
						String key = buf.substring(0, end).trim();
						classKeys.add(key); // known or unknown key
					}
				}
				posMark = buf.indexOf(mark); // another key in the line ?
			}

			in.close();
			return classKeys;

		} catch (Exception e) {
			Log.println(Log.WARNING, "SwapMap.extractSwapKeys ()",
					"Exception: ", e);
			return null;
		}
	}

	/**
	 * Return a list of full class name
	 * 
	 * @throws ClassNotFoundException
	 */
	private Collection<String> getRelatedClasseNames(String className)
			throws ClassNotFoundException {

		Collection<String> ret = new ArrayList<String>();
		Class<?> mainClass;

		// Get the main class
		mainClass = Class.forName(className);
		ret.add(mainClass.getName());

		// Get the related class (embeded classes)
		for (Class<?> c : mainClass.getDeclaredClasses()) {
			ret.add(c.getName());
		}

		// Get the related class (embeded classes)
		Class<?> superclass = mainClass.getSuperclass();
		ret.add(superclass.getName());

		// Get field class
		for (Field f : mainClass.getDeclaredFields()) {
			Class<?> c = f.getType();
			if (c.getName().startsWith("java")) {
				continue;
			} // ignore java classes
			ret.add(c.getName());
		}
		return ret;
	}

	/** Define default basename */
	private void getDefaultBaseName(URL url, Collection<String> keys,
			String className) {

		// Try to find an 'addBundle' in the file
		defaultBaseName = extractDefaultBaseName(url);

		// if not found, try to detect if a module baseName must be created
		if (defaultBaseName == null) {
			try {
				String moduleName = className.substring(0,
						className.indexOf("."));
				for (String s : ModelManager.getInstance().getPackageNames()) {

					if (moduleName.equals(s)) {
						IdCard idCard = ModelManager.getInstance().getIdCard(s);
						defaultBaseName = idCard.getModelBundleBaseName();
					}
				}
			} catch (Exception e) {
			} // next case
		}

		// if not found, baseName = className
		if (defaultBaseName == null) {
			defaultBaseName = className;
		}
	}

	/**
	 * -- Extraction of the default base name from the className by looking for
	 * a "addBundle" command ex:
	 * Translator.addBundle("capsis.util.doccenter.DocCenter"); ->
	 * capsis.util.doccenter.DocCenter if not found, returns null
	 */
	private String extractDefaultBaseName(URL url) {
		try {
			InputStream stream = url.openStream();

			String baseName = null;
			String mark = "Translator.addBundle";

			BufferedReader in = new BufferedReader(
					new InputStreamReader(stream));

			StringBuffer buffer = new StringBuffer();
			String line = null;
			while ((line = in.readLine()) != null) {
				buffer.append(line.replace('\n', ' '));
			}
			String buf = buffer.toString();

			int posMark = buf.indexOf(mark);
			if (posMark != -1) {
				buf = buf.substring(posMark + mark.length()).trim();
				if (buf.startsWith("(")) {
					buf = buf.substring(1).trim();
					if (buf.startsWith("\"")) {
						buf = buf.substring(1).trim();
						int end = buf.indexOf("\"");
						baseName = buf.substring(0, end).trim();
					}
				}
				// ~ posMark = buf.indexOf (mark); // another key in the line ?
			}
			in.close();

			return baseName;

		} catch (Exception e) {
			Log.println(Log.WARNING, "SwapMap.extractDefaultBaseName ()",
					"Exception: ", e);
			return null;
		}
	}

	/** Table Model functions */
	public String getColumnName(int col) {
		return columnNames[col].toString();
	}

	public int getRowCount() {
		return db.size();
	}

	public int getColumnCount() {
		return columnNames.length;
	}

	/** Build String array for table model */
	protected void buildRowData() {

		rowData = new String[getRowCount()][getColumnCount()];
		Set<String> keys = db.keySet();
		TreeSet<String> l = new TreeSet<String>(keys);
		Iterator<String> iter = l.iterator();

		for (int i = 0; i < getRowCount(); i++) {
			String key = iter.next();

			rowData[i][0] = key;
			rowData[i][1] = "";
			rowData[i][2] = "";
			rowData[i][3] = defaultBaseName;

			if (!db.containsKey(key))
				continue;

			String basename = db.get(key);

			EditableLexicon lex;

			int col = 0;
			for (Locale lg : langs) {
				col++;
				lex = getLexicon(basename, lg);

				if (lex.containsKey(key)) {
					rowData[i][col] = lex.getProperty(key);
				}
			}

			rowData[i][3] = basename;
		}
	}

	/** Parse String array (rowdata) and apply values */
	protected void validateRowData() {

		for (int i = 0; i < getRowCount(); i++) {

			String key = rowData[i][0];
			String basename = rowData[i][3];

			if (basename == null || basename.equals("")) {
				continue;
			}

			db.put(key, basename);

			EditableLexicon lex;

			int col = 0;
			// for each col
			for (Locale lg : langs) {
				col++;

				String val = rowData[i][col];
				lex = getLexicon(basename, lg);
				if (lex.get(key) == null || !lex.get(key).equals(val)) {
					lex.put(key, val);
					lex.setModified();
				}

			}

		}
	}

	public Object getValueAt(int row, int col) {
		return rowData[row][col];
	}

	public boolean isCellEditable(int row, int col) {
		if (col == 0) {
			return false;
		}

		String baseName = rowData[row][3];
		if (!editStytemBundle && Translator.isSystem(baseName)) {
			return false;
		}
		return true;
	}

	public void setValueAt(Object value, int row, int col) {
		rowData[row][col] = (String) value;
		fireTableCellUpdated(row, col);
	}

	
	
	
	
	
	/**
	 * Translation Lexicon base on 'Properties' object
	 * 
	 * @author S. Dufour
	 */
	protected static class EditableLexicon extends OrderedProperties {

		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		private String baseName;
		private Locale lang;
		private Boolean modified = false;

		/** Constructor */
		public EditableLexicon(String basename, Locale lang) {

			this.baseName = basename;
			this.lang = lang;
			String fileName = getFileName(basename, lang);

			try {
				InputStream stream = EditableLexicon.class.getClassLoader()
						.getResourceAsStream(fileName);
				this.load(stream);
			} catch (Exception e) {
				Log.println("Cannot load lexicon : " + fileName + "\n");
			}
		}

		public void setModified() {
			this.modified = true;
		}

		public boolean isModified() {
			return this.modified;
		}

		/** Remove empty entry in the lexicon */
		public void removeEmptyEntry() {
			for (Object key : new ArrayList<Object>(super.keySet())) {
				String val = (String) this.get(key);
				if (val == null || val.equals("")) {
					this.remove(key);
				}
			}
		}

		/** Write on disk the lexicon */
		public void store() {

			String comment = "File generated by the Translation assistant";
			String fileName = getFileName(baseName, this.lang);

			String prefix = PathManager.getDir("bin") + File.separatorChar;
			fileName = prefix + fileName;

			OutputStream stream;
			try {
				stream = new FileOutputStream(fileName);
				this.store(stream, comment);
				stream.flush();
				stream.close();
				Log.println("Write lexicon :" + fileName + "\n");

			} catch (Exception e) {
				Log.println("Cannot write lexicon :" + fileName + " " + e
						+ "\n");
			}
		}

		public String getBaseName() {
			return baseName;
		}

		public Locale getLocale() {
			return lang;
		}

		/** Return the filename of the properties file */
		static public String getFileName(String basename, Locale lang) {

			String fileName = basename.replace('.', File.separatorChar);
			fileName.trim();
			fileName += "_" + lang.getLanguage() + ".properties";

			return fileName;
		}

	}

}
