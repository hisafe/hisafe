package capsis.extension.dataextractor;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.DefaultNumberFormat;

/**
 * A list of categories with a name.
 * 
 * @author F. de Coligny - October 2015
 */
public class Categories {

	/**
	 * An entry in the categories list.
	 */
	static public class Entry {
		public String label;
		public double value;
		
		public Entry (String label, double value) {
			this.label = label;
			this.value = value;
		}
		
		public String toString () {
			return ""+label+":"+DefaultNumberFormat.getInstance ().format(value);
		}
	}
	
	private String name;
	private Color color;
	private List<Entry> entries;

	/**
	 * Constructor
	 */
	public Categories (String name, Color color) {
		this.name = name;
		this.color = color;
		entries = new ArrayList<> ();
	}
	
	public void addEntry (String label, double value) {
		entries.add (new Entry (label, value));
	}

	public String getName() {
		return name;
	}

	public Color getColor() {
		return color;
	}

	public List<Entry> getEntries () {
		return entries;
	}

	public int size () {
		return entries == null ? 0 : entries.size ();
	}
	
	public String toString () {
		StringBuffer b = new StringBuffer ("Categories name: "+name+" #entries: "+entries.size ());
		for (Entry e : entries) {
			b.append(" ");
			b.append(e);
		}
		return b.toString ();
	}
	
	
	
	
}
