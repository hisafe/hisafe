package capsis.extension.dataextractor.configuration;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.annotation.ListParam;
import jeeb.lib.util.annotation.Param;

/**
 * A property container based on a List to be compliant with the ParamMap used
 * to save the extractor properties (does not like the maps). This class was
 * added to solve the opened extractors configuration propagation bug (e.g.
 * changing perHectare on an instance was chaiging in other opened instances)
 * 
 * @author F. de Coligny - August 2021
 */
public class ListOfConfigProperties<T> {

	// fc-30.8.2021
	// This replaces MapProperty2 (works for booleans), adding generics for double,
	// String, int...

	// Trying to get rid of the configuration propagation bug, the maps in
	// DESettings have the same hashCode, trying to encapsulate these maps in
	// another object

	// Different instances of this class should have different hashCodes

//		@Param
//		private TreeMap map;

	// fc-30.8.2021 ParamMap seems not to manage Maps, try with a list of couples

	static private class ConfigProperty<T> {
		@Param
		public String name;
		@Param
		public T value;

		public ConfigProperty(String name, T value) {
			this.name = name;
			this.value = value;
		}
		
		@Override
		public String toString() { // fc-31.1.2024
			return "ConfigProperty, name: "+name+", value: "+value;
		}
	}

	@ListParam
	private List<ConfigProperty<T>> list;

	/**
	 * Constructor
	 */
	public ListOfConfigProperties() {
		list = new ArrayList<ConfigProperty<T>>();
	}

	// Redirections

	public boolean containsKey(String propertyName) {
		for (ConfigProperty prop : list) {
			if (prop.name.equals(propertyName))
				return true;
		}
		return false;

//			return map.containsKey(propertyName);
	}

	public T get(String propertyName) {
		for (ConfigProperty prop : list) {
			if (prop.name.equals(propertyName))
				return (T) prop.value; // Takes the first
		}
		return null;

//			return map.get(propertyName);
	}

	public void put(String propertyName, T value) {
//		Boolean b = (Boolean) value;

		list.add(new ConfigProperty<T>(propertyName, value));

//			map.put(propertyName, value);
	}

	public void remove(String propertyName) {

		for (Iterator i = list.iterator(); i.hasNext();) {
			ConfigProperty prop = (ConfigProperty) i.next();
			if (prop.name.equals(propertyName)) {
				i.remove();
				break;
			}
		}

//			map.remove(propertyName);
	}

	public int size() {
		return list.size();

//			return map.size();
	}

	public boolean isEmpty() {
		return list.isEmpty();

//			return map.isEmpty();
	}

	public void clear() {
		list.clear();

//			map.clear();
	}

	public Set keySet() {
		Set s = new HashSet();
		for (ConfigProperty prop : list) {
			s.add(prop.name);
		}
		return s;

//			return map.keySet();
	}

	public Collection values() {
		Collection l = new ArrayList();
		for (ConfigProperty prop : list) {
			l.add(prop.value);
		}
		return l;

//			return map.values();
	}

	@Override
	public String toString() {
		StringBuffer b = new StringBuffer();
		for (ConfigProperty prop : list) {
			b.append(" ");
			b.append(prop.name);
			b.append(": ");
			b.append(prop.value);
		}
		return b.toString();
	}

}
