package capsis.util.categorycalculator;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * A list of categories of individual objects (e.g. trees), can be used with
 * Calculators.
 * 
 * @author F. de Coligny - May 2021
 */
public class ListOfCategories {

	private String name;
	private String calculatorName; // Optional, may be "";

	// LHM keeps insertion order
	private LinkedHashMap<String, Category> categoryMap;

//	private List<Category> categories;

	/**
	 * Constructor
	 */
	public ListOfCategories() {
		name = ""; // Default value
		calculatorName = ""; // Default value
		categoryMap = new LinkedHashMap<>();
	}

	/**
	 * Constructor
	 */
	public ListOfCategories(String name) {
		this();
		this.name = name;
	}

	public List<String> getCategoryNames () {
		return new ArrayList<> (categoryMap.keySet ());
	}
	
	public Category getCategory (String categoryName) {
		return categoryMap.get(categoryName); // null if not found
	}
	
	/**
	 * Sorts the categories according to their names. Sometimes, the insertion order
	 * will be kept, sometimes, this sort method could be useful (e.g. speciesNames)
	 */
	public void sort() {
		Map<String, Category> map = new TreeMap<>(categoryMap); // sorted on keys

		if (map.size() != categoryMap.size())
			return; // some keys may be equal, stop

		categoryMap.clear ();
		for (String cn : map.keySet ()) {
			Category c = map.get(cn);
			categoryMap.put(c.getName(), c);
		}
	}

	// Note: Some applyClassifier (Classifier, collection)) method could be added

	/**
	 * For each object in the given collection, find the matching category and add
	 * the object value.
	 */
	public void applyCalculator(Calculator calc, Collection objects) {

		clearValues();

		for (Object o : objects) {
			this.addValueInCategory(o, calc.getValue(o));
		}

		calculatorName = calc.getName();
	}

	/**
	 * Finds the category matching the object and add the given value in it. If no
	 * category found, dies nothing.
	 */
	public void addValueInCategory(Object o, double value) {
		Category c = whichCategory(o);

		if (c == null)
			return; // Value is not added

		c.addValue(value);
	}

	/**
	 * Returns the category of the object or null if not found.
	 */
	public Category whichCategory(Object o) {
		for (Category c : categoryMap.values())
			if (c.contains(o))
				return c;
		System.out.println("ListOfCategories "+getName ()+" could not find which category for "+o+", passed");
		return null; // not found
	}

	public String getName() {
		return name;
	}

	public String getFullName() {
		String nam = name; // name of the loc
		if (getCalculatorName().length() > 0) // name of the calc (if any)
			nam = nam + " - " + getCalculatorName();
		return nam;
	}

	public String getCalculatorName() {
		return calculatorName;
	}

	public List<Category> getCategories() {
		return new ArrayList<>(categoryMap.values());
	}

	public void addCategory(Category c) {
		this.categoryMap.put(c.getName(), c);
	}

	public void clearValues() {
		for (Category c : getCategories())
			c.clearValue();
	}

	public void clear() {
		categoryMap.clear();
	}

	@Override
	public String toString() {

		StringBuffer b = new StringBuffer(getFullName() + "[");
		for (Iterator<Category> i = getCategories().iterator(); i.hasNext();) {
			Category c = i.next();
			b.append("" + c);
			if (i.hasNext())
				b.append(", ");

		}
		b.append("]");

		return "" + b;
	}

}
