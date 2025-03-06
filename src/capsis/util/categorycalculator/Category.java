package capsis.util.categorycalculator;

import java.text.NumberFormat;

import jeeb.lib.util.DefaultNumberFormat;

/**
 * A category of individual objects (e.g. trees), see ListOfCategories, can be
 * used with Calculators.
 * 
 * @author F. de Coligny - May 2021
 */
public abstract class Category implements Comparable {

	NumberFormat nf = DefaultNumberFormat.getInstance(2);

	private String name;
	private double value;

	/**
	 * Constructor
	 */
	public Category() {
	}

	/**
	 * Constructor
	 */
	public Category(String name) {
		this();
		this.name = name;
	}

	/**
	 * Name of the category
	 */
	public String getName() {
		return name;
	}

	/**
	 * The name may be changed (e.g. if it contains thresholds...)
	 */
	public void setName(String name) {
		this.name = name;
	}

	public void setValue(double value) {
		this.value = value;
	}

	public void addValue(double value) {
		this.value += value;
	}

	public double getValue() {
		return value;
	}

	public void clearValue() {
		this.value = 0;
	}

	/**
	 * Returns true if the category contains the given Object. Can be the
	 * speciesName, or a pair of thresholds on tree dbh, height... Can be overriden
	 * on the fly in a subclass.
	 */
	public abstract boolean contains(Object o);

	/**
	 * Comparable, can be overriden
	 */
	public int compareTo(Object o) {
		Category other = (Category) o;
		return getName().compareTo(other.getName());
	}

	@Override
	public String toString() {
		return name + ": " + nf.format(value);
	}

}
