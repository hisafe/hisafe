package capsis.util.categorycalculator;

/**
 * A calculator working on categories.
 * 
 * @author F. de Coligny - May 2021
 */
public abstract class Calculator {

	private String name;

	/**
	 * Constructor
	 */
	public Calculator(String name) {
		this.name = name;
	}

	/**
	 * Returns the value for the given Object. E.g. a basalAreaCalculator will be
	 * passed a tree and will return the tree basal area. Can be overriden on the
	 * fly in a subclass.
	 */
	public abstract double getValue(Object o);

	public String getName() {
		return name;
	}
}
