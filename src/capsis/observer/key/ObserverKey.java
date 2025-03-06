package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * An abstract class to define the key of an abstract observer
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public abstract class ObserverKey {

	private ObservableVariable observableVariable;

	/**
	 * Constructor
	 */
	protected ObserverKey(ObservableVariable observableVariable) {
		this.observableVariable = observableVariable;
	}

	/**
	 * getObservableVariable()
	 */
	public ObservableVariable getObservableVariable() {
		return observableVariable;
	}

	/**
	 * equals()
	 */
	public abstract boolean equals(ObserverKey observer);

	/**
	 * getLabel()
	 */
	public abstract String getLabel();
}
