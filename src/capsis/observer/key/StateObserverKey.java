package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * An class to define the key of a StateObserver
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public class StateObserverKey extends ObserverKey {

	private String label;

	/**
	 * Constructor
	 */
	public StateObserverKey(ObservableVariable observableVariable) {
		super(observableVariable);
		label = "state";
	}

	@Override
	public boolean equals(ObserverKey ok) {
		if (!ok.getClass().equals(this.getClass()))
			return false;
		StateObserverKey sok = (StateObserverKey) ok;
		return (this.getObservableVariable().equals(sok.getObservableVariable()));
	}

	@Override
	public String getLabel() {
		return label;
	}

}
