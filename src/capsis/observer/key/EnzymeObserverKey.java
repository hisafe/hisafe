package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * An class to define the key of an EnzymeObserver
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public class EnzymeObserverKey extends ObserverKey {

	private String enzymeName;
	private String label;

	/**
	 * Constructor
	 */
	public EnzymeObserverKey(ObservableVariable observableVariable, String enzymeName) {
		super(observableVariable);
		this.enzymeName = enzymeName;
		label = enzymeName;
	}

	/**
	 * getEnzymeName()
	 */
	public String getEnzymeName() {
		return enzymeName;
	}

	@Override
	public boolean equals(ObserverKey ok) {
		if (!ok.getClass().equals(this.getClass()))
			return false;
		EnzymeObserverKey eok = (EnzymeObserverKey) ok;
		return (this.getObservableVariable().equals(eok.getObservableVariable()))
				&& this.enzymeName.equals(eok.getEnzymeName());
	}

	@Override
	public String getLabel() {
		return label;
	}
}
