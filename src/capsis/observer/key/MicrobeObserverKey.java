package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * An class to define the key of a MicrobeObserver
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public class MicrobeObserverKey extends ObserverKey {

	private String microbeName;
	private String label;

	/**
	 * Constructor
	 */
	public MicrobeObserverKey(ObservableVariable observableVariable, String microbeName) {
		super(observableVariable);
		this.microbeName = microbeName;
		label = microbeName;
	}

	/**
	 * getMicrobeName()
	 */
	public String getMicrobeName() {
		return microbeName;
	}

	@Override
	public boolean equals(ObserverKey ok) {
		if (!ok.getClass().equals(this.getClass()))
			return false;

		MicrobeObserverKey mok = (MicrobeObserverKey) ok;
		return (this.getObservableVariable().equals(mok.getObservableVariable()))
				&& this.microbeName.equals(mok.getMicrobeName());
	}

	@Override
	public String getLabel() {
		return label;
	}
}
