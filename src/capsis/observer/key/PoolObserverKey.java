package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;
import capsis.lib.cstability.parameter.SubstrateAccessibility;
import capsis.lib.cstability.state.PoolKey;

/**
 * An class to define the key of a PoolObserver
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public class PoolObserverKey extends ObserverKey {

	private String biochemicalClassName;
	private SubstrateAccessibility accessibility;
	private String label;

	/**
	 * Constructor
	 */
	public PoolObserverKey(ObservableVariable observableVariable, String poolName,
			SubstrateAccessibility accessibility) {
		super(observableVariable);
		this.biochemicalClassName = poolName;
		this.accessibility = accessibility;
		label = PoolKey.generateKey(biochemicalClassName, accessibility.getKey());
	}

	/**
	 * getPoolName()
	 */
	public String getBiochemicalClassName() {
		return biochemicalClassName;
	}

	/**
	 * getAccessibility()
	 */
	public SubstrateAccessibility getAccessibility() {
		return accessibility;
	}

	@Override
	public boolean equals(ObserverKey ok) {
		if (!ok.getClass().equals(this.getClass()))
			return false;
		PoolObserverKey pok = (PoolObserverKey) ok;
		return (this.getObservableVariable().equals(pok.getObservableVariable()))
				&& this.biochemicalClassName.equals(pok.getBiochemicalClassName())
				&& this.accessibility.equals(pok.getAccessibility());
	}

	@Override
	public String getLabel() {
		return label;
	}
}
