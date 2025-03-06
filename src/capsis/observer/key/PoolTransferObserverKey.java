package capsis.lib.cstability.observer.key;

import capsis.lib.cstability.observer.ObservableVariable;
import capsis.lib.cstability.parameter.PoolTransferTraits;

/**
 * An class to define the key of a PoolTransferObserver
 *
 * @author J. Sainte-Marie, P. Santenoise - February 2024
 */
public class PoolTransferObserverKey extends ObserverKey {

	private String biochemicalClassName;
	private String originName;
	private String arrivalName;
	private String label;

	/**
	 * Constructor
	 */
	public PoolTransferObserverKey(ObservableVariable observableVariable, String biochemicalClassName,
			String originName, String arrivalName) {
		super(observableVariable);

		this.biochemicalClassName = biochemicalClassName;
		this.originName = originName;
		this.arrivalName = arrivalName;
		label = PoolTransferTraits.buildKey(biochemicalClassName, originName, arrivalName);
	}

	/**
	 * getBcName()
	 */
	public String getBiochemicalClassName() {
		return biochemicalClassName;
	}

	/**
	 * getOriginName()
	 */
	public String getOriginName() {
		return originName;
	}

	/**
	 * getArrivalName()
	 */
	public String getArrivalName() {
		return arrivalName;
	}

	@Override
	public boolean equals(ObserverKey ok) {
		if (!ok.getClass().equals(this.getClass()))
			return false;
		PoolTransferObserverKey ptok = (PoolTransferObserverKey) ok;
		return (this.getObservableVariable().equals(ptok.getObservableVariable()))
				&& this.biochemicalClassName.equals(ptok.getBiochemicalClassName())
				&& this.originName.equals(ptok.getOriginName()) && this.arrivalName.equals(ptok.getArrivalName());
	}

	@Override
	public String getLabel() {
		return label;
	}
}
