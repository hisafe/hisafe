package capsis.lib.cstability.observer.observation;

import java.io.BufferedWriter;
import java.io.Serializable;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * An observation of a Variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public abstract class Observation implements Serializable {

	protected int date;
	protected ObservableVariable observableVariable;

	/**
	 * Constructor
	 */
	public Observation(int date, ObservableVariable vt) {
		this.date = date;
		this.observableVariable = vt;
	}

	/**
	 * write()
	 */
	public abstract void write(BufferedWriter bw, String separator) throws Exception;

	/**
	 * getDate()
	 */
	public double getDate() {
		return date;
	}

	/**
	 * getObservableVariable()
	 */
	public ObservableVariable getObservableVariable() {
		return observableVariable;
	}

	/**
	 * getHeader()
	 */
	public abstract String getHeader(String separator);
	
}
