package capsis.lib.cstability.observer.observation;

import java.io.BufferedWriter;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * A double observation of a Variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class DoubleObservation extends Observation {

	private double scalar;

	/**
	 * Constructor
	 */
	public DoubleObservation(int date, ObservableVariable ov, double scalar) {
		super(date, ov);
		this.scalar = scalar;
	}

	/**
	 * write()
	 */
	public void write(BufferedWriter bw, String separator) throws Exception {
		bw.write("" + date + separator + scalar);
	}

	/**
	 * getLabel()
	 */
	public String getLabel() {
		return observableVariable.getName();
	}

	/**
	 * getHeader()
	 */
	public String getHeader(String separator) {
		return "date" + separator + getLabel();
	}

	/**
	 * getScalar()
	 */
	public double getScalar() {
		return scalar;
	}
}
