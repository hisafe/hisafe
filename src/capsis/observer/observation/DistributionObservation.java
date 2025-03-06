package capsis.lib.cstability.observer.observation;

import java.io.BufferedWriter;

import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.observer.ObservableVariable;

/**
 * A distribution observation of a Variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class DistributionObservation extends Observation {

	private DiscreteDistribution distribution;
	private String valueXLabel;
	private String valueYLabel;

	/**
	 * Constructor
	 */
	public DistributionObservation(int date, ObservableVariable vt, DiscreteDistribution distribution,
			String valueXLabel, String valueYLabel) {
		super(date, vt);
		this.distribution = distribution;
		this.valueXLabel = valueXLabel;
		this.valueYLabel = valueYLabel;
	}

	/**
	 * write()
	 */
	public void write(BufferedWriter bw, String separator) throws Exception {
		String linePrefix = "" + date;
		bw.write(distribution.getFormated(linePrefix, separator));
	}

	/**
	 * getHeader()
	 */
	public String getHeader(String separator) {
		return distribution.getHeader("date", valueXLabel, valueYLabel, separator);
	}

	/**
	 * getDistribution()
	 */
	public DiscreteDistribution getDistribution() {
		return distribution;
	}

	/**
	 * getValueXLabel()
	 */
	public String getValueXLabel() {
		return valueXLabel;
	}

	/**
	 * getValueYLabel()
	 */
	public String getValueYLabel() {
		return valueYLabel;
	}

}
