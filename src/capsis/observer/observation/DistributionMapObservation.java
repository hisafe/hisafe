package capsis.lib.cstability.observer.observation;

import java.io.BufferedWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.observer.ObservableVariable;

/**
 * A distribution map observation of a Variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class DistributionMapObservation extends Observation {

	private Map<String, DiscreteDistribution> distributionMap;
	private String keyLabel;
	private String valueXLabel;
	private String valueYLabel;

	/**
	 * Constructor
	 */
	public DistributionMapObservation(int date, ObservableVariable vt,
			Map<String, DiscreteDistribution> distributionMap, String keyLabel, String valueXLabel,
			String valueYLabel) {
		super(date, vt);
		this.distributionMap = distributionMap;
		this.keyLabel = keyLabel;
		this.valueXLabel = valueXLabel;
		this.valueYLabel = valueYLabel;
	}

	/**
	 * write()
	 */
	public void write(BufferedWriter bw, String separator) throws Exception {
		for (Iterator<String> i = distributionMap.keySet().iterator(); i.hasNext();) {
			String key = i.next();
			DiscreteDistribution dd = distributionMap.get(key);
			String linePrefix = date + separator + key;
			bw.write(dd.getFormated(linePrefix, separator));
			if (i.hasNext())
				bw.newLine();
		}
	}

	/**
	 * getHeader()
	 */
	public String getHeader(String separator) {
		return "date" + separator + keyLabel + separator + valueXLabel + separator + valueYLabel;
	}

	/**
	 * getDistribution()
	 */
	public DiscreteDistribution getDistribution(String key) {
		return distributionMap.get(key);
	}

	/**
	 * getDistributionMapKeySet()
	 */
	public Set<String> getDistributionMapKeySet() {
		return distributionMap.keySet();
	}

	/**
	 * String getKeyLabel()
	 */
	public String getKeyLabel() {
		return keyLabel;
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
