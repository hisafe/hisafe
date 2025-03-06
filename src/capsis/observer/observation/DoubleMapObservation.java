package capsis.lib.cstability.observer.observation;

import java.io.BufferedWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.observer.ObservableVariable;

/**
 * A double map observation of a Variable of C-STABILITY
 *
 * @author J. Sainte-Marie - June 2024
 */
@SuppressWarnings("serial")
public class DoubleMapObservation extends Observation {

	private Map<String, Double> doublemap;
	private String keyLabel;

	/**
	 * Constructor
	 */
	public DoubleMapObservation(int date, ObservableVariable vt, Map<String, Double> distributionMap, String keyLabel,
			String valueXLabel, String valueYLabel) {
		super(date, vt);
		this.doublemap = distributionMap;
		this.keyLabel = keyLabel;
	}

	/**
	 * write()
	 */
	public void write(BufferedWriter bw, String separator) throws Exception {
		for (Iterator<String> i = doublemap.keySet().iterator(); i.hasNext();) {
			String key = i.next();
			bw.write(date + separator + key + separator + doublemap.get(key));
			if (i.hasNext())
				bw.newLine();
		}
	}

	/**
	 * getHeader()
	 */
	public String getHeader(String separator) {
		return "date" + separator + keyLabel;
	}

	/**
	 * getDistribution()
	 */
	public Double getDistribution(String key) {
		return doublemap.get(key);
	}

	/**
	 * getDistributionMapKeySet()
	 */
	public Set<String> getDistributionMapKeySet() {
		return doublemap.keySet();
	}

	/**
	 * String getKeyLabel()
	 */
	public String getKeyLabel() {
		return keyLabel;
	}
}
