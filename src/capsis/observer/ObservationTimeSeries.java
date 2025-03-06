package capsis.lib.cstability.observer;

import java.io.Serializable;

import capsis.lib.cstability.util.XYseries;

/**
 * An ObservationTimeSeries is an XYseries that can be build from a list of
 * DoubleObservation
 *
 * @author J. Sainte-Marie - March 2024
 */
@SuppressWarnings("serial")
public class ObservationTimeSeries extends XYseries implements Serializable {

	private String yLabel;

	public ObservationTimeSeries(double[] valuesX, double[] valuesY, String yLabel) throws Exception {
		super(valuesX, valuesY);
		this.yLabel = yLabel;
	}

	/**
	 * getLabel()
	 */
	public String getLabel() {
		return yLabel;
	}

}
