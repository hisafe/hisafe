package capsis.lib.cstability.observer;

import java.io.Serializable;
import java.util.List;

import capsis.lib.cstability.observer.observation.DoubleObservation;
import capsis.lib.cstability.observer.observation.Observation;

/**
 * A factory to create observationTimeSeries
 *
 * @author J. Sainte-Marie - March 2024
 */
@SuppressWarnings("serial")
public class ObservationTimeSeriesFactory implements Serializable {

	public ObservationTimeSeriesFactory() {

	}

	public ObservationTimeSeries getObservationTimeSeries(List<Observation> ol) throws Exception {

		if (ol.size() == 0)
			throw new Exception(
					"ObservationTimeSeries.getObservationTimeSeries(): list of observation must contain at least one element");

		double[] valuesX = new double[ol.size()];
		double[] valuesY = new double[ol.size()];

		for (int i = 0; i < ol.size(); ++i) {
			if (ol.get(i) instanceof DoubleObservation) {
				DoubleObservation obs = (DoubleObservation) ol.get(i);
				valuesX[i] = obs.getDate();
				valuesY[i] = obs.getScalar();
			} else
				throw new Exception(
						"ObservationTimeSeries.getObservationTimeSeries(): list of Observation must contain DoubleObservation observations");
		}

		return new ObservationTimeSeries(valuesX, valuesY, ol.get(0).getObservableVariable().getName());

	}
}
