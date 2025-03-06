package capsis.lib.cstability.observer;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.observer.key.ObserverKey;
import capsis.lib.cstability.observer.observation.Observation;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Log;

/**
 * An abstract observer of C-Stability.
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public abstract class Observer implements Decodable, Serializable {

	protected ObservableVariable observableVariable;
	protected List<Integer> datesToObserve;
	protected List<Observation> observations;
	protected ObserverKey key;

	/**
	 * Constructor: default for Decodable
	 */
	public Observer() {
	}

	/**
	 * Constructor
	 */
	public Observer(ObservableVariableType variableType, String variableName, List<Integer> datesToObserve)
			throws Exception {
		this.observableVariable = ObservableVariable.getObservableVariable(variableType, variableName);
		this.datesToObserve = datesToObserve;
		this.observations = new ArrayList<>();
	}

	/**
	 * observe()
	 */
	public abstract void observe(State s, int date) throws Exception;

	/**
	 * write()
	 */
	public void write(String outputDir, boolean appendMode) throws Exception {

		String filePath = "";
		if (observableVariable.getType().equals(ObservableVariableType.STATE)) {
			filePath = outputDir + "/" + observableVariable.getType().getName () + "_" + observableVariable.getName() + ".csv";
		} else {
			filePath = outputDir + "/" + observableVariable.getType().getName () + "_" + getObservedItemName() + "_"
					+ observableVariable.getName() + ".csv";
		}

		Log.trace("	write " + filePath);

		String separator = "\t";

		try {
			File f = new File(filePath);
			boolean headerNeeded = !f.exists();

			BufferedWriter out = new BufferedWriter(new FileWriter(f, appendMode));

			for (Observation o : observations) {
				if (headerNeeded) {
					headerNeeded = false;
					out.write(o.getHeader(separator));
					out.newLine();
				}
				o.write(out, separator);
				out.newLine();
			}
			out.close();

		} catch (Exception e) {
			throw new Exception("Observer.write() could not write in: " + filePath, e);
		}
	}

	/**
	 * getVariableType()
	 */
	public ObservableVariable getObservableVariable() {
		return observableVariable;
	}

	/**
	 * getDatesToObserve()
	 */
	public List<Integer> getDatesToObserve() {
		return datesToObserve;
	}

	/**
	 * getObservations()
	 */
	public List<Observation> getObservations() {
		return observations;
	}

	/**
	 * getObservedItemName()
	 */
	public abstract String getObservedItemName() throws Exception;

	/**
	 * getKey()
	 */
	public ObserverKey getKey() {
		return key;
	}

}
