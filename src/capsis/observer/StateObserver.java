package capsis.lib.cstability.observer;

import java.util.List;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.observer.key.StateObserverKey;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Date;

/**
 * The observer which extract data from current state.
 *
 * @author J. Sainte-Marie, F. de Coligny - June 2021
 */
@SuppressWarnings("serial")
public class StateObserver extends Observer {

	/**
	 * Constructor: default for Decodable
	 */
	public StateObserver() {
		super();
	}

	/**
	 * Constructor
	 */
	public StateObserver(String variableName, List<Integer> datesToObserve) throws Exception {
		super(ObservableVariableType.STATE, variableName, datesToObserve);
		this.key = new StateObserverKey(observableVariable);
	}

	/**
	 * getObservedItemName()
	 */
	public String getObservedItemName() {
		return null;
	}

	/**
	 * observe()
	 */
	public void observe(State s, int date) throws Exception {
		if (datesToObserve.contains(date)) {
			observations.add(observableVariable.getValue(date, s));
		}
	}

	/**
	 * decode()
	 */
	@Override
	public StateObserver decode(String encodedString, Parameters p, Context c) throws Exception {

		try {
			String s = encodedString.trim();
			StringTokenizer st = new StringTokenizer(s, "\t");

			String flag = st.nextToken().trim();
			// STATE_OBSERVER respiration [365]
			if (flag.equals("STATE_OBSERVER")) {

				String variableName = st.nextToken().trim();
				String datesString = st.nextToken().trim();

				return new StateObserver(variableName, Date.read(datesString));
			} else {
				throw new Exception("Wrong flag, expect STATE_OBSERVER");
			}

		} catch (Exception e) {
			throw new Exception("StateObserver.decode (), could not parse this encodedString: " + encodedString, e);
		}
	}
}
