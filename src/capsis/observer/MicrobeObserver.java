package capsis.lib.cstability.observer;

import java.util.List;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.observer.key.MicrobeObserverKey;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.Microbe;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Date;

/**
 * The observer which extract data from current state of microbe.
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class MicrobeObserver extends Observer {

	private String microbeName;

	/**
	 * Constructor: default for Decodable
	 */
	public MicrobeObserver() {
		super();
	}

	/**
	 * Constructor
	 */
	public MicrobeObserver(String microbeName, String variableName, List<Integer> datesToObserve) throws Exception {
		super(ObservableVariableType.MICROBE, variableName, datesToObserve);
		this.microbeName = microbeName;
		this.key = new MicrobeObserverKey(observableVariable, microbeName);
	}

	/**
	 * getMicrobeName()
	 */
	public String getMicrobeName() {
		return microbeName;
	}

	/**
	 * getObservedItemName()
	 */
	public String getObservedItemName() {
		return microbeName;
	}

	/**
	 * observe()
	 */
	public void observe(State s, int date) throws Exception {
		if (datesToObserve.contains(date)) {
			Microbe m = s.getMicrobe(microbeName);
			observations.add(observableVariable.getValue(date, m));
		}
	}

	/**
	 * decode()
	 */
	@Override
	public MicrobeObserver decode(String encodedString, Parameters p, Context c) throws Exception {

		try {
			String s = encodedString.trim();
			StringTokenizer st = new StringTokenizer(s, "\t");

			String flag = st.nextToken().trim();

			if (flag.equals("MICROBE_OBSERVER")) {

				String microbeName = st.nextToken().trim();
				String variableName = st.nextToken().trim();
				String datesString = st.nextToken().trim();

				return new MicrobeObserver(microbeName, variableName, Date.read(datesString));
			} else {
				throw new Exception("Wrong flag, expect MICROBE_OBSERVER");
			}

		} catch (Exception e) {
			throw new Exception("MicrobeObserver.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}
}
