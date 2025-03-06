package capsis.lib.cstability.observer;

import java.util.List;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.observer.key.EnzymeObserverKey;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Date;

/**
 * The observer which extract data from current state of enzyme.
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class EnzymeObserver extends Observer {

//	private String enzymeName;

	/**
	 * Constructor: default for Decodable
	 */
	public EnzymeObserver() {
		super();
	}

	/**
	 * Constructor
	 */
	public EnzymeObserver(String enzymeName, String variableName, List<Integer> datesToObserve) throws Exception {
		super(ObservableVariableType.ENZYME, variableName, datesToObserve);
		this.key = new EnzymeObserverKey(observableVariable, enzymeName);
	}

	/**
	 * getObservedItemName()
	 */
	public String getObservedItemName() {
		return ((EnzymeObserverKey) key).getEnzymeName();
	}

	/**
	 * observe()
	 */
	public void observe(State s, int date) throws Exception {
		if (datesToObserve.contains(date)) {
			Enzyme e = s.getEnzyme(((EnzymeObserverKey) key).getEnzymeName());
			observations.add(observableVariable.getValue(date, e));
		}
	}

	/**
	 * decode()
	 */
	@Override
	public EnzymeObserver decode(String encodedString, Parameters p, Context c) throws Exception {

		try {
			String s = encodedString.trim();
			StringTokenizer st = new StringTokenizer(s, "\t");

			String flag = st.nextToken().trim();

			if (flag.equals("ENZYME_OBSERVER")) {

				String enzymeName = st.nextToken().trim();
				String variableName = st.nextToken().trim();
				String datesString = st.nextToken().trim();

				return new EnzymeObserver(enzymeName, variableName, Date.read(datesString));
			} else
				throw new Exception("Wrong flag, expect ENZYME_OBSERVER");

		} catch (Exception e) {
			throw new Exception("EnzymeObserver.decode (), could not parse this encodedString: " + encodedString, e);
		}

	}

}
