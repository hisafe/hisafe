package capsis.lib.cstability.observer;

import java.util.List;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.observer.key.PoolTransferObserverKey;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.SubstrateAccessibility;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Date;

/**
 * The observer which extract data from current state of a poll transfer.
 *
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class PoolTransferObserver extends Observer {

	/**
	 * Constructor: default for Decodable
	 */
	public PoolTransferObserver() {
		super();
	}

	/**
	 * Constructor
	 */
	public PoolTransferObserver(String bcName, String originName, String arrivalName, String variableName,
			List<Integer> datesToObserve, Parameters p) throws Exception {

		super(ObservableVariableType.POOL_TRANSFER, variableName, datesToObserve);

		key = new PoolTransferObserverKey(observableVariable, bcName, originName, arrivalName);

		// check if pool transfer traits exist in parameters
		if (!p.getPoolTransferTraitsMap().keySet().contains(key.getLabel())) {
			throw new Exception("PoolTransferObserver.constructor(), unknown originKey" + originName);
		}

	}

	/**
	 * observe()
	 */
	@Override
	public void observe(State s, int date) throws Exception {
		if (datesToObserve.contains(date)) {
			PoolTransfer pt = s.getPoolTransfer(key.getLabel());
			observations.add(observableVariable.getValue(date, pt));
		}
	}

	/**
	 * getObservedItemName()
	 */
	@Override
	public String getObservedItemName() throws Exception {
		PoolTransferObserverKey ptok = (PoolTransferObserverKey) key;
		return ptok.getBiochemicalClassName() + "-"
				+ SubstrateAccessibility.getSubstrateAccessibility(ptok.getOriginName()).getStatus() + "_to_"
				+ SubstrateAccessibility.getSubstrateAccessibility(ptok.getArrivalName()).getStatus();
	}

	/**
	 * decode()
	 */
	@Override
	public Decodable decode(String encodedString, Parameters p, Context c) throws Exception {

		try {
			String s = encodedString.trim();
			StringTokenizer st = new StringTokenizer(s, "\t");

			String flag = st.nextToken().trim();

			// POOL_TRANSFER_OBSERVER [cellulose,INACCESSIBLE_EMBEDMENT,ACCESSIBLE]
			// flux_distribution [0,1]
			if (flag.equals("POOL_TRANSFER_OBSERVER")) {

				String temp = st.nextToken().trim();
				temp = temp.replace("[", "");
				temp = temp.replace("]", "");
				StringTokenizer st2 = new StringTokenizer(temp, ",");

				String bcName = st2.nextToken().trim();
				String originName = st2.nextToken().trim();
				String arrivalName = st2.nextToken().trim();

				String variableName = st.nextToken().trim();
				String datesString = st.nextToken().trim();

				return new PoolTransferObserver(bcName, originName, arrivalName, variableName, Date.read(datesString),
						p);
			} else {
				throw new Exception("Wrong flag, expect POOL_TRANSFER_OBSERVER");
			}

		} catch (Exception e) {
			throw new Exception("PoolTransferObserver.decode (), could not parse this encodedString: " + encodedString,
					e);
		}
	}
}
