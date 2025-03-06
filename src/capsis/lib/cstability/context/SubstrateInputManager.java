package capsis.lib.cstability.context;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.poolinput.FunctionalPoolInputManager;
import capsis.lib.cstability.context.poolinput.PoolInput;
import capsis.lib.cstability.context.poolinput.PoolInputManager;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.functions.ConstantInputFunction;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.SubstrateAccessibility;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.state.Substrate;

/**
 * A manager of substrate temporal inputs of C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class SubstrateInputManager implements Decodable {

	private Map<String, PoolInputManager> poolInputManagerMap;
	private Substrate substrate;

	/**
	 * Constructor
	 */
	public SubstrateInputManager() {
		poolInputManagerMap = new HashMap<>();
	}

	/**
	 * addPoolInputManager()
	 */
	public void addPoolInputManager(String poolKey, PoolInputManager pim) {
		poolInputManagerMap.put(poolKey, pim);
	}

	/**
	 * getPoolInputManager()
	 */
	public PoolInputManager getPoolInputManager(String poolKey) {
		return poolInputManagerMap.get(poolKey);
	}

	/**
	 * getPoolInput()
	 */
	public PoolInput getPoolInput(Parameters p, Context c, State s, String poolKey, double date) throws Exception {
		PoolInputManager pim = poolInputManagerMap.get(poolKey);
		if (pim == null)
			return null;
		return pim.getInput(p, c, s, date);
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

			if (!flag.equals("POOL_INPUT"))
				throw new Exception("Wrong flag for poolInput line: " + flag + ", expected POOL_INPUT");

			// e.g. POOL_INPUT cellulose ACCESSIBLE encodedInputManager
			String bcName = st.nextToken().trim();
			SubstrateAccessibility accessibility = SubstrateAccessibility
					.getSubstrateAccessibility(st.nextToken().trim());

			String encodedInputManager = st.nextToken().trim();
			try {
				Function inputFunction = (Function) Decodable.pleaseDecode(ConstantInputFunction.class,
						encodedInputManager, p, c);
				addPoolInputManager(bcName + "_" + accessibility.getKey(),
						new FunctionalPoolInputManager(bcName, accessibility.getKey(), inputFunction));
				return this;
			} catch (Exception e) {
				// try with next input function
			}

			// inputManager has not been decoded correctly
			throw new Exception("SubstrateInputManager.decode(), could not decode inputManager:" + encodedInputManager);

		} catch (Exception e) {
			if (e instanceof NullPointerException)
				e.printStackTrace(System.out);
			throw new Exception("SubstrateInputManager.decode(), could not parse this encodedString: " + encodedString,
					e);
		}
	}

}