package capsis.lib.cstability.filereader;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.SubstrateAccessibility;

/**
 * A decoder to feed substrateAccessibilityMap of parameters.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class PoolDecoder implements Decodable {

	private Map<String, Function> poolFunctionMap;

	/**
	 * Constructor()
	 */
	public PoolDecoder() {

	}

	/**
	 * decode()
	 */
	public Decodable decode(String encodedString, Parameters p, Context c) throws Exception {

		try {

			String s = encodedString.trim();

			StringTokenizer st = new StringTokenizer(s, "\t");
			String flag = st.nextToken().trim();

			if (flag.equals(CodedFlags.POOL_ACCESSIBILITY)) {

				String biochemicalClassName = st.nextToken().trim();
				BiochemicalClass biochemicalClass = p.getBiochemicalClassMap().get(biochemicalClassName);
				if (biochemicalClass == null)
					throw new Exception("Unknown biochemicalClass name: " + biochemicalClassName);

				String accessToken = null;
				try {
					accessToken = st.nextToken().trim();
				} catch (Exception e) {
					throw new Exception(
							"SubstrateAccessibilityDecoder.decode (), could not find accessibility list, verify tabulation separator");
				}

				if (!(accessToken.startsWith("[")) || !(accessToken.endsWith("]")))
					throw new Exception(
							"SubstrateAccessibilityDecoder.decode (), missing [ ] around substrateAccessibility list");

				accessToken = accessToken.replace("[", "");
				accessToken = accessToken.replace("]", "");

				StringTokenizer st2 = new StringTokenizer(accessToken, ",");

				while (st2.hasMoreTokens()) {
					String token = st2.nextToken().trim();
					SubstrateAccessibility sa = SubstrateAccessibility.getSubstrateAccessibility(token);
					p.addSubstrateAccessibility(biochemicalClassName, sa);
				}

			} else if (flag.equals(CodedFlags.POOL_INITIALIZATION)) {

				String bcName = st.nextToken().trim();
				BiochemicalClass bc = p.getBiochemicalClassMap().get(bcName);
				if (bc == null)
					throw new Exception("Unknown biochemicalClass name: " + bcName);
				String accessKey = st.nextToken().trim();
				// check if accessibility exists
				SubstrateAccessibility.getSubstrateAccessibility(accessKey);
				// gaussian(1.2;0.1)
				Function poolFunction = Function.getFunction(st.nextToken().trim(), p, c);
				if (poolFunctionMap == null)
					poolFunctionMap = new HashMap<>();
				poolFunctionMap.put(createKey(bcName, accessKey), poolFunction);

			} else {

				throw new Exception(
						"wrong flag, expect" + CodedFlags.POOL_ACCESSIBILITY + " or " + CodedFlags.POOL_INITIALIZATION);

			}
			// everything has been added in parameters in the while loop above.
			return this;

		} catch (Exception e) {
			throw new Exception("SubstratePoolDecoder.decode (), could not parse this encodedString: " + encodedString,
					e);
		}
	}

	/**
	 * createKey()
	 */
	private String createKey(String bcName, String accessKey) {
		return bcName + "&" + accessKey;
	}

	/**
	 * getBiochemicalClassName(): key has been created by createKey()
	 */
	public String getBiochemicalClassName(String key) {
		return key.substring(0, key.indexOf("&"));
	}

	/**
	 * getAccessibilityKey(): key has been created by createKey()
	 */
	public String getAccessibilityKey(String key) {
		return key.substring(key.indexOf("&") + 1);
	}

	/**
	 * getPoolFunctionMap()
	 */
	public Map<String, Function> getPoolFunctionMap() {
		return poolFunctionMap;
	}
}
