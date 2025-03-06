package capsis.lib.cstability.parameter;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.CodedFlags;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.util.Encodable;

/**
 * A PoolTranfer definition between two pools of C-STABILITY
 * 
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
@SuppressWarnings("serial")
public class PoolTransferTraits implements Encodable, Decodable, Serializable {

	private String key;
	private BiochemicalClass biochemicalClass;
	private SubstrateAccessibility origin;
	private SubstrateAccessibility arrival;
	private Function transferFunction;

	public static final Encoder POOL_TRANSFER = new Encoder(CodedFlags.POOL_TRANSFER);
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(POOL_TRANSFER);
	}

	/**
	 * Default constructor
	 */
	public PoolTransferTraits() {
	}

	/**
	 * Constructor
	 */
	public PoolTransferTraits(BiochemicalClass bc, SubstrateAccessibility origin, SubstrateAccessibility arrival,
			Function transferFunction) {
		key = buildKey(bc.getName(), origin.getKey(), arrival.getKey());
		biochemicalClass = bc;
		this.origin = origin;
		this.arrival = arrival;
		this.transferFunction = transferFunction;
	}

	/**
	 * buildKey()
	 */
	public static String buildKey(String bcName, String originKey, String arrivalKey) {
		return bcName + "_" + originKey + "_" + arrivalKey;
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

			// # pool biochemicalClass origin arrival transferFunction
			// #POOL_TRANSFER cellulose INACCESSIBLE_EMBEDMENT ACCESSIBLE
			// linear_enzymatic_transfer(ligninase,1)
			if (flag.equals(CodedFlags.POOL_TRANSFER)) {

				String biochemicalClassName = st.nextToken().trim();
				BiochemicalClass biochemicalClass = p.getBiochemicalClassMap().get(biochemicalClassName);
				if (biochemicalClass == null)
					throw new Exception("Unknown biochemicalClass name: " + biochemicalClassName);

				SubstrateAccessibility origin = SubstrateAccessibility.getSubstrateAccessibility(st.nextToken().trim());
				SubstrateAccessibility arrival = SubstrateAccessibility
						.getSubstrateAccessibility(st.nextToken().trim());

				// check if origin and arrival are available pool for biochemicalClass
				boolean originTest = false;
				boolean arrivalTest = false;
				for (SubstrateAccessibility sa : p.getSubstrateAccessibilities(biochemicalClassName)) {
					if (sa.getKey().equals(origin.getKey()))
						originTest = true;
					if (sa.getKey().equals(arrival.getKey()))
						arrivalTest = true;
				}
				if (!originTest)
					throw new Exception("Unknown SubstrateAccessibility: " + origin.getKey() + " for biochemiclass "
							+ biochemicalClassName);
				if (!arrivalTest)
					throw new Exception("Unknown SubstrateAccessibility: " + arrival.getKey() + " for biochemiclass "
							+ biochemicalClassName);

				return new PoolTransferTraits(biochemicalClass, origin, arrival,
						Function.getFunction(st.nextToken().trim(), p, c));
			} else {
				throw new Exception("Wrong flag, expect " + CodedFlags.POOL_TRANSFER);
			}

		} catch (Exception e) {
			throw new Exception("PoolTransferTraits.decode (), could not parse this encodedString: " + encodedString,
					e);
		}
	}

	/**
	 * getKey()
	 */
	public String getKey() {
		return key;
	}

	/**
	 * getBiochemicalClass()
	 */
	public BiochemicalClass getBiochemicalClass() {
		return biochemicalClass;
	}

	/**
	 * getOrigin()
	 */
	public SubstrateAccessibility getOrigin() {
		return origin;
	}

	/**
	 * getArrival()
	 */
	public SubstrateAccessibility getArrival() {
		return arrival;
	}

	/**
	 * getOriginName()
	 */
	public String getOriginName() {
		return origin.getKey();
	}

	/**
	 * getArrivalName()
	 */
	public String getArrivalName() {
		return arrival.getKey();
	}

	/**
	 * getTransferFunction()
	 */
	public Function getTransferFunction() {
		return transferFunction;
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> encodedMap = new HashMap<>();
		encodedMap.put(POOL_TRANSFER, CodedFlags.POOL_TRANSFER + TAB + getBiochemicalClass().getName() + TAB
				+ origin.getKey() + TAB + arrival.getKey() + TAB + transferFunction.getEncodedFunction());
		return encodedMap;
	}

	@Override
	public String encode(Encoder encoder) {
		return encode().get(encoder);
	}

	@Override
	public Set<Encoder> getEncoders() {
		return ENCODERS;
	}

}
