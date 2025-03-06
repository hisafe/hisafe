package capsis.lib.cstability.util;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.filereader.Decodable;
import capsis.lib.cstability.parameter.Parameters;

/**
 * Polymerization of a biochemical class.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Interval<T extends Number & Comparable> implements Decodable, Encodable, Serializable {

	private T min;
	private T max;

	public static final Encoder INTERVAL = new Encoder("INTERVAL");
	public static final Set<Encoder> ENCODERS = new HashSet<>();

	static {
		ENCODERS.add(INTERVAL);
	}

	/**
	 * Default constructor
	 */
	public Interval() {
	}

	/**
	 * Constructor
	 */
	public Interval(T min, T max) throws Exception {

		if (max.compareTo(min) < 0)
			throw new Exception("Wrong Interval, max: " + max + " < min: " + min);

		this.min = min;
		this.max = max;
	}

	/**
	 * Copy Constructor
	 */
	public Interval(Interval<T> interval) {
		min = interval.min;
		max = interval.max;
	}

	/**
	 * decode(): decoding method from an encoded string
	 */
	@Override
	public Interval<Double> decode(String encodedString, Parameters p, Context c) throws Exception {

		// e.g. [0.12,0.86]
		try {
			String s = encodedString.trim();
			s = s.replace("[", "");
			s = s.replace("]", "");
			StringTokenizer st = new StringTokenizer(s, ",");
			double min = Double.parseDouble(st.nextToken().trim());
			double max = Double.parseDouble(st.nextToken().trim());

			return new Interval<Double>(min, max);

		} catch (Exception e) {
			throw new Exception("Interval(), could not parse this encodedString: " + encodedString, e);
		}

	}

	/**
	 * setMin()
	 */
	public void setMin(T min) {
		this.min = min;
	}

	/**
	 * setMax()
	 */
	public void setMax(T max) {
		this.max = max;
	}

	/**
	 * getMin()
	 */
	public T getMin() {
		return min;
	}

	/**
	 * getMax()
	 */
	public T getMax() {
		return max;
	}

	/**
	 * length()
	 */
	public double length() {
		return max.doubleValue() - min.doubleValue();
	}

	/**
	 * contains()
	 */
	public boolean contains(T x) {
		return (min.compareTo(x) <= 0) && (x.compareTo(max) <= 0);
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "Interval [" + min + ", " + max + "]";
	}

	@Override
	public Map<Encoder, String> encode() {
		Map<Encoder, String> map = new HashMap<>();
		map.put(INTERVAL, "[" + min + "," + max + "]");
		return map;
	}

	@Override
	public Set<Encoder> getEncoders() {
		return ENCODERS;
	}

	@Override
	public String encode(Encoder encoder) {
		return encode().get(INTERVAL);
	}

}
