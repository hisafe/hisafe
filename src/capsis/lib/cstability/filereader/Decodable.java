package capsis.lib.cstability.filereader;

import java.io.Serializable;
import java.lang.reflect.Constructor;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.parameter.Parameters;

/**
 * An interface for object which can be decoded from a String.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
public interface Decodable extends Serializable {

	/**
	 * pleaseDecode(): a static method calling decode in the class, creates a
	 * prototype on the fly. e.g. Interval<Double> domain =
	 * Interval.pleaseDecode(st.nextToken().trim(), p, c);
	 */
	public static Decodable pleaseDecode(Class klass, String encodedString, Parameters p, Context c) throws Exception {

		Constructor ctr = null;
		try {
			ctr = klass.getConstructor();
		} catch (Exception e) {
			throw new Exception("Decodable.decodeStatic (), missing default constructor in class: " + klass);
		}

		try {
			Decodable prototype = (Decodable) ctr.newInstance();
			return prototype.decode(encodedString, p, c);
		} catch (Exception e) {
			throw new Exception("Decodable.decodeStatic (), class: " + klass + " could not decode: " + encodedString,
					e);
		}
	}

	/**
	 * decode()
	 */
	public Decodable decode(String encodedString, Parameters p, Context c) throws Exception;

}
