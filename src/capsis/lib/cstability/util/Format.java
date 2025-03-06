package capsis.lib.cstability.util;

import java.text.NumberFormat;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;

/**
 * Tools to format strings
 *
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
public class Format {

	private static final NumberFormat nf;

	static {
		nf = NumberFormat.getInstance(Locale.ENGLISH);
		nf.setGroupingUsed(false);
		nf.setMaximumFractionDigits(3);
	}

	/**
	 * toString()
	 */
	public static String toString(double v) {
		return nf.format(v);
	}

	/**
	 * toString()
	 */
	public static String toString(double[] tab) {

		StringBuffer b = new StringBuffer("[");
		for (int i = 0; i < tab.length; i++) {

			b.append(toString(tab[i]));

			if (i < tab.length)
				b.append(", ");
		}
		b.append("]");
		return "" + b;
	}

	/**
	 * toString()
	 */
	public static String toString(Collection c) {
		String CR = "\n";

		StringBuffer b = new StringBuffer();

		if (c == null) {
			b.append("null");
			return "" + b;
		}

		for (Iterator i = c.iterator(); i.hasNext();) {

			b.append(CR);

			b.append(i.next()); // toString ()
			if (i.hasNext())
				b.append(", ");
		}

//		b.append(CR);

		return "" + b;
	}

	/**
	 * toString()
	 */
	public static String toString(Collection c, String sep) {

		StringBuffer b = new StringBuffer();

		if (c == null) {
			b.append("null");
			return "" + b;
		}

		for (Iterator i = c.iterator(); i.hasNext();) {
			b.append(i.next()); // toString ()
			if (i.hasNext())
				b.append(sep);
		}
		return "" + b;
	}

	/**
	 * printKeys()
	 */
	public static String printKeys(Map m) {
		String CR = "\n";

		StringBuffer b = new StringBuffer();

		if (m == null) {
			b.append("null");
			return "" + b;
		}
		for (Iterator i = m.keySet().iterator(); i.hasNext();) {

			Object key = i.next();
			b.append(key); // toString ()
			if (i.hasNext())
				b.append(", ");
		}

		b.append(CR);

		return "" + b;

	}

	/**
	 * toString()
	 */
	public static String toString(Map m) {
		String CR = "\n";

		StringBuffer b = new StringBuffer();

		if (m == null) {
			b.append("null");
			return "" + b;
		}

		for (Iterator i = m.keySet().iterator(); i.hasNext();) {

			b.append(CR);

			Object key = i.next();
			Object value = m.get(key);

			b.append(key); // toString ()
			b.append(": ");

			if (value instanceof Collection) {
				b.append(toString((Collection) value));
			} else {
				b.append(value); // toString ()
			}

		}

		b.append(CR);

		return "" + b;
	}

}
