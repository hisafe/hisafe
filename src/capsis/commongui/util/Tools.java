/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.commongui.util;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Event;
import java.io.File;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JComponent;

import capsis.defaulttype.Speciable;
import capsis.defaulttype.Species;
import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.defaulttype.TreeWithCrownProfile;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Settings;

/**
 * Tools proposes a collection of static methods.
 *
 * @author F. de Coligny - june 1999
 */
public class Tools extends AmapTools {
	// ~ public final static int CUT_MODE = 0; // fc - 28.11.2007 - DEPRECATED
	public final static int HTML_MODE = 0; // fc - 28.11.2007
	public final static int WRAP_MODE = 1;

	public static int capsisCtrlMask = -1; // initialized at first use : see
											// getCtrlMask ()

	// fc - 4.9.2008 - colors for getCrownColor ()
	public final static Color[] treeColors = { Color.ORANGE, Color.RED, Color.BLUE, Color.GREEN, Color.PINK, Color.CYAN,
			Color.MAGENTA, Color.YELLOW, Color.BLACK };

	

	/**
	 * Checks wether the 2 collections contain the same entries (order ignored, not
	 * like List.equals ()).
	 */
	static public boolean sameEntries(Collection c1, Collection c2) {

		// fc-11.10.2023
		
		if (c1 == null && c2 != null)
			return false;
		if (c1 != null && c2 == null)
			return false;
		if (c1 == null && c2 == null)
			return true;

		if (c1.size() != c2.size())
			return false;

		Set copy = new HashSet(c2); // faster on contains()
		for (Object o : c1)
			if (!copy.contains(o))
				return false;

		return true; // all items are in both collections
	}

	
	
	/**
	 * Encodes a Collection of String elements into a single String with
	 * elements separated by ", ". The given Collection can be null or empty.
	 * Returns a message if an element is not a String. See decode ().
	 */
	static public String encode(Collection collection) {

		// fc-2.8.2018
		
		// A null collection is ok
		if (collection == null)
			return "";

		StringBuffer b = new StringBuffer();
		for (Object o : collection) {
			if (!(o instanceof String))
				return
						"Tools.encode () accepts only collections of strings, could not process this element: " + o;

			if (b.length() > 0)
				b.append(", ");
			b.append((String) o);

		}

		return b.toString();

	}
	
	/**
	 * A method to encode a String[] into a single String, see encode(Collection).
	 */
	static public String encode(String[] elements) {
		return encode (Arrays.asList(elements));
	}

	/**
	 * Processes the given encoded String, split it with a ", " separator to get
	 * elements and add them in the returned Strng[]. If the encodedString is null
	 * or empty, return an empty String[]. See encode ().
	 */
	static public String[] decode(String encodedString) {

		// fc-2.8.2018

		if (encodedString == null || encodedString.isEmpty())
			return new String[0];

		String[] elts = encodedString.split(", ");

		return elts;
	}

	/**
	 * Correct file separators according to the OS.
	 */
	public static String correctFileSeparators(String fileName) {
		return fileName.replace('\\', File.separatorChar).replace('/', File.separatorChar);
	}

	/**
	 * Returns true if the network is available. If the network wire is not
	 * plugged on the computer, returns false immediately. If the network wire
	 * is not plugged in the wall socket, returns false after a while (approx.
	 * 30s.)
	 */
	static public boolean isNetworkAvailable() {
		boolean seemsConnected = false;
		try {
			URL url = new URL(Settings.getProperty("capsis.url", "http://www.google.com"));
			url.openConnection().connect();
			seemsConnected = true;
		} catch (Exception e) {
		}
		return seemsConnected;
	}

	/**
	 * Returns a color for the tree crown.
	 */
	static public Color getCrownColor(Object t) { // fc - 4.9.2008
		if (t instanceof SimpleCrownDescription) {
			SimpleCrownDescription scd = (SimpleCrownDescription) t;
			Color crownColor = scd.getCrownColor();
			if (crownColor != null) {
				return crownColor;
			} // 1. SimpleCrownDescrition
		}
		if (t instanceof TreeWithCrownProfile) {
			TreeWithCrownProfile cp = (TreeWithCrownProfile) t;
			Color crownColor = cp.getCrownColor();
			if (crownColor != null) {
				return crownColor;
			} // 2. TreeWithCrownProfile
		}

		if (t instanceof Speciable) {
			Species species = ((Speciable) t).getSpecies();
			try {
				return treeColors[species.getValue()]; // 3. Speciable
			} catch (Exception e) {
				return Color.GRAY; // 3.1. (too many species values)
			}
		}

		return Color.GRAY; // 4. Default
	}

	static public int getSmallButtonSize() {
		return 23;
	}

	/**
	 * If the given parameter is a Collection, returns it else return a
	 * Collection containing it.
	 */
	public static Collection<Object> intoCollection(Object o) {
		if (o instanceof Collection) {
			return (Collection<Object>) o;
		}
		Collection<Object> aux = new ArrayList<Object>();
		aux.add(o);
		return aux;
	}

	/**
	 * Examine a String containing numbers and extracts them in a String array.
	 * Numbers must respect the given tokenType. Text can contain (1) one single
	 * value (ex: "12") ; (2) some values separated by ',' (ex: "12,13,16") ;
	 * (3) an int interval (ex: "12-15").
	 */
	// fc - 4.2.2004
	static public String[] hackEnumeration(String text, Class tokenType) throws Exception {
		if (tokenType != int.class && tokenType != double.class) {
			throw new Exception("wrong use: tokenType must be int.class or double.class");
		}

		String[] result = null;
		String singleValue = null;

		text = text.trim();

		boolean idsInterval = false; // 12-15
		boolean idsEnumeration = false; // 12, 15, 16
		boolean singleId = false; // 12

		if (text.indexOf('-') != -1) {
			if (tokenType != int.class) {
				throw new Exception("wrong use: interval only possible between two int values");
			}
			int a = text.indexOf('-');
			int b = text.lastIndexOf('-');
			if (a == b) {
				idsInterval = true;
			} // only one '-' is permitted
		}
		if (text.indexOf(',') != -1) {
			idsEnumeration = true;
		}
		try {
			if (tokenType == int.class) {
				new Integer(text).intValue();

			} else if (tokenType == double.class) {
				new Double(text).intValue();
			}
			singleValue = text;
			singleId = true;
		} catch (Exception e) {
		}
		if ((idsInterval && !idsEnumeration && !singleId) || (!idsInterval && idsEnumeration && !singleId)
				|| (!idsInterval && !idsEnumeration && singleId)) {
			// correct
		} else {
			throw new Exception("wrong syntax: use \"i\", \"i1, i2,... in\" or \"i1-i2\"");
		}

		if (singleId) {
			result = new String[1];
			result[0] = singleValue;

		} else if (idsEnumeration) {
			StringTokenizer st = new StringTokenizer(text, ",");
			result = new String[st.countTokens()];
			int k = 0;
			while (st.hasMoreTokens()) {
				String token = st.nextToken().trim();

				try {
					if (tokenType == int.class) {
						new Integer(token).intValue();
					} else if (tokenType == double.class) {
						new Double(token).intValue();
					}
					result[k++] = token;

				} catch (Exception e) {
					result = null;
					throw new Exception("wrong syntax: element with wrong type in enumeration");
				}
			}

		} else if (idsInterval) {
			String a = text.substring(0, text.indexOf('-')).trim();
			String b = text.substring(text.indexOf('-') + 1).trim();
			System.out.println("a-b: a=" + a + " b=" + b);
			int c = 0;
			int d = 0;
			try {
				c = new Integer(a).intValue();
				d = new Integer(b).intValue();
			} catch (Exception e) {
				throw new Exception("wrong syntax: interval should be defined by two int values)");
			}

			if (c >= d) {
				throw new Exception("wrong syntax: interval first value must be lower than second value)");
			}

			result = new String[d - c + 1];
			int k = 0;
			for (int i = c; i <= d; i++) {
				result[k++] = "" + i;
			}

		}

		return result;
	}

	/**
	 * Return a mask for "control key" : either Ctrl, Alt, Shift or AltGr...
	 */
	static public int getCtrlMask() {
		if (capsisCtrlMask == -1) {
			String ctrlKey = Settings.getProperty("capsis.ctrl.key", ""); // use
																			// capsis.properties
																			// file
			if (ctrlKey.equals("ALT")) {
				capsisCtrlMask = Event.ALT_MASK;
			} else if (ctrlKey.equals("SHIFT")) {
				capsisCtrlMask = Event.SHIFT_MASK;
			} else if (ctrlKey.equals("META")) {
				capsisCtrlMask = Event.META_MASK;
			} else {
				capsisCtrlMask = Event.CTRL_MASK; // default
			}
		}
		return capsisCtrlMask;
	}

	/**
	 * Sets a fixed size to the component, decided by user.
	 */
	// ~ static public void setFixedSize (JComponent component, Dimension size)
	// {
	// ~ component.setPreferredSize (size);
	// ~ component.setMinimumSize (size);
	// ~ component.setMaximumSize (size);
	// ~ component.setSize (size);
	// ~ }

	/**
	 * Export the attributes of the given object as strings in a vector. From P.
	 * Ancelin 11/10/2002
	 */
	static public Vector<String> exportAttributes(Object subject) {

		// vector containing name then value, name then value...
		Vector<String> attributes;
		String name;
		Object value;
		try {
			attributes = null;

			// 1. Methods (public accessors)
			Method m[] = subject.getClass().getMethods();

			for (int i = 0; i < m.length; i++) {

				// discard methods with arguments or returning nothing
				if (m[i].getDeclaringClass() == Object.class || m[i].getParameterTypes().length != 0
						|| m[i].getReturnType() == Void.class) {
					continue;
				}

				// invocation of these methods could open JDialogs... ;-)
				if (java.awt.Component.class.isAssignableFrom(m[i].getReturnType())) {
					continue;
				}

				// getXXX and isXXX
				if ((m[i].getName().startsWith("get")
						&& (m[i].getReturnType() == int.class || m[i].getReturnType() == double.class))
						|| (m[i].getReturnType() == boolean.class && m[i].getName().startsWith("is"))) {
					if (attributes == null) {
						attributes = new Vector<String>();
					}

					if (m[i].getName().startsWith("get")) {
						name = m[i].getName().substring(3);
					} else {
						name = m[i].getName().substring(2);
					}
					attributes.add(name);

					try {
						// ~ value = m[i].invoke (subject, (Object) null); // fc
						// - 2.12.2004 - cast for varargs, jdk1.5
						value = m[i].invoke(subject); // fc - 2.12.2004 -
														// varargs in jdk1.5
					} catch (Exception e) {
						value = e.getMessage();
					}
					attributes.add(value.toString());
				}
			}

			// 2. Fields (public instance variables)
			Field f[] = subject.getClass().getFields();

			for (int i = 0; i < f.length; i++) {

				if (attributes == null) {
					attributes = new Vector<String>();
				}

				name = f[i].getName();
				attributes.add(name);

				try {
					value = f[i].get(subject);
				} catch (Exception e) {
					value = e.getMessage();
				}
				attributes.add(value.toString());
			}

		} catch (Exception e) {
			attributes = null;
		}

		return attributes;
	}

	/**
	 * Set size exactly for the component
	 */
	static public void setSizeExactly(JComponent c, int w, int h) {
		// fc - 26.8.2008 - fails under mac
		// fc - 6.1.2004 - tuning better buttons size under all look and feels
		//
		/*
		 * if (c instanceof JButton) { setSizeExactly ((JButton) c); return; }
		 */
		// fc - 26.8.2008 - fails under mac

		Dimension d = new Dimension(w, h);
		c.setSize(d);
		c.setPreferredSize(d);
		c.setMinimumSize(d);
		c.setMaximumSize(d);
	}

	/**
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * /** Cut a long String in shorter lines. Mode: "HTML_MODE", "WRAP_MODE"
	 * Reformated by fc - 28.11.2007, added HTML_MODE, removed CUT_MODE
	 */
	public static String wordWrapString(String s, int length, int mode) {
		if (s == null) {
			return "null";
		} // fc - 13.7.2005
		if (length <= 3) {
			return s.substring(0, 3) + "...";
		}

		StringTokenizer st = new StringTokenizer(s, " ");
		StringBuffer target = new StringBuffer();
		if (mode == HTML_MODE) {
			target.append("<html>");
		}

		String line = "";
		while (st.hasMoreTokens()) {
			String token = st.nextToken();

			// token shorter than line length
			if (token.length() <= length) {
				// token too long : next line
				if (token.length() > length - line.length()) {
					target.append(line);
					if (mode == WRAP_MODE) {
						target.append("\n");
					}
					if (mode == HTML_MODE) {
						target.append("<br>");
					}
					line = "";
					line += token + " ";
					// token short enough : added to line
				} else {
					line += token + " ";
				}
			} else {
				// if already big line, write it and go to next linec
				if (line.length() > length / 2) {
					target.append(line);
					if (mode == WRAP_MODE) {
						target.append("\n");
					}
					if (mode == HTML_MODE) {
						target.append("<br>");
					}
					line = "";
				}

				// token length > line length : cut it into pieces on several
				// lines
				while (token.length() > length - line.length()) {
					int rest = length - line.length();

					line += token.substring(0, rest);

					String buf = token.substring(rest);
					token = buf;

					target.append(line);
					if (mode == WRAP_MODE) {
						target.append("\n");
					}
					if (mode == HTML_MODE) {
						target.append("<br>");
					}

					line = "";
				}
				line += token + " ";
			}
		}
		target.append(line);
		if (mode == HTML_MODE) {
			target.append("</html>");
		}

		s = target.toString();
		// ~ }
		return s;
	}

	/**
	 * Return a mod b.
	 */
	public static int getAModuloB(int a, int b) {
		int aux = a / b;
		int aux2 = a - aux * b;
		if (aux2 < 0) {
			aux2 += b;
		}
		return aux2;
	}

	/**
	 * Set the Wait cursor for the component.
	 */
	public static void setWaitCursor(Component comp) {
		comp.setCursor(new Cursor(Cursor.WAIT_CURSOR));
	}

	/**
	 * Set the Default cursor for the component.
	 */
	public static void setDefaultCursor(Component comp) {
		comp.setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	}

	/**
	 * Set any cursor for the component.
	 */
	public static void setCursor(Component comp, int cursorType) {
		comp.setCursor(new Cursor(cursorType));
	}

	/**
	 * Return true if a is a multiple of b.
	 */

	public static boolean isAMultipleOfB(double a, double b) {
		if (a == 0 || b == 0)
			return false;

		boolean ret = false;
		if ((int) (a / b) * b == a) {
			ret = true;
		}
		return ret;
	}

	/**
	 * Run the garbage collector.
	 */
	public static void collectGarbage() {
		Runtime rt = Runtime.getRuntime();
		long free = rt.freeMemory();
		long oldFree;
		do {
			oldFree = free;
			rt.gc();
			free = rt.freeMemory();
		} while (free > oldFree);
	}

	/**
	 * Debug tool : return the different sizes of a Component.
	 */
	public static String componentSize(Component cmp) {
		String s = ("cmp.getClass () :" + cmp.getClass().toString() + "\n")
				+ ("   cmp.getSize () :" + cmp.getSize().toString() + "\n")
				+ ("   cmp.getMinimumSize () :" + cmp.getMinimumSize().toString() + "\n")
				+ ("   cmp.getMaximumSize () :" + cmp.getMaximumSize().toString() + "\n")
				+ ("   cmp.getPreferredSize () :" + cmp.getPreferredSize().toString() + "\n")
				+ ("   cmp.getWidth () :" + cmp.getWidth() + "\n") + ("   cmp.getHeight () :" + cmp.getHeight() + "\n")
				+ ("   cmp.getBounds () :" + cmp.getBounds().toString() + "\n")
				+ ("   cmp.getX () :" + cmp.getX() + "\n") + ("   cmp.getY () :" + cmp.getY() + "\n");
		if (cmp instanceof Container) {
			s += (((Container) cmp).getInsets() + "\n");
		}
		return s;

	}

	public static String traceVector(Vector<Object> v) {
		if (v == null) {
			return "null";
		}

		StringBuffer b = new StringBuffer("{");
		for (Enumeration<Object> enu = v.elements(); enu.hasMoreElements();) {
			Object obj = enu.nextElement();
			b.append(obj);
			if (enu.hasMoreElements()) {
				b.append(", ");
			}
		}
		b.append("}");
		b.append(" (n=");
		b.append(v.size());
		b.append(")");
		return b.toString();
	}

	/**
	 * Debug tool.
	 */
	public static String traceHashMap(HashMap hsh) {
		if (hsh == null) {
			return "null";
		}

		String str = "{";
		// str+=": ";
		Set keys = hsh.keySet();
		Collection elts = hsh.values();

		for (Object key : hsh.keySet()) {
			Object elt = hsh.get(key);
			str += "(" + key.toString() + ", " + elt.toString() + "),";
		}

		str += "}";
		str += " (n=" + hsh.size() + ")";
		return str;
	}

	/** List all class names in a particular package */
	public static Collection<String> getClasses(String pckgname) throws ClassNotFoundException {

		Set<String> classes = new HashSet<String>();
		// Get a File object for the package
		File directory = null;
		try {
			ClassLoader cld = Tools.class.getClassLoader();
			if (cld == null) {
				throw new ClassNotFoundException("Can't get class loader.");
			}
			String path = pckgname.replaceAll("\\.", "/");
			URL resource = cld.getResource(path);
			if (resource == null) {
				throw new ClassNotFoundException("No resource for " + path);
			}
			directory = new File(resource.getFile());
		} catch (NullPointerException x) {
			throw new ClassNotFoundException(pckgname + " (" + directory + ") does not appear to be a valid package");
		}
		if (directory.exists()) {
			// Get the list of the files contained in the package
			String[] files = directory.list();

			for (int i = 0; i < files.length; i++) {

				// we are only interested in .java files
				if (files[i].endsWith(".java") || files[i].endsWith(".class")) {
					// removes the .class extension
					String n = pckgname + '.' + files[i].substring(0, files[i].length() - 5);
					if (n.endsWith(".")) {
						n = n.substring(0, n.length() - 1);
					}
					classes.add(n);
				}
				// If it is a directory call recursively the function
				else if (new File(directory.getAbsolutePath() + "/" + files[i]).isDirectory()) {

					try {
						classes.addAll(getClasses(pckgname + "." + files[i]));
					} // Ignore sub exception
					catch (Exception e) {
					}
				}
			}
		} else {
			throw new ClassNotFoundException(pckgname + " does not appear to be a valid package");
		}

		return classes;
	}

}