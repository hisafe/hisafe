/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2015 INRA 
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
package capsis.gui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.Collections;
import java.util.Set;
import java.util.WeakHashMap;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Settings;
import jeeb.lib.util.WxHString;

/**
 * Stores and restores location and size of Frames.
 * 
 * @author F. de Coligny - December 2015
 */
public class LocationSizeManager {
	
	// Weak reference set: contains listeners
	static protected Set<Component> registeredListeners = Collections
			.newSetFromMap(new WeakHashMap<Component, Boolean>());


	static private void storeLocation(String className, Point p) {

		WxHString wh = new WxHString(p.x, p.y);
		Settings.setProperty(className + ".location", wh.toString());

	}
	
	static private void storeSize(String className, Dimension d) {

		WxHString s = new WxHString(d);
		Settings.setProperty(className + ".size", s.toString());

	}

	// fc-4.4.2019
	static public boolean hasStoredSize (String className) {
		String l = Settings.getProperty(className + ".size", "");
		return !l .equals("");
	}

	static public Dimension restoreSize(String className) {

		try {
			String l = Settings.getProperty(className + ".size", "");
			WxHString wh = new WxHString(l);
			Dimension d = wh.getDimension();
			return d;
		} catch (Exception e) {
			return null;
		}

	}

	// fc-4.4.2019
	static public boolean hasStoredLocation (String className) {
		String l = Settings.getProperty(className + ".location", "");
		return !l .equals("");
	}
	
	static public Point restoreLocation(String className) {

		try {
			String l = Settings.getProperty(className + ".location", "");
			WxHString wh = new WxHString(l);
			Point p = new Point(wh.getW(), wh.getH());
			return p;
		} catch (Exception e) {
			return null;
		}

	}

	/**
	 * Adds a listener to store size / location when resized / moved. May be
	 * used for internal frames (for dialogs, see in AmapDialog)
	 */
	static public void registerListener(Component cmp, final String className) {

		// Add a size & location listener if needed
		if (cmp != null && !registeredListeners.contains(cmp)) {

			ComponentListener cl = new ComponentAdapter() {
				@Override
				public void componentMoved(ComponentEvent evt) {
					Component c = (Component) evt.getSource();
					storeLocation(className, c.getLocation());
				}

				@Override
				public void componentResized(ComponentEvent evt) {
					Component c = (Component) evt.getSource();
					storeSize(className, c.getSize());
				}
			};
			cmp.addComponentListener(cl);
			registeredListeners.add(cmp);
		}
	}

	/**
	 * Removes the given component from our listener list.
	 */
	static public void unregisterListener (Component comp) {
		
		// fc-14.8.208 Better memory management to help GC
		
//		System.out.println();
//		System.out.println("    LocationSizeManager.unregisterListener ()...");
//		System.out.println("    BEFORE unregistering");
//		System.out.println("    registeredListeners: " + AmapTools.toString(registeredListeners));
		
		if (comp != null && registeredListeners.contains(comp)) {
			registeredListeners.remove(comp);
		}

//		System.out.println();
//		System.out.println("    AFTER unregistering");
//		System.out.println("    registeredListeners: " + AmapTools.toString(registeredListeners));
//		System.out.println();

		
	}

}
