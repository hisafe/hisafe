/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.commongui.util;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * Browser can be used to show urls from anywhere. Usage: Browser.showPage(url);
 * 
 * @author F. de Coligny, S. Dufour-Kowalski - August 2008
 */
public class Browser {

	static {
		Translator.addBundle("capsis.commongui.Labels");
	}

	/**
	 * Show an helper page.
	 */
	public static void showPage(String url) {

		// This property will be false if not found (can be ignored by caller)
		boolean useCustomBrowser = Settings.getProperty("capsis.use.custom.browser", false);

		if (useCustomBrowser) {
			// Custom browser
			showWithCustomBrowser(url);

		} else {
			// Integrated browser
			showWithIntegratedBrowser(url);
		}
	}

	/**
	 * Show an URL with capsis custom browser.
	 */
	public static void showWithCustomBrowser(String url) {
		try {
			// fc-8.8.2018 fixed an error, these are Settings properties
			String command = Settings.getProperty("capsis.custom.browser.command", "").trim().toLowerCase();
			// String command = System.getProperty
			// ("capsis.custom.browser.command").trim ().toLowerCase ();
			if (command.indexOf("$u") == -1) {
				command += " $u";
			}
			int uPos = command.indexOf("$u");
			command = command.substring(0, uPos) + url + command.substring(uPos + 2);
			Runtime.getRuntime().exec(command); // may throw exception

		} catch (Exception e) {
			Log.println(Log.ERROR, "Browser.showWithCustomBrowser ()", "Exception: ", e);
			MessageDialog.print(null, Translator.swap("Shared.couldNotOpenURL") + "\n" + url, e);
		}
	}

	/**
	 * Tool method to create a BrowserDialog
	 */
	private static void showWithIntegratedBrowser(String url) {

		try {
			Desktop.getDesktop().browse(new URI(url));
			return;
		} catch (Exception e) {

			try {
				// Try MightyPork DesktopApi
				// added by fc-30.1.2014 (failed under Linux Ubuntu 13.10 /
				// Gnome)
				DesktopApi.browse(new URI(url));

			} catch (Exception e2) {
				Log.println(Log.ERROR, "Browser.showWithIntegratedBrowser ()", "Exception: ", e);
				MessageDialog.print(null, Translator.swap("Shared.couldNotOpenURL") + "\n" + url, e);
			}
		}

	}

	// A code to try to browse an url in case the java Desktop API is not
	// supported
	// MightyPork implementation, StackOverflow, 1.8.2013
	// http://stackoverflow.com/questions/18004150/desktop-api-is-not-supported-on-the-current-platform

	static private class DesktopApi {

		public static boolean browse(URI uri) {

			if (openSystemSpecific(uri.toString()))
				return true;

			if (browseDESKTOP(uri))
				return true;

			return false;
		}

		public static boolean open(File file) {

			if (openSystemSpecific(file.getPath()))
				return true;

			if (openDESKTOP(file))
				return true;

			return false;
		}

		public static boolean edit(File file) {

			// you can try something like
			// runCommand("gimp", "%s", file.getPath())
			// based on user preferences.

			if (openSystemSpecific(file.getPath()))
				return true;

			if (editDESKTOP(file))
				return true;

			return false;
		}

		private static boolean openSystemSpecific(String what) {

			EnumOS os = getOs();

			if (os.isLinux()) {
				if (runCommand("kde-open", "%s", what))
					return true;
				if (runCommand("gnome-open", "%s", what))
					return true;
				if (runCommand("xdg-open", "%s", what))
					return true;
			}

			if (os.isMac()) {
				if (runCommand("open", "%s", what))
					return true;
			}

			if (os.isWindows()) {
				if (runCommand("explorer", "%s", what))
					return true;
			}

			return false;
		}

		private static boolean browseDESKTOP(URI uri) {

			logOut("Trying to use Desktop.getDesktop().browse() with " + uri.toString());
			try {
				if (!Desktop.isDesktopSupported()) {
					logErr("Platform is not supported.");
					return false;
				}

				if (!Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)) {
					logErr("BORWSE is not supported.");
					return false;
				}

				Desktop.getDesktop().browse(uri);

				return true;
			} catch (Throwable t) {
				logErr("Error using desktop browse.", t);
				return false;
			}
		}

		private static boolean openDESKTOP(File file) {

			logOut("Trying to use Desktop.getDesktop().open() with " + file.toString());
			try {
				if (!Desktop.isDesktopSupported()) {
					logErr("Platform is not supported.");
					return false;
				}

				if (!Desktop.getDesktop().isSupported(Desktop.Action.OPEN)) {
					logErr("OPEN is not supported.");
					return false;
				}

				Desktop.getDesktop().open(file);

				return true;
			} catch (Throwable t) {
				logErr("Error using desktop open.", t);
				return false;
			}
		}

		private static boolean editDESKTOP(File file) {

			logOut("Trying to use Desktop.getDesktop().edit() with " + file);
			try {
				if (!Desktop.isDesktopSupported()) {
					logErr("Platform is not supported.");
					return false;
				}

				if (!Desktop.getDesktop().isSupported(Desktop.Action.EDIT)) {
					logErr("EDIT is not supported.");
					return false;
				}

				Desktop.getDesktop().edit(file);

				return true;
			} catch (Throwable t) {
				logErr("Error using desktop edit.", t);
				return false;
			}
		}

		private static boolean runCommand(String command, String args, String file) {

			logOut("Trying to exec:\n   cmd = " + command + "\n   args = " + args + "\n   %s = " + file);

			String[] parts = prepareCommand(command, args, file);

			try {
				Process p = Runtime.getRuntime().exec(parts);
				if (p == null)
					return false;

				try {
					int retval = p.exitValue();
					if (retval == 0) {
						logErr("Process ended immediately.");
						return false;
					} else {
						logErr("Process crashed.");
						return false;
					}
				} catch (IllegalThreadStateException itse) {
					logErr("Process is running.");
					return true;
				}
			} catch (IOException e) {
				logErr("Error running command.", e);
				return false;
			}
		}

		private static String[] prepareCommand(String command, String args, String file) {

			List<String> parts = new ArrayList<String>();
			parts.add(command);

			if (args != null) {
				for (String s : args.split(" ")) {
					s = String.format(s, file); // put in the filename thing

					parts.add(s.trim());
				}
			}

			return parts.toArray(new String[parts.size()]);
		}

		private static void logErr(String msg, Throwable t) {
			System.err.println(msg);
			t.printStackTrace();
		}

		private static void logErr(String msg) {
			System.err.println(msg);
		}

		private static void logOut(String msg) {
			System.out.println(msg);
		}

		public static enum EnumOS {
			linux, macos, solaris, unknown, windows;

			public boolean isLinux() {

				return this == linux || this == solaris;
			}

			public boolean isMac() {

				return this == macos;
			}

			public boolean isWindows() {

				return this == windows;
			}
		}

		public static EnumOS getOs() {

			String s = System.getProperty("os.name").toLowerCase();

			if (s.contains("win")) {
				return EnumOS.windows;
			}

			if (s.contains("mac")) {
				return EnumOS.macos;
			}

			if (s.contains("solaris")) {
				return EnumOS.solaris;
			}

			if (s.contains("sunos")) {
				return EnumOS.solaris;
			}

			if (s.contains("linux")) {
				return EnumOS.linux;
			}

			if (s.contains("unix")) {
				return EnumOS.linux;
			} else {
				return EnumOS.unknown;
			}
		}
	}

}
