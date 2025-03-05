/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2018 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 2.1 of the License,
 * or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis. If not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.commongui.util;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Locale;

import capsis.commongui.HelpPageImmutable;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * This class deals with help. The help files are searched on the application
 * web site with an url naming convention, see helpFor () and getHelpResourceURL
 * (). Help files are searched in french or english depending on the current
 * application Locale. In an help file is not found and a pageChecker is
 * provided, user can ask for another help url, it will be asked to the
 * pageChecker (e.g. the result of a search on the web site for pages containing
 * a module name. User can choose to see the 'not existing yet' page to edit it
 * in the wiki and enter the expected documentation from now on.
 * 
 * A License can be asked for a module, if not found, a default license file
 * will be shown, see licenseFor (). All help files are opened in the system
 * html browser.
 * 
 * The Helper can also open a given url, see showPage ().
 * 
 * Note: Helper can not be moved to jeeb.lib.util because .jar files (e.g. a
 * castanea saved library) assert it is in capsis.commongui. So we let it in
 * capsis.commongui. capsis.commongui is jared in capsis-kernel.jar so Helper
 * can be used in AMAPstudio applications
 * 
 * @author F. de Coligny, S. Dufour-Kowalski - October 2008
 */
public class Helper {

	// fc-10.8.2018 REMOVED offline help downloading, update system out of
	// service
	// static {
	// // define default value if not defined
	// Settings.setProperty("capsis.download.offline.help.freq",
	// Settings.getProperty("capsis.download.offline.help.freq", "2"));
	// }

	// fc-7.8.2018 Added this variable, see init ()
	static private String applicationWebURL;

	// fc-8.8.2018 optional, may be null. Helps propose an alternative help page
	// to user in case an help url is not available
	static private PageChecker pageChecker;

	/**
	 * Inits the Helper, must be called before any other Helper method.
	 * Optional: set a pageChecker may be passed null) to help find alternative
	 * help pages if a given page does not exist.
	 * 
	 * @param applicationWebURL:
	 *            the web url where the help pages will be found, e.g. for
	 *            Capsis: http://capsis.cirad.fr
	 * @param pageChecker:
	 *            may be null
	 */
	static public void init(String applicationWebURL, PageChecker pageChecker) {
		Helper.applicationWebURL = applicationWebURL;
		Helper.pageChecker = pageChecker; // fc-8.8.2018 can be null
	}

	// fc-7.8.2018 MOVED to starter
	/**
	 * Capsis URL
	 */
	// static protected String getCapsisURL() {
	// return Settings.getProperty("capsis.url", "http://capsis.cirad.fr");
	// }

	// fc-7.8.2018 REMOVED
	/** Show tutorial */
	// public static void showTutorial() throws Exception {
	// URL url = new URL(getCapsisURL() + "/documentation/tutorial_" +
	// Locale.getDefault().getLanguage());
	// Helper.showPage(url);
	// }

	// fc-7.8.2018 REMOVED
	/** Show tutorial */
	// public static void showFAQ() throws Exception {
	// URL url = new URL(getCapsisURL() + "/documentation/FAQ");
	// Helper.showPage(url);
	// }

	/**
	 * Shows the licence for the given module. If modulePackageName is null,
	 * shows the Capsis licence.
	 */
	public static void licenseFor(String modulePackageName) {
		if (modulePackageName == null) {
			return;
		}
		String name = null;

		try {
			// Look for a license in .html
			String path = modulePackageName.replaceAll("\\.", "/");
			name = path + "/license" + "_" + Locale.getDefault().getLanguage() + ".html";

			URL pageName = Helper.class.getClassLoader().getResource(name);

			// Try to find a license in .txt
			if (pageName == null) {
				name = path + "/license" + "_" + Locale.getDefault().getLanguage() + ".txt";
				pageName = Helper.class.getClassLoader().getResource(name);
			}

			Helper.showPage(pageName);

		} catch (Exception e) {
			if (modulePackageName.toLowerCase().equals("capsis")) {
				Log.println(Log.ERROR, "Helper.licenseFor (\"capsis\")", "Could not find capsis license: " + name);
				return;
			}

			// Shows special text : "module without license"
			name = "moduleWithoutLicense" + "_" + Locale.getDefault().getLanguage() + ".html";

			URL pageName = ClassLoader.getSystemClassLoader().getResource(name);
			try {
				Helper.showPage(pageName);

			} catch (Exception e2) {
				Log.println(Log.ERROR, "Helper.licenseFor (" + modulePackageName + ")",
						"Can not get license for module due to ", e2);
			}
		}
	}

	static boolean isWebSiteAccessible() {

		// REMOVED this to be able to detect if the network comes back (was not
		// checking any more)
		// if (!networkOk) return false;

		try {
			// URL web = new URL(System.getProperty("capsis.url"));
			URL web = new URL("http://www.google.com");
			URLConnection webconnect = web.openConnection();
			webconnect.setConnectTimeout(1000);
			webconnect.connect();

		} catch (MalformedURLException e) {
			// fc-23.3.2015 ONF trouble: cd can not see the Deesses help pages
			Log.println(Log.WARNING, "Helper.isWebSiteAccessible ()", "Could not open a web connection, returned false",
					e);
			return false;

		} catch (IOException e) {
			// fc-23.3.2015 ONF trouble: cd can not see the Deesses help pages
			Log.println(Log.WARNING, "Helper.isWebSiteAccessible ()", "Could not open a web connection, returned false",
					e);

			// networkOk = false;
			return false;
		}

		return true;
	}

	/**
	 * Returns the help Resource URL for a given a className
	 */
	protected static URL getHelpResourceURL(String className) {

		URL pageName;
		if (className == null) {
			return null;
		}
		String name;

		// fc-21.12.2011 REMOVED local pages access -> all should be copied to
		// the web site wiki
		// // Try local version
		//
		// // Extension help file are formed like this : classname_Help_en.html
		// className = className.replace ('.', '/');
		// name = className + "_Help_" + Locale.getDefault ().getLanguage ()
		// + ".html";
		//
		// pageName = Helper.class.getClassLoader ().getResource (name);
		// if (pageName != null) { return pageName; }
		//
		// // Other help file are formed like this : classname_index_en.html
		// name = className + File.separator + "index_"
		// + Locale.getDefault ().getLanguage () + ".html";
		// pageName = ClassLoader.getSystemClassLoader ().getResource (name);
		// if (pageName != null) { return pageName; }

		// Try External URL : capsis.url/help_en/directories

		name = applicationWebURL;
		// name = getCapsisURL();

		className = className.replace('.', '/');
		name += "/help_" + Locale.getDefault().getLanguage() + "/" + className;
		try {
			pageName = new URL(name);
		} catch (MalformedURLException e) {
			pageName = null;
		}

		return pageName;

	}

	/** Does helper have a helper file for the given className ? */
	public static boolean hasHelpFor(String className) {
		return getHelpResourceURL(className) != null;
	}

	/**
	 * Shows an information page about the given className. ClassName can be
	 * "capsis" -> capsis help, an extension className -> extension help, a
	 * modulePackageName -> module help.
	 */
	public static void helpFor(String className) {
		try {

			String url = "" + getHelpResourceURL(className);

			// PageChecker checks the page before opening and may propose an
			// alternative url
			try {
				url = getCheckedURL(url, className);
			} catch (Exception e) {
				// user cancelled
				return;
			}

			Helper.showPage(url);

		} catch (Exception e) {
			Log.println(Log.WARNING, "Helper.helpFor (" + className + ")", "Error due to ", e);
		}
	}

	/**
	 * Provides help related to the subject in interactive mode. It can be
	 * triggered for example by a help button in a dialog box. Subject must be
	 * an instance of JDialog or JFrame.
	 */
	public static void helpFor(Object subject) {
		try {
			String className;
			if (subject instanceof HelpPageImmutable) {
				className = ((HelpPageImmutable) subject).getHelpPageAddress();
			} else {
				className = subject.getClass().getName();
			}
			URL resource = getHelpResourceURL(className);
			if (resource == null) {
				StatusDispatcher.print(Translator.swap("Helper.helpFileNotFound") + ": " + resource);
				return;
			}

			String url = "" + resource;

			// PageChecker checks the page before opening and may propose an
			// alternative url
			try {
				url = getCheckedURL(url, className);
			} catch (Exception e) {
				// user cancelled
				return;
			}

			Helper.showPage(url);

		} catch (Exception e) {
			Log.println(Log.WARNING, "Helper.helpFor (Object)", "Could not find help for " + subject + " due to ", e);
		}
	}

	/**
	 * Checks the url: if the help page is found, returns same url, else ask
	 * user if he wants an alternative page, if so return another url (e.g. a
	 * search result on the Web site pages), if user cancels, sends an
	 * exception.
	 */
	static private String getCheckedURL(String url, String className) throws Exception {

		// fc-8.8.2018 if pageChecker available, check the page and ask user if
		// he wants another page

		if (pageChecker == null)
			return url;

		if (!pageChecker.pageExists(url)) {

			if (pageChecker.userWantsAnAlternativePage())
				url = pageChecker.getAlternativeUrl(url, className);

		}

		System.out.println("Helper alt url: " + url);

		return url;
	}

	/**
	 * Show an URL If the url begin by http://capsis.server Test first network
	 * connection If it fails, then try to open local version
	 */
	public static void showPage(URL url) throws Exception {

		String urlstr = url.toString();

		System.out.println("Helper.showPage (), url: " + url + "...");

		// fc-7.8.2018 desactivated the offline help downloading, zip is no more
		// updated
		// if (!isWebSiteAccessible()) {
		//
		// urlstr = convertWebUrlToLocalUrl(urlstr);
		//
		// }

		// System.out.println("Helper showPage: "+urlstr);

		Browser.showPage(urlstr);

	}

	public static void showPage(String url) throws Exception {
		showPage(new URL(url));
	}

	/** Transform a web url to a local url for help file */
	/*
	 * fc-7.8.2018 DESACTIVATED the offline help downloading, the offline zip is
	 * not updated any more public static String convertWebUrlToLocalUrl(String
	 * weburl) {
	 * 
	 * // fc-23.3.2015 if weburl is getCapsisURL() (CapsisPreferences > //
	 * External tools > System browser > Test), it does not start with //
	 * getCapsisURL() + '/' -> changed // String baseurl = getCapsisURL() + '/';
	 * 
	 * System.out.println("Converting url ["+weburl+"] to local url...");
	 * 
	 * if (!weburl.startsWith(applicationWebURL)) { // if
	 * (!weburl.startsWith(getCapsisURL())) {
	 * 
	 * System.out.
	 * println("Unexpected url, does not start with applicationWebURL ["
	 * +weburl+"], aborted");
	 * 
	 * return weburl; }
	 * 
	 * String baseurl = applicationWebURL + '/'; // String baseurl =
	 * getCapsisURL() + '/';
	 * 
	 * String localurl = "file://" + PathManager.getDir("doc") +
	 * "/capsis_help/";
	 * 
	 * localurl = localurl.replace('\\', '/'); // fc-30.8.2012 - local url pb //
	 * under windows (CM & TL, pp3)
	 * 
	 * String urlstr = weburl.replaceFirst(baseurl, ""); urlstr =
	 * urlstr.replace('/', '-'); urlstr = urlstr.concat(".html");
	 * 
	 * urlstr = localurl + urlstr; urlstr = urlstr.toLowerCase();
	 * 
	 * System.out.println("Local url: ["+urlstr+"]");
	 * 
	 * return urlstr; }
	 * 
	 */

	/**
	 * Download an offline version of the website help files.
	 */
	/*
	 * fc-7.8.2018 DESACTIVATED the offline help downloading, the offline zip is
	 * not updated any more public static void downloadOfflineData(String
	 * destinationDirectory) throws IOException {
	 * 
	 * // fc-7.8.2018 added destinationDirectory
	 * 
	 * downloadOfflineData(destinationDirectory, false); }
	 */

	/*
	 * fc-7.8.2018 DESACTIVATED the offline help downloading, the offline zip is
	 * not updated any more public static void downloadOfflineData(final String
	 * destinationDirectory, final boolean force) throws IOException {
	 * 
	 * // fc-7.8.2018 added destinationDirectory
	 * 
	 * System.out.println("Downloading help files into [" + destinationDirectory
	 * + "]...");
	 * 
	 * // Download in a thread to avoid blocking new Thread(new Runnable() {
	 * 
	 * public void run() { try {
	 * 
	 * if (!isWebSiteAccessible()) return;
	 * 
	 * int downloadFreq =
	 * Check.intValue(Settings.getProperty("capsis.download.offline.help.freq",
	 * "2")); if (!force && downloadFreq < 1) {
	 * Log.println("Help files were not downloaded: day frequency (" +
	 * downloadFreq + ") is < 1 "); return; }
	 * 
	 * // // Destination directory // String destdir =
	 * PathManager.getDir("doc"); // // // Determine if the file must be
	 * download // File dir = new File(destdir + "/capsis_help/"); File dir =
	 * new File(destinationDirectory);
	 * 
	 * if (!force && dir.exists()) { // if (!force && dir.exists()) {
	 * 
	 * // Compare day Date filedate = new Date(dir.lastModified()); Calendar cal
	 * = new GregorianCalendar(); cal.setTime(filedate);
	 * 
	 * // if difference is lower than download frequency : // abort int fileday
	 * = cal.get(Calendar.DAY_OF_YEAR); int currentday = new
	 * GregorianCalendar().get(Calendar.DAY_OF_YEAR); if (Math.abs(currentday -
	 * fileday) < downloadFreq) { return; } }
	 * 
	 * String zipUrl = applicationWebURL + "/offline/capsis_help.zip"; // String
	 * zipUrl = getCapsisURL() + // "/offline/capsis_help.zip";
	 * 
	 * // System.out.print("Downloading Help files from [" + zipUrl // +
	 * "]... ");
	 * 
	 * // Create an URL final URL url = new URL(zipUrl);
	 * 
	 * getZip(url, destinationDirectory); // getZip(url, destdir);
	 * 
	 * // Touch directory dir.setLastModified(System.currentTimeMillis());
	 * 
	 * } catch (Throwable e) { Log.println(Log.ERROR,
	 * "Helper.downloadOfflineData ()", "Could not download help", e);
	 * System.out.println("Could not download help (see Log file): " + e); } }
	 * }).start();
	 * 
	 * }
	 */

	/**
	 * Download the zip file from the given url and unzip it in the given
	 * directory.
	 */
	/*
	 * fc-7.8.2018 DESACTIVATED the offline help downloading, the offline zip is
	 * not updated any more public static void downloadOfflineData(final String
	 * destinationDirectory, final boolean force) throws IOException { static
	 * void getZip(URL zipUrl, String destDir) throws IOException {
	 * 
	 * System.out.print("Downloading Help files from [" + zipUrl + "]... ");
	 * 
	 * // Get an input stream for reading URLConnection webconnect =
	 * zipUrl.openConnection(); webconnect.setConnectTimeout(1000);
	 * webconnect.connect(); InputStream in = webconnect.getInputStream();
	 * 
	 * BufferedInputStream bufIn = new BufferedInputStream(in);
	 * 
	 * // Create temp file. File temp = File.createTempFile("helper_doc",
	 * ".zip"); // File temp = File.createTempFile("capsis_doc", ".zip");
	 * temp.deleteOnExit();
	 * 
	 * // Write to temp file FileOutputStream fos = new FileOutputStream(temp);
	 * 
	 * try { byte[] buf = new byte[1024]; int i = 0; while ((i =
	 * bufIn.read(buf)) != -1) { fos.write(buf, 0, i); } } catch (Throwable e) {
	 * Log.println(Log.ERROR, "Helper.getZip ()", "Error while downloading zip",
	 * e); throw new IOException(e); } finally { if (in != null) in.close(); if
	 * (fos != null) fos.close(); }
	 * 
	 * System.out.println("done"); System.out.println("Unzipping archive...");
	 * 
	 * // Unzip UnZip u = new UnZip(destDir); u.setMode(UnZip.EXTRACT);
	 * u.unZip(temp.getPath()); // u.unZip(temp.toString());
	 * 
	 * }
	 * 
	 * public static void main(String[] args) throws Exception {
	 * 
	 * // Test String destinationDirectory = PathManager.getDir("doc") +
	 * "/capsis_help/"; // fc-7.7.2018
	 * Helper.downloadOfflineData(destinationDirectory, true);
	 * 
	 * }
	 */

}
