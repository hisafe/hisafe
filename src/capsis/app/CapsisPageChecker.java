/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski
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

package capsis.app;

import java.awt.Window;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.util.Vector;

import javax.net.ssl.HttpsURLConnection;
import javax.swing.JButton;

import capsis.commongui.util.PageChecker;
import capsis.gui.MainFrame;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;

/**
 * Checks if an helper web page is available. Proposes an alternative web page (
 * e.g. the result of a search on the web site).
 * 
 * @author F. de Coligny - August 2018
 */
public class CapsisPageChecker implements PageChecker {

	static {
		Translator.addBundle("capsis.app.CapsisPageChecker");
	}

	/**
	 * Reads the given web page and evaluate if it exists. E.g. in Capsis, the web
	 * site is a dokuwiki and if we find the text "This topic does not exist yet",
	 * we can deduce the page does not exist.
	 */
	@Override
	public boolean pageExists(String url) {

		// Here, 'Page exists' if it does not contain this text
		// This is a text written on dokuWiki pages when not created yet
		String notYet = "This topic does not exist yet";

		// fc-10.12.2021 detect redirection if any
		url = followRedirectionIfAny(url);

		String page = readURL(url);

		return !page.contains(notYet);

	}

	/**
	 * In case of redirection (http code 301 permanent, 302 temporary (307, 308 not
	 * managed here)), search the new url to follow the redirection, returns the
	 * page at this new url.
	 */
	private String followRedirectionIfAny(String url) {

		// fc-10.12.2021 evaluating redirection detection...

		try {
			
			// Open connection
			URL urlObj = new URL(url);
			URLConnection urlConn = urlObj.openConnection();

			// Note: an HttpsURLConnection is a HttpURLConnction (heritage)
			// -> Checking Http seems enough here
			if (!(urlConn instanceof HttpURLConnection)) {
				return url; // not http, return same url

			} else {

				// Check connection response
				HttpURLConnection httpConn = (HttpURLConnection) urlConn;
				int responseCode = httpConn.getResponseCode();
				String responseMessage = httpConn.getResponseMessage();

				boolean redirectionDetected = responseCode == HttpURLConnection.HTTP_MOVED_TEMP
						|| responseCode == HttpURLConnection.HTTP_MOVED_PERM;

				if (!redirectionDetected) {
					return url; // no redirection, return same url

				} else {
					// Redirection detected

					// fc-10.12.2021 To show the code of the original page (may contain a redirection)
//					String page = readURL(url);
//					Log.println(Log.INFO, "CapsisPageChecker.followRedirectionIfAny ()", "REDIRECTION DETECTED in original url: " + url + "\n" + page);

					// New url should be in a 'Location' header
					// null if not found
					String newUrl = httpConn.getHeaderField("Location");
					
//					if (newUrl != null)
//						Log.println(Log.INFO, "CapsisPageChecker.followRedirectionIfAny ()", "REDIRECTION DETECTED newUrl was found by Location header field");

					if (newUrl == null) {
						// Try to find new url...

						// Calling getURL () after getInputStream() should give the newUrl
						httpConn.getInputStream(); // supposed to follow the redirection
						newUrl = httpConn.getURL().toString();
						
//						if (newUrl != null)
//							Log.println(Log.INFO, "CapsisPageChecker.followRedirectionIfAny ()", "REDIRECTION DETECTED newUrl was found by getInputStream () then getURL ()");

					}

					if (newUrl != null) {

//						Log.println(Log.INFO, "CapsisPageChecker.followRedirectionIfAny ()", "REDIRECTION DETECTED newUrl: "
//								+ newUrl);
						return newUrl;

					} else {
						Log.println(Log.INFO, "CapsisPageChecker.followRedirectionIfAny ()", "REDIRECTION DETECTED, could not find new url, returned original");
						return url;
					}

				}

			}

		} catch (Exception e) {
			return url; // trouble, return same url
		}

	}

	/**
	 * Ask user if he wants an alternative help page, returns true if so. If user
	 * cancels, send an exception to tell no page is expected to be opened.
	 */
	@Override
	public boolean userWantsAnAlternativePage() throws Exception {

		Window parentComponent = MainFrame.getInstance();

		String title = Translator.swap("CapsisPageChecker.helperPageNotFilledIn");
		String question = Translator.swap(
				"CapsisPageChecker.thisHelperPageDoesNotExistYetWouldYouLikeToSeeItAnywayOrToSeeAnAlernativeHelperPage");
		JButton alternateButton = new JButton(Translator.swap("CapsisPageChecker.seeAnAlternativePage"));
		JButton anywayButton = new JButton(Translator.swap("CapsisPageChecker.seeThePageAnyway"));
		JButton cancelButton = new JButton(Translator.swap("CapsisPageChecker.cancel"));
		Vector buttons = new Vector();
		buttons.add(alternateButton);
		buttons.add(anywayButton);
		buttons.add(cancelButton);

		JButton choice = UserDialog.promptUser(parentComponent, title, question, buttons, alternateButton);

		if (choice.equals(cancelButton))
			throw new Exception("User cancelled");

		return choice.equals(alternateButton);
	}

	/**
	 * Make and return an alternative url to be opened. E.g. it is possible to
	 * search in the scope of a dokuwiki web site for a given word.
	 */
	@Override
	public String getAlternativeUrl(String url, String className) {

		System.out.println("CapsisPageChecker getAlternativeUrl() className: " + className);

		StringBuffer alt = new StringBuffer();

		// className e.g. walltree.model.WalInitialParameters
		// Note: split takes a regular expression, the dot is a special
		// character, it must be escaped
		String[] members = className.split("\\.");

		// word e.g. walltree
		String word = members[0];

		alt.append("http://capsis.cirad.fr/capsis/home?do=search&id=" + word);

		return "" + alt;
	}

	/**
	 * Reads the given url into a String.
	 */
	static String readURL(String _url) {

		// fc-8.8.2018 Read a URL (we can then look inside the text to see if we
		// have 'This topic does not exist yet", meaning the page does not exist
		// on the Capsis web site

		StringBuffer b = new StringBuffer();

		try {

			URL url = new URL(_url);
			BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));

			String line;
			while ((line = in.readLine()) != null) {
				b.append(line);
				b.append("\n"); // newline
			}

			in.close();
		} catch (Exception e) {
			// Could not read the url
		}

		return "" + b;
	}

}
