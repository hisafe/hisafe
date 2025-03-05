/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.extension.generictool;

import java.awt.Window;
import java.io.File;
import java.util.Locale;

import javax.swing.SwingUtilities;

import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.util.MethodProviderDoc;

/**
 * MethodProviderDocumenter creates and shows an html doc for the method
 * providers in capsis.util.methodprovider
 * 
 * @author F. de Coligny - september 2005
 */
public class MethodProviderDocumenter {

	static {
		Translator.addBundle ("capsis.extension.generictool.MethodProviderDocumenter");
	}

	/**
	 * Default constructor.
	 */
	public MethodProviderDocumenter (Window window) throws Exception {

		try {

			// methodprovider.html file is rebuilt if :
			// EITHER capsis.method.provider.html not set (capsis.options was
			// deleted)
			// OR capsis.method.provider.htm is a wrong file name
			// (methodprovider.html was deleted)
			//
			String url = Settings.getProperty ("capsis.method.provider.html", (String) null);

			// consider language
			if (url != null) {
				int i0 = url.indexOf ("_"); // language
				if (i0 == -1) {
					url = null;
				} else {
					url = url.substring (0, i0) + "_" + Locale.getDefault ().getLanguage () + url.substring (i0 + 3);
				}
			}

			// page found : open it and quit;
			if (url != null && new File (url).exists ()) {
				Helper.showPage ("file://" + url);
				StatusDispatcher.print (Translator.swap ("MethodProviderDoc.howToRebuildTheDoc") + " " + url);
				return;
			}

			// page not found and no sources, abort
			if (url == null || !new File (url).exists ()) {
				File[] files = MethodProviderDoc.getMethodProviderFiles ();
				if (files == null || files.length == 0) {
					StatusDispatcher.print (Translator.swap ("MethodProviderDoc.noSourcesFound"));
					return;
				}
			}

			// Launch a thread for doc creation
			// Open page when thread is over
			//
			SwingWorker3 worker = new SwingWorker3 () {

				// Runs in new Thread
				//
				public Object construct () {
					try {

						MethodProviderDoc d = new MethodProviderDoc ();
						d.writeHTMLdoc ();
						String url = d.getPageUrl ();
						Settings.setProperty ("capsis.method.provider.html", url);
						return d;

					} catch (final Throwable e) {
						Log.println (Log.ERROR, "MethodProviderDocumenter ()", "Exception/Error in construct ()", e);

						SwingUtilities.invokeLater (new Runnable () {
							public void run () {
								StatusDispatcher.print (Translator.swap ("MethodProviderDoc.errorSeeLog"));
							}
						});

						return null;
					}
				}

				// Runs in dispatch event thread when construct is over
				//
				public void finished () {
					try {
						MethodProviderDoc d = (MethodProviderDoc) get ();
						String url = d.getPageUrl ();
						Helper.showPage ("file://" + url);
						StatusDispatcher.print (Translator.swap ("MethodProviderDoc.howToRebuildTheDoc") + " " + url);
					} catch (Exception e) {
						Log.println (Log.ERROR, "MethodProviderDocumenter ()", "Exception/Error in finished ()", e);
						StatusDispatcher.print (Translator.swap ("MethodProviderDoc.errorSeeLog"));
					}
				}
			};
			worker.start (); // launch the thread

		} catch (Exception e) {
			Log.println (Log.ERROR, "MethodProviderDocumenter.c ()", "Error in constructor", e);
			throw e;
		}
	}


}
