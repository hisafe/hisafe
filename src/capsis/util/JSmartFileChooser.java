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

package capsis.util;

import java.io.File;

import javax.swing.JFileChooser;
import javax.swing.UIManager;

import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Translator;

/** 
 * JSmartFileChooser is a JFileChooser with internationalization,
 * default ok button, support for escape...
 * 
 * @author F. de Coligny - october 2000
 */
public class JSmartFileChooser extends JFileChooser {

	static {
		// 5. Translate cancel button text
		UIManager.put ("FileChooser.cancelButtonText", Translator.swap ("Shared.cancel"));
	}


	// Fast constructor
	public JSmartFileChooser (	String title, 
								String approveButtonText, 
								String defaultDirectory) {
		this (title, approveButtonText, approveButtonText, defaultDirectory, false);
		
		new FileAccessory (this);
		
	}
									
	// Detailed constructor
	public JSmartFileChooser (	String title, 
								String approveButtonText, 
								String approveButtonToolTipText, 
								String defaultDirectory, 
								boolean directoriesOnly) {
		super ();
		
		//1. Title
		setDialogTitle (title);
		
		// 2. Approve button text
		setApproveButtonText (approveButtonText);
		
		// 3. Approve button tool tip text
		setApproveButtonToolTipText (approveButtonToolTipText);
		
		// 4. Directories only if required
		if (directoriesOnly) {setFileSelectionMode (JFileChooser.DIRECTORIES_ONLY);}
		
//		// 5. Translate cancel button text
//		UIManager.put ("FileChooser.cancelButtonText", Translator.swap ("Shared.cancel"));
		
		// 6. Set default directory
		if ((defaultDirectory == null) || defaultDirectory.equals ("")) {
			defaultDirectory = PathManager.getInstallDir();
		}
		
		// fc - 30.10.2003 - defaultDirectory may be a file, consider parent directory (bug lsa)
		//
		File f = new File (defaultDirectory);
		if (!f.isDirectory ()) {
			f = f.getParentFile ();
		}
		
		setCurrentDirectory (f);

		// fc - 25.8.2008 - removed the current mac restriction, N. Robert tested the accessory successfully
		// Mac os x : disable Accessory (trouble with mac file chooser)
		//if (!Settings.getProperty ("os.name").toLowerCase ().startsWith ("mac"), null) {
		
		new FileAccessory (this);

		//}
		
	}


}