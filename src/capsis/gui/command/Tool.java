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

package capsis.gui.command;

import java.awt.Window;

import jeeb.lib.defaulttype.Extension;
import jeeb.lib.util.Command;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.app.CapsisExtensionManager;
import capsis.extensiontype.GenericTool;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Command Tools | Tool.
 * 
 * @author F. de Coligny - october 2000
 */
public class Tool implements Command {
	private String className;
	private Window window;

	/**
	 * Constructor.
	 */
	public Tool (String className, Window window) {
		this.className = className;
		this.window = window;
	}

	/**
	 * Use ExtensionManager.loadExtension () to load tool from its className. If
	 * the tool needs parameters, it must provide devices to get them.
	 */
	public int execute () {

		try {

			try {
				Extension ext = CapsisExtensionManager.getInstance ().loadInitData (className,
						new GenericExtensionStarter ());
				((GenericTool) ext).init (window);

			} catch (Exception e) {
				MessageDialog.print (this, Translator.swap ("Tool.unableToLoadGenericTool"), e);
				Log.println (Log.ERROR, "Tool.execute ()", "Exception caught: " + e);
				return 2;
			}

		} catch (Throwable e) { // fc - 30.7.2004 - catch Errors in every
								// command (for OutOfMemory)
			Log.println (Log.ERROR, "Tool.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
