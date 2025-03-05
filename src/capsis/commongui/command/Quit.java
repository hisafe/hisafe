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

package capsis.commongui.command;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.kernel.Engine;

/**	Command Quit.
  *	@author F. de Coligny - october 2000, april 2010
 */
public class Quit extends AbstractAction implements ActionCommand, ListenedTo {
	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("Quit.quit");
	
	static private Collection<Listener> listeners;
	
	private JFrame frame;
	
	
	/**	Constructor
	*/
	public Quit (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("quit_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("quit_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		this.putValue (Action.ACCELERATOR_KEY, 
				KeyStroke.getKeyStroke (KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
	}

	/**	Action interface
	 */
	public void actionPerformed (ActionEvent e) {
		execute ();
	}
	
	/**	Command interface
	 */
	public int execute () {
		
		try {
			// Hook for listeners, they may do something at closing time
			Quit.tellListeners (this, null);
			
			// Remove the ShutdownHook, we are closing properly
			try {
				ShutdownHook.getInstance().removeShutdownHook ();
			} catch (Throwable e) {
				// ShutdownHook may not have been set on the application: ignore
			} 
			
			// Save main properties file
			Settings.savePropertyFile ();
		
			// First, try to close session (saving...)
			new CloseSession (frame).execute ();
			
			// CloseSession may have been canceled
			if (Engine.getInstance ().getSession () != null) {
				System.out.println ("Quit, aborted by user");
				return 1;
			}	

			System.exit (0);
				
		} catch (Throwable e) {		// Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "Quit.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
		}
		return 0;
	}

	/**	This is the preferred method to be told when a Quit command 
	 * 	is about to close the application.
	 */
	public static void staticAddListener (Listener l) {
		if (listeners == null) {listeners = new ArrayList<Listener> ();}
		listeners.add(l);
	}

	public static void staticRemoveListener (Listener l) {
		if (listeners == null) {return;}
		listeners.remove(l);
	}
	
	/**	Tell the listeners that it's closing time.
	 */
	private static void tellListeners (ListenedTo source, Object param) {
		if (listeners == null) {return;}
		for (Listener l : listeners) {
			l.somethingHappened(source, param);
		}
	}
		
		/**	ListenedTo interface. staticAddListener() is preferred.
		 */
		@Override
		public void addListener(Listener l) {
			Quit.staticAddListener (l);
		}
	
		/**	ListenedTo interface. staticRemoveListener() is preferred.
		 */
		@Override
		public void removeListener(Listener l) {
			Quit.staticRemoveListener (l);
		}
	
		/**	ListenedTo interface. tellListeners() is preferred.
		 */
		@Override
		public void tellSomethingHappened(Object param) {
			Quit.tellListeners (this, param);
		}
	
}
