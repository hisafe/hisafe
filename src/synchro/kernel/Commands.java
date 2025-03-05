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

package synchro.kernel;

import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.SwingWorker3;

/**
 * A tool to launch commands (and get their return code);
 * 
 * @author F. de Coligny - april 2003
 */
public class Commands {
	
	public static int launchAndWait (String command) {
		int returnCode = -1;
		try {
			Process p = Runtime.getRuntime ().exec (command);
			returnCode = p.waitFor ();
			
		} catch (final Exception exc) {}
		return returnCode;
	}

	
	public static void launchAndForget (String command, Caller caller, Object other) {	// other is for convenience
		
		final Caller finalCaller = caller;
		final String finalCommand = command;
		final Object finalOther = other;
		
		SwingWorker3 worker = new SwingWorker3 () {
			
			// Runs in new Thread
			//
			public Object construct() {
				try {
					StatusDispatcher.print (finalCommand);
					
					//~ Process p = Runtime.getRuntime ().exec (finalCommand, null, file);
					Process p = Runtime.getRuntime ().exec (finalCommand);
					
					int exitCode = p.waitFor ();
					return new Integer (exitCode);
					
				} catch (final Exception exc) {
System.out.println ("Commands.construct (): Exception: "+exc);
exc.printStackTrace (System.out);
					return new Integer (-1);
					
				}
			}	// construct ()
			
			// Runs in dispatch event thread when construct is over
			//
			public void finished() {
				int returnCode = ((Integer) getValue ()).intValue ();
				
				if (finalCaller != null) {
					finalCaller.callBack (finalCommand, returnCode, finalOther);
				}
			}	// finished ()
		};
		
		worker.start ();	// starts thread and continue
		
	}
	

}



