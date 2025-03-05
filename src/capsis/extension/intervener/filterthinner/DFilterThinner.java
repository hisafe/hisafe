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

package capsis.extension.intervener.filterthinner;

import javax.swing.JPanel;

import jeeb.lib.util.Log;
import capsis.gui.DialogWithOkCancel;
import capsis.gui.FilterManager;
import capsis.util.Redoable;
import capsis.util.Undoable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * DFilterThinner is a dialog box which can show a FilterManager.
 * It is used in a thinning context.
 * The difference with its superclass is the checks on ok.
 * 
 * @author F. de Coligny - March 2001
 */
public class DFilterThinner extends DialogWithOkCancel {
	private FilterManager filterManager;

	public DFilterThinner (JPanel pan, String dialogTitle) {
		super (pan, dialogTitle, true, "Center"); // modal = true, panel is put in the CENTER of the BorderLayout
//		super (pan, dialogTitle);
		
	}
	
	/** 
	 * Hook for DUser subclasses.
	 */
	protected void beforeShow () {
		// disable default button ok (see AmapDialog)
		ok.setDefaultCapable (false);
		getRootPane ().setDefaultButton (null);
		
	}

	/** 
	 * May be redefined in subclasses.
	 */
	public void okAction () {
		
		// Check filters configuration and abort ok if trouble 
		// (filter is responsible for information dialog about trouble)
		try {
			filterManager = (FilterManager) panel;
			if (!filterManager.tryFilterConfig ()) {return;}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DFilterThinner.c ()", "Given panel is not instance of FilterManager", e);
		}
		
		
		setValidDialog (true);	// if all is ok, dialog is declared valid
	}

	//~ public void reposition () {
		
		//~ int x0 = MainFrame.getInstance ().getLocation ().x;
		//~ int y0 = MainFrame.getInstance ().getLocation ().y;
		//~ int w0 = MainFrame.getInstance ().getWidth ();
		//~ int h0 = MainFrame.getInstance ().getHeight ();
		
		//~ int w = getWidth ();
		//~ int h = getHeight ();
		
		//~ setLocation (x0+w0/2-w, y0+h0/2-h);	// let place for slave panels
	//~ }

	/**	Called on ctrl-Z. Redefines AmapDialog.ctrlZPressed ()
	*/
	protected void ctrlZPressed () {	// fc - 1.3.2005
		filterManager = (FilterManager) panel;
		if (filterManager != null) {
			ConfigurationPanel p = filterManager.getCurrentConfigPanel ();
			if (p != null && p.isEnabled () && p instanceof Undoable) {
				Undoable u = (Undoable) p;
				u.undo ();
			}
		}
	}

	/**	Called on ctrl-R. Redefines AmapDialog.ctrlRPressed ()
	*/
	protected void ctrlRPressed () {	// fc - 2.3.2005
		filterManager = (FilterManager) panel;
		if (filterManager != null) {
			ConfigurationPanel p = filterManager.getCurrentConfigPanel ();
			if (p != null && p.isEnabled () && p instanceof Redoable) {
				Redoable r = (Redoable) p;
				r.redo ();
			}
		}
	}

}

