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

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;


/**
 * This info dialog can be used by other classes. It inherits some capabilities
 * from its AmapDialog superclass (Esc closes, setDefaultButton (aButton)...).
 * 
 * @author F. de Coligny - january 2002
 */
public class InfoDialog extends AmapDialog {
	private JPanel panel;	// fc - 7.12.2007
	
	public InfoDialog () {
		super ();
		getContentPane ().setLayout (new GridLayout (1, 1));
	}

	public InfoDialog (JDialog d) {
		super (d);
		getContentPane ().setLayout (new GridLayout (1, 1));
	}

	public InfoDialog (JFrame f) {
		super (f);
		getContentPane ().setLayout (new GridLayout (1, 1));
	}

	// Cancel this property by empty redefinition
	//~ public void reposition () {}	// fc - 27.2.2004 - use AmapDialog's default

	// Dispose all the embedded panel2D : close their infoDialog if needed.
	private void disposePanel2DIn (Container container) {
		Component[] components = container.getComponents ();
		for (int i = 0; i < components.length; i++) {
			Component c = components[i];
			if (c instanceof Panel2D) {
				((Panel2D) c).dispose ();
			} else if (c instanceof Container) {
				disposePanel2DIn ((Container) c);
			}
		}
	}

	public void close () {
		disposePanel2DIn (getContentPane ());
	}

//~ <<<<<<< InfoDialog.java
	// fc - 7.12.2007
	public void destroy () {
		setVisible (false);
		close ();
		dispose ();
		panel = null;
	}
	
	// fc - 7.12.2007
	public void setPanel (JPanel panel) {
		this.panel = panel;
		setTitle (panel.getName ());
		getContentPane ().removeAll ();
		getContentPane ().add (panel);
	}
	
	// fc - 7.12.2007
	public JPanel getPanel () {return panel;}
	
//~ =======
	/**	Called on ctrl-Z. Can trigger an undo () method.
	*/
	protected void ctrlZPressed () {	// fc - 18.6.2007
System.out.println ("InfoDialog.ctrlZPressed ()");
		try {
			//~ capsis.util.sketch.util.UndoManager.undo ();
		} catch (Error e) {
			
		}
	}

	/**	Called on ctrl-Y. Can trigger a redo () method.
	*/
	protected void ctrlYPressed () {	// fc - 18.6.2007
System.out.println ("InfoDialog.ctrlYPressed ()");
		try {
			//~ capsis.util.sketch.util.UndoManager.redo ();
		} catch (Error e) {
			
		}
	}


//~ >>>>>>> 1.1.1.1.4.2
}



