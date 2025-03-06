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

package capsis.gui;

import java.awt.BorderLayout;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextField;
import javax.swing.RepaintManager;
import javax.swing.SwingUtilities;

import jeeb.lib.util.ProgressListener;
import jeeb.lib.util.StatusListener;

/**
 * Static methods to write down to the main frame's status bar from anywhere.
 *
 * @author F. de Coligny - may 2001
 */
public class StatusMultiBar extends JPanel implements StatusListener, ProgressListener {

	private class StatusLine extends JPanel {
		public JTextField statusField;
		public JProgressBar progressBar;
		public StatusLine () {
			statusField = new JTextField ();
			statusField.setEditable (false);
			
			progressBar = new JProgressBar ();
			
			setLayout (new BorderLayout ());
			add (statusField, BorderLayout.CENTER);
			add (progressBar, BorderLayout.WEST);
			
		}
	}
	
	private Hashtable lines;

	// private-methods -------------------------------------------------------------------
	//
	synchronized private void ensureLineExists (Thread t) {
		if (!lines.containsKey (t)) {		
			
			StatusLine line = new StatusLine ();		// create StatusLine
			lines.put (t, line);
			add (line);		// add it in StatusMultiBar
			
			//this.paintImmediately (getVisibleRect ());
			
			line.repaint ();
			
			//RepaintManager.currentManager (this).markCompletelyDirty (this);
			RepaintManager.currentManager (line.statusField).paintDirtyRegions ();
			RepaintManager.currentManager (line.progressBar).paintDirtyRegions ();
		}
	}
		
	// Must be called in EventDispatchThread
	synchronized private void eraseOtherLines (Thread t) {
		Vector v = new Vector ();
		for (Iterator i = lines.keySet ().iterator (); i.hasNext ();) {
			Thread key = (Thread) i.next ();
			if (!key.equals (t)) {v.add (key);}
		}
		for (Iterator i = v.iterator (); i.hasNext ();) {
			Thread k = (Thread) i.next ();
			eraseLine (k);
		}
	}
		
	// Must be called in EventDispatchThread
	synchronized private void eraseLine (Thread t) {
		if (lines.containsKey (t)) {		
			StatusLine line = (StatusLine) lines.get (t);	// retrieve StatusLine
			
			remove (line);			// remove it from StatusMultiBar
			lines.remove (t);		// remove it from table
			RepaintManager.currentManager (this).paintDirtyRegions ();
		}
	}
	// end-of-private-methods ----------------------------------------------------------------

	public StatusMultiBar () {
		super ();
		
		lines = new Hashtable ();
		setLayout (new BoxLayout (this, BoxLayout.Y_AXIS));
	}
	
	/**
	 * This method is executed in some thread.
	 * It can be called from anywhere (see StatusDispatcher).
	 */
	public void print (String msg) {
		final Thread t = Thread.currentThread ();
		final String m = msg;
		
		// StatusLines management
		boolean st = false;
		if (SwingUtilities.isEventDispatchThread()) {st = true;}
		final boolean swingThread = st;
		
		if (!SwingUtilities.isEventDispatchThread()) {
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					
					if (!swingThread) {eraseLine (Thread.currentThread ());}
					
					print (t, m);
				}
			});
			return;
		}
		
		print (t, m);
	}

	// This method always executes in the event dispatch thread.
	synchronized private void print (Thread t, String msg) {
		ensureLineExists (t);
		
		StatusLine line = (StatusLine) lines.get (t);
		
		line.statusField.setText (msg);
		line.statusField.repaint ();
		
		RepaintManager.currentManager (line.statusField).paintDirtyRegions ();	// ok!
	}
	
	/**
	 * This method is executed in some thread.
	 * It can be called from anywhere (see ProgressDispatcher).
	 */
	public void setMinMax (int min, int max) {
		final Thread t = Thread.currentThread ();
		final int fMin = min;
		final int fMax = max;
		
		if (!SwingUtilities.isEventDispatchThread()) {
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					setMinMax (t, fMin, fMax);
				}
			});
			return;
		}
		
		setMinMax (t, fMin, fMax);
	}

	// This method always executes in the event dispatch thread.
	synchronized private void setMinMax (Thread t, int min, int max) {
		ensureLineExists (t);
		
		StatusLine line = (StatusLine) lines.get (t);
		
		line.progressBar.setMinimum (min);
		line.progressBar.setMaximum (max);
		line.progressBar.setStringPainted (true);
		
		line.progressBar.repaint ();
		RepaintManager.currentManager (line.progressBar).paintDirtyRegions ();
	}

	/**
	 * This method is executed in some thread.
	 * It can be called from anywhere (see ProgressDispatcher).
	 */
	public void setValue (int value) {
		final Thread t = Thread.currentThread ();
		final int fValue = value;
		
		if (!SwingUtilities.isEventDispatchThread()) {
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					setValue (t, fValue);
				}
			});
			return;
		}
		
		setValue (t, fValue);
	}
	
	// This method always executes in the event dispatch thread.
	synchronized public void setValue (Thread t, int value) {
		ensureLineExists (t);
		
		StatusLine line = (StatusLine) lines.get (t);
		
		// value
		line.progressBar.setValue (value);
		
		// text
		try {
			int percent = value*100/line.progressBar.getMaximum ();
			line.progressBar.setString (percent+"%");
		} catch (Exception e) {}	// case divide by zero
			
		
		line.progressBar.repaint ();
		RepaintManager.currentManager (line.progressBar).paintDirtyRegions ();
	}

	/**
	 * This method is executed in some thread.
	 * It can be called from anywhere (see ProgressDispatcher).
	 */
	public void stop () {
		final Thread t = Thread.currentThread ();
		
		if (!SwingUtilities.isEventDispatchThread()) {
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					stop (t);
				}
			});
			return;
		}
		
		stop (t);
	}
	
	// This method always executes in the event dispatch thread.
	synchronized public void stop (Thread t) {
		ensureLineExists (t);
		
		StatusLine line = (StatusLine) lines.get (t);
		
		// Reset and erase text
		line.progressBar.setValue (line.progressBar.getMinimum ());
		line.progressBar.setStringPainted (false);
		line.progressBar.repaint ();
		RepaintManager.currentManager (line.progressBar).paintDirtyRegions ();
		
	}
	

}


