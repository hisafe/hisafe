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

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.IOException;

import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;

import jeeb.lib.util.Log;

/**
 * An input stream which takes its data in a JTextArea.
 * 
 * @author F. de Coligny - august 2001.
 */
public class JTextAreaReader {
	private JTextArea area;

	public JTextAreaReader (JTextArea area) {
		this.area = area;
	}
	
	public String readLine () throws IOException {
		// Launch a Thread which registers on area and concatenates
		// all typed characters until an <EOL>
		// The main execution flow blocks in this method, waiting
		// for the thread end with t.join ().
		//When the thread is over, get its line with getLine (), 
		// dispose the thread and return the line.
		
		SwingUtilities.invokeLater (new Runner () {
			public void run () {
				area.setEditable (true);
				area.getCaret ().setVisible (true);
				area.setCaretPosition (area.getDocument ().getEndPosition ().getOffset ());
			}
		});
		
		
		JTextAreaListener t = new JTextAreaListener (area);
		t.start ();
		try {
			t.join ();
		} catch (InterruptedException e) {}	

		area.setEditable (false);
		area.getCaret ().setVisible (false);
		
		return t.getLine ();
	}
	
	public void close () throws IOException {}



	class JTextAreaListener extends Thread implements DocumentListener, KeyListener {
		private JTextArea area;
		private int begin;
		private boolean shouldStop;
		
		private String line;

		public JTextAreaListener (JTextArea area) {
			this.area = area;
			shouldStop = false;
			begin = -1;
			area.getDocument ().addDocumentListener (this);
			area.addKeyListener (this);
		}

		public void run () {
			while (!shouldStop) {}
		}

		public void changedUpdate (DocumentEvent evt) {}
		public void removeUpdate (DocumentEvent evt) {}
		public void insertUpdate (DocumentEvent evt) {
			int offset = evt.getOffset ();

			try {
				String s = area.getDocument ().getText (offset, 1);
				//System.out.println ("insert> "+s);
				if (s.equals ("\n")) {
					//System.out.println ("CR was inserted");
					
					int length = offset - begin;
					line = "";
					if (length > 0) {	// who knows...
						line = area.getDocument ().getText (begin, length);
					}
					//System.out.println ("line=<"+line+">");
					area.getDocument ().removeDocumentListener (this);
					shouldStop = true;
				}
			} catch (BadLocationException e) {
				Log.println (Log.ERROR, "JTextAreaListener.insertUpdate ()", "Error: ", e);
			}
		}
		
		// Begin is the offset of the beginning of the line in the JTextArea.
		// Begin is hard to compute : when execution flow arrives here, 
		// some JTextArea.append ("text") can still occur (not yet finished 
		// and executed in the dispatch event thread).
		// So we wait till the user has hit the first key to compute Begin.
		public void keyPressed (KeyEvent evt) {setBegin ();}
		public void keyReleased (KeyEvent evt) {setBegin ();}
		public void keyTyped (KeyEvent evt) {setBegin ();}
		private void setBegin () {
			if (begin == -1) {
				//begin = area.getDocument ().getEndPosition ().getOffset () - 1;	// -1: we just entered a letter
				begin = area.getCaretPosition ();
				//System.out.println ("begin was just set: "+begin);
			}
			try {
				area.removeKeyListener (this);	// if not already done
			} catch (Exception e) {}
		}
		
		public String getLine () {
			return line;
		}
	}

}
