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

package capsis.util.equalizer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.ColumnPanel;
import capsis.commongui.util.Tools;


/**	Equalizer Slider.
* 
*	@author F. de Coligny - september 2002
*/
public class EqSlider extends ColumnPanel implements ChangeListener, ActionListener {
	
	private Collection changeListeners;
	private int mode;
	private int rank;
	private JSlider slider;
	private JTextField valueZone;
	private int initialValue;
	
	static Font userFont = new Font ("SansSerif", Font.PLAIN, 10);

	
	/**	Constructor
	*/
	public EqSlider (int orientation, int min, int max, int value, int rank, String label, int mode) {
		super ();
		this.mode = mode;
		this.rank = rank;
		initialValue = value;
		
		// Text field for value
		valueZone = new FocusTextField (""+max);	// focus lost -> validate textfield
		
		Dimension ref = valueZone.getPreferredSize ();
		Tools.setSizeExactly (valueZone, ref.width, ref.height);
		valueZone.setFont (userFont);
		valueZone.setHorizontalAlignment (JTextField.RIGHT);
		valueZone.setText (""+value);
		valueZone.addActionListener (this);
		JPanel aux = new JPanel (new FlowLayout (FlowLayout.CENTER));
		aux.add (valueZone);
		aux.setOpaque (true);
		aux.setBackground (Color.WHITE);
		add (aux);
		
		// Vertical slider
		slider = new JSlider (orientation, min, max, value);
		slider.addChangeListener (this);
		// Create the label table
		Hashtable labelTable = new Hashtable();
		labelTable.put (new Integer (value), new JLabel (" -"));
		slider.setLabelTable (labelTable);
		slider.setPaintLabels (true);
		slider.setBorder (BorderFactory.createEmptyBorder ());
		slider.setBackground (Color.white);
		add (slider);
		
		int w = slider.getPreferredSize ().width;
		slider.setPreferredSize (new Dimension (w, 800));	// fc - 7.4.2003
		
		//~ System.out.println ("Slider size: "+Tools.componentSize (slider));
		
		// Class label
		if (label == null) {
			label = ""+rank;
		}
		JLabel l = new JLabel (label, SwingConstants.CENTER);
		l.setFont (userFont);
		JPanel aux2 = new JPanel (new BorderLayout ());
		aux2.add (l, BorderLayout.CENTER);
		aux2.setOpaque (true);
		aux2.setBackground (Color.white);
		add (aux2);
		
		addStrut0 ();
		
		setOpaque (true);
		setBackground (Color.white);
		
		//~ System.out.println ("slider heights="+Tools.getHeights (slider));
		
	}

	
	/**	Enter on text field
	*/
	public void actionPerformed (ActionEvent evt) {
		validateTextField ();
	}
	
	
	/**	Validation of a value in the textfield.
	*	If ok, returns true.
	*	If trouble, restores the original value and returns false.
	*/
	private boolean validateTextField () {
		String value = valueZone.getText ().trim ();
		try {
			int v = new Integer (value).intValue ();
			slider.setValue (v);
			valueZone.setText (""+slider.getValue ());	// else: two errors -> no change
			return true;
		} catch (Exception e) {
			slider.setValue (initialValue);
			valueZone.setText (""+initialValue);	// else: two errors -> no change
			return false;
		}
	}

	
	/**	This is triggerred when the slider is moved
	*/
	@Override
	public void stateChanged (ChangeEvent evt) {
		
		final JSlider s = (JSlider) evt.getSource ();
		
		// ValueZone is updated while adjusting the slider
		valueZone.setText (""+s.getValue ());

		if (s.getValueIsAdjusting ()) {return;}

		// There may be a max value to be respected
		if (mode == Equalizer.NOT_MORE_THAN_INITIAL) {
			if (s.getValue () > initialValue) {
				s.setValue (initialValue);
			}
		}
		
		// Slider has been released : tell the listeners
		fireStateChanged ();
		
		// Refresh slider UI
		// s.updateUI () was problematic, bug#570, exception
		// invokeLater solves the bug
		Runnable doHelloWorld = new Runnable() {
			 public void run() {
				s.updateUI ();
			 }
		 };		
	
		SwingUtilities.invokeLater (doHelloWorld);
		
	}

	
	public int getRank () {return rank;}

	
	public int getValue () {return slider.getValue ();}

	
	public boolean setValue (int v) {
		valueZone.setText (""+v);
		boolean success = validateTextField ();
		return success;
	}
	
	
	/**	Some listeners can listen to the equalizer
	*/
	public void addChangeListener (ChangeListener l) {
		if (changeListeners == null) {changeListeners = new Vector ();}
		changeListeners.add (l);
	}
	
	
	/**	Tell the listeners somthing just happened
	*/
	private void fireStateChanged () {
		for (Iterator i = changeListeners.iterator (); i.hasNext ();) {
			ChangeListener l = (ChangeListener) i.next ();
			l.stateChanged (new ChangeEvent (this));
		}
	}
	
	
	private class FocusTextField extends JTextField implements FocusListener {
		public FocusTextField (String label) {
			super (label);
			addFocusListener (this);
		}
		public void focusGained (FocusEvent evt) {}
		public void focusLost (FocusEvent evt) {
			validateTextField ();
		}
	}

	
	/**	Used by Equalizer to modify its focus order 
	*/
	public JTextField getTextField () {return valueZone;}
	
	
	public JSlider getSlider () {return slider;}

}
