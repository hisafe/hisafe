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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Random;
import java.util.Vector;

import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import capsis.util.CustomFocusPolicy;

/**	Equalizer 
* 
*	@author F. de Coligny - september 2002
*/
public class Equalizer extends JPanel implements ChangeListener {

	public static final int FREE_SLIDERS = 0;
	public static final int NOT_MORE_THAN_INITIAL = 1;
	public static final int NOT_LESS_THAN_INITIAL = 2;

	private Collection changeListeners;
	
	private Random random;

	private int mode;			// not more than initial values, not less or free
	private int min;
	private int max;
	private int[] values;
	private int numberOfValues;		// values.length
	private int[] initialValues;	// copy at build time
	private String[] labels;	// optional, labels.length == values.length

	private Collection<EqSlider> eqSliders;
	
	
	/**	Constructor
	*/
	public Equalizer (int[] values, String[] labels, int mode) {
		super ();
		setLayout (new BorderLayout ());
		random = new Random ();
		
		min = 0;
		max = 0;
		for (int i = 0; i < values.length; i++) {max = Math.max (max, values[i]);}
		
		this.mode = mode;	// default mode, sliders are free
		this.values = values;
		this.numberOfValues = values.length;
		
		this.initialValues = new int[numberOfValues];
		for (int i = 0; i < numberOfValues; i++) {initialValues[i] = values[i];}
		
		this.labels = labels;
		
		createUI ();		
	}
	
	
	/**	Change the values of the equalizer by program. 
	*	Returns true if no errors. 
	*	If trouble, writes in Log and returns false. 
	*/
	public boolean setValues (int[] newValues) {
		// Test the array
		if (newValues == null 
				|| newValues.length != numberOfValues) {
			Log.println (Log.ERROR, "Equalizer.setValues ()", 
					"Could not set values (passed). Equalizer size="+numberOfValues
					+" newValues size="+newValues.length
					+" newValues="+newValues);
			return false;	// abort
		}
		// Set the values
		int i = 0;
		for (EqSlider s : eqSliders) {
			boolean success = s.setValue (newValues[i]);
			if (!success) {
				Log.println (Log.ERROR, "Equalizer.setValues ()", 
						"Could not set values (passed). Problem with "+(i+1)+"th value="+newValues[i]);
				return false;	// abort
			}
			i++;
		}
		return true;
	}
	
	
	/**	This is triggerred when a slider is moved
	*/
	public void stateChanged (ChangeEvent evt) {
		EqSlider s = (EqSlider) evt.getSource ();
		
		//~ System.out.println (" - Equalizer was moved - ");
		values[s.getRank ()] = s.getValue ();
		fireStateChanged ();
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
		if (changeListeners == null) {return;}
		for (Iterator i = changeListeners.iterator (); i.hasNext ();) {
			ChangeListener l = (ChangeListener) i.next ();
			l.stateChanged (new ChangeEvent (this));
		}
	}
	
	
	/**	Creates the equalizer gui
	*/
	private void createUI () {
		LinePanel l1 = new LinePanel ();
		eqSliders = new ArrayList<EqSlider> ();
		
		for (int i = 0; i < numberOfValues; i++) {
			
			String label = "";
			try {label = labels[i];} catch (Exception e) {}	// labels may be null
			
			EqSlider s = new EqSlider (JSlider.VERTICAL, min, max, values[i], i, label, mode);
			eqSliders.add (s);
			s.addChangeListener (this);
			l1.add (s);
		}
		l1.addStrut0 ();
		l1.setOpaque (true);
		l1.setBackground (Color.white);
		
		// Change focus order in equalizer : first textfields, then sliders
		CustomFocusPolicy fp = new CustomFocusPolicy ();
		for (EqSlider s : eqSliders) {
			fp.addComponent (s.getTextField ());
		}
		for (EqSlider s : eqSliders) {
			fp.addComponent (s.getSlider ());
		}
		setFocusTraversalPolicy (fp);
		setFocusCycleRoot (true);
		
		add (l1, BorderLayout.CENTER);
		setOpaque (true);
		setBackground (Color.WHITE);
	}
	
	
	public int getN () {return numberOfValues;}
	
	
	public int[] getValues () {return values;}
	
	
	public int[] getInitialValues () {return initialValues;}
	
	
	/**	Test
	*/
	public static void main (String[] args) {
		HistoFrame frame = new HistoFrame ("Equalizer");
		
		int[] tab = {125, 160, 390, 351, 295};
		String[] labels = {"0-10", "10-20", "20-30", "30-40", "40-50"};
		
		Equalizer histo = new Equalizer (tab, labels, Equalizer.NOT_MORE_THAN_INITIAL);
		histo.addChangeListener (frame);
		
		frame.getContentPane ().setLayout (new BorderLayout ());
		frame.getContentPane ().add (histo, BorderLayout.CENTER);
		frame.setSize (600, 400);
		frame.setVisible (true);
		
	}


}

