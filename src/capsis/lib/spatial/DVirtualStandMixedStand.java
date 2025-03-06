/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2003 Francois Goreaud.
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

package capsis.lib.spatial;

import java.awt.BorderLayout;
import java.awt.Checkbox;
import java.awt.CheckboxGroup;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * DVirtualStandMixedStand - Dialog box to set the parameters concerning 
 * the various sub population of a mixed virtual stand.
 * Parameters will be stored in the VirtualParameterMixedStand object.
 *
 * @author F Goreaud - 25/11/05
 */
public class DVirtualStandMixedStand extends AmapDialog implements ActionListener {
	
	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
	private VirtualParametersMixedStand vParamMS;
	
	private Checkbox cb1;
	private Checkbox cb2;
	private Checkbox cb3;
	private Checkbox cb4;
	
	private JTextField p10;
	private JTextField p11;
	private JTextField p12;
	private JTextField p13;
	private JTextField p14;
	
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constructor.
	 */
	public DVirtualStandMixedStand (VirtualParametersMixedStand vpMS) {
		super ();
		
		vParamMS = vpMS;	// default values
		
		createUI ();
		// location is set by AmapDialog
		pack ();
		show ();
	}
	
	// When choosing the OK button.
	//
	private void okAction () {
		// Here, we only verify that the parameters are ok... and put them in vParam
		// the simulation itself will take place in VistualStandSimulator !
		
		// Xmin.
		if (Check.isEmpty (p10.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p10IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p10.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p10IsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParamMS.virtualStandXmin = new Double (p10.getText ()).doubleValue ();
		
		// Xmax.
		if (Check.isEmpty (p11.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p11IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p11.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p11IsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParamMS.virtualStandXmax = new Double (p11.getText ()).doubleValue ();
		
		// Ymin.
		if (Check.isEmpty (p12.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p12IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p12.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p12IsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParamMS.virtualStandYmin = new Double (p12.getText ()).doubleValue ();
		
		// Ymax.
		if (Check.isEmpty (p13.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p13IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p13.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p13IsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParamMS.virtualStandYmax = new Double (p13.getText ()).doubleValue ();
		
		// Number of SubPopulations.
		if (Check.isEmpty (p14.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandMixedStand.p14IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isInt (p14.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandMixedStand.p14IsNotInt"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
	   vParamMS.numberOfPopulation = new Integer (p14.getText ()).intValue ();
		if (vParamMS.numberOfPopulation<1) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandMixedStand.p14IsNotPositive"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		// now we need to ask for the parameters for each population
		// first, create memory  !!!
		vParamMS.param = new VirtualParameters[vParamMS.numberOfPopulation+2];
		
		for(int pop=1; pop<=vParamMS.numberOfPopulation; pop=pop+1)
		{	System.out.println("population "+pop+"/"+vParamMS.numberOfPopulation);
			// default value of the parameters
			VirtualParameters popParam = new VirtualParameters();
			
			// asking for species, D, H, and definition of XY
			DVirtualStandSubPopEDH dlg = new DVirtualStandSubPopEDH (popParam, pop,vParamMS.numberOfPopulation );
			if (dlg.isValidDialog ()) {    // Ok button
				popParam = dlg.getParameters ();
				
				// now set Xmin Xmax	
				popParam.virtualStandXmin=vParamMS.virtualStandXmin;
				popParam.virtualStandXmax=vParamMS.virtualStandXmax;
				popParam.virtualStandYmin=vParamMS.virtualStandYmin;
				popParam.virtualStandYmax=vParamMS.virtualStandYmax;
				
				// now parameters for spatial structure
				if (popParam.virtualStandD !=5)	// only if x,y not loaded !
				{			
				if (popParam.virtualStandXYmode == 1)	// independant structure
				{	DVirtualStandSubPopXY dlg2 = new DVirtualStandSubPopXY (popParam, pop,vParamMS.numberOfPopulation );
					if (dlg2.isValidDialog ()) {    // Ok button
						popParam = dlg2.getParameters ();
					}
				}
				else				// dependant
				{	DVirtualStandSubPopXYDep dlg3 = new DVirtualStandSubPopXYDep (popParam, pop,vParamMS.numberOfPopulation );
					if (dlg3.isValidDialog ()) {    // Ok button
						popParam = dlg3.getParameters ();
					}
				}
				}
				vParamMS.param[pop] = popParam;
				
			} else {
				// cancel button : nothing to do
			}

		}
		
		// back to main programm !
		
		this.setValidDialog (true);
	}
	
	/**
	* Manage gui events.
	*/
	public void actionPerformed (ActionEvent evt) {
		
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			// fc - 20.10.2009
				//~ Helper.helpFor (this);
		} 
	}
		
	/**
	* Create the gui.
	*/
	private void createUI () {
		Box part1 = Box.createVerticalBox ();
		Border etched = BorderFactory.createEtchedBorder ();
		
		// 1. Number of population.
		JPanel li03 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li03.add (new JWidthLabel (Translator.swap ("DVirtualStandMixedStand.Intro")+" : ", 20));
		part1.add(li03);
		
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li4.add (new JWidthLabel (Translator.swap ("DVirtualStandMixedStand.PopulationNumber")+" : ", 20));
		p14 = new JTextField (5);
		li4.add (p14);
		p14.setText(""+vParamMS.numberOfPopulation);
		
		part1.add(li4);
		
		// Location panel.
		JPanel XYPanel = new JPanel (new FlowLayout (FlowLayout.LEFT));
		Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandMixedStand.title"));
		XYPanel.setBorder (bor);
		
		Box box = Box.createVerticalBox ();
		CheckboxGroup cbg = new CheckboxGroup();
				
		JPanel li2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li2.add (new JWidthLabel ( Translator.swap ("DVirtualStandXY.xmin"), 10));
		p10 = new JTextField (5);
		p10.setText(""+vParamMS.virtualStandXmin);
		li2.add (p10);
		li2.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandXY.xmax"), 10));
		p11 = new JTextField (5);
		p11.setText(""+vParamMS.virtualStandXmax);
		li2.add (p11);
		box.add (li2);
		
		JPanel li3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li3.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.ymin"), 10));
		p12 = new JTextField (5);
		p12.setText(""+vParamMS.virtualStandYmin);
		li3.add (p12);
		li3.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandXY.ymax"), 10));
		p13 = new JTextField (5);
		p13.setText(""+vParamMS.virtualStandYmax);
		li3.add (p13);
		box.add (li3);
		XYPanel.add (box);
		part1.add (XYPanel);
	
		// Control panel.
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);
		
		setDefaultButton (ok);	// from AmapDialog
		
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, "Center");
		getContentPane ().add (pControl, "South");
		
		setTitle (Translator.swap ("DVirtualStandMixedStand"));
		
		setModal (true);
	}
	
	
	/**
	* From DialogItem interface.
	*/
	public void dispose () {super.dispose ();}
	
	public VirtualParametersMixedStand getParameters () {return vParamMS;}	// use only if validDialog
	
}



