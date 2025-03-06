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
 * DVirtualStandSubPopXYDep - Dialog box to set the parameters concerning the locations of trees 
 * for a virtual stand.
 * Parameters can be stored in the module settings object.
 *
 * @author F Goreaud - june 2001 -> 25/01/06
 */
public class DVirtualStandSubPopXYDep extends AmapDialog implements ActionListener {
//checked for c4.1.1_09 - fc - 5.2.2003
	
	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
	private VirtualParameters vParam;
	private int pop;
	private int popNumber;
	
	private Checkbox cb1;
	private JTextField p14;
	private JTextField p17;
	private JTextField p1a;
	private JTextField p1b;
	private JTextField p1c;
	private JTextField p2a;
	private JTextField p2b;
	private JTextField p2c;
	private JTextField p3a;
	private JTextField p3b;
	private JTextField p3c;
	
	private JButton browseButton;
	
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constructor.
	 */
	public DVirtualStandSubPopXYDep (VirtualParameters vp, int p, int pn) {
		super ();
		
		vParam = vp;
		pop=p;
		popNumber = pn;
		
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
		
 		// Precision for X and Y simulation.
		if (Check.isEmpty (p14.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p14IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p14.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p14IsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParam.virtualStandPrecision = new Double (p14.getText ()).doubleValue ();
		if (vParam.virtualStandPrecision<=0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p14IsNotPositive"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		// Nnumber of iteration.
		if (Check.isEmpty (p17.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p17IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isInt (p17.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p17IsNotInteger"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParam.virtualStandInteractionIteration = new Integer (p17.getText ()).intValue ();
		if (vParam.virtualStandInteractionIteration<=0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p17IsNotPositive"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		int interactionNumber = 0;
		
		// First interaction.
		// pop
		if (Check.isEmpty (p1a.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandSubPopXY.p1aIsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isInt (p1a.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1aIsNotInt"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParam.virtualStandInteractionPop[1] = new Integer (p1a.getText ()).intValue ();
		if (vParam.virtualStandInteractionPop[1]<0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1aIsNotPositive"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (vParam.virtualStandInteractionPop[1]>pop) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1aIsTooHight"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		// radius
		if (Check.isEmpty (p1b.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandSubPopXY.p1bIsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p1b.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1bIsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParam.virtualStandInteractionR[1] = new Double (p1b.getText ()).doubleValue ();
		if (vParam.virtualStandInteractionR[1]<=0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1bIsNotPositive"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		// First cost.
		if (Check.isEmpty (p1c.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1cIsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isDouble (p1c.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p1cIsNotDouble"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		vParam.virtualStandInteractionCost[1] = new Double (p1c.getText ()).doubleValue ();
		interactionNumber=1;
		
		// second interaction
		if ((!Check.isEmpty (p2a.getText ()))&&(Check.isInt (p2a.getText ()))
				&&(!Check.isEmpty (p2b.getText ()))&&(Check.isDouble (p2b.getText ()))
				&&(!Check.isEmpty (p2c.getText ()))&&(Check.isDouble (p2c.getText ()))) 
		{	
			vParam.virtualStandInteractionPop[2] = new Integer (p2a.getText ()).intValue ();
			vParam.virtualStandInteractionR[2] = new Double (p2b.getText ()).doubleValue ();
			vParam.virtualStandInteractionCost[2] = new Double (p2c.getText ()).doubleValue ();
			// we should do some verifications here 
			interactionNumber=2;
				
				// troisieme interaction
				if ((!Check.isEmpty (p3a.getText ()))&&(Check.isInt (p3a.getText ()))
						&&(!Check.isEmpty (p3b.getText ()))&&(Check.isDouble (p3b.getText ()))
						&&(!Check.isEmpty (p3c.getText ()))&&(Check.isDouble (p3c.getText ()))) 
				{	
					vParam.virtualStandInteractionPop[3] = new Integer (p3a.getText ()).intValue ();
					vParam.virtualStandInteractionR[3] = new Double (p3b.getText ()).doubleValue ();
					vParam.virtualStandInteractionCost[3] = new Double (p3c.getText ()).doubleValue ();
					// we should do some verifications here
					interactionNumber=3;
				}
			}
			vParam.virtualStandInteractionNumber = interactionNumber;
			//JOptionPane.showMessageDialog (this, ""+vParam.virtualStandGibbsInterval, Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );

		setValidDialog (true);
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
		
		// Location panel.
		JPanel XYPanel = new JPanel (new FlowLayout (FlowLayout.LEFT));
		Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandSubPopXYDep.treePattern"));
		XYPanel.setBorder (bor);
		Box box = Box.createVerticalBox ();
		CheckboxGroup cbg = new CheckboxGroup();
		
	
	
		// 1. Precision.
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li4.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.accuracy")+" : ", 10));
		p14 = new JTextField (5);
		li4.add (p14);
		p14.setText(""+vParam.virtualStandPrecision);
		box.add (li4);
		
		
		// 4. Gibbs.
		JPanel li7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li7.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.iterations")+" : ", 10));
		p17 = new JTextField (5);
		li7.add (p17);
		p17.setText(""+vParam.virtualStandInteractionIteration);
		box.add (li7);
		
		JPanel li8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li8.add (new JWidthLabel (Translator.swap ("DVirtualStandSubPopXYDep.costFunction"), 10));
		box.add (li8);
		
		JPanel li9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li9.add (new JWidthLabel (Translator.swap ("DVirtualStandSubPopXYDep.pop")+" : ", 10));
		p1a = new JTextField (5);
		li9.add (p1a);
		li9.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p1b = new JTextField (5);
		li9.add (p1b);
		li9.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p1c = new JTextField (5);
		li9.add (p1c);
		if (vParam.virtualStandInteractionNumber>0)
		{	p1a.setText(""+vParam.virtualStandInteractionPop[1]);
			p1b.setText(""+vParam.virtualStandInteractionR[1]);
			p1c.setText(""+vParam.virtualStandInteractionCost[1]);
		}
		box.add (li9);
		
		JPanel li10 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li10.add (new JWidthLabel (Translator.swap ("DVirtualStandSubPopXYDep.pop")+" : ", 10));
		p2a = new JTextField (5);
		li10.add (p2a);
		li10.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p2b = new JTextField (5);
		li10.add (p2b);
		li10.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p2c = new JTextField (5);
		li10.add (p2c);
		if (vParam.virtualStandInteractionNumber>1)
		{	p1a.setText(""+vParam.virtualStandInteractionPop[2]);
			p1b.setText(""+vParam.virtualStandInteractionR[2]);
			p1c.setText(""+vParam.virtualStandInteractionCost[2]);
		}
		box.add (li10);
		
		JPanel li11 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li11.add (new JWidthLabel (Translator.swap ("DVirtualStandSubPopXYDep.pop")+" : ", 10));
		p3a = new JTextField (5);
		li11.add (p3a);
		li11.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p3b = new JTextField (5);
		li11.add (p3b);
		li11.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p3c = new JTextField (5);
		li11.add (p3c);
		if (vParam.virtualStandInteractionNumber>2)
		{	p1a.setText(""+vParam.virtualStandInteractionPop[3]);
			p1b.setText(""+vParam.virtualStandInteractionR[3]);
			p1c.setText(""+vParam.virtualStandInteractionCost[3]);
		}
		box.add (li11);
		XYPanel.add (box);
		
		
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
		
		part1.add (XYPanel);
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, "Center");
		getContentPane ().add (pControl, "South");
		
		setTitle (Translator.swap ("DVirtualStandSubPopXYDep")+" : "+pop+" / "+popNumber);
		
		setModal (true);
	}
	
	
	/**
	* From DialogItem interface.
	*/
	public void dispose () {super.dispose ();}
	
	public VirtualParameters getParameters () {return vParam;}	// use only if validDialog
	
}



