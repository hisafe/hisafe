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
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * DVirtualStandSubPopXY - Dialog box to set the parameters concerning the locations of trees 
 * for a virtual stand.
 * Parameters can be stored in the module settings object.
 *
 * @author F Goreaud - june 2001 -> 25/01/06
 */
public class DVirtualStandSubPopXY extends AmapDialog implements ActionListener {
//checked for c4.1.1_09 - fc - 5.2.2003
	
	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
	private VirtualParameters vParam;
	private int pop;
	private int popNumber;
	
	private Checkbox cb1;
	private Checkbox cb2;
	private Checkbox cb3;
	private Checkbox cb4;
	
	private JRadioButton rdRandom;
	private JRadioButton rdNeymanScott;
	private JRadioButton rdGibbs;
	private ButtonGroup rdGroup1;
	
	private JTextField fldXYInventory;
	private JTextField p14;
	private JTextField p15;
	private JTextField p16;
	private JTextField p17;
	private JTextField p20;
	private JTextField p21;
	private JTextField p22;
	private JTextField p23;
	private JTextField p24;
	private JTextField p25;
	private JTextField p26;
	private JTextField p27;
	private JTextField p28;
	private JTextField p29;
	
	private JButton browseButton;
	
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constructor.
	 */
	public DVirtualStandSubPopXY (VirtualParameters vp, int p, int pn) {
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
		
		boolean random = rdGroup1.getSelection ().equals (rdRandom.getModel ());
		boolean neymanScott = rdGroup1.getSelection ().equals (rdNeymanScott.getModel ());
		boolean gibbs = rdGroup1.getSelection ().equals (rdGibbs.getModel ());
		
	
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
		
		if (random)	{	// random pattern
			vParam.virtualStandXY=1;
			
		} else if (neymanScott)	{	// neyman scott pattern
			vParam.virtualStandXY=2;
			// Cluster number.
			if (Check.isEmpty (p15.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p15IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isInt (p15.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p15IsNotInteger"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandClusterNumber = new Integer (p15.getText ()).intValue ();
			if (vParam.virtualStandClusterNumber<1) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p15IsNotPositive"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			
			// Cluster radius.
			if (Check.isEmpty (p16.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p16IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p16.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p16IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandClusterRadius = new Double (p16.getText ()).doubleValue ();
			if (vParam.virtualStandClusterRadius<=0) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p16IsNotPositive"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			
		} else {	// gibbs pattern
			vParam.virtualStandXY=3;
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
			vParam.virtualStandGibbsIteration = new Integer (p17.getText ()).intValue ();
			if (vParam.virtualStandGibbsIteration<=0) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p17IsNotPositive"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			
			int intervalNumber = 0;
			
			// First radius.
			if (Check.isEmpty (p20.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p20IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p20.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p20IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandGibbsR1 = new Double (p20.getText ()).doubleValue ();
			if (vParam.virtualStandGibbsR1<=0) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p20IsNotPositive"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			// First cost.
			if (Check.isEmpty (p21.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p21IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p21.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandXY.p21IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			
			vParam.virtualStandGibbsCost1 = new Double (p21.getText ()).doubleValue ();
			intervalNumber=1;
			
			if ((!Check.isEmpty (p22.getText ()))
					&&(Check.isDouble (p22.getText ()))
					&&(!Check.isEmpty (p23.getText ()))
					&&(Check.isDouble (p23.getText ()))) {	
				vParam.virtualStandGibbsR2 = new Double (p22.getText ()).doubleValue ();
				if (vParam.virtualStandGibbsR2>vParam.virtualStandGibbsR1) {
					vParam.virtualStandGibbsCost2 = new Double (p23.getText ()).doubleValue ();
					intervalNumber=2;
					
					if ((!Check.isEmpty (p24.getText ()))
							&&(Check.isDouble (p24.getText ()))
							&&(!Check.isEmpty (p25.getText ()))
							&&(Check.isDouble (p25.getText ()))) {	
						vParam.virtualStandGibbsR3 = new Double (p24.getText ()).doubleValue ();
						if (vParam.virtualStandGibbsR3>vParam.virtualStandGibbsR2) {
							vParam.virtualStandGibbsCost3 = new Double (p25.getText ()).doubleValue ();
							intervalNumber=3;
						} else {
							vParam.virtualStandGibbsR3=0;
						}
					}
				} else {
					vParam.virtualStandGibbsR2=0;
				}
			}
			vParam.virtualStandGibbsInterval = intervalNumber;
			//JOptionPane.showMessageDialog (this, ""+vParam.virtualStandGibbsInterval, Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
		}
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
		} else if (evt.getSource ().equals (rdRandom)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdNeymanScott)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdGibbs)) {
			rdGroup1Action ();
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
		Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandSubPopXY.treePattern"));
		XYPanel.setBorder (bor);
		Box box = Box.createVerticalBox ();
		CheckboxGroup cbg = new CheckboxGroup();
		
	
		rdGroup1 = new ButtonGroup ();
		
		// 1. Precision.
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li4.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.accuracy")+" : ", 10));
		p14 = new JTextField (5);
		li4.add (p14);
		p14.setText(""+vParam.virtualStandPrecision);
		box.add (li4);
		
		// 2. Random.
		JPanel li5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdRandom = new JRadioButton (Translator.swap ("DVirtualStandXY.randomPattern"));
		rdRandom.addActionListener (this);
		rdGroup1.add (rdRandom);
		//cb2 = new Checkbox(Translator.swap ("DVirtualStandSubPopXY.randomPattern"),(vParam.virtualStandXY==1),cbg);
		li5.add (rdRandom);
		box.add (li5);
		
		// 3. Neyman Scott.
		JPanel li6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdNeymanScott = new JRadioButton (Translator.swap ("DVirtualStandXY.neymanScottPattern"));
		rdNeymanScott.addActionListener (this);
		rdGroup1.add (rdNeymanScott);
		//cb3 = new Checkbox(Translator.swap ("DVirtualStandSubPopXY.neymanScottPattern"),(vParam.virtualStandXY==2),cbg);
		li6.add (rdNeymanScott);
		box.add (li6);
		
		JPanel li6b = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li6b.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.aggregateNumber")+" : ", 10));
		p15 = new JTextField (5);
		li6b.add (p15);
		p15.setText(""+vParam.virtualStandClusterNumber);
		li6b.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.radius")+" : ", 10));
		p16 = new JTextField (5);
		li6b.add (p16);
		p16.setText(""+vParam.virtualStandClusterRadius);
		box.add (li6b);
		
		// 4. Gibbs.
		JPanel li7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdGibbs = new JRadioButton (Translator.swap ("DVirtualStandXY.gibbsPattern"));
		rdGibbs.addActionListener (this);
		rdGroup1.add (rdGibbs);
		//cb4 = new Checkbox(Translator.swap ("DVirtualStandSubPopXY.gibbsPattern"),(vParam.virtualStandXY==3),cbg);
		li7.add (rdGibbs);
		li7.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.iterations")+" : ", 10));
		p17 = new JTextField (5);
		li7.add (p17);
		p17.setText(""+vParam.virtualStandGibbsIteration);
		box.add (li7);
		
		JPanel li8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li8.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costFunction"), 10));
		box.add (li8);
		
		JPanel li9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li9.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p20 = new JTextField (5);
		li9.add (p20);
		li9.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p21 = new JTextField (5);
		li9.add (p21);
		if (vParam.virtualStandGibbsR1>0)  {	
			p20.setText(""+vParam.virtualStandGibbsR1);
			p21.setText(""+vParam.virtualStandGibbsCost1);
		}
		box.add (li9);
		
		JPanel li10 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li10.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p22 = new JTextField (5);
		li10.add (p22);
		li10.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p23 = new JTextField (5);
		li10.add (p23);
		if (vParam.virtualStandGibbsR2>vParam.virtualStandGibbsR1) {
			p22.setText(""+vParam.virtualStandGibbsR2);
			p23.setText(""+vParam.virtualStandGibbsCost2);
		}
		box.add (li10);
		
		JPanel li11 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li11.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.costRange")+" : ", 10));
		p24 = new JTextField (5);
		li11.add (p24);
		li11.add (new JWidthLabel (",   "+Translator.swap ("DVirtualStandXY.cost")+" : ", 10));
		p25 = new JTextField (5);
		li11.add (p25);
		if (vParam.virtualStandGibbsR3>vParam.virtualStandGibbsR2) {
			p24.setText(""+vParam.virtualStandGibbsR3);
			p25.setText(""+vParam.virtualStandGibbsCost3);
		}
		box.add (li11);
		XYPanel.add (box);
		
		initRdGroup1 ();
		
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
		
		setTitle (Translator.swap ("DVirtualStandSubPopXY")+" : "+pop+" / "+popNumber);
		
		setModal (true);
	}
	
	// To be called once on gui creation.
	//
	private void initRdGroup1 () {
		if (vParam.virtualStandXY == 1) {
			rdGroup1.setSelected (rdRandom.getModel (), true);
		} else if (vParam.virtualStandXY == 2) {
			rdGroup1.setSelected (rdNeymanScott.getModel (), true);
		} else {
			rdGroup1.setSelected (rdGibbs.getModel (), true);
		}
		rdGroup1Action ();	// enables /disables the radio text fields
	}
	
	// To be called on action of each radio button.
	//
	private void rdGroup1Action () {
		boolean random = rdGroup1.getSelection ().equals (rdRandom.getModel ());
		boolean neymanScott = rdGroup1.getSelection ().equals (rdNeymanScott.getModel ());
		boolean gibbs = rdGroup1.getSelection ().equals (rdGibbs.getModel ());
		
		p15.setEnabled (neymanScott);
		p16.setEnabled (neymanScott);
		
		p17.setEnabled (gibbs);
		p20.setEnabled (gibbs);
		p21.setEnabled (gibbs);
		p22.setEnabled (gibbs);
		p23.setEnabled (gibbs);
		p24.setEnabled (gibbs);
		p25.setEnabled (gibbs);
	}
	
	/**
	* From DialogItem interface.
	*/
	public void dispose () {super.dispose ();}
	
	public VirtualParameters getParameters () {return vParam;}	// use only if validDialog
	
}



