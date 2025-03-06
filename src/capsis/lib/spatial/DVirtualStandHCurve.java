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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
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
 * DVirtualStandHCurve - Dialog box to set the parameters for H=f(D) curve.
 * 
 * @author F Goreaud - 6/9/01 -> 18/8/02
 */
public class DVirtualStandHCurve extends AmapDialog implements ActionListener {
//checked for c4.1.1_09 - fc - 5.2.2003
	
	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
	private VirtualParameters vParam;
	
	private JRadioButton rdOption1;
	private JRadioButton rdOption2;
	private JRadioButton rdOption3;
	private ButtonGroup rdGroup1;
	
	private JTextField p11;
	private JTextField p12;
	private JTextField p13;
	private JTextField p14;
	private JTextField p21;
	private JTextField p22;
	private JTextField p23;
	private JTextField p24;
	private JTextField p31;
	private JTextField p32;
	private JTextField p33;
	private JTextField p34;
	
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constuctor.
	 */
	public DVirtualStandHCurve (VirtualParameters vp) {
		super ();
		
		vParam = vp;
		
		createUI ();
		// location is set by AmapDialog
		pack ();
		show ();
	}
	
	// When choosing the OK button.
	private void okAction () {
		// Here, we only verify that the parameters are ok... and put them in vParam
		// the simulation itself will take place in VistualStandSimulator !
		
		boolean option1 = rdGroup1.getSelection ().equals (rdOption1.getModel ());
		boolean option2 = rdGroup1.getSelection ().equals (rdOption2.getModel ());
		boolean option3 = rdGroup1.getSelection ().equals (rdOption3.getModel ());
		
		if (option1) {	// first curve
			vParam.virtualStandHCurve=1;
			// Parameter a.
			if (Check.isEmpty (p11.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p11IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p11.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p11IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH1a=new Double (p11.getText ()).doubleValue ();
			
			// Parameter b.
			if (Check.isEmpty (p12.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p12IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p12.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p12IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH1b=new Double (p12.getText ()).doubleValue ();
			
			// Pparameter c.
			if (Check.isEmpty (p13.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p13IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p13.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p13IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH1c=new Double (p13.getText ()).doubleValue ();
			
			// Parameter d.
			if (Check.isEmpty (p14.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p14IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p14.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p14IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH1d=new Double (p14.getText ()).doubleValue ();
			
		} else {	// we still modify settings values, but no obligation ...
			if ((!Check.isEmpty (p11.getText ()))&&(Check.isDouble (p11.getText ()))) {
				vParam.virtualStandH1a=new Double (p11.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p12.getText ()))&&(Check.isDouble (p12.getText ()))) {
				vParam.virtualStandH1b=new Double (p12.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p13.getText ()))&&(Check.isDouble (p13.getText ()))){
				vParam.virtualStandH1c=new Double (p13.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p14.getText ()))&&(Check.isDouble (p14.getText ()))){
				vParam.virtualStandH1d=new Double (p14.getText ()).doubleValue ();	
			}
		}
		
		if (option2) {	// second curve
			vParam.virtualStandHCurve=2;
			
			// Parameter a.
			if (Check.isEmpty (p21.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p21IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p21.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p21IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH2a=new Double (p21.getText ()).doubleValue ();
			
			// Parameter K.
			if (Check.isEmpty (p22.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p22IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p22.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p22IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH2K=new Double (p22.getText ()).doubleValue ();
			
			// Parameter b.
			if (Check.isEmpty (p23.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p23IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p23.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p23IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH2b=new Double (p23.getText ()).doubleValue ();
			
			// Parameter d.
			if (Check.isEmpty (p24.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p24IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p24.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p24IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH2d=new Double (p24.getText ()).doubleValue ();
			
		} else {	// we still modify settings values, but no obligation ...
			
			if ((!Check.isEmpty (p21.getText ()))&&(Check.isDouble (p21.getText ()))) {
				vParam.virtualStandH2a=new Double (p21.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p22.getText ()))&&(Check.isDouble (p22.getText ()))) {
				vParam.virtualStandH2K=new Double (p22.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p23.getText ()))&&(Check.isDouble (p23.getText ()))) {
				vParam.virtualStandH2b=new Double (p23.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p24.getText ()))&&(Check.isDouble (p24.getText ()))) {
				vParam.virtualStandH2d=new Double (p24.getText ()).doubleValue ();	
			}
		}
		
		if (option3) {	// third curve
			
			vParam.virtualStandHCurve=3;
			
			// Parameter a.
			if (Check.isEmpty (p31.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p31IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p31.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p31IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH3a=new Double (p31.getText ()).doubleValue ();
			
			// Parameter b.
			if (Check.isEmpty (p32.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p32IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p32.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p32IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH3b=new Double (p32.getText ()).doubleValue ();
			
			// Parameter c.
			if (Check.isEmpty (p33.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p33IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p33.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p33IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH3c=new Double (p33.getText ()).doubleValue ();
			
			// Parameter d.
			if (Check.isEmpty (p34.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p34IsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isDouble (p34.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandHCurve.p34IsNotDouble"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandH3d=new Double (p34.getText ()).doubleValue ();
			
		} else {	// we still modify settings values, but no obligation ...
			
			if ((!Check.isEmpty (p31.getText ()))&&(Check.isDouble (p31.getText ()))) {
				vParam.virtualStandH3a=new Double (p31.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p32.getText ()))&&(Check.isDouble (p32.getText ()))) {
				vParam.virtualStandH3b=new Double (p32.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p33.getText ()))&&(Check.isDouble (p33.getText ()))) {
				vParam.virtualStandH3c=new Double (p33.getText ()).doubleValue ();	
			}
			
			if ((!Check.isEmpty (p34.getText ()))&&(Check.isDouble (p34.getText ()))) {
				vParam.virtualStandH3d=new Double (p34.getText ()).doubleValue ();	
			}
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
		} else if (evt.getSource ().equals (rdOption1)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdOption2)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdOption3)) {
			rdGroup1Action ();
		}
	}
	
	/**
	* Create the GUI.
	*/
	private void createUI () {
		Box part1 = Box.createVerticalBox ();
		Border etched = BorderFactory.createEtchedBorder ();
		
		rdGroup1 = new ButtonGroup ();
		
		// Option 1.
		JPanel panel1 = new JPanel ();
		panel1.setLayout (new BoxLayout (panel1, BoxLayout.Y_AXIS));
		Border b1 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandHCurve.option1Name"));
		panel1.setBorder (b1);
		
		JPanel li1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdOption1 = new JRadioButton (Translator.swap ("DVirtualStandHCurve.option1"));
		rdOption1.addActionListener (this);
		rdGroup1.add (rdOption1);
		//cb1 = new Checkbox("H = a + b*D + c*sqrt(D) + d*N(0,1)",(vParam.virtualStandHCurve==1),cbg);
		li1.add (rdOption1);
		panel1.add (li1);
		
		JPanel li2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li2.add (new JWidthLabel ("a=", 10));
		p11 = new JTextField (5);
		li2.add (p11);
		p11.setText(""+vParam.virtualStandH1a);
		li2.add (new JWidthLabel ("b=", 10));
		p12 = new JTextField (5);
		li2.add (p12);
		p12.setText(""+vParam.virtualStandH1b);
		li2.add (new JWidthLabel ("c=", 10));
		p13 = new JTextField (5);
		li2.add (p13);
		p13.setText(""+vParam.virtualStandH1c);
		li2.add (new JWidthLabel ("d=", 10));
		p14 = new JTextField (5);
		li2.add (p14);
		p14.setText(""+vParam.virtualStandH1d);
		panel1.add (li2);
		
		// Option 2.
		JPanel panel2 = new JPanel ();
		panel2.setLayout (new BoxLayout (panel2, BoxLayout.Y_AXIS));
		Border b2 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandHCurve.option2Name"));
		panel2.setBorder (b2);
		
		JPanel li3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdOption2 = new JRadioButton (Translator.swap ("DVirtualStandHCurve.option2"));
		rdOption2.addActionListener (this);
		rdGroup1.add (rdOption2);
		//cb2 = new Checkbox("H = a+K*(1-exp(-b*D)) + d*N(0,1)",(vParam.virtualStandHCurve==2),cbg);
		li3.add (rdOption2);
		panel2.add (li3);
		
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li4.add (new JWidthLabel ("a=", 10));
		p21 = new JTextField (5);
		li4.add (p21);
		p21.setText(""+vParam.virtualStandH2a);
		li4.add (new JWidthLabel ("K=", 10));
		p22 = new JTextField (5);
		li4.add (p22);
		p22.setText(""+vParam.virtualStandH2K);
		li4.add (new JWidthLabel ("b=", 10));
		p23 = new JTextField (5);
		li4.add (p23);
		p23.setText(""+vParam.virtualStandH2b);
		li4.add (new JWidthLabel ("d=", 10));
		p24 = new JTextField (5);
		li4.add (p24);
		p24.setText(""+vParam.virtualStandH2d);
		panel2.add (li4);
		
		// Option 3.
		JPanel panel3 = new JPanel ();
		panel3.setLayout (new BoxLayout (panel3, BoxLayout.Y_AXIS));
		Border b3 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandHCurve.option3Name"));
		panel3.setBorder (b3);
		
		JPanel li5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdOption3 = new JRadioButton (Translator.swap ("DVirtualStandHCurve.option3"));
		rdOption3.addActionListener (this);
		rdGroup1.add (rdOption3);
		//cb3 = new Checkbox("H = a + D²/(b + c*D)² + d*N(0,1)",(vParam.virtualStandHCurve==3),cbg);
		li5.add (rdOption3);
		panel3.add (li5);
		
		JPanel li6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li6.add (new JWidthLabel ("a=", 10));
		p31 = new JTextField (5);
		li6.add (p31);
		p31.setText(""+vParam.virtualStandH3a);
		li6.add (new JWidthLabel ("b=", 10));
		p32 = new JTextField (5);
		li6.add (p32);
		p32.setText(""+vParam.virtualStandH3b);
		li6.add (new JWidthLabel ("c=", 10));
		p33 = new JTextField (5);
		li6.add (p33);
		p33.setText(""+vParam.virtualStandH3c);
		li6.add (new JWidthLabel ("d=", 10));
		p34 = new JTextField (5);
		li6.add (p34);
		p34.setText(""+vParam.virtualStandH3d);
		panel3.add (li6);
		
		//~ HCurvePanel.add (box);
		
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
		
		part1.add (panel1);
		part1.add (panel2);
		part1.add (panel3);
		
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setTitle (Translator.swap ("DVirtualStandHCurve"));
		
		setModal (true);
	}
	
	// To be called once on gui creation.
	//
	private void initRdGroup1 () {
		if (vParam.virtualStandHCurve == 1) {
			rdGroup1.setSelected (rdOption1.getModel (), true);
		} else if (vParam.virtualStandHCurve == 2) {
			rdGroup1.setSelected (rdOption2.getModel (), true);
		} else {
			rdGroup1.setSelected (rdOption3.getModel (), true);
		}
		rdGroup1Action ();	// enables /disables the radio text fields
	}
	
	// To be called on action of each radio button.
	//
	private void rdGroup1Action () {
		boolean option1 = rdGroup1.getSelection ().equals (rdOption1.getModel ());
		boolean option2 = rdGroup1.getSelection ().equals (rdOption2.getModel ());
		boolean option3 = rdGroup1.getSelection ().equals (rdOption3.getModel ());
		
		p11.setEnabled (option1);
		p12.setEnabled (option1);
		p13.setEnabled (option1);
		p14.setEnabled (option1);
		
		p21.setEnabled (option2);
		p22.setEnabled (option2);
		p23.setEnabled (option2);
		p24.setEnabled (option2);
		
		p31.setEnabled (option3);
		p32.setEnabled (option3);
		p33.setEnabled (option3);
		p34.setEnabled (option3);
		
	}
	
}



