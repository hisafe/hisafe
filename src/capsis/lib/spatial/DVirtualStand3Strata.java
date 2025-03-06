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
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * DVirtualStand3Strata - Dialog box to define the different sub populations (strata) of 
 * a complex virtual stand.
 * The parameters are stored in the given Virtual3StrataParameters object.
 *
 * @author F. Goreaud - june 2004 -> 18/6/04
 */
//~ public class DVirtualStand3Strata extends AmapDialog implements ActionListener {
public class DVirtualStand3Strata extends AmapDialog implements ActionListener {

	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
//	private Virtual3StrataParameters vParam;	// the parameters will be stored here
	private VirtualParameters vParam;	// the parameters will be stored here
		
	// Items for the dialog interface
	private JTextField species1;
	private JTextField species2;
	private JTextField species3;

	private JTextField number1;
	private JTextField number2;
	private JTextField number3;
	
	private JButton DH1Button;
	private JButton DH2Button;
	private JButton DH3Button;
	private JButton SS1Button;
	private JButton SS2Button;
	private JButton SS3Button;
	private JButton SSInterButton;
	
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constructor.
	 */
//	public DVirtualStand3Strata (Virtual3StrataParameters vp) {
	public DVirtualStand3Strata (VirtualParameters vp) {
		super ();
		vParam = vp;
		
		createUI ();
		// location is set by AmapDialog
		pack ();
		show ();
	}

	// we have to put here the methods for all buttons.
	//
	
	// When choosing the OK button.
	//
	private void okAction () {
		// Here, we only verify that the parameters are ok... and put them in vParam
		// the simulation itself will take place in VistualStandSimulator !
		
/*		boolean fileNDH = rdGroup1.getSelection ().equals (rdFileNDH.getModel ());
		boolean fileND = rdGroup1.getSelection ().equals (rdFileND.getModel ());
		boolean fileHistoD = rdGroup1.getSelection ().equals (rdFileHistoD.getModel ());
		boolean gaussianD = rdGroup1.getSelection ().equals (rdGaussianD.getModel ());
		boolean curveHD = rdGroup2.getSelection ().equals (rdCurveHD.getModel ());
		boolean gaussianH = rdGroup2.getSelection ().equals (rdGaussianH.getModel ());
		
		if (fileNDH) {	// common file for n°, D, and H
			vParam.virtualStandD=1;
			// Verify the file name...
			if (Check.isEmpty (fldDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isFile (fldDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsNotFile"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandnDHFile = fldDHInventory.getText();
			
		} else {	// we have to do D and H separately 
			// D first.
			if (fileND)	{	// file for n°, D
				vParam.virtualStandD=2;
				// Verify the file name...
				if (Check.isEmpty (fldDInventory.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isFile (fldDInventory.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsNotFile"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandnDFile = fldDInventory.getText(); 
				
			} else if (fileHistoD) {	// file D histogram
				vParam.virtualStandD=3;
				// Verify the file name...
				if (Check.isEmpty (fldDHistogram.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isFile (fldDHistogram.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.fileNameIsNotFile"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDHistFile = fldDHistogram.getText(); 
				
			} else {	// gaussianD	// random distribution for D
				vParam.virtualStandD=4;
				// Dmean.
				if (Check.isEmpty (p1.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p1IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p1.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p1IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDMean = new Double (p1.getText ()).doubleValue ();
				if (vParam.virtualStandDMean<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.DMean"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// D standard deviation.
				if (Check.isEmpty (p2.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p2IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p2.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p2IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDDeviation = new Double (p2.getText ()).doubleValue ();
				if (vParam.virtualStandDDeviation<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.DDeviation"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// Tree number.
				if (Check.isEmpty (p3.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p3IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isInt (p3.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p3IsNotInteger"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandTreeNumber = new Integer (p3.getText ()).intValue ();
				if (vParam.virtualStandTreeNumber<1) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.treeNumber"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
			}
			
			// H now.
			if (curveHD) {	// when using a curve H=f(D)
				vParam.virtualStandH=1;
				
			} else {	// gaussianH	// random distribution for H
				vParam.virtualStandH=2;
				// H mean.
				if (Check.isEmpty (p4.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p4IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p4.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p4IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandHMean = new Double (p4.getText ()).doubleValue ();
				if (vParam.virtualStandHMean<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.HMean"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// H standard deviation.
				if (Check.isEmpty (p5.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p5IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p5.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStand3Strata.p5IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandHDeviation = new Double (p5.getText ()).doubleValue ();
				if (vParam.virtualStandHDeviation<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.HDeviation"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				
			}
		}*/
		setValidDialog (true);
	}
	
	/**
	* Manage gui events.
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		}  else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			// fc - 20.10.2009
				//~ Helper.helpFor (this);
		} 
// to be completed with more buttons
	}
	
	/**
	* Create the gui.
	*/
	private void createUI () {
		Box part1 = Box.createVerticalBox ();
		Border etched = BorderFactory.createEtchedBorder ();
		
		// Main frame for defining the stratas
		
		JPanel panel1 = new JPanel ();
		panel1.setLayout (new BoxLayout (panel1, BoxLayout.Y_AXIS));
		Border b1 = BorderFactory.createTitledBorder (etched, "Defining the different stratas" );
		panel1.setBorder (b1);
		
		// 1st line : title
		JPanel li1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li1.add (new JWidthLabel("n°     species     Tree number  :", 10));
		panel1.add (li1);
		
		// 2nd line : 1st sub population : species, number , DH Button, SS Button
		JPanel li2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li2.add (new JWidthLabel("1  ", 10));

		species1 = new JTextField (5);
		li2.add (species1);
		species1.setText("");       //+vParam.virtualStandTreeNumber);

		li2.add (new JWidthLabel("  ",10));
		number1 = new JTextField (5);
		li2.add (number1);
		number1.setText("");        //+vParam.virtualStandDMean);

		li2.add (new JWidthLabel ("  ", 10));
		DH1Button = new JButton ("D&H simulation");
		DH1Button.addActionListener (this);
		li2.add (DH1Button);

		li2.add (new JWidthLabel ("  ", 10));
		SS1Button = new JButton ("Spatial Structure");
		SS1Button.addActionListener (this);
		li2.add (SS1Button);

		panel1.add (li2);
		
		// 3rd line : 2nd sub population : species, number , DH Button, SS Button
		JPanel li3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li3.add (new JWidthLabel("2  ", 10));

		species2 = new JTextField (5);
		li3.add (species2);
		species2.setText("");       //+vParam.virtualStandTreeNumber);

		li3.add (new JWidthLabel("  ",10));
		number2 = new JTextField (5);
		li3.add (number2);
		number2.setText("");        //+vParam.virtualStandDMean);

		li3.add (new JWidthLabel ("  ", 10));
		DH2Button = new JButton ("D&H simulation");
		DH2Button.addActionListener (this);
		li3.add (DH2Button);

		li3.add (new JWidthLabel ("  ", 10));
		SS2Button = new JButton ("Spatial Structure");
		SS2Button.addActionListener (this);
		li3.add (SS2Button);

		panel1.add (li3);

		// 4th line : 3rd sub population : species, number , DH Button, SS Button
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li4.add (new JWidthLabel("3  ", 10));

		species3 = new JTextField (5);
		li4.add (species3);
		species3.setText("");       //+vParam.virtualStandTreeNumber);

		li4.add (new JWidthLabel("  ",10));
		number3 = new JTextField (5);
		li4.add (number3);
		number3.setText("");        //+vParam.virtualStandDMean);

		li4.add (new JWidthLabel ("  ", 10));
		DH3Button = new JButton ("D&H simulation");
		DH3Button.addActionListener (this);
		li4.add (DH3Button);

		li4.add (new JWidthLabel ("  ", 10));
		SS3Button = new JButton ("Spatial Structure");
		SS3Button.addActionListener (this);
		li4.add (SS3Button);

		panel1.add (li4);

		// line 5 : interactions
		JPanel li5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li5.add (new JWidthLabel("Interactions between sub population", 10));
		SSInterButton = new JButton ("Spatial Structure");
		SSInterButton.addActionListener (this);
		li5.add (SSInterButton);
		panel1.add (li5);

		part1.add (panel1);
		
		
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
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setTitle ("Simulating multi Stratas virtual stand"); //Translator.swap ("DVirtualStand3Strata"));
		
		setModal (true);
	}
	
	/**
	* From DialogItem interface.
	*/
	public void dispose () {super.dispose ();}
	
//	public Virtual3StrataParameters getParameters () {return vParam;}	// use only if validDialog
	public VirtualParameters getParameters () {return vParam;}	// use only if validDialog
	
}
