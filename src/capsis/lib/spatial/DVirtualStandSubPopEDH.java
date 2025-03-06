/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2006 Francois Goreaud.
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
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * DVirtualStandSubPopEDH - Dialog box to set the parameters concerning species, D and H 
 * for a sub population of a virtual mixed stand.
 * The parameters are stored in the given VirtualParameters object.
 *
 * @author F. Goreaud - 25/11/05 -> 9/3/06
 */
public class DVirtualStandSubPopEDH extends AmapDialog implements ActionListener {

	static {
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	} 
	
	private VirtualParameters vParam;	// the parameters for D & H simulation will be stored here
	private int pop;
	private int popNumber;
	
	// Items for the dialog interface
	
	// box "species"
	private JTextField p0;

	// box "diameter"	
	private ButtonGroup rdGroup1;

	private JRadioButton rdFileNxyDH;
	private JTextField fldxyDHInventory;
	private JButton browseButton0;

	private JRadioButton rdFileNDH;
	private JTextField fldDHInventory;
	private JButton browseButton;

	private JRadioButton rdFileND;
	private JTextField fldDInventory;
	private JButton browseButton2;

	private JRadioButton rdFileHistoD;
	private JTextField fldDHistogram;
	private JButton browseButton3;

	private JRadioButton rdGaussianD;
	private JTextField p3;
	private JTextField p1;
	private JTextField p1b;
	private JTextField p2;

// box "height"
	private ButtonGroup rdGroup2;

	private JRadioButton rdCurveHD;
	private JButton HCurveButton;

	private JRadioButton rdGaussianH;
	private JTextField p4;
	private JTextField p5;
	private JTextField p5b;

// box "intertype"
	private ButtonGroup rdGroup3;
	private JRadioButton rdIndependant;
	private JRadioButton rdInteraction;
	
// final box
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	
	/**
	 * Constructor.
	 */
	public DVirtualStandSubPopEDH (VirtualParameters vp, int p, int pn) {
		super ();
		vParam = vp;
		pop=p;
		popNumber = pn;
		
		createUI ();
		// location is set by AmapDialog
		pack ();
		show ();
	}

	// When defining the parameters for the H=f(D) Curve.
	//
	private void hCurveAction ()	{
		DVirtualStandHCurve dlgHCurve = new DVirtualStandHCurve (vParam);
		if (!dlgHCurve.isValidDialog ()) {	// if cancel, set dlg to null
			dlgHCurve.dispose ();
			dlgHCurve = null;
			vParam.virtualStandHCurve = 0;
			// ??? error message ?
		}
	}
	
	// When defining the name of the file for n, x,y, D and H.
	//
	private void browse0Action() {
		JFileChooser chooser = new JFileChooser(Settings.getProperty ("capsis.inventory.path",  PathManager.getDir ("data")));
		// fc - 20.10.2009
			//~ ProjectFileAccessory acc = new ProjectFileAccessory ();
			//~ chooser.setAccessory (acc);
			//~ chooser.addPropertyChangeListener (acc);
		int returnVal = chooser.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			vParam.virtualStandnxyDHFile = chooser.getSelectedFile ().toString ();
			fldxyDHInventory.setText (vParam.virtualStandnxyDHFile);
		}
		// ??? otherwise : error message ?
	}

	// When defining the name of the file for n, D and H.
	//
	private void browseAction() {
		JFileChooser chooser = new JFileChooser(Settings.getProperty ("capsis.inventory.path",  PathManager.getDir ("data")));
		// fc - 20.10.2009
			//~ ProjectFileAccessory acc = new ProjectFileAccessory ();
			//~ chooser.setAccessory (acc);
			//~ chooser.addPropertyChangeListener (acc);
		int returnVal = chooser.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			vParam.virtualStandnDHFile = chooser.getSelectedFile ().toString ();
			fldDHInventory.setText (vParam.virtualStandnDHFile);
		}
		// ??? otherwise : error message ?
	}
	
	// When defining the name of the file for n and D.
	//
	private void browse2Action() {
		JFileChooser chooser = new JFileChooser(Settings.getProperty ("capsis.inventory.path",  PathManager.getDir ("data")));
		// fc - 20.10.2009
			//~ ProjectFileAccessory acc = new ProjectFileAccessory ();
			//~ chooser.setAccessory (acc);
			//~ chooser.addPropertyChangeListener (acc);
		int returnVal = chooser.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			vParam.virtualStandnDFile = chooser.getSelectedFile ().toString ();
			fldDInventory.setText (vParam.virtualStandnDFile);
		}
		// ??? otherwise : error message ?
	}

	// When defining the name of the file containing the histogram of D.
	//
	private void browse3Action() {
		JFileChooser chooser = new JFileChooser(Settings.getProperty ("capsis.inventory.path",  PathManager.getDir ("data")));
		// fc - 20.10.2009
			//~ ProjectFileAccessory acc = new ProjectFileAccessory ();
			//~ chooser.setAccessory (acc);
			//~ chooser.addPropertyChangeListener (acc);
		int returnVal = chooser.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			vParam.virtualStandDHistFile = chooser.getSelectedFile ().toString ();
			fldDHistogram.setText (vParam.virtualStandDHistFile);
		}
		// ??? otherwise : error message ?
	}
	
	// When choosing the OK button.
	//
	private void okAction () {
		// Here, we only verify that the parameters are ok... and put them in vParam
		// the simulation itself will take place in VistualStandSimulator !
		
		boolean fileNxyDH = rdGroup1.getSelection ().equals (rdFileNxyDH.getModel ());
		boolean fileNDH = rdGroup1.getSelection ().equals (rdFileNDH.getModel ());
		boolean fileND = rdGroup1.getSelection ().equals (rdFileND.getModel ());
		boolean fileHistoD = rdGroup1.getSelection ().equals (rdFileHistoD.getModel ());
		boolean gaussianD = rdGroup1.getSelection ().equals (rdGaussianD.getModel ());
		boolean curveHD = rdGroup2.getSelection ().equals (rdCurveHD.getModel ());
		boolean gaussianH = rdGroup2.getSelection ().equals (rdGaussianH.getModel ());
		
		// species
		if (Check.isEmpty (p0.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandSubPopEDH.p0IsEmpty"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (!Check.isInt (p0.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandSubPopEDH.p0IsNotInteger"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		vParam.virtualStandSpeciesCode = new Integer (p0.getText ()).intValue ();
		if (vParam.virtualStandSpeciesCode<0) {
			JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.speciesCode"),
			Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		
		
		// DH
		
		if (fileNDH) {	// common file for n�, D, and H
			vParam.virtualStandD=1;
			// Verify the file name...
			if (Check.isEmpty (fldDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isFile (fldDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsNotFile"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandnDHFile = fldDHInventory.getText();
			
		} else if (fileNxyDH) {	// common file for n�, D, and H
			vParam.virtualStandD=5;
			// Verify the file name...
			if (Check.isEmpty (fldxyDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (!Check.isFile (fldxyDHInventory.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsNotFile"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			vParam.virtualStandnxyDHFile = fldxyDHInventory.getText();
			
		} else
		
		{	// we have to do D and H separately 
			// D first.
			if (fileND)	{	// file for n�, D
				vParam.virtualStandD=2;
				// Verify the file name...
				if (Check.isEmpty (fldDInventory.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isFile (fldDInventory.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsNotFile"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandnDFile = fldDInventory.getText(); 
				
			} else if (fileHistoD) {	// file D histogram
				vParam.virtualStandD=3;
				// Verify the file name...
				if (Check.isEmpty (fldDHistogram.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isFile (fldDHistogram.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.fileNameIsNotFile"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDHistFile = fldDHistogram.getText(); 
				
			} else {	// gaussianD	// random distribution for D
				vParam.virtualStandD=4;
				// Dmean.
				if (Check.isEmpty (p1.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p1IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p1.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p1IsNotDouble"),
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
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p2IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p2.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p2IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDDeviation = new Double (p2.getText ()).doubleValue ();
				if (vParam.virtualStandDDeviation<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.DDeviation"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// D Min.
				if (Check.isEmpty (p1b.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p1bIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p1b.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p1bIsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandDMin = new Double (p1b.getText ()).doubleValue ();
				if (vParam.virtualStandDMin<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.DMin"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// Tree number.
				if (Check.isEmpty (p3.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p3IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isInt (p3.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p3IsNotInteger"),
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
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p4IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p4.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p4IsNotDouble"),
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
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p5IsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p5.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p5IsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandHDeviation = new Double (p5.getText ()).doubleValue ();
				if (vParam.virtualStandHDeviation<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.HDeviation"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				// H min.
				if (Check.isEmpty (p5b.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p5bIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				if (!Check.isDouble (p5b.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandDH.p5bIsNotDouble"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				vParam.virtualStandHMin = new Double (p5b.getText ()).doubleValue ();
				if (vParam.virtualStandHMin<0) {
					JOptionPane.showMessageDialog (this, Translator.swap ("VirtualStandSimulator.HMin"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
							
			}
		}
		// spatial structure
				
		boolean independant = rdGroup3.getSelection ().equals (rdIndependant.getModel ());
		boolean interaction = rdGroup3.getSelection ().equals (rdInteraction.getModel ());
		if (independant==true)
		{	vParam.virtualStandXYmode = 1;
			System.out.println("independant");
		}
		else
		{	vParam.virtualStandXYmode = 2;
			System.out.println("interaction");
		}
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
		}  else if (evt.getSource ().equals (rdFileNxyDH)) {
			rdGroup1Action ();
		}  else if (evt.getSource ().equals (rdFileNDH)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdFileND)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdFileHistoD)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdGaussianD)) {
			rdGroup1Action ();
		} else if (evt.getSource ().equals (rdCurveHD)) {
			rdGroup2Action ();
		} else if (evt.getSource ().equals (rdGaussianH)) {
			rdGroup2Action ();
		} else if (evt.getSource ().equals (HCurveButton)) {
			hCurveAction();
		} else if (evt.getSource ().equals (browseButton)) {
			browseAction();
		} else if (evt.getSource ().equals (browseButton0)) {
			browse0Action();
		} else if (evt.getSource ().equals (browseButton2)) {
			browse2Action();
		} else if (evt.getSource ().equals (browseButton3)) {
			browse3Action();
		}
	}
	
	/**
	* Create the gui.
	*/
	private void createUI () {
		Box part1 = Box.createVerticalBox ();
		Border etched = BorderFactory.createEtchedBorder ();
		
		// Species
		JPanel panel0 = new JPanel ();
		panel0.setLayout (new BoxLayout (panel0, BoxLayout.Y_AXIS));
		Border b0 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandSubPopEDH.Species"));
		panel0.setBorder (b0);
		JPanel li0 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li0.add (new JWidthLabel("   "+Translator.swap ("DVirtualStandSubPopEDH.SpeciesCode")+" :", 10));
		p0 = new JTextField (5);
		li0.add (p0);
		p0.setText(""+vParam.virtualStandSpeciesCode);
		panel0.add (li0);
		part1.add (panel0);
		
		
		
		// Diameter & Height panel.
		
		// First radio button group contains 4 buttons.
		JPanel panel1 = new JPanel ();
		panel1.setLayout (new BoxLayout (panel1, BoxLayout.Y_AXIS));
		Border b1 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandDH.Diameters"));
		panel1.setBorder (b1);
		rdGroup1 = new ButtonGroup ();
		
		JPanel li00 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdFileNxyDH = new JRadioButton (Translator.swap ("DVirtualStandDH.xyDHcommonfile")+" :");
		rdFileNxyDH.addActionListener (this);
		rdGroup1.add (rdFileNxyDH);
		li00.add (rdFileNxyDH);
		fldxyDHInventory = new JTextField (15);
		browseButton0 = new JButton (Translator.swap ("Shared.browse"));
		browseButton0.addActionListener (this);
		li00.add (fldxyDHInventory);
		fldxyDHInventory.setText(vParam.virtualStandnxyDHFile);
		li00.add (browseButton0);
		panel1.add (li00);

		JPanel li1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdFileNDH = new JRadioButton (Translator.swap ("DVirtualStandDH.DHcommonfile")+" :");
		rdFileNDH.addActionListener (this);
		rdGroup1.add (rdFileNDH);
		li1.add (rdFileNDH);
		fldDHInventory = new JTextField (15);
		browseButton = new JButton (Translator.swap ("Shared.browse"));
		browseButton.addActionListener (this);
		li1.add (fldDHInventory);
		fldDHInventory.setText(vParam.virtualStandnDHFile);
		li1.add (browseButton);
		panel1.add (li1);
		
		JPanel li2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdFileND = new JRadioButton (Translator.swap ("DVirtualStandDH.Dfile")+" :");
		rdFileND.addActionListener (this);
		rdGroup1.add (rdFileND);
		//cb2 = new Checkbox( Translator.swap ("DVirtualStandSubPopEDH.Dfile"),(vParam.virtualStandD==2),cbg);
		li2.add (rdFileND);
		fldDInventory = new JTextField (15);
		browseButton2 = new JButton (Translator.swap ("Shared.browse"));
		browseButton2.addActionListener (this);
		li2.add (fldDInventory);
		fldDInventory.setText(vParam.virtualStandnDFile);
		li2.add (browseButton2);
		panel1.add (li2);
		
		JPanel li3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdFileHistoD = new JRadioButton (Translator.swap ("DVirtualStandDH.DHistogram")+" :");
		rdFileHistoD.addActionListener (this);
		rdGroup1.add (rdFileHistoD);
		//cb3 = new Checkbox( Translator.swap ("DVirtualStandSubPopEDH.DHistogram"),(vParam.virtualStandD==3),cbg);
		li3.add (rdFileHistoD);
		fldDHistogram = new JTextField (15);
		browseButton3 = new JButton (Translator.swap ("Shared.browse"));
		browseButton3.addActionListener (this);
		li3.add (fldDHistogram);
		fldDHistogram.setText(vParam.virtualStandDHistFile);
		li3.add (browseButton3);
		panel1.add (li3);
		
		JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdGaussianD = new JRadioButton (Translator.swap ("DVirtualStandDH.GaussianD")+" :");
		rdGaussianD.addActionListener (this);
		rdGroup1.add (rdGaussianD);
		//cb4 = new Checkbox( Translator.swap ("DVirtualStandSubPopEDH.GaussianD"),(vParam.virtualStandD==4),cbg);
		li4.add (rdGaussianD);
		li4.add (new JWidthLabel("          "+Translator.swap ("DVirtualStandDH.TreeNumber")+" :", 10));
		p3 = new JTextField (5);
		li4.add (p3);
		p3.setText(""+vParam.virtualStandTreeNumber);
		panel1.add (li4);
		
		JPanel li5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li5.add (new JWidthLabel("          "+  Translator.swap ("DVirtualStandDH.meanD")+" :",10));
		p1 = new JTextField (5);
		li5.add (p1);
		p1.setText(""+vParam.virtualStandDMean);
		li5.add (new JWidthLabel (Translator.swap ("DVirtualStandDH.standardError")+" :", 10));
		p2 = new JTextField (5);
		li5.add (p2);
		p2.setText(""+vParam.virtualStandDDeviation);
		li5.add (new JWidthLabel( Translator.swap ("DVirtualStandDH.DMin")+" :",10));
		p1b = new JTextField (5);
		li5.add (p1b);
		p1b.setText(""+vParam.virtualStandDMin);
		panel1.add (li5);
		
		initRdGroup1 ();
		part1.add (panel1);
		
		// Second radio button group contains 2 buttons.
		JPanel panel2 = new JPanel ();
		panel2.setLayout (new BoxLayout (panel2, BoxLayout.Y_AXIS));
		Border b2 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandDH.Heights"));
		panel2.setBorder (b2);
		rdGroup2 = new ButtonGroup ();
		
		JPanel li6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdCurveHD = new JRadioButton (Translator.swap ("DVirtualStandDH.HDCurve")+" :");
		rdCurveHD.addActionListener (this);
		rdGroup2.add (rdCurveHD);
		//cb5 = new Checkbox( Translator.swap ("DVirtualStandSubPopEDH.HDCurve"),(vParam.virtualStandH==1),cbg2);
		li6.add (rdCurveHD);
		HCurveButton = new JButton (Translator.swap ("DVirtualStandDH.parameters"));
		li6.add (HCurveButton);
		HCurveButton.addActionListener (this);
		panel2.add (li6);
		
		JPanel li7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdGaussianH = new JRadioButton (Translator.swap ("DVirtualStandDH.GaussianH")+" :");
		rdGaussianH.addActionListener (this);
		rdGroup2.add (rdGaussianH);
		//cb6 = new Checkbox( Translator.swap ("DVirtualStandSubPopEDH.GaussianH"),(vParam.virtualStandH==2),cbg2);
		li7.add (rdGaussianH);
		panel2.add (li7);
		
		JPanel li8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		li8.add (new JWidthLabel("          "+ Translator.swap ("DVirtualStandDH.meanH")+" :",10));
		p4 = new JTextField (5);
		li8.add (p4);
		p4.setText(""+vParam.virtualStandHMean);
		li8.add (new JWidthLabel (Translator.swap ("DVirtualStandDH.standardError")+" :", 10));
		p5 = new JTextField (5);
		li8.add (p5);
		p5.setText(""+vParam.virtualStandHDeviation);
		li8.add (new JWidthLabel( Translator.swap ("DVirtualStandDH.HMin")+" :",10));
		p5b = new JTextField (5);
		li8.add (p5b);
		p5b.setText(""+vParam.virtualStandHMin);
		panel2.add (li8);
		
		part1.add (panel2);
		initRdGroup2 ();


		// spatial structure
		// Third radio button group contains 2 buttons.
		JPanel panel3 = new JPanel ();
		panel3.setLayout (new BoxLayout (panel3, BoxLayout.Y_AXIS));
		Border b3 = BorderFactory.createTitledBorder (etched, Translator.swap ("DVirtualStandSubPopEDH.SpatialStructure"));
		panel3.setBorder (b3);
		rdGroup3 = new ButtonGroup ();
		
		JPanel li16 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdIndependant = new JRadioButton (Translator.swap ("DVirtualStandSubPopEDH.Independant"));
		rdIndependant.addActionListener (this);
		rdGroup3.add (rdIndependant);
		li16.add (rdIndependant);
		panel3.add (li16);
		
		JPanel li17 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdInteraction = new JRadioButton (Translator.swap ("DVirtualStandSubPopEDH.Interaction"));
		rdInteraction.addActionListener (this);
		rdGroup3.add (rdInteraction);
		li17.add (rdInteraction);
		panel3.add (li17);
				
		part1.add (panel3);
		initRdGroup3 ();


		
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
		
		setTitle (Translator.swap ("DVirtualStandSubPopEDH")+pop+" / "+popNumber);
		
		setModal (true);
	}
	
	// To be called once on gui creation.
	//
	private void initRdGroup1 () {
		if (vParam.virtualStandD == 1) {
			rdGroup1.setSelected (rdFileNDH.getModel (), true);
		} else if (vParam.virtualStandD == 2) {
			rdGroup1.setSelected (rdFileND.getModel (), true);
		}  else if (vParam.virtualStandD == 3) {
			rdGroup1.setSelected (rdFileHistoD.getModel (), true);
		}  else if (vParam.virtualStandD == 4) {
			rdGroup1.setSelected (rdGaussianD.getModel (), true);
		} else {
			rdGroup1.setSelected (rdFileNxyDH.getModel (), true);
		}
		rdGroup1Action ();	// enables /disables the radio text fields
	}

	// To be called once on gui creation.
	//
	private void initRdGroup2 () {
		if (vParam.virtualStandH == 1) {
			rdGroup2.setSelected (rdCurveHD.getModel (), true);
		} else {
			rdGroup2.setSelected (rdGaussianH.getModel (), true);
		} 
		rdGroup2Action ();	// enables /disables the radio text fields
	}

	// To be called once on gui creation.
	//
	private void initRdGroup3 () {
		if (vParam.virtualStandXYmode == 1) {
			rdGroup3.setSelected (rdIndependant.getModel (), true);
		} else {
			rdGroup3.setSelected (rdInteraction.getModel (), true);
		} 
		rdGroup3Action ();	// enables /disables the radio text fields
	}

	// To be called on action of each of the first group radio buttons.
	//
	private void rdGroup1Action () {
		boolean fileNxyDH = rdGroup1.getSelection ().equals (rdFileNxyDH.getModel ());
		boolean fileNDH = rdGroup1.getSelection ().equals (rdFileNDH.getModel ());
		boolean fileND = rdGroup1.getSelection ().equals (rdFileND.getModel ());
		boolean fileHistoD = rdGroup1.getSelection ().equals (rdFileHistoD.getModel ());
		boolean gaussianD = rdGroup1.getSelection ().equals (rdGaussianD.getModel ());
		
		fldxyDHInventory.setEnabled (fileNxyDH);
		browseButton0.setEnabled (fileNxyDH);

		fldDHInventory.setEnabled (fileNDH);
		browseButton.setEnabled (fileNDH);
		
		fldDInventory.setEnabled (fileND);
		browseButton2.setEnabled (fileND);
		
		fldDHistogram.setEnabled (fileHistoD);
		browseButton3.setEnabled (fileHistoD);
		
		p1.setEnabled (gaussianD);
		p2.setEnabled (gaussianD);
		p3.setEnabled (gaussianD);
	}
	
	// To be called on action of each of the second group radio buttons.
	//
	private void rdGroup2Action () {
		boolean curveHD = rdGroup2.getSelection ().equals (rdCurveHD.getModel ());
		boolean gaussianH = rdGroup2.getSelection ().equals (rdGaussianH.getModel ());
		
		HCurveButton.setEnabled (curveHD);
		
		p4.setEnabled (gaussianH);
		p5.setEnabled (gaussianH);
	}

	// To be called on action of each of the second group radio buttons.
	//
	private void rdGroup3Action () {
		boolean independant = rdGroup3.getSelection ().equals (rdIndependant.getModel ());
		boolean interaction = rdGroup3.getSelection ().equals (rdInteraction.getModel ());
	}
	
	/**
	* From DialogItem interface.
	*/
	public void dispose () {super.dispose ();}
	
	public VirtualParameters getParameters () {return vParam;}	// use only if validDialog
	
}
