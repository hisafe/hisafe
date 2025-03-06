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
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * DVirtualStandOakPineTypo - Dialog box to set the parameters concerning 
 * the simulation of Oak Pine mixed stands from typology.
 * Parameters will be stored in the VirtualParameterOakPineTypo object.
 *
 * @author F Goreaud - 12/06/07
 */
    public class DVirtualStandOakPineTypo extends AmapDialog implements ActionListener {
   
      static {
         Translator.addBundle("capsis.lib.spatial.SpatialLabels");
      } 
   
      private VirtualParametersOakPineTypo vParamOPT;
      
      private JTextField p10;
      private JTextField p11;
      private JTextField p12;
      private JTextField p13;
      private JTextField p20;
      private JTextField p21;
      private JTextField p22;
      private JTextField p23;
   
      private JButton ok;
      private JButton cancel;
      private JButton help;
   	
  
   /**
    * Constructor.
    */
       public DVirtualStandOakPineTypo (VirtualParametersOakPineTypo vpOPT) {
         super ();
      
         vParamOPT = vpOPT;	// default values
      
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
         vParamOPT.virtualStandXmin = new Double (p10.getText ()).doubleValue ();
      
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
         vParamOPT.virtualStandXmax = new Double (p11.getText ()).doubleValue ();
      
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
         vParamOPT.virtualStandYmin = new Double (p12.getText ()).doubleValue ();
      
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
         vParamOPT.virtualStandYmax = new Double (p13.getText ()).doubleValue ();
      
      //	numberOak
         if (Check.isEmpty (p20.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         if (!Check.isInt (p20.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         vParamOPT.numberOak = new Integer (p20.getText ()).intValue ();
         if (vParamOPT.numberOak<1) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
			
      //	numberPine
         if (Check.isEmpty (p21.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         if (!Check.isInt (p21.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p21IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         vParamOPT.numberPine = new Integer (p21.getText ()).intValue ();
         if (vParamOPT.numberPine<1) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
   
	      //	numberOakUnder
         if (Check.isEmpty (p22.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p22IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         if (!Check.isInt (p22.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p22IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         vParamOPT.numberOakUnder = new Integer (p22.getText ()).intValue ();
         if (vParamOPT.numberOakUnder<1) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p22IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
			
        //	numberPineUnder
         if (Check.isEmpty (p23.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p23IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         if (!Check.isInt (p23.getText ())) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p23IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
         }
         vParamOPT.numberPineUnder = new Integer (p23.getText ()).intValue ();
         if (vParamOPT.numberPineUnder<1) {
            JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOakPineTypo.p23IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
            return;
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
         } 
         else if (evt.getSource ().equals (cancel)) {
            setValidDialog (false);
         } 
         else if (evt.getSource ().equals (help)) {
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
         Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandOakPineTypo.location"));
         XYPanel.setBorder (bor);
      
         Box box = Box.createVerticalBox ();
         CheckboxGroup cbg = new CheckboxGroup();
      		
         JPanel li2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li2.add (new JWidthLabel ( Translator.swap ("DVirtualStandXY.xmin"), 10));
         p10 = new JTextField (5);
         p10.setText(""+vParamOPT.virtualStandXmin);
         li2.add (p10);
         li2.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandXY.xmax"), 10));
         p11 = new JTextField (5);
         p11.setText(""+vParamOPT.virtualStandXmax);
         li2.add (p11);
         box.add (li2);
      
         JPanel li3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li3.add (new JWidthLabel (Translator.swap ("DVirtualStandXY.ymin"), 10));
         p12 = new JTextField (5);
         p12.setText(""+vParamOPT.virtualStandYmin);
         li3.add (p12);
         li3.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandXY.ymax"), 10));
         p13 = new JTextField (5);
         p13.setText(""+vParamOPT.virtualStandYmax);
         li3.add (p13);
         box.add (li3);
         XYPanel.add (box);
         part1.add (XYPanel);
      
      
      // 1. introduction.
         JPanel li4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li4.add (new JWidthLabel (Translator.swap ("DVirtualStandOakPineTypo.Intro")+" : ", 20));
         part1.add(li4);
      
       
      // 2 can0py
         JPanel p2 =  new ColumnPanel ();
         JPanel li5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li5.add (new JWidthLabel ( Translator.swap ("DVirtualStandOakPineTypo.NbOakCanopy"), 10));
         p20 = new JTextField (5);
         p20.setText(""+vParamOPT.numberOak);
         li5.add (p20);
         p2.add(li5);
         JPanel li6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li6.add (new JWidthLabel ( Translator.swap ("DVirtualStandOakPineTypo.NbPineCanopy"), 10));
         p21 = new JTextField (5);
         p21.setText(""+vParamOPT.numberPine);
         li6.add (p21);
         p2.add(li6);
         JPanel li7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li7.add (new JWidthLabel ( Translator.swap ("DVirtualStandOakPineTypo.NbOakUnder"), 10));
         p22 = new JTextField (5);
         p22.setText(""+vParamOPT.numberOakUnder);
         li7.add (p22);
         p2.add(li7);
         JPanel li8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li8.add (new JWidthLabel ( Translator.swap ("DVirtualStandOakPineTypo.NbPineUnder"), 10));
         p23 = new JTextField (5);
         p23.setText(""+vParamOPT.numberPineUnder);
         li8.add (p23);
         p2.add(li8);
      
         part1.add(p2);
      
      
      // conclusion
         JPanel li9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li9.add (new JWidthLabel (Translator.swap ("DVirtualStandOakPineTypo.Conclu")+" : ", 20));
         part1.add(li9);
      
      
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
      
         setTitle (Translator.swap ("DVirtualStandOakPineTypo"));
         
         setModal (true);
      }
   
   
   /**
   * From DialogItem interface.
   */
       public void dispose () {super.dispose ();}
   
       public VirtualParametersOakPineTypo getParameters () {
         return vParamOPT;}	// use only if validDialog
   
   }



