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
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * DVirtualStandOPTCanopy - Dialog box to set the parameters concerning 
 * the simulation of the Canopy of Oak Pine mixed stands from typology.
 * Parameters will be stored in the VirtualParameterOakPineTypo object.
 *
 * @author F Goreaud - 12/06/07
 */
    public class DVirtualStandOPTCanopy extends AmapDialog implements ActionListener {
   
      static {
         Translator.addBundle("capsis.lib.spatial.SpatialLabels");
      } 
   
      private VirtualParametersOakPineTypo vParamOPT;
   
      private Checkbox cb1;
      private Checkbox cb2;
      private Checkbox cb3;
      private Checkbox cb4;
   
      private JTextField p20;
      private JTextField p21;
      private JTextField p22;
      private JTextField p23;
      private JTextField p24;
      private JTextField p25;
      private JTextField p30;
      private JTextField p31;
      private JTextField p32;
      private JTextField p33;
      private JTextField p34;
      private JTextField p35;
      private JTextField p42;
      private JTextField p43;
      private JTextField p44;
      private JTextField p45;
      private JTextField p46;
      private JTextField p52;
      private JTextField p53;
      private JTextField p54;
      private JTextField p55;
   
      private JButton ok;
      private JButton cancel;
      private JButton help;
   	
      private JRadioButton Type1;
      private JRadioButton Type2;
      private JRadioButton Type3;
      private JRadioButton Type4;
      private ButtonGroup TypeChoice;
   
   /**
    * Constructor.
    */
       public DVirtualStandOPTCanopy (VirtualParametersOakPineTypo vpOPT) {
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
      
   
     	
      // Type 1
         if (Type1.isSelected())
         {	vParamOPT.type=1;
         //	NbAgOak
            if (Check.isEmpty (p20.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p20.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1NbAgOak = new Integer (p20.getText ()).intValue ();
            if (vParamOPT.Type1NbAgOak<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // ROak
            if (Check.isEmpty (p21.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p21.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1ROak = new Double (p21.getText ()).doubleValue ();
            if (vParamOPT.Type1ROak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         
         //	NbAgPine
            if (Check.isEmpty (p22.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p22.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1NbAgPine = new Integer (p22.getText ()).intValue ();
            if (vParamOPT.Type1NbAgPine<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // RPine
            if (Check.isEmpty (p23.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p23.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1RPine = new Double (p23.getText ()).doubleValue ();
            if (vParamOPT.Type1RPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistIntertype
            if (Check.isEmpty (p24.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p24.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1DistIntertype = new Double (p24.getText ()).doubleValue ();
            if (vParamOPT.Type1DistIntertype<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistPine
            if (Check.isEmpty (p25.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p25.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type1DistPine = new Double (p25.getText ()).doubleValue ();
            if (vParamOPT.Type1DistPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         }
         
         // Type 2
         else if (Type2.isSelected())
        {	vParamOPT.type=2;
         //	NbAgOak
            if (Check.isEmpty (p30.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p30.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2NbAgOak = new Integer (p30.getText ()).intValue ();
            if (vParamOPT.Type2NbAgOak<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // ROak
            if (Check.isEmpty (p31.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p31.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2ROak = new Double (p31.getText ()).doubleValue ();
            if (vParamOPT.Type2ROak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         
         //	NbAgPine
            if (Check.isEmpty (p32.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p32.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2NbAgPine = new Integer (p32.getText ()).intValue ();
            if (vParamOPT.Type2NbAgPine<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // RPine
            if (Check.isEmpty (p33.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p33.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2RPine = new Double (p33.getText ()).doubleValue ();
            if (vParamOPT.Type2RPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistIntertype
            if (Check.isEmpty (p34.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p34.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2DistIntertype = new Double (p34.getText ()).doubleValue ();
            if (vParamOPT.Type2DistIntertype<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistPine
            if (Check.isEmpty (p35.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p35.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type2DistPine = new Double (p35.getText ()).doubleValue ();
            if (vParamOPT.Type2DistPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         }
         
         // Type 3
         else if (Type3.isSelected())
        {	vParamOPT.type=3;
         //	NbAgPine
            if (Check.isEmpty (p42.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p42.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type3NbAgPine = new Integer (p42.getText ()).intValue ();
            if (vParamOPT.Type3NbAgPine<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // RPine
            if (Check.isEmpty (p43.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p43.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type3RPine = new Double (p43.getText ()).doubleValue ();
            if (vParamOPT.Type3RPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistIntertype
            if (Check.isEmpty (p44.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p44.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type3DistIntertype = new Double (p44.getText ()).doubleValue ();
            if (vParamOPT.Type3DistIntertype<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistPine
            if (Check.isEmpty (p45.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p45.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type3DistPine = new Double (p45.getText ()).doubleValue ();
            if (vParamOPT.Type3DistPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
				
				// Proba
            if (Check.isEmpty (p46.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p46IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p46.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p46IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type3Proba = new Double (p46.getText ()).doubleValue ();
            if ((vParamOPT.Type3Proba<0)||(vParamOPT.Type3Proba>1)) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p46IsNot01"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }       

         }
         
         // Type 4
         else if (Type4.isSelected())
        {	vParamOPT.type=4;
         //	NbAgPine
            if (Check.isEmpty (p52.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p52.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type4NbAgPine = new Integer (p52.getText ()).intValue ();
            if (vParamOPT.Type4NbAgPine<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p22IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // RPine
            if (Check.isEmpty (p53.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p53.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type4RPine = new Double (p53.getText ()).doubleValue ();
            if (vParamOPT.Type4RPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p23IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistIntertype
            if (Check.isEmpty (p54.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p54.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type4DistIntertype = new Double (p54.getText ()).doubleValue ();
            if (vParamOPT.Type4DistIntertype<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistPine
            if (Check.isEmpty (p55.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p55.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.Type4DistPine = new Double (p55.getText ()).doubleValue ();
            if (vParamOPT.Type4DistPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTCanopy.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
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
      
      
      // border.
         JPanel CanopyPanel = new JPanel (new FlowLayout (FlowLayout.LEFT));
         Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandOPTCanopy.title"));
         CanopyPanel.setBorder (bor);
      
         Box box = Box.createVerticalBox ();
         CheckboxGroup cbg = new CheckboxGroup();
      	      
      
      // 1. intro.
          JPanel p2 =  new ColumnPanel ();
        JPanel li0 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li0.add (new JWidthLabel (Translator.swap ("DVirtualStandOPTCanopy.Intro")+" : ", 20));
         p2.add(li0);
      
       
      //2. types
         //a. radio button different types
         Type1 = new JRadioButton (Translator.swap ("DVirtualStandOPTCanopy.type1"));
         Type2 = new JRadioButton (Translator.swap ("DVirtualStandOPTCanopy.type2"));
         Type3 = new JRadioButton (Translator.swap ("DVirtualStandOPTCanopy.type3"));
         Type4 = new JRadioButton (Translator.swap ("DVirtualStandOPTCanopy.type4"));
         TypeChoice = new ButtonGroup ();
         TypeChoice.add (Type1);
         TypeChoice.add (Type2);
         TypeChoice.add (Type3);
         TypeChoice.add (Type4);
         Type1.setSelected (true);
      
      // b. first line : type1
         LinePanel l1 = new LinePanel ();
         l1.add(Type1);
         l1.addGlue ();
         p2.add(l1);
         JPanel l1b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l1b.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.NbAgOak"), 10));
         p20 = new JTextField (5);
         p20.setText(""+vParamOPT.Type1NbAgOak);
         l1b.add (p20);
         l1b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.ROak"), 10));
         p21 = new JTextField (5);
         p21.setText(""+vParamOPT.Type1ROak);
         l1b.add (p21);
         l1b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.NbAgPine"), 10));
         p22 = new JTextField (5);
         p22.setText(""+vParamOPT.Type1NbAgPine);
         l1b.add (p22);
         l1b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.RPine"), 10));
         p23 = new JTextField (5);
         p23.setText(""+vParamOPT.Type1RPine);
         l1b.add (p23);
         p2.add(l1b);
         JPanel l1d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l1d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.DistIntertype"), 10));
         p24 = new JTextField (5);
         p24.setText(""+vParamOPT.Type1DistIntertype);
         l1d.add (p24);
         l1d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.DistPine"), 10));
         p25 = new JTextField (5);
         p25.setText(""+vParamOPT.Type1DistPine);
         l1d.add (p25);
         p2.add(l1d);
      
      				
      // c. second line : type2
         LinePanel l2 = new LinePanel ();
         l2.add(Type2);
         l2.addGlue ();
         p2.add(l2);
         JPanel l2b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l2b.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.NbAgOak"), 10));
         p30 = new JTextField (5);
         p30.setText(""+vParamOPT.Type2NbAgOak);
         l2b.add (p30);
         l2b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.ROak"), 10));
         p31 = new JTextField (5);
         p31.setText(""+vParamOPT.Type2ROak);
         l2b.add (p31);
         l2b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.NbAgPine"), 10));
         p32 = new JTextField (5);
         p32.setText(""+vParamOPT.Type2NbAgPine);
         l2b.add (p32);
         l2b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.RPine"), 10));
         p33 = new JTextField (5);
         p33.setText(""+vParamOPT.Type2RPine);
         l2b.add (p33);
         p2.add(l2b);
         JPanel l2d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l2d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.DistIntertype"), 10));
         p34 = new JTextField (5);
         p34.setText(""+vParamOPT.Type2DistIntertype);
         l2d.add (p34);
         l2d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.DistPine"), 10));
         p35 = new JTextField (5);
         p35.setText(""+vParamOPT.Type2DistPine);
         l2d.add (p35);
         p2.add(l2d);
      
      // d. third line : type3
         LinePanel l3 = new LinePanel ();
         l3.add(Type3);
         l3.addGlue ();
         p2.add(l3);
          JPanel l3b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l3b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.NbAgPine"), 10));
         p42 = new JTextField (5);
         p42.setText(""+vParamOPT.Type3NbAgPine);
         l3b.add (p42);
         l3b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.RPine"), 10));
         p43 = new JTextField (5);
         p43.setText(""+vParamOPT.Type3RPine);
         l3b.add (p43);
         l3b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.DistPine"), 10));
         p45 = new JTextField (5);
         p45.setText(""+vParamOPT.Type3DistPine);
         l3b.add (p45);
         p2.add(l3b);
         JPanel l3d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l3d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.DistIntertype"), 10));
         p44 = new JTextField (5);
         p44.setText(""+vParamOPT.Type3DistIntertype);
         l3d.add (p44);
         l3d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.Proba"), 10));
         p46 = new JTextField (5);
         p46.setText(""+vParamOPT.Type3Proba);
         l3d.add (p46);
         p2.add(l3d);
     
      // e. fourth line : type4
         LinePanel l4 = new LinePanel ();
         l4.add(Type4);
         l4.addGlue ();
         p2.add(l4);
           JPanel l4b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l4b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.NbAgPine"), 10));
         p52 = new JTextField (5);
         p52.setText(""+vParamOPT.Type4NbAgPine);
         l4b.add (p52);
         l4b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.RPine"), 10));
         p53 = new JTextField (5);
         p53.setText(""+vParamOPT.Type4RPine);
         l4b.add (p53);
         l4b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTCanopy.DistPine"), 10));
         p55 = new JTextField (5);
         p55.setText(""+vParamOPT.Type4DistPine);
         l4b.add (p55);
         p2.add(l4b);
         JPanel l4d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l4d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTCanopy.DistIntertype"), 10));
         p54 = new JTextField (5);
         p54.setText(""+vParamOPT.Type4DistIntertype);
         l4d.add (p54);
         p2.add(l4d);

         CanopyPanel.add (p2);
         part1.add (CanopyPanel);

      // conclusion
         JPanel li9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li9.add (new JWidthLabel (Translator.swap ("DVirtualStandOPTCanopy.Conclu")+" : ", 20));
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



