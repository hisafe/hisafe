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
 * DVirtualStandOPTUnder - Dialog box to set the parameters concerning 
 * the simulation of the Understorey of Oak Pine mixed stands from typology.
 * Parameters will be stored in the VirtualParameterOakPineTypo object.
 *
 * @author F Goreaud - 12/06/07
 */
    public class DVirtualStandOPTUnder extends AmapDialog implements ActionListener {
   
      static {
         Translator.addBundle("capsis.lib.spatial.SpatialLabels");
      } 
   
      private VirtualParametersOakPineTypo vParamOPT;
   
      private Checkbox cb1;
      private Checkbox cb2;
      private Checkbox cb3;
   
      private JTextField p20;
      private JTextField p21;
      private JTextField p24;
      private JTextField p25;
      private JTextField p30;
      private JTextField p31;
      private JTextField p34;
      private JTextField p35;
      private JTextField p40;
      private JTextField p41;
      private JTextField p44;
      private JTextField p45;
   
      private JButton ok;
      private JButton cancel;
      private JButton help;
   	
      private JRadioButton Type1;
      private JRadioButton Type2;
      private JRadioButton Type3;
      private ButtonGroup TypeChoice;
   
   /**
    * Constructor.
    */
       public DVirtualStandOPTUnder (VirtualParametersOakPineTypo vpOPT) {
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
         {	vParamOPT.typeUnder=1;
         //	NbAgOak
            if (Check.isEmpty (p20.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p20.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType1NbAgOak = new Integer (p20.getText ()).intValue ();
            if (vParamOPT.UnderType1NbAgOak<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // ROak
            if (Check.isEmpty (p21.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p21.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType1ROak = new Double (p21.getText ()).doubleValue ();
            if (vParamOPT.UnderType1ROak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
              
         // DistRepOak
            if (Check.isEmpty (p24.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p24.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType1DistRepOak = new Double (p24.getText ()).doubleValue ();
            if (vParamOPT.UnderType1DistRepOak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistRepPine
            if (Check.isEmpty (p25.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p25.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType1DistRepPine = new Double (p25.getText ()).doubleValue ();
            if (vParamOPT.UnderType1DistRepPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         }
         
         // Type 2
         else if (Type2.isSelected())
        {	vParamOPT.typeUnder=2;
         //	NbAgOak
            if (Check.isEmpty (p30.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p30.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType2NbAgOak = new Integer (p30.getText ()).intValue ();
            if (vParamOPT.UnderType2NbAgOak<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // ROak
            if (Check.isEmpty (p31.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p31.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType2ROak = new Double (p31.getText ()).doubleValue ();
            if (vParamOPT.UnderType2ROak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
             
         // DistAttOak
            if (Check.isEmpty (p34.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p34IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p34.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p34IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType2DistAttOak = new Double (p34.getText ()).doubleValue ();
            if (vParamOPT.UnderType2DistAttOak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p34IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistRepPine
            if (Check.isEmpty (p35.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p35.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType2DistRepPine = new Double (p35.getText ()).doubleValue ();
            if (vParamOPT.UnderType2DistRepPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p25IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         }
         
         // Type 3
         else if (Type3.isSelected())
        {	vParamOPT.typeUnder=3;
          //	NbAgOak
            if (Check.isEmpty (p40.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isInt (p40.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotInt"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType3NbAgOak = new Integer (p40.getText ()).intValue ();
            if (vParamOPT.UnderType3NbAgOak<1) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p20IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // ROak
            if (Check.isEmpty (p41.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p41.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType3ROak = new Double (p41.getText ()).doubleValue ();
            if (vParamOPT.UnderType3ROak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p21IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
             
         // DistRepOak
            if (Check.isEmpty (p44.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p44.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType3DistRepOak = new Double (p44.getText ()).doubleValue ();
            if (vParamOPT.UnderType3DistRepOak<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p24IsNotPositive"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
         
         // DistRepPine
            if (Check.isEmpty (p45.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p45IsEmpty"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            if (!Check.isDouble (p45.getText ())) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p45IsNotDouble"),
                  Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
               return;
            }
            vParamOPT.UnderType3DistAttPine = new Double (p45.getText ()).doubleValue ();
            if (vParamOPT.UnderType3DistAttPine<=0) {
               JOptionPane.showMessageDialog (this, Translator.swap ("DVirtualStandOPTUnder.p45IsNotPositive"),
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
         Border bor = BorderFactory.createTitledBorder (etched,  Translator.swap ("DVirtualStandOPTUnder.title"));
         CanopyPanel.setBorder (bor);
      
         Box box = Box.createVerticalBox ();
         CheckboxGroup cbg = new CheckboxGroup();
      	      
      
      // 1. intro.
          JPanel p2 =  new ColumnPanel ();
        JPanel li0 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li0.add (new JWidthLabel (Translator.swap ("DVirtualStandOPTUnder.Intro")+" : ", 20));
         p2.add(li0);
      
       
      //2. types
         //a. radio button different types
         Type1 = new JRadioButton (Translator.swap ("DVirtualStandOPTUnder.type1"));
         Type2 = new JRadioButton (Translator.swap ("DVirtualStandOPTUnder.type2"));
         Type3 = new JRadioButton (Translator.swap ("DVirtualStandOPTUnder.type3"));
         TypeChoice = new ButtonGroup ();
         TypeChoice.add (Type1);
         TypeChoice.add (Type2);
         TypeChoice.add (Type3);
         Type1.setSelected (true);
      
      // b. first line : type1
         LinePanel l1 = new LinePanel ();
         l1.add(Type1);
         l1.addGlue ();
         p2.add(l1);
         JPanel l1b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l1b.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTUnder.NbAgOak"), 10));
         p20 = new JTextField (5);
         p20.setText(""+vParamOPT.UnderType1NbAgOak);
         l1b.add (p20);
         l1b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.ROak"), 10));
         p21 = new JTextField (5);
         p21.setText(""+vParamOPT.UnderType1ROak);
         l1b.add (p21);
         p2.add(l1b);

         JPanel l1d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l1d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTUnder.DistRepOak"), 10));
         p24 = new JTextField (5);
         p24.setText(""+vParamOPT.UnderType1DistRepOak);
         l1d.add (p24);
         l1d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.DistRepPine"), 10));
         p25 = new JTextField (5);
         p25.setText(""+vParamOPT.UnderType1DistRepPine);
         l1d.add (p25);
         p2.add(l1d);
      
      				
      // c. second line : type2
         LinePanel l2 = new LinePanel ();
         l2.add(Type2);
         l2.addGlue ();
         p2.add(l2);
         JPanel l2b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l2b.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTUnder.NbAgOak"), 10));
         p30 = new JTextField (5);
         p30.setText(""+vParamOPT.UnderType2NbAgOak);
         l2b.add (p30);
         l2b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.ROak"), 10));
         p31 = new JTextField (5);
         p31.setText(""+vParamOPT.UnderType2ROak);
         l2b.add (p31);
         p2.add(l2b);
         JPanel l2d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l2d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTUnder.DistAttOak"), 10));
         p34 = new JTextField (5);
         p34.setText(""+vParamOPT.UnderType2DistAttOak);
         l2d.add (p34);
         l2d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.DistRepPine"), 10));
         p35 = new JTextField (5);
         p35.setText(""+vParamOPT.UnderType2DistRepPine);
         l2d.add (p35);
         p2.add(l2d);
      
      // d. third line : type3
         LinePanel l3 = new LinePanel ();
         l3.add(Type3);
         l3.addGlue ();
         p2.add(l3);
          JPanel l3b = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l3b.add (new JWidthLabel (Translator.swap ("DVirtualStandOPTUnder.NbAgOak"), 10));
         p40 = new JTextField (5);
         p40.setText(""+vParamOPT.UnderType3NbAgOak);
         l3b.add (p40);
         l3b.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.ROak"), 10));
         p41 = new JTextField (5);
         p41.setText(""+vParamOPT.UnderType3ROak);
         l3b.add (p41);
         p2.add(l3b);
         JPanel l3d = new JPanel (new FlowLayout (FlowLayout.LEFT));
         l3d.add (new JWidthLabel ( Translator.swap ("DVirtualStandOPTUnder.DistRepOak"), 10));
         p44 = new JTextField (5);
         p44.setText(""+vParamOPT.UnderType3DistRepOak);
         l3d.add (p44);
         l3d.add (new JWidthLabel (",    "+Translator.swap ("DVirtualStandOPTUnder.DistAttPine"), 10));
         p45 = new JTextField (5);
         p45.setText(""+vParamOPT.UnderType3DistAttPine);
         l3d.add (p45);
         p2.add(l3d);
     

         CanopyPanel.add (p2);
         part1.add (CanopyPanel);

      // conclusion
         JPanel li9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
         li9.add (new JWidthLabel (Translator.swap ("DVirtualStandOPTUnder.Conclu")+" : ", 20));
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



