/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.rockfornet;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Locale;
import java.util.Map;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.extension.DialogModelTool;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.methodprovider.RockfornetStand;

/**
 * Rockfornet simulator
 *
 * @author Eric Mermin, Eric Maldonado - November 2006
 */
public class RockfornetDialog extends DialogModelTool implements ActionListener {

	static {
		Translator.addBundle("capsis.extension.modeltool.rockfornet.RockfornetDialog");
	}

	// nb-07.08.2018
	//static public final String AUTHOR="E. Mermin, E. Maldonado";
	//static public final String VERSION="1.0";

	private NumberFormat nf;
	
	private Step step;
	
	private RockfornetStand stand;
	//private SamsaStand;

	private JComboBox rockType;
	private JComboBox rockShape;
	private Map rockTypeMap;
	private Map rockShapeMap;
		
	private Vector rockTypeVector;
	private Vector rockShapeVector;
	private Hashtable rockTypeDensityHashtable;
	
	private JTextField slope;
	private JTextField lengthNonForestedSlope;
	private JTextField lengthForestedSlope;
	private JTextField heightCliff;

	private JTextField rockDiameter1,rockDiameter2,rockDiameter3;
	private JButton calculate;

	
	private JButton close;	// after confirmation
	private JButton help;

	private JLabel probableResidualRockfallHazard;
	private JLabel targetStand;
	private JLabel standDensity;
	private JLabel meanStemDiameter;
		
	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public RockfornetDialog () {		
		super();
	}
	
	@Override
	public void init(GModel m, Step s){

		try {
			// fc - 5.7.2007 - to format the String representations of this result
			nf = NumberFormat.getInstance (Locale.ENGLISH);
			nf.setGroupingUsed (false);
			nf.setMaximumFractionDigits (3);
			
			
			step = s;
			
			stand = (RockfornetStand) step.getScene ();

			setTitle (getName ()+" - "+step.getCaption ());

			init ();
			createUI ();

			setSize (500, 600);
			//pack ();	// sets the size
			setVisible (true);
			setModal (false);
			
		} catch (Exception exc) {
			Log.println (Log.ERROR, "RockfornetDialog.c ()", exc.toString (), exc);
		}

	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene std = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(std instanceof RockfornetStand)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "RockfornetDialog.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("RockfornetDialog.name");
	}

	@Override
	public String getAuthor() {
		return "E. Mermin, E. Maldonado";
	}

	@Override
	public String getDescription() {
		return Translator.swap("RockfornetDialog.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	private void init () {
		rockTypeMap = new HashMap ();
		rockTypeMap.put (Translator.swap ("RockfornetDialog.granite"), 0);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.basalte"), 1);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.amphibolite"), 2);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.diorite"), 3);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.dolomite"), 4);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.gneiss"), 5);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.gypsum"), 6);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.limestone"), 7);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.marlstone"), 8);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.micaschist"), 9);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.sandstone"), 10);
		rockTypeMap.put (Translator.swap ("RockfornetDialog.shale"), 11);
		
		
		rockTypeVector = new Vector (rockTypeMap.keySet ());
		
		rockTypeDensityHashtable= new Hashtable();
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.granite"),2800);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.basalte"),2900);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.amphibolite"),3000);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.diorite"),2850);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.dolomite"),2700);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.gneiss"),2800);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.gypsum"),2300);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.limestone"),2500);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.marlstone"),2400);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.micaschist"),2700);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.sandstone"),2500);
		rockTypeDensityHashtable.put(Translator.swap ("RockfornetDialog.shale"),2450);

		
		rockShapeMap = new HashMap ();
		rockShapeMap.put (Translator.swap ("RockfornetDialog.sphere"), RockfornetSettings.SPHERE);
		rockShapeMap.put (Translator.swap ("RockfornetDialog.disc"), RockfornetSettings.DISC);
		rockShapeMap.put (Translator.swap ("RockfornetDialog.ellipsoid"), RockfornetSettings.ELLIPSOID);
		rockShapeMap.put (Translator.swap ("RockfornetDialog.rectangular"), RockfornetSettings.RECTANGULAR);
		
		rockShapeVector = new Vector (rockShapeMap.keySet ());
		
		
	}
	
	private void calculateAction () {
		// checks
		if (!Check.isDouble (slope.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RockfornetDialog.slopeMustBeADouble"));
			return;
		}
		
		
		if (!Check.isDouble (rockDiameter1.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RockfornetDialog.rockDiameterMustBeADouble"));
			return;
		}
		
		if (!Check.isDouble (rockDiameter2.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RockfornetDialog.rockDiameterMustBeADouble"));
			return;
		}
		
		if (!Check.isDouble (rockDiameter3.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RockfornetDialog.rockDiameterMustBeADouble"));
			return;
		}
		
		
		// create the RockfornetSettings
		System.out.println("Stand1");
		RockfornetSettings s = new RockfornetSettings (stand);
		//s.stand = this.stand;
		
		String key = (String) rockType.getSelectedItem ();
			System.out.println("s.rocktype = " + key);
	
		//s.rockType = (Integer) rockTypeMap.get (key);
		s.rockType =(Integer)rockTypeDensityHashtable.get(key); 
		System.out.println("s.rocktype = " + s.rockType );
		
		//~ s.rockShape = (String)rockShape.getSelectedItem();	// fc - 5.7.2007 - replaced by following
		String rockShapeName = (String) rockShape.getSelectedItem ();
		s.rockShape = (Integer) rockShapeMap.get (rockShapeName);
		
		s.slope = Check.doubleValue (slope.getText ().trim ());
		s.lengthNonForestedSlope= Check.doubleValue (lengthNonForestedSlope.getText().trim());
		s.lengthForestedSlope=Check.doubleValue (lengthForestedSlope.getText().trim());
		s.heightCliff= Check.doubleValue (heightCliff.getText().trim());

		s.rockDiameter1 = Check.doubleValue(rockDiameter1.getText().trim());
		s.rockDiameter2 = Check.doubleValue(rockDiameter2.getText().trim());
		s.rockDiameter3 = Check.doubleValue(rockDiameter3.getText().trim());

		// Launch calculation
		Rockfornet r = new Rockfornet (s);
		r.execute ();
		
		RockfornetResult result = r.getResult ();
		
		//~ probableResidualRockfallHazard.setText(Translator.swap ("RockfornetDialog.probableResidualRockfallHazard")+ " "+result.probableResidualRockfallHazard);
		//~ standDensity.setText(Translator.swap ("RockfornetDialog.standDensity") + " " +result.standDensity);
		//~ meanStemDiameter.setText(Translator.swap ("RockfornetDialog.meanStemDiameter") + " "+result.meanStemDiameter);
		
		// fc - 5.7.2007 - replaced the 3 lines above by 3 below
		probableResidualRockfallHazard.setText (
				Translator.swap ("RockfornetDialog.probableResidualRockfallHazard")
				+" "
				+nf.format (result.getProbableResidualRockfallHazard ()));
		
		standDensity.setText (Translator.swap ("RockfornetDialog.standDensity")
				+" " 
				+nf.format (result.getStandDensityMin ())
				+" - "
				+nf.format (result.getStandDensityMax ()));
		
		meanStemDiameter.setText (Translator.swap ("RockfornetDialog.meanStemDiameter")
				+" "
				+nf.format (result.getMeanStemDiameterMin ())
				+" - "
				+nf.format (result.getMeanStemDiameterMax ()));

		System.out.println ("RockfornetDialog result="+result);
	}

	/**	From ActionListener interface.
	*	Buttons management.
	*/
	public void actionPerformed (ActionEvent evt) {
		// update the object viewer
		System.out.println("actionPerformed");
		if (evt.getSource ().equals (calculate)) {
			calculateAction ();

		} else if (evt.getSource ().equals (close)) {
			if (Question.ask (MainFrame.getInstance (),
					Translator.swap ("RockfornetDialog.confirm"), Translator.swap ("RockfornetDialog.confirmClose"))) {
				dispose ();
			}

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**	Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	*/
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("RockfornetDialog.confirm"), Translator.swap ("RockfornetDialog.confirmClose"))) {
			dispose ();
		}
	}

	
	
	/**	User interface definition
	*/
	private void createUI () {

		// Main panel
		ColumnPanel mainPanel = new ColumnPanel ();
		
		LinePanel l0 = new LinePanel ();
		l0.add (new JWidthLabel (Translator.swap ("RockfornetDialog.rockCharacteristics")+" : ", 150));
		l0.addStrut0 ();
		mainPanel.add (l0);
		
		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel (Translator.swap ("RockfornetDialog.rockDiameters")+" : ", 150));
		
		rockDiameter1 = new JTextField ();
		rockDiameter2 = new JTextField ();
		rockDiameter3 = new JTextField ();
		l2.add (rockDiameter1);
		l2.add (rockDiameter2);
		l2.add (rockDiameter3);
		l2.addStrut0 ();
		mainPanel.add (l2);
		
		
		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel (Translator.swap ("RockfornetDialog.rockType")+" : ", 150));
		rockType = new JComboBox (rockTypeVector);
		l3.add (rockType);
		l3.addStrut0 ();
		mainPanel.add (l3);
	
  
		LinePanel l4 = new LinePanel ();
		l4.add (new JWidthLabel (Translator.swap ("RockfornetDialog.rockShape")+" : ", 150));
		rockShape = new JComboBox (rockShapeVector);
		l4.add (rockShape);
		l4.addStrut0 ();
		mainPanel.add (l4);
		
		
		LinePanel l5 = new LinePanel ();
		l5.add (new JWidthLabel (Translator.swap ("RockfornetDialog.slopeCharacteristics")+" : ", 150));
		l5.addStrut0 ();
		mainPanel.add (l5);
		
		
		LinePanel l6 = new LinePanel ();
		l6.add (new JWidthLabel (Translator.swap ("RockfornetDialog.slope")+" : ", 150));
		slope = new JTextField ();
		l6.add (slope);
		l6.addStrut0 ();
		mainPanel.add (l6);
		
		LinePanel l7 = new LinePanel ();
		l7.add (new JWidthLabel (Translator.swap ("RockfornetDialog.heightCliff")+" : ", 150));
		heightCliff = new JTextField ();
		l7.add (heightCliff);
		l7.addStrut0 ();
		mainPanel.add (l7);
		
		LinePanel l8 = new LinePanel ();
		l8.add (new JWidthLabel (Translator.swap ("RockfornetDialog.lengthForestedSlope")+" : ", 150));
		lengthForestedSlope= new JTextField ();
		l8.add (lengthForestedSlope);
		l8.addStrut0 ();
		mainPanel.add (l8);
		
		LinePanel l9 = new LinePanel ();
		l9.add (new JWidthLabel (Translator.swap ("RockfornetDialog.lengthNonForestedSlope")+" : ", 150));
		lengthNonForestedSlope= new JTextField ();
		l9.add (lengthNonForestedSlope);
		l9.addStrut0 ();
		mainPanel.add (l9);
		
		
		LinePanel l25 = new LinePanel ();
		calculate = new JButton (Translator.swap ("RockfornetDialog.calculate"));
		calculate.addActionListener (this);
		l25.add (calculate);
		l25.addStrut0 ();
		mainPanel.add (l25);
		
		LinePanel l26 = new LinePanel ();
		probableResidualRockfallHazard= new JLabel(Translator.swap ("RockfornetDialog.probableResidualRockfallHazard"));
		l26.add (probableResidualRockfallHazard);
		l26.addStrut0 ();
		mainPanel.add (l26);
		
		LinePanel l27 = new LinePanel ();
		targetStand= new JLabel(Translator.swap ("RockfornetDialog.targetStand"));
		l27.add (targetStand);
		l27.addStrut0 ();
		mainPanel.add (l27);
		
		LinePanel l28 = new LinePanel ();
		standDensity= new JLabel(Translator.swap ("RockfornetDialog.standDensity"));
		l28.add (standDensity);
		l28.addStrut0 ();
		mainPanel.add (l28);
		
		LinePanel l29 = new LinePanel ();
		meanStemDiameter= new JLabel(Translator.swap ("RockfornetDialog.meanStemDiameter"));
		l29.add (meanStemDiameter);
		l29.addStrut0 ();
		mainPanel.add (l29);
		
		
		
		// Control panel at the bottom: Close / Help
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (close);
		pControl.add (help);

	
		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);


		getContentPane ().add (mainPanel, BorderLayout.NORTH);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}


