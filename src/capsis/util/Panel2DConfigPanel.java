/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.util;

import java.awt.BorderLayout;

import javax.swing.JCheckBox;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A Configuration Panel for Panel2D objects. 
 * 
 * @author F. de Coligny - may 2002
 */
public class Panel2DConfigPanel extends ConfigurationPanel implements Controlable {
	
	private Panel2D panel2D;
	private JTextField pencilSize;
	private JCheckBox antiAliased;
	private JTextField selectionSquareSize;
	
	public Panel2DConfigPanel (Configurable c) {
		super (c);
		panel2D = (Panel2D) c;
		createUI ();
	}
	
	/**
	 * From Controlable interface.
	 */
	public boolean isControlSuccessful () {return checksAreOk ();}
	
	public boolean checksAreOk () {
		
		// Check that pencilSize contains a decimal value
		if (!Check.isDouble (pencilSize.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("Panel2DConfigPanel.pencilSizeMustBeDecimal"));
			return false;
		}
		
		if (!Check.isInt (selectionSquareSize.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("Panel2DConfigPanel.selectionSquareSizeMustBeInteger"));
			return false;
		}
		
		panel2D.getSettings ().setPencilSize ((float) Check.doubleValue (pencilSize.getText ().trim ()));
		panel2D.getSettings ().setAntiAliased (antiAliased.isSelected ());
		panel2D.getSettings ().setSelectionSquareSize (Check.intValue (selectionSquareSize.getText ().trim ()));
		
		return true;
	}
	
	private void createUI () {
		ColumnPanel main = new ColumnPanel ();
		setLayout (new BorderLayout ());
		add (main, BorderLayout.NORTH);
		//~ setAlignmentX (Component.LEFT_ALIGNMENT);
		//~ setLayout (new BoxLayout (this, BoxLayout.Y_AXIS));
		
		// 1. pencilSize
		LinePanel l1 = new LinePanel ();
		pencilSize = new JTextField (5);
		pencilSize.setText (""+panel2D.getSettings ().getPencilSize ());
		l1.add (new JWidthLabel (Translator.swap ("Panel2DConfigPanel.pencilSize")+" : ", 50));
		l1.add (pencilSize);
		l1.addStrut0 ();
		main.add (l1);
		
		// 2. size of the selection square on left double click [PhD request]
		LinePanel l3 = new LinePanel ();
		selectionSquareSize = new JTextField (5);
		selectionSquareSize.setText (""+panel2D.getSettings ().getSelectionSquareSize ());
		l3.add (new JWidthLabel (Translator.swap ("Panel2DConfigPanel.selectionSquareSize")+" : ", 50));
		l3.add (selectionSquareSize);
		l3.addStrut0 ();
		main.add (l3);
		
		// 3. antiAliased
		LinePanel l2 = new LinePanel ();
		antiAliased = new JCheckBox (Translator.swap ("Panel2DConfigPanel.antiAliased"));
		antiAliased.setSelected (panel2D.getSettings ().isAntiAliased ());
		l2.add (antiAliased);
		l2.addGlue ();
		main.add (l2);
		
		
		// 4. user guide
		ColumnPanel c1 = new ColumnPanel (Translator.swap ("Panel2D.userGuide"));
		main.add (c1);
		
		JTextArea area = new JTextArea ();
		area.append (Translator.swap ("Panel2D.userGuide1"));
		area.append ("\n");
		area.append (Translator.swap ("Panel2D.userGuide2"));
		area.append ("\n");
		area.append (Translator.swap ("Panel2D.userGuide3"));
		area.append ("\n");
		area.append (Translator.swap ("Panel2D.userGuide4"));
		area.append ("\n");
		area.append (Translator.swap ("Panel2D.userGuide5"));
		area.append ("\n");
		area.setLineWrap (true);
		area.setWrapStyleWord (true);
		c1.add (new JScrollPane (area));
		
		//~ LinePanel l10 = new LinePanel ();
		//~ l10.add (new JLabel (Translator.swap ("Panel2D.userGuide1")));
		//~ l10.addGlue ();
		//~ c1.add (l10);
		
		//~ LinePanel l11 = new LinePanel ();
		//~ l11.add (new JLabel (Translator.swap ("Panel2D.userGuide2")));
		//~ l11.addGlue ();
		//~ c1.add (l11);
		
		//~ LinePanel l12 = new LinePanel ();
		//~ l12.add (new JLabel (Translator.swap ("Panel2D.userGuide3")));
		//~ l12.addGlue ();
		//~ c1.add (l12);
		
		//~ LinePanel l13 = new LinePanel ();
		//~ l13.add (new JLabel (Translator.swap ("Panel2D.userGuide4")));
		//~ l13.addGlue ();
		//~ c1.add (l13);
		
		//~ LinePanel l14 = new LinePanel ();
		//~ l14.add (new JLabel (Translator.swap ("Panel2D.userGuide5")));
		//~ l14.addGlue ();
		//~ c1.add (l14);
		
		
		
		// all JComponents in BoxLayout must have max value maximum size to allow their X extension
		//~ setMaximumSize (new Dimension (Integer.MAX_VALUE, Integer.MAX_VALUE));
	}
	
}	// end of Panel2DConfigPanel
	



