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

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.kernel.Engine;
import capsis.kernel.IdCard;
import capsis.kernel.Step;

/**
 * DStepProperties gives the properties on one given step.
 * 
 * @author F. de Coligny - May 2002
 */
public class DStepProperties extends AmapDialog implements ActionListener {
	
	protected Engine engine;	// used several times
	protected JButton ok;

	private Step step;

	
	/**
	 * Constructor.
	 */
	public DStepProperties (Step step) {
		super ();
		this.step = step;	
		
		engine = Engine.getInstance ();
		createUI ();
		
		// location was set by AmapDialog
		
		// fc-3.1.2017
		setSize (new Dimension (600, 300));
//		pack ();
		
		setVisible (true);
		
	}

	private void okAction () {
		dispose ();
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		}
	}

	/** 
	 * Initialize the dialog's GUI. 
	 */
	protected void createUI () {
		
		ColumnPanel part1 = new ColumnPanel ();
		
		// Step name
		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel (Translator.swap ("DStepProperties.stepName")+" : ", 80));
		//~ l1.add (new JTextField (step.getName (), 15));
		l1.add (new JTextField (step.getCaption (), 15));
		l1.addStrut0 ();
		part1.add (l1);
		
		// Scenario name
		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel (Translator.swap ("DStepProperties.scenarioName")+" : ", 80));
		l2.add (new JTextField (step.getProject ().getName (), 15));
		l2.addStrut0 ();
		part1.add (l2);
		
		// Model properties
		LinePanel l3 = new LinePanel ();
		JWidthLabel lab = new JWidthLabel (Translator.swap ("DStepProperties.modelProperties")+" :", 80);
		ColumnPanel component1 = new ColumnPanel ();
		component1.add (lab);
		component1.addGlue ();
		
		JTextArea area = new JTextArea (getFormatedString (step.getProject ().getModel ().getIdCard ()));
		area.setLineWrap (true);
		area.setWrapStyleWord (true);
		area.setEditable (false);
		//~ area.setHighlighter (null);		// fc - 14.3.2008 - for S. Turbis
		JScrollPane panModel = new JScrollPane (area);
		area.setCaretPosition (0);
		panModel.setPreferredSize (new Dimension (10, 100));
		
		JPanel centered = new JPanel (new BorderLayout ());
		centered.add (panModel, BorderLayout.CENTER);
		
		l3.add (component1);
		l3.add (centered);
		l3.addStrut0 ();
		part1.add (l3);
		
		// Step creation reason
		LinePanel l4 = new LinePanel ();
		JWidthLabel lab2 = new JWidthLabel (Translator.swap ("DStepProperties.stepReason")+" : ", 80);
		ColumnPanel component2 = new ColumnPanel ();
		component2.add (lab2);
		component2.addGlue ();
		
		// Let's format a little : discard words beginning by "class="
		String formatedReason = "";
		for (StringTokenizer st = new StringTokenizer (step.getReason (), " []", true); st.hasMoreTokens ();) {
			String s = st.nextToken ();
			if (!s.startsWith ("class=")) {
				formatedReason += s;
			}
		}
		formatedReason = formatedReason.trim ();		
		
		JTextArea area2 = new JTextArea (formatedReason);
		area2.setLineWrap (true);
		area2.setWrapStyleWord (true);
		area2.setEditable (false);
		//~ area2.setHighlighter (null);		// fc - 14.3.2008 - for S. Turbis
		JScrollPane panModel2 = new JScrollPane (area2);
		area2.setCaretPosition (0);
		panModel2.setPreferredSize (new Dimension (10, 100));
		
		JPanel centered2 = new JPanel (new BorderLayout ());
		centered2.add (panModel2, BorderLayout.CENTER);
		
		l4.add (component2);
		l4.add (centered2);
		l4.addStrut0 ();
		
		ColumnPanel part2 = new ColumnPanel ();
		part2.add (l4);
		part2.addGlue ();
		
		// 5. control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		ok = new JButton (Translator.swap ("Shared.ok"));
		pControl.add (ok);
		ok.addActionListener (this);
		
		// Set ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);
		
		// Sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);
		
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (part2, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setTitle (Translator.swap ("DStepProperties.stepProperties"));
		
		setModal (true);
	}

	public String getFormatedString (IdCard idCard) {
		StringBuffer b = new StringBuffer ();
		b.append (Translator.swap ("IdCard.model"));
		b.append (": ");
		b.append (idCard.getModelName());
		b.append ("\n");
		b.append (Translator.swap ("IdCard.version"));
		b.append (": ");
		b.append (idCard.getModelVersion ());
		b.append ("\n");
		b.append (Translator.swap ("IdCard.type"));
		b.append (": ");
		b.append (idCard.getModelType ());
		b.append ("\n");
		b.append (Translator.swap ("IdCard.author"));
		b.append (": ");
		b.append (idCard.getModelAuthor ());
		b.append ("\n");
		b.append (Translator.swap ("IdCard.institute"));
		b.append (": ");
		b.append (idCard.getModelInstitute ());
		return b.toString ();
	}

	
}



