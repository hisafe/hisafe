/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.spatializer;

import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.border.Border;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.InstantPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**	IdentitySpatializerPanel: a configuration panel
*	for IdentitySpatializer
*
*	@author F. de Coligny - july 2006
*/
public class IdentitySpatializerPanel extends InstantPanel {

	private IdentitySpatializerStarter s;


	/**	Constructor.
	*/
	public IdentitySpatializerPanel (IdentitySpatializer subject) {
		super (subject);
		s = (IdentitySpatializerStarter)
				((IdentitySpatializer) subject).getStarter ();
		createUI ();
	}

	/**	ConfigPanel interface.
	*/
	public boolean isCorrect () {
		// 1. controls
		//~ if (!Check.isDouble (x0Border.getText ().trim ())) {
			//~ MessageDialog.promptError (Translator.swap ("IdentitySpatializerPanel.x0BorderShouldBeADouble"));
			//~ return false ;
		//~ }

		// 2. all controls ok, report new configuration
		//~ s.x0Border = Check.intValue (x0Border.getText ().trim ());

		return true;
	}

	/**	Called when something changes in config
	*	(ex: a check box was changed...)
	*	It will notify the Drawer listener.
	*/
	//~ public void actionPerformed (ActionEvent e) {
		//~ super.actionPerformed (e);
	//~ }

	/**	Synchronize the radio buttons / check box
	*/
	//~ private void synchronizeOptions () {
		//~ interX.setEnabled (interXEnabled.isSelected ());
		//~ interY.setEnabled (interYEnabled.isSelected ());
	//~ }

	/**	Initializes the GUI.
	*/
	private void createUI () {
		ColumnPanel part1 = new ColumnPanel (0, 0);
		Border etched = BorderFactory.createEtchedBorder ();
		/*Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("IdentitySpatializer"));
		part1.setBorder (b);*/
		
		// label
		//~ ColumnPanel p1 = new ColumnPanel (0, 0);
		//~ Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("IdentitySpatializer.borders"));
		//~ p1.setBorder (b);

		LinePanel l2 = new LinePanel ();
		String s = Translator.swap ("IdentitySpatializer.configPanelExplanation");
		JTextArea text = new JTextArea (s);
		text.setLineWrap (true);
		text.setWrapStyleWord (true);
		text.setEditable (false);
		JScrollPane scrollPane = new JScrollPane (text);
		scrollPane.setPreferredSize (new Dimension (100, 150));
		l2.add (scrollPane);
		l2.addStrut0 ();

		part1.add (l2);


		setLayout (new BorderLayout ());
		add (part1, BorderLayout.NORTH);

		//~ synchronizeOptions ();

	}

}



