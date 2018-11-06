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
package safe.extension.standviewer.safe;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.border.Border;

import jeeb.lib.util.ColumnPanel;
/**
 * GenericOptionPanel - A generic component to choose display options
 *
 * @author N.Marchais - September 2003
 */
public class GenericOptionPanel extends ColumnPanel {
// review - fc - 11.3.2004

	protected ButtonGroup rdGroup;
	protected JRadioButton [] radButton;
	protected JPanel panel;
	protected SVSafe svSafe;
	protected SVSafeSettings settings;


	/**	Constructor.
	*/
	public GenericOptionPanel (SVSafeSettings sets, SVSafe svSafe, String title) {
		super ();	// fc
		settings = sets;
		this.svSafe = svSafe;

		// fc - panel = new JPanel ();
		//setLayout (new BoxLayout (this, BoxLayout.Y_AXIS));
		Border etched = BorderFactory.createEtchedBorder ();
		Border lightBorder = BorderFactory.createTitledBorder (etched, title);
		setBorder (lightBorder);

		panel = this;	// fc - further panel.add will be redirected in this.add

		// fc - add (panel,BorderLayout.CENTER);
		//setVisible(true);		// never on a JPanel
	}
}



