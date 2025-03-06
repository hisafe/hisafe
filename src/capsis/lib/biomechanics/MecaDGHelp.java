/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.PathManager;

/**
 * MecaDGHelp - Interface for help dialog.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDGHelp extends AmapDialog implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003
// NOTE: could be replaced by Helper.helpFor (this) for each help button - fc - 3.2.2003

	/*static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDGHelp");
	}*/


	/**
	 * Constructor.
	 */
	public MecaDGHelp (AmapDialog parent, String title/*, String name*/) {
		super (parent);
		setTitle (title);

		createUI (/*name*/);
		// location is set by AmapDialog
		
		setModal (false);
		pack ();
		show ();
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {}

	/**
	 * User interface definition.
	 */
	private void createUI (/*String name*/) {
		JPanel p1 = new JPanel () {
			public void paintComponent (Graphics g) {
				super.paintComponent (g);
				String name = PathManager.getDir("etc") + File.separator + "windHelp.jpg";
				Image image = Toolkit.getDefaultToolkit ().getImage (name);
				g.drawImage (image, 0, 0, this);
			}
		};
		p1.setPreferredSize (new Dimension (696, 522));	// nice size indeed - fc ;-)
		getContentPane ().add (p1, BorderLayout.CENTER);
	}

}


