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

package capsis.extension.generictool;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;

import capsis.kernel.Engine;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.inspector2.AmapInspectorPanel;

/**
 * Inspector is a browser for data structure introspection.
 * 
 * @author F. de Coligny - June 2001
 */
public class Inspector extends AmapDialog implements ActionListener {

	static {
		Translator.addBundle ("capsis.extension.generictool.Inspector");
	}

	private JPanel pControl;
	private JButton close;

	/**
	 * Constructor.
	 */
	public Inspector (Window window) throws Exception {
		super (window);

		try {
			createUI ();

			setModal (false);

			activateSizeMemorization (getClass ().getName ());
			pack (); // sets the size
			setVisible (true);

		} catch (Exception e) {
			Log.println (Log.ERROR, "Inspector.c ()", "Error in constructor", e);
			throw e;
		}
	}

	/** Closes the viewer (no confirmation asked for). */
	public void close () {
		dispose ();
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			close ();
		}
	}

	/**
	 * Init the GUI.
	 */
	private void createUI () {
		setTitle (Translator.swap ("Inspector.name"));

		// fc-7.8.2018 changed to inspector2
//		AmapInspectorPanel p = new AmapInspectorPanel (Engine.getInstance ());
		
		JPanel p = AmapTools.createInspectorPanel(Engine.getInstance ());
		
		getContentPane ().add (p, BorderLayout.CENTER);

		// A control panel with a close button
		pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		close = new JButton (Translator.swap ("Inspector.close"));
		pControl.add (close);
		close.addActionListener (this);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}
