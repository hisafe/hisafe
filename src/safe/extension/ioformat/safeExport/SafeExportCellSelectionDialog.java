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

package safe.extension.ioformat.safeExport;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;

import javax.swing.JButton;
import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;

/**
 * A dialog to select Safe Cells (subclasses of GCells, ex : SquareCells, PolygonalCells in  a Collection).
 *
 * @see SafeExportCellSelectionPanel
 * @author F. de Coligny / R. Tuquet Laburre - august 2003
 */
 public class SafeExportCellSelectionDialog extends AmapDialog implements ActionListener {

	private SafeExportCellSelectionPanel cellSelectionPanel;
	private Collection selectedCellIds;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor.
	 */
	public SafeExportCellSelectionDialog (Collection cells, Collection alreadySelectedIds) {

		super ();

		cellSelectionPanel = new SafeExportCellSelectionPanel (cells, alreadySelectedIds);
		createUI ();

		// location is set by AmapDialog
		setDefaultCloseOperation (DO_NOTHING_ON_CLOSE);
		pack ();
		setVisible (true);

	}

	/**
	 * Main accessor, called after ok, returns the collection of selected cells ids.
	 */
	public Collection getSelectedCellIds () {return selectedCellIds;}

	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (ok)) {
			okAction ();
		} else if (e.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (e.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	// Redefinition of AmapDialog's method
	//
	protected void escapePressed () {
		cancelAction ();
		setVisible(false);
	}

	private void okAction () {
		selectedCellIds = cellSelectionPanel.getSelectedCellIds ();
		setValidDialog (true);
	}

	private void cancelAction () {
		setValidDialog (false);
	}

	private void createUI () {
		// 1. Cell selection panel
		getContentPane ().add (cellSelectionPanel, BorderLayout.CENTER);

		// 2. Control panel
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

		// Set ok as default (see AmapDialog)
		setDefaultButton (ok);
		getContentPane ().add (pControl, BorderLayout.SOUTH);

		setTitle (Translator.swap ("SafeExport.cellSelectionDialogTitle"));
		
		setModal (true);
	}

}

