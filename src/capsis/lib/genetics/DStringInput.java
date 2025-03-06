/* 
 * The Genetics library for Capsis4
 * 
 * Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
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
package capsis.lib.genetics;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import capsis.commongui.util.Helper;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;

/**
 * Dialog box to select a String among several.
 * 
 * @author F. de Coligny - August 2002
 */
public class DStringInput extends AmapDialog implements ActionListener {

	private String dlgTitle;
	private String dlgText;
	private String selectedString;

	private JTextField text;
	private JButton ok;
	private JButton cancel;
	private JButton helpButton;

	/**
	 * Contructor.
	 */
	public DStringInput(String title, String explanationText) {
		super();
		dlgTitle = title;
		dlgText = explanationText;

		selectedString = "";

		createUI();

		pack();
		show();

	}

	/**
	 * Action on Ok button.
	 */
	public void okAction() {
		selectedString = text.getText();
		// Back to the caller
		setValidDialog(true);
	}

	/**
	 * Actions on various events.
	 */
	public void actionPerformed(ActionEvent evt) {
		// Ok button
		if (evt.getSource().equals(ok)) {
			okAction();
			// Cancel button
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
			// Help button
		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Initialize the dialog's user interface.
	 */
	private void createUI() {

		// 1. Explanation text
		JLabel label = new JLabel(dlgText);

		// 2 input text
		JPanel textPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		text = new JTextField(50);
		textPanel.add(text);

		// 3. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		helpButton = new JButton(Translator.swap("Shared.help"));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(helpButton);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		helpButton.addActionListener(this);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(label, BorderLayout.NORTH);
		getContentPane().add(textPanel, BorderLayout.CENTER);
		getContentPane().add(pControl, BorderLayout.SOUTH);

		setTitle(dlgTitle);

		setModal(true);
	}

	public String getSelectedString() {
		return selectedString;
	}

}
