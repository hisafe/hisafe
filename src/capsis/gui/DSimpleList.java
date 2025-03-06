/*
 * This file is part of the QuebecMRNF module for Capsis 4
 *
 * Copyright (C) 2009 Gouvernement du Qu�bec 
 * 	(Mathieu Fortin, Minist�re des Ressources naturelles et de la Faune du Qu�bec)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * This dialog is a generic dialog that shows a list. 
 * The user can select only one entry of the list.
 * It is used to select the stratum in Artemis module for example
 * @author Mathieu Fortin - August 2009
 *
 */
public class DSimpleList extends AmapDialog implements ActionListener{
	/**
	 * Members of this class
	 */
	private static final long serialVersionUID = 1L;
	
	private JList m_oList;
	private JButton ok;
	private JButton cancel;
	private String strText;

	/**
	 * Constructor
	 * Requires a set of stratum names
	 */
	public DSimpleList(Vector<String> vector,
						String title,
						String text) {
		super ();
		
		m_oList = new JList(vector);
		strText = text;
		
		
		ok = new JButton (Translator.swap ("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon ("ok_16.png");
		ok.setIcon(icon);
		ok.addActionListener (this);

		cancel = new JButton (Translator.swap ("Shared.cancel"));
		icon = IconLoader.getIcon ("cancel_16.png");
		cancel.setIcon(icon);
		cancel.addActionListener (this);
		
		createUI ();
		setTitle (title);
		setModal (true);
		pack ();	// uses component's preferredSize
		show ();
	}
		
	/*
	 * List of getters.
	 */
	public int getSelectedIndex() {
		return m_oList.getSelectedIndex();				// return the selected value of the JList object
	}

	/*
	 * 	Action on ok button.
	 */
	private void okAction () {
		setValidDialog (true);
	}

	/*
	 * Action on cancel button. 
	 */
	private void cancelAction () {
		setValidDialog (false);
	}

	/* 
	 * Someone hit a button.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		}
	}
		
	/**
	 * Create the dialog box user interface.
	 */ 
	private void createUI () {

		m_oList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		m_oList.setSelectedIndex(0);

		// 1. selection panel
		LinePanel panel1 = new LinePanel ();
		LinePanel panel2 = new LinePanel ();
		ColumnPanel subPanel1 = new ColumnPanel();
		ColumnPanel subPanel2 = new ColumnPanel();
		JScrollPane	panelList = new JScrollPane(m_oList);

		subPanel1.addStrut1();
		JTextArea label1 = new JTextArea(strText + " :");
		label1.setWrapStyleWord(true);
		label1.setLineWrap(true);
		label1.setEditable(false);
		label1.setBackground(this.getBackground());
		label1.setFont(new Font("ArialBold12",Font.BOLD,12));
		label1.setSize(200, 150);
		subPanel1.add (label1);
		subPanel1.addGlue ();

		panel1.addStrut0();
		panel1.add (subPanel1);
		panel1.addStrut0();

		subPanel2.addStrut1();
		subPanel2.add(panelList);
		subPanel2.addGlue();

		panel2.addStrut0();
		panel2.add (subPanel2);
		panel2.addStrut0();

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (panel1, BorderLayout.WEST);
		getContentPane ().add (panel2, BorderLayout.CENTER);

		// 2. control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		pControl.add (ok);
		pControl.add (cancel);

		MouseListener mouseListener = new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					okAction();
				}
			}
		};
		
		m_oList.addMouseListener(mouseListener); 		// to enable the double-click option in the intervention dialog

		getContentPane ().add (pControl, BorderLayout.SOUTH);

		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);
		setMinimumSize(new Dimension(400, 170));
	}
}


