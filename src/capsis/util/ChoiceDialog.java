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
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.UIManager;

import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;

/**	A dialog box t interact with user. Returns a selected button.
*	Do not rely on MainFrame/AmapDialog : can be used outside Capsis (ex: during Install)
*	@author F. de Coligny - may 2005
*/
public class ChoiceDialog extends JDialog implements ActionListener {
	private JButton selectedButton;

	
	public ChoiceDialog (Frame owner, String title, String msg, Vector buttons, JButton defaultB) {
		super (owner);
		//~ setLocationRelativeTo (owner);
		setTitle (title);
		selectedButton = null;	// should be changed soon !
		
		// 1. A funny logo
		Icon icon = UIManager.getIcon ("OptionPane.questionIcon");
		JLabel logo = new JLabel (icon);
		logo.setAlignmentY (Component.BOTTOM_ALIGNMENT);
		
		// 2. Message panel
		JTextArea area = new JTextArea (msg);
		area.setColumns (35);
		area.setRows (8);
		area.setLineWrap (true);
		area.setWrapStyleWord (true);
		JComponent part1 = new JScrollPane (area);
		
		// 3. MainPanel = logo + message
		LinePanel mainPanel = new LinePanel ();
		mainPanel.add (new JWidthLabel (10));
		mainPanel.add (logo);
		mainPanel.add (new JWidthLabel (10));
		mainPanel.add (part1);
		
		// 4. A custom control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		pControl.add (new JWidthLabel (20));
		for (Iterator ite = buttons.iterator (); ite.hasNext ();) {
			JButton b = (JButton) ite.next ();
			pControl.add (b);
			b.addActionListener (this);
		}
		
		// 5. All in the dialog box
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		// sets defaultB as default
		defaultB.setDefaultCapable (true);
		getRootPane ().setDefaultButton (defaultB);
		//setDefaultCloseOperation(JDialog.EXIT_ON_CLOSE);		
		setResizable (true);
		setModal (true);
		pack ();	// validate (); does not work for JDialogs...
		setLocationRelativeTo (owner);
		setVisible (true);
	}

	/**
	 * This method opens a dialog with specified title and message to ask for a user choice.
	 * The buttons in the Vector are disposed under the text, the default button is set to default.
	 * The chosen button is returned.
	 * <PRE>
	 *	JButton saveButton = new JButton (Translator.swap ("<saveKey>"));
	 *	JButton ignoreButton = new JButton (Translator.swap ("<ignoreKey>"));
	 *	JButton cancelButton = new JButton (Translator.swap ("<cancelKey>"));
	 * 	Vector buttons = new Vector ();
	 * 	buttons.add (saveButton);
	 * 	buttons.add (ignoreButton);
	 * 	buttons.add (cancelButton);
	 *	
	 *	JButton choice = MessageDialog.promptUser (Translator.swap ("<titleKey>"), 
	 *			Translator.swap ("<messageKey>"), buttons, saveButton);
	 *	if (choice.equals (saveButton)) {
	 *		action1 ();
	 *	} else if (choice.equals (ignoreButton)) {
	 *		action2 ();
	 *	} else if (choice.equals (cancelButton)) {
	 *		action3 ();
	 *	} 
	 * </PRE>
	 */
	public static JButton promptUser (Frame owner, String title, String msg, Vector proposedButtons, JButton defaultButton) {
		ChoiceDialog dlg = new ChoiceDialog (owner, title, msg, proposedButtons, defaultButton);
		
		JButton answer = dlg.getSelectedButton ();
		dlg.dispose ();
		return answer;
	}

	public void actionPerformed (ActionEvent evt) {
		selectedButton = (JButton) evt.getSource ();
		setVisible (false);
	}

	public JButton getSelectedButton () {
		return selectedButton;
	}

}
