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

package synchro.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.WindowConstants;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;

/**
 * Static methods to send user messages in formatted dialog boxes.
 * 
 * @author F. de Coligny - april 2003
 */
public class DMessage {
	public static final int LINE_LENGTH = 50;

	
	/**
	 * Default constructor.
	 */
	public DMessage () {}

	public static String readLine (Frame owner, String msg) {
		LineInputDialog dlg = new LineInputDialog (owner, msg);
		String s = dlg.getLine ();
		dlg.dispose ();
		return s;
	}
	
	public static void promptError (Frame owner, String msg) {
		promptError (owner, msg, null);
	}
	
	public static void promptError (Frame owner, String msg, Exception exc) {	// writes down stack to Log

		MessageDialog dlg = new MessageDialog (owner, "Error", msg, exc);
		dlg.dispose ();
	}

	public static void promptWarning (Frame owner, String msg) {
		promptWarning (owner, msg, null);
	}
	
	public static void promptWarning (Frame owner, String msg, Exception exc) {	// DOES NOT write down stack to Log

		MessageDialog dlg = new MessageDialog (owner, "Warning", msg, exc);
		dlg.dispose ();
	}

	/**
	 * This method opens a dialog with specified title and message to ask for a user choice.
	 * The buttons in the Vector are disposed under the text, the default button is set to default.
	 * The chosen button is returned.
	 * <PRE>
	 *	JButton saveButton = new JButton ("<saveKey>");
	 *	JButton ignoreButton = new JButton ("<ignoreKey>");
	 *	JButton cancelButton = new JButton ("<cancelKey>");
	 * 	Vector buttons = new Vector ();
	 * 	buttons.add (saveButton);
	 * 	buttons.add (ignoreButton);
	 * 	buttons.add (cancelButton);
	 *	
	 *	JButton choice = MessageDialog.promptUser ("<titleKey>", 
	 *			"<messageKey>", buttons, saveButton);
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
		UserDialog dlg = new UserDialog (owner, title, msg, proposedButtons, defaultButton);
		//~ if (!dlg.isValidDialog ()) {return null;}	// escape hit on dialog
		
		JButton answer = dlg.getSelectedButton ();
		dlg.dispose ();
		return answer;
	}

}	

/**
 * A dialog box to tell a message to the user. If an exception is given, 
 * Its text appears in a JTextArea.
 */
class MessageDialog extends JDialog implements ActionListener {
	private JButton ok;

	public MessageDialog (Frame owner, String title, String msg, Exception e) {
		super (owner);
		
		// 1. Logo panel : part0
		JPanel part0 = new JPanel (new BorderLayout ());
		Icon icon = UIManager.getIcon ("OptionPane.warningIcon");
		JLabel logo = new JLabel (icon);
		logo.setPreferredSize (new Dimension (75, 60));
		logo.setAlignmentY (Component.BOTTOM_ALIGNMENT);
		part0.add (logo, BorderLayout.NORTH);
		
		// 2. Main panel : part1
		ColumnPanel part1 = new ColumnPanel ();
		
		StringBuffer text = new StringBuffer (msg);
		if (e != null) {
			text.append ("\n");
			text.append (e.toString ());
		}
		JTextArea area = new JTextArea (text.toString ());
		area.setEditable (false);
		area.setLineWrap (true);
		area.setWrapStyleWord (true);
		area.select (0, 0);	// begin of text will be visible in scroll
		
		JScrollPane scroll = new JScrollPane (area);
		scroll.setPreferredSize (new Dimension (300, 100));
		
		// 4. A custom control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		ok = new JButton ("Ok");	
		ok.addActionListener (this);
		//~ validDialog = true;		// will change if escape hit
		pControl.add (ok);
		//~ setDefaultButton (ok);
		
		// 5. Everything in the dialog box
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part0, BorderLayout.WEST);
		getContentPane ().add (scroll, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setTitle (title);
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		setResizable (true);
		setModal (true);
		
		setLocationRelativeTo (owner);
		
		pack ();	// validate () does not work for JDialogs...
		setVisible (true);
	}

	public void actionPerformed (ActionEvent evt) {
		setVisible (false);
		dispose ();
	}
	
}	

/**
 * A dialog box t interact with user. Returns a selected button.
 */
class UserDialog extends JDialog implements ActionListener {
	private JButton selectedButton;

	public UserDialog (Frame owner, String title, String msg, Vector buttons, JButton defaultB) {
		super (owner);
		setTitle (title);
		selectedButton = null;	// should be changed soon !
		//~ validDialog = true;		// will change if escape hit
		
		// 1. A funny logo
		Icon icon = UIManager.getIcon ("OptionPane.questionIcon");
		JLabel logo = new JLabel (icon);
		logo.setAlignmentY (Component.BOTTOM_ALIGNMENT);
		
		// 2. Message panel
		JPanel part1 = new JPanel ();
		part1.setLayout (new BoxLayout (part1, BoxLayout.Y_AXIS));
		for (StringTokenizer st = new StringTokenizer (msg, "\n"); st.hasMoreTokens ();) {
			String line = st.nextToken ();
			JLabel lab = new JLabel (line);
			lab.setForeground (UIManager.getColor ("controlText"));
			
			JPanel p1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			p1.add (lab);
			part1.add (p1);
		}
		JPanel pSpace = new JPanel (new FlowLayout (FlowLayout.LEFT));
		pSpace.add (new JWidthLabel (50));
		part1.add (Box.createVerticalGlue ());
		part1.add (pSpace);
		
		// 3. MainPanel = logo + message
		JPanel mainPanel = new JPanel ();
		mainPanel.setLayout (new BoxLayout (mainPanel, BoxLayout.X_AXIS));
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
		//~ setDefaultButton (defaultB);
		
		// 5. All in the dialog box
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (mainPanel, BorderLayout.NORTH);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		
		setModal (true);
		
		setLocationRelativeTo (owner);
		
		pack ();	// validate (); does not work for JDialogs...
		setVisible (true);
	}

	public void actionPerformed (ActionEvent evt) {
		selectedButton = (JButton) evt.getSource ();
		setVisible (false);
	}

	public JButton getSelectedButton () {
		return selectedButton;
	}

}

/**
 * A dialog box t interact with user. Returns a selected button.
 */
class LineInputDialog extends JDialog implements ActionListener {
	private JButton ok;
	private JTextField input;

	public LineInputDialog (Frame owner, String message) {
		super (owner);
		setTitle ("Input line");
		
		JPanel part1 = new JPanel ();
		part1.setLayout (new BoxLayout (part1, BoxLayout.Y_AXIS));
		
		// 1. Message panel
		JPanel l1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l1.add (new JWidthLabel (message, 50));
		
		// 2. Input text field
		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		input = new JTextField (30);
		l2.add (input);
		input.addActionListener (this);
		
		// 3. A custom control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		ok = new JButton ("Ok");
		ok.addActionListener (this);
		pControl.add (ok);
		
		//~ setDefaultButton (ok);
		
		part1.add (l1);
		part1.add (l2);
		
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
		
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		
		setModal (true);
		
		setLocationRelativeTo (owner);
		
		pack ();	// validate (); does not work for JDialogs...
		setVisible (true);
		
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource () == ok) {
			setVisible (false);
		}
	}
	
	public String getLine () {
		return input.getText ().trim ();
	}
	
	
}
