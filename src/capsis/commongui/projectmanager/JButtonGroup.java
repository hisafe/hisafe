/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
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
package capsis.commongui.projectmanager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.JButton;


/**	The class javax.swingButtonGroup is only for JToggleButton instances.
*	This class does the same for JButton instances.
*	@see StepButton
*	@author F. de Coligny - december 2009
*/
public class JButtonGroup implements ActionListener {
	
	private Collection<JButton> buttons;
	
	
	/**	Constructor
	*/
	public JButtonGroup () {}
	
	/**	Add a JButton in the group
	*/
	public void add (JButton b) {
		if (buttons == null) {buttons = new ArrayList<JButton> ();}
		buttons.add (b);
		b.addActionListener (this);
	}
	
	/**	When a button is clicked, manage the selection for the group
	*/
	public void actionPerformed (ActionEvent e) {
		JButton z = (JButton) e.getSource ();
		
		// Select the button
		z.setSelected (true);
		
		// Deselect all the other buttons
		for (JButton b : buttons) {
			if (b.equals (z)) {continue;}
			b.setSelected (false);
		}
	}




}