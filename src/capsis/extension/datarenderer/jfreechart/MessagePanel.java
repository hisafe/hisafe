/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2011  F. de Coligny, S. Dufour-Kowalski
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
package capsis.extension.datarenderer.jfreechart;

import java.awt.Color;
import java.awt.GridLayout;
import java.awt.event.MouseListener;

import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.LinePanel;

/**	A message panel for the JFC data renderers.
 * 	In case they can not show the dataset, they may wrute a message.
 * 	This panel supports the data renderer popup to swith to another renderer. 
 *	@author F. de Coligny - february 2011
 */
public class MessagePanel extends JPanel {

	/**	Constructor.
	 */
	public MessagePanel (String message, MouseListener caller) {
		
		setLayout (new GridLayout (1, 1));
		
		LinePanel l = new LinePanel ();
		l.addGlue ();
		l.add (new JLabel (message));
		l.addGlue ();
		
		l.setBackground (Color.WHITE);
		l.setOpaque (true);
		
		add (l);
		
		this.addMouseListener (caller);
		
	}
	
	
}
