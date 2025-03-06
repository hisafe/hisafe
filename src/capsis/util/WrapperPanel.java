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

import javax.swing.JPanel;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;

/**
 * A panel to add space arround a component.
 * 
 * @author F. de Coligny - october 2002
 */
public class WrapperPanel extends JPanel {

	public WrapperPanel (Component component, int horizontal, int vertical) {
		super (new BorderLayout ());
		LinePanel l1 = new LinePanel (horizontal, 0);
		ColumnPanel c1 = new ColumnPanel (vertical, 0);
		c1.add (component);
		c1.addStrut0 ();
		l1.add (c1);
		l1.addStrut0 ();
		add (l1, BorderLayout.CENTER);
	}
}
