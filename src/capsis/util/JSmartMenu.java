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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

/** 
 * JSmartMenu is a JMenu with facilities to create the items.
 * 
 * @author F. de Coligny - may 1999
 */
public class JSmartMenu extends JMenu {

	public JSmartMenu (String label) {
		super (label);
	}

	public JSmartMenu (String label, String mnemonic) {
		super (label);
		if (!mnemonic.equals ("")) {
			char ch = mnemonic.charAt (0);
			setMnemonic (ch);
		}
	}

	public JSmartMenu (String label, String mnemonic, Icon icon) {
		super (label);
		if (icon != null) {
			init (label, icon);
		}
		if (!mnemonic.equals ("")) {
			char ch = mnemonic.charAt (0);
			setMnemonic (ch);
		}
	}

	/** 
	 * Adds an Item in the menu (from an Action) with accelerator and mnemonic. 
	 */
	public void add (Action action, int accelerator, String mnemonic) {
		add (action, accelerator, mnemonic, InputEvent.CTRL_MASK);	// default modifier : ctrl
	}
	public void add (Action action, int accelerator, String mnemonic, int modifier) {
		super.add (action);
		int posItem = this.getItemCount () - 1;
		JMenuItem item = this.getItem (posItem);
		if (accelerator != KeyEvent.VK_UNDEFINED) {
			item.setAccelerator (KeyStroke.getKeyStroke (accelerator, modifier));
		}
		if (!mnemonic.equals ("")) {
			char ch = mnemonic.charAt (0);
			item.setMnemonic (ch);
		}
		
	}
	
	/** 
	 * Adds an Item in the menu : Action only. 
	 */
	public JMenuItem add (Action action) {
		add (action, KeyEvent.VK_UNDEFINED, "");
		return null;	// for compatibility with redifined method (unused)
	}

	/** 
	 * Adds an Item in the menu : Action + Accelerator. 
	 */
	public void add (Action action, int accelerator) {
		add (action, accelerator, "");
	}

	/** 
	 * Adds an Item in the menu : Action + Mnemonic. 
	 */
	public void add (Action action, String mnemonic) {
		add (action, KeyEvent.VK_UNDEFINED, mnemonic);
	}

}