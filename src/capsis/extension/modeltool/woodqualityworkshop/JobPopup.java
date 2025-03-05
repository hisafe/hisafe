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

package capsis.extension.modeltool.woodqualityworkshop;

import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JTable;

import jeeb.lib.util.Translator;

/**	JobPopup is a JPopupMenu for thE wqw job table(s) management..
* 
*	@author F. de Coligny - march 2006
*/
public class JobPopup extends JPopupMenu {
	
	public static final int DELETE_ROW = 1;
	public static final int INSPECT = 2;
	
	private JTable table;
	private ActionListener listener;


	/**	Constructor
	*	When a menu item is chosen, the listener is called
	*/
	public JobPopup (JTable table, ActionListener listener) {
		super ();
		this.table = table;
		this.listener = listener;
		
		boolean multipleSelection = false;
		if (table.getSelectedRows ().length > 1) {multipleSelection = true;}
		
		
		// DELETE_ROW
		// multiple selection allowed for delete row(s)
		JMenuItem delete = new JMenuItem (Translator.swap ("JobPopup.delete"), DELETE_ROW);
		delete.addActionListener (listener);
		add (delete);
		
		// INSPECT
		// multiple selection allowed for delete row(s)
		JMenuItem inspect = new JMenuItem (Translator.swap ("JobPopup.inspect"), INSPECT);
		inspect.addActionListener (listener);
		add (inspect);
		
		// inspect is available only if one single selection
		//~ if (multipleSelection) {
			//~ inspect.setEnabled (false);
		//~ }
		
		//~ addSeparator ();
		
	}

}
