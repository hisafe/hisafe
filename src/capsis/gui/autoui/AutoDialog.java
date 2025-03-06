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

package capsis.gui.autoui;

import java.awt.event.ActionListener;

import jeeb.lib.util.autoui.SimpleAutoDialog;
import jeeb.lib.util.autoui.validators.ObjectValidator;
import capsis.commongui.util.Helper;
import capsis.gui.Positioner;
import capsis.gui.Repositionable;

/* Automatic Dialog base on AutoPanel*/
public class AutoDialog<DataType> extends SimpleAutoDialog<DataType>  implements ActionListener, Repositionable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;
	
	public AutoDialog(DataType obj, boolean withExtra) throws Exception {
		super(obj, withExtra);
		
	}

	public AutoDialog(DataType obj, ObjectValidator v, boolean withExtra)
	throws Exception {
		super(obj, v, withExtra);

	}


	/** Display help */
	@Override
	protected void help() {
		Helper.helpFor (object);
	}

	@Override
	public void reposition() {
		
		// Position is now managed in AmapDialog.setVisible (true);
//		try {
//			Pilot.getPositioner ().layOut (this);
//		} catch (Exception e) {
//			System.out.println(e);
//			
//			Positioner.layoutDialog (this);
//		}
	
		
	}

	@Override
	public void setLayout(Positioner p) {
		// Position is now managed in AmapDialog.setVisible (true);
//		p.layoutComponent(this);
		
	}
	
	/**	Redefinition of setVisible () to manage reposition	*/
	@Override
	public void setVisible (boolean v) {
		if (v) {reposition ();}
		super.setVisible (v);
	}

	/**	Redefinition of show () to manage reposition */
	@Override
	public void show () {
		reposition();
		super.show();
	}

	
}
