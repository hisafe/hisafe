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

package capsis.gui.command;

import java.awt.print.PageFormat;
import java.awt.print.Printable;

import jeeb.lib.util.Command;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.gui.DPrintPreview;
import capsis.util.Page;
import capsis.util.PrintContext;

/**
 * Command File | PrintPreview.
 *
 * @author F. de Coligny - october 2000
 */
public class PrintPreview implements Command {

	/**
	 * Constructor.
	 */
	public PrintPreview () {}
	
	public int execute () {
		
		try {
			// 1. Printable
			Printable printable = null;
			printable = PrintContext.getPrintable ();
			if (printable == null) {return 1;}
			
			// 2. Page Format		
			PageFormat pf = PrintContext.getPageFormat ();
			
			// 3. Page number
			int pageNumber = 1;
			if (printable instanceof Page) {
				pageNumber = ((Page) printable).getPageNumber ();
			}
			
			PrintContext.setPrintPreview (true);
			DPrintPreview dlg = new DPrintPreview (printable, pf, pageNumber);
			PrintContext.setPrintPreview (false);
			
		} catch (Throwable e) {		// fc - 30.7.2004 - catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "PrintPreview.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 2;
		}
		return 0;
		
	}

}
