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

import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterJob;

import capsis.gui.Pilot;

/**
 * PrintContext knows about current print settings.
 *
 * @author F. de Coligny - december 2000
 */
public class PrintContext {

	private static PageFormat pageFormat = PrinterJob.getPrinterJob ().defaultPage ();
	private static boolean fitToSize = true;	
	private static boolean printPreview = false;	// must be explicitly set
	
	static public Printable getPrintable () {
		return (Printable) Pilot.getPositioner ();
	}

	static public void setPageFormat (PageFormat pf) {pageFormat = pf;}
	static public PageFormat getPageFormat () {return pageFormat;}

	static public void setFitToSize (boolean v) {fitToSize = v;}
	static public boolean isFitToSize () {return fitToSize;}

	static public void setPrintPreview (boolean v) {printPreview = v;}
	static public boolean isPrintPreview () {return printPreview;}

}

