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

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.JDesktopPane;

/**
 * JDesktopPane with some stuff to Print the components inside.
 * 
 * @author F. de Coligny - december 2000
 */
public class PrintableJDesktopPane extends JDesktopPane implements Printable {

	public PrintableJDesktopPane () {
		super ();
	}

	/**
	 * Print method. Called by a printJob. See capsis.gui.command.PrintPreview 
	 * and capsis.gui.command.Print.
	 */
	public int print (Graphics g, PageFormat pf, int pi) throws PrinterException {
		if (pi >= 1) {return Printable.NO_SUCH_PAGE;}
		Graphics2D g2 = (Graphics2D) g;

/*		Font f = new Font ("Monospaced", Font.PLAIN, 10);
		g2.setFont (f); */
		
		g2.translate (pf.getImageableX (), pf.getImageableY ());

		// optionally : scale to fit to page size
		if (PrintContext.isFitToSize ()) {
			double xScale = pf.getImageableWidth () / getWidth ();
			double yScale = pf.getImageableHeight () / getHeight ();
			double scale = Math.min (xScale, yScale);
			g2.scale (scale, scale);
		} else {
			// else, clip
			Rectangle2D.Double r2d = new Rectangle2D.Double ();
			r2d.setRect (0, 0, pf.getImageableWidth (), pf.getImageableHeight ());
			g2.clip (r2d);
		}

//		print (g2);		// -> Preview: good, Printer: thin lines & axes labels (1994...) too long
//		paint (g2);		// -> Preview: dirty, Printer: good labels & lines size (only small pbs near borders)

		if (PrintContext.isPrintPreview ()) {
			// To the screen: print () gives the best effect...
			print (g2);		// troubles with Of width in Strokes
		} else {
			
			// To Printer, paint avoids trouble of curves with one device pixel width: 
			// not wide enough to be seen... (Stroke with width=0 to be x & y transform independant)
			paint (g2);		// WORKS WORKS WORKS !!!!!!!!!! (lines width)
		}
		
		return Printable.PAGE_EXISTS;
	}




}
