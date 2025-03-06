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

import javax.swing.JInternalFrame;

/**
 * JInternal frame with some stuff to Print the component.
 * 
 * These Frames will contains Outputs (i.e. DataExtractors results rendered
 * in a DataRenderer). For some of them (curves, plots and histos), an util
 * pane is used for drawing. It contains affine transforms along x & y axes (or z)
 * to make map user space units (meters, years, dbhs...) with device pixels.
 * In this panel, when drawing, pen Stroke width must be set to 0 to avoid 
 * pen deformations (higher then wide for example). Using a 0 width tells
 * Java2D to draw one device pixel wide.
 * This works well on screens because they have quite a low resolution, but 
 * we get into troubles on printer devices which have a better resolution : 
 * the curves are too thin or even not drawn.
 * That's why we "print" on PrintPreview panels and we "paint" on printer.
 * 
 * @author F. de Coligny - december 2000
 */
public class PrintableJInternalFrame extends JInternalFrame implements Printable {

	public PrintableJInternalFrame (String s, boolean a, boolean b, boolean c, boolean d) {
		super (s, a, b, c, d);
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

		// To the screen: print () gives the best effect...
		if (PrintContext.isPrintPreview ()) {
			print (g2);	// troubles with Of width in Strokes
			
		// To Printer, paint avoids trouble of curves with one device pixel width: 
		// not wide enough to be seen... (Stroke with width=0 to be x & y transform independant)
		} else {
/*			boolean prefersPaint = false;
			try {
				DataRenderer r = (DataRenderer) getContentPane ().getComponent (0);
				prefersPaint = r.prefersPaintToPrinter ();
			} catch (Exception e) {
				Log.println (Log.ERROR, "PrintableJInternalFrame.print ()", 
						"DataRenderer.prefersPaintToPrinter () raises Exception", e);
			}
			if (prefersPaint) { */
			
			
			paint (g2);	// WORKS WORKS WORKS !!!!!!!!!!!!!!!!!!!!!!!!!!!!


/*			} else {
				getContentPane ().print (g2);
			} */
		}

		return Printable.PAGE_EXISTS;
	}




}
