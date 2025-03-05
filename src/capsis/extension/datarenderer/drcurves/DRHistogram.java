/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.extension.datarenderer.drcurves;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataExtractor;

/**
 * Histogram drawer. Version 2.
 * 
 * @author F. de Coligny - september 2003
 */
public class DRHistogram extends DRCurves {

	static {
		Translator.addBundle ("capsis.extension.datarenderer.drcurves.DRCurves");
	}

	// nb-06.08.2018
	//static final public String NAME = Translator.swap ("DRHistogram");
	//static final public String VERSION = "2.1";
	//static final public String AUTHOR = "F. de Coligny";
	//static final public String DESCRIPTION = Translator.swap ("DRHistogram.description");

	/**
	 * Compatibility method.
	 */
	static public boolean matchWith (Object target) {
		boolean b = false;
		if (target instanceof DataExtractor && target instanceof DFCurves) {
			b = true;
		}
		return b;
	}
	
	@Override
	public String getName() {
		return Translator.swap("DRHistogram.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRHistogram.description");
	}
	
	@Override
	public String getVersion() {
		return "2.1";
	}
	/**
	 * Special case : each point has a label. If true, label of each point is drawn vertically
	 */
	protected boolean isYFullLabelledVertically () {
		return true;
	}

	/**
	 * Histograms like their y axis to begin at least at zero. At least means they can also have
	 * negative bars.
	 */
	protected boolean yIsAtLeastZero () {
		return true;
	}

	/**
	 * This method draws the point. It may be redefined in subclasses to draw differently.
	 * DRHistogram draws bars. We are working on the i th extractor among n. Each extractor can
	 * contain several curves.
	 */
	@Override
	protected double draw (Graphics2D g2, int i, int n, double ux, double uy, double px, double py, int curveRank, int curveCount, int pw, int numberOfBars) {
		if (Double.isNaN (px) || Double.isNaN (py)) return px;

		// If size is null, do not draw anything
		if (uy == 0) return px;
		
		Color currentColor = g2.getColor ();
		
		double newX = px; // for the moment

		// Lets forecast "negative bars" - fc - 16.10.2001
		// ~ double y0 = 0;
		// ~ double y0 = p0.y;
		// ~ double h = y;
		// ~ if (y < 0) {
		// ~ y0 = y;
		// ~ h = -y;
		// ~ }

		// x shift according to rank of curve among total number of curves
		//
		Rectangle2D r = new Rectangle2D.Double ();
		int xPixel = 1;

		// Draws a bar
		Color color = g2.getColor ();
		Color darker = Color.DARK_GRAY;

		// For histograms, if several curves and not colored curves, force different colors
//		if (!someExtractorsAreColored && curveCount > 1) {
//			color = DefaultColorProvider.getInstance ().getColor (curveRank);
//		}
		
		if (curveCount == 1) {
			// fc - 21.3.2005 - managing the bars width
			int barSize = (int) (pw / numberOfBars) - 3; // bar width in pixels
			if (barSize > 50) {
				barSize = 50;
			}
			if (barSize < 2) {
				barSize = 2;
			}
			int barWidth = (int) (barSize / n);

			if (!enlargedMode) {
				barWidth = Math.min (6, barWidth);
			}

			if (barWidth > 20) {
				barWidth = 20;
			}
			int halfBarWidth = (int) (barWidth / 2);

			double xDec = (-n * halfBarWidth) + ((i - 1) * barWidth);

			newX = xDec + px - 1;
			r.setRect (newX, py, barWidth, p0.y - py);

		} else {
			
			// Cumulated histograms with possibly labels inside larger bars
			double xDec = 11 * (-n / 2 + i - 1);
			newX = xDec + px - 1;

			// Shift the bars when several in the same extractor
			int a = 2;
			int b = 3;
			if (pw > 200) {
				a = 4;
				b = 6;
			}
			newX = newX - curveCount * a + (curveRank - 1) * b;

			r.setRect (newX, py, 9, p0.y - py);
		}

//		if (curveCount == 1 || someExtractorsAreColored) { // fc 16.6.2009
			g2.setColor (color);
			g2.fill (r);
			g2.setColor (darker);
			g2.draw (r);
//			g2.setColor (color);
//		} else {
//			g2.draw (r);
//		}
		
		// Restore color
		g2.setColor (currentColor);
		
		return newX; // was recalculated according to the number / width of bars

	}

	/**
	 * Writes the right label right to the (x,y) representation or in some other place (called only
	 * if there is a right label).
	 */
	protected void writeRightLabel (Graphics2D g2, DFCurves extr, int extrRank, int extrCount, int curveRank, int curveCount, 
			double lastKnownX, double lastKnownY, String label, Color currentColor) {

//		System.out.println ("DRHistogram p2.y "+p2.y+" lastKnownX "+(float) u2px (lastKnownX)+" extrRank "+extrRank+" extrCount "+extrCount+" curveRank "+curveRank+" curveCount "+curveCount+" label "+label);

		if (extrRank > 1) return;
		
		// For histograms, write the label somewhere in the graph
		// Only for the first extractor, take care of the colors matching

		Color color = currentColor;
		
//		// For histograms, if several curves and not colored curves, force different colors
//		if (!someExtractorsAreColored && curveCount > 1) {
//			color = DefaultColorProvider.getInstance ().getColor (curveRank);
//		}
		
		g2.setColor (color);
		
		double yAnchor = p2.y + curveRank * 15;
		
		g2.drawString (label, (float) u2px (lastKnownX), (float) yAnchor);
		
		// Restore color
		g2.setColor (currentColor);

	}

}
