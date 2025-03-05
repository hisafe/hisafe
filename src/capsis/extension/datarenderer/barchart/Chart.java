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
package capsis.extension.datarenderer.barchart;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

import javax.swing.JPanel;
import javax.swing.JViewport;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.ColorManager;

/**	A Chart is a bar set in a BarChart renderer. Maybe several 
*	histograms in a chart.
*
*	@author F. de Coligny - december 2004
*/
public class Chart extends JPanel {
	
	private Collection<Bar> bars;
	private String xAxisName;
	private String yAxisName;
	private int exNumber;	// number of extractors
	
	private NumberFormat nf;	// to format number
	private FontMetrics fm;		// to mesure strings
	private int upMargin;
	private int leftMargin;
	private int rightMargin;
	private int downMargin;
	private int captionMargin;
	private int noteMargin;

	private int lineHeight;
	private int interLine;
	
	private double[] maxValue;	// for each extractor, size of the largest bar
	private double valueReductor;
	private int x0;
	private	int[] x1;	// for each extractor
	private	int[] width;	// for each extractor
	private	int x0x1;
	
	private Collection<Caption> captions;	// one for each extractor
	
		// A class to describe extractors captions
		//
		private class Caption {
			public String text;
			public Color color;
			public Caption (String text, Color color) {
				this.text = text;
				this.color = color;
			}
		}
		
	
	/**	Constructor. 
	*/
	public Chart (Collection<Bar> bars, String xAxisName, String yAxisName, int exNumber) {
		this.bars = bars;
		this.xAxisName = xAxisName;
		this.yAxisName = yAxisName;
		this.exNumber = exNumber;
		init ();
	}
	
	/**	Paints this Chart with all its bars.
	*	A bar may be made of several rectangles in case of cumulated histograms.
	*/
	public void paintComponent (Graphics g) {
		super.paintComponent (g);
		
		// How many pixels wide available
		int availableWidth = getSize ().width;
		try {	// control accurate width if in jscrollpane
			JViewport port = (JViewport) getParent ();
			availableWidth = port.getExtentSize ().width;
		} catch (Exception e) {}
		
		Graphics2D g2 = (Graphics2D) g;
		
		try {
			
			// Basic control (may happen)
			if (bars.size () <= 0) {
				addWarningMessage (g);
				return;
			}
			
			int totalHeight = 0;	// will be augmented during drawing
			
			// Check if enough space in width
			int availableSpaceForBars = availableWidth - (x0 + x0x1 + rightMargin);
			if (availableSpaceForBars < 10 * exNumber) {	// at least 10 pixels per extractor
				addEnlargeMessage (g);
				return;
			}
			
			// Width reductor to draw bars
			double neededWidth = 0d;
			double availableWidthInPixels = (double) availableSpaceForBars;
			for (int i = 0; i < exNumber; i++) {
				neededWidth += maxValue[i];
				availableWidthInPixels -= x0x1;
			}
			valueReductor = 1;
			if (neededWidth != 0) {
				valueReductor = availableWidthInPixels / neededWidth;
			}
			
			// Calculate x anchor for each extractor
			// Extractors drawings will be side by side from left to right
			x1[0] = x0 + x0x1;
			width[0] = (int) (maxValue[0] * valueReductor);
			for (int i = 1; i < exNumber; i++) {
				x1[i] = x1[i-1] + (width[i-1] + x0x1);
				width[i] = (int) (maxValue[i] * valueReductor);
			}
			
			// Draw axes names
			g2.drawString (xAxisName, x0 - fm.stringWidth (xAxisName) , upMargin - 2);
			int yAxisNameWidth = fm.stringWidth (yAxisName);
			boolean oneOnlyYAxisName = false;
			for (int i = 0; i < exNumber; i++) {
				if (width[i] <= yAxisNameWidth) {
					oneOnlyYAxisName = true;
					break;
				}
			}
			if (oneOnlyYAxisName) {
				g2.drawString (yAxisName, x1[0] , upMargin - 2);
			} else {
				for (int i = 0; i < exNumber; i++) {
					g2.drawString (yAxisName, x1[i] , upMargin - 2);
				}
			}
			
			// Draw bars for all the extractors
			// ex: 3 extractors result in 3 drawings in 3 columns 0, 1 and 2
			for (Bar b: bars) {
				// Y for the bar
				int y = (int) (upMargin + lineHeight + (lineHeight + interLine) * b.line);
				
				// Draw x label of the bar (ex: className)
				String label = b.label;
				try {
					label = nf.format (new Double (label));
				} catch (Exception e) {}
				int labelSize = fm.stringWidth (label);
				g2.drawString (label, x0 - labelSize, y);
				
				// Draw the bars : one (general case) or several rectangles
				Iterator<Double> vs = b.values.iterator ();
				Iterator<String> ns = b.notes.iterator ();
				int rectNumber = b.values.size ();
				double xAnchor = x1[b.col];
				
				while (vs.hasNext() && ns.hasNext ()) {
					double value = vs.next ();
					String note = ns.next ();
					if (Double.isNaN (value)) {continue;}
					double size = value * valueReductor;	// Important : make this in double precision
					drawRect (g2, b.line, b.col, (int) xAnchor, (int) size, 
							note, b.color, rectNumber, y);
					xAnchor += size;
				}
				
				// Increment total height of the drawing
				totalHeight = (int) y;
			}
			
			// Write coloured captions
			totalHeight += captionMargin;
			int i = totalHeight;
			for (Caption c: captions) {
				i += lineHeight;
				g2.setColor (c.color);
				int textSize = fm.stringWidth (c.text);
				g2.drawString (c.text, availableWidth - rightMargin - textSize, i);
			}
			
			// End total height calculation
			totalHeight = i;
			totalHeight += downMargin;
			
			int preferredWidth = availableWidth;
			int preferredHeight = totalHeight;
			setPreferredSize (new Dimension (preferredWidth, preferredHeight));
		} catch (Exception e){
			Log.println (Log.ERROR, "Chart.paintComponent ()", "Exception ", e);
		}
	}

	//	Draw a bar rectangle. A bar may contain one or several rectangles
	//	Simple bar : one single rectangle, cumulated histograms : several rectangles.
	//
	private void drawRect (Graphics2D g2, int line, int col, 
			int xAnchor, int rectSize, String note, Color color, 
			int rectNumber, int y) {
		
		// Format note if numeric
		try {
			note = nf.format (new Double (note));
		} catch (Exception e) {}
		int noteSize = fm.stringWidth (note);
		
		// X anchor for the note
		int xNote = 0;
		boolean writeNote = true;
		Color noteColor = Color.BLACK;
		if (rectSize > noteSize + noteMargin * 2) {	// enough space : note inside the bar
			xNote = xAnchor + noteMargin + rectSize / 2 - noteSize / 2;
			noteColor = ColorManager.getForegroundColor (color);
		} else {	// not enough space : note after the bar
			if (rectNumber != 1) {writeNote = false;}	// several rectangles : draw notes only inside rectangles
			if (rectSize + noteMargin + noteSize <= width[col]) {
				xNote = xAnchor + rectSize + noteMargin;
			} else {
				writeNote = false;
			}
		}
		
		int xBar = xAnchor;
		int yBar = y + 2 - lineHeight;
		int wBar = rectSize;
		int hBar = lineHeight;
		
		//~ g2.setColor (DefaultColorProvider.getLighterColor (color));
		g2.setColor (color);
		g2.fillRect (xBar, yBar, wBar, hBar);
		
		g2.setColor (ColorManager.getDarkerColor (color));
		g2.drawRect (xBar, yBar, wBar, hBar);
		
		if (writeNote) {
			g2.setColor (noteColor);	// fc - 23.12.2004
			g2.drawString (note, xNote, y);
		}
		
		g2.setColor (Color.BLACK);
	}

	// Calculate some margins and sizes for layout
	//
	private void init () {
		setBackground (Color.WHITE);
		
		captions = new ArrayList<Caption> ();
		
		nf = NumberFormat.getInstance (Locale.ENGLISH);
		nf.setMaximumFractionDigits (3);
		nf.setMinimumFractionDigits (0);
		nf.setGroupingUsed (false);
		
		// Font reduction
		Font gFont = getFont ();	
		Font userFont = new Font (gFont.getName (), gFont.getStyle (), gFont.getSize ()-2);
		setFont (userFont);	// fc - NEW
		
		fm = getFontMetrics (getFont ());
		int maxLabelSize = fm.stringWidth (xAxisName);
		
		maxValue = new double[exNumber];
		x1 = new int[exNumber];
		width = new int[exNumber];
		
		for (Bar bar: bars) {
			maxLabelSize = Math.max (maxLabelSize, fm.stringWidth (bar.label));
			maxValue[bar.col] = Math.max (maxValue[bar.col], bar.getValue ());
			maxValue[bar.col] = Math.max (maxValue[bar.col], 5);	// fc - 8.12.2004
		}
		
		upMargin = 15;
		leftMargin = 5;
		rightMargin = 5;
		downMargin = 5;
		captionMargin = 2;
		noteMargin = 2; 
		x0x1 = 2;
		interLine = 2;
		lineHeight = fm.getHeight ();
		x0 = leftMargin + maxLabelSize;
	}
	
	//	Prints a message in the graphics ("enlarge the panel")
	//
	private void addEnlargeMessage (Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		Font f = g2.getFont ();
		FontMetrics fm = g2.getFontMetrics (f);
		int fontAscent = fm.getAscent ();
		int fontDescent = fm.getDescent ();
		int fontHeight = fontAscent+fontDescent;
		g2.drawString (Translator.swap ("Shared.enlargePanel"), 0f, (float) fontHeight);
		
	}
		
	//	Prints a warning message in the graphics ("see configuration")
	//
	private void addWarningMessage (Graphics g) {
		Graphics2D g2 = (Graphics2D) g;
		Font f = g2.getFont ();
		FontMetrics fm = g2.getFontMetrics (f);
		int fontAscent = fm.getAscent ();
		int fontDescent = fm.getDescent ();
		int fontHeight = fontAscent+fontDescent;
		g2.drawString (Translator.swap ("Shared.seeConfiguration"), 0f, (float) fontHeight);
		
	}
	
	public void addCaption (String text, Color color) {
		captions.add (new Caption (text, color));
	}

}
