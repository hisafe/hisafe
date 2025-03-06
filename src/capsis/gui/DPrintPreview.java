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

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.print.Book;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A Print Preview dialog.
 * 
 * @version 1.00 1999-09-11
 * @author Cay Horstmann
 * @author F. de Coligny - may 2002 / september 2003
 */
public class DPrintPreview extends AmapDialog implements ActionListener {
	
	/**
	 * Constructor 1.
	 */
	public DPrintPreview (Printable p, PageFormat pf, int pages) {
		super ();
		numberOfPages = pages;
		Book book = new Book ();
		book.append (p, pf, pages);
		layoutUI (book);
	}

	/**
	 * Constructor 2.
	 */
	public DPrintPreview (Book b) {
		layoutUI (b);
	}

	public void layoutUI (Book book) {
		getContentPane ().setLayout (new BorderLayout ());
		
		// Preview panel
		//
		canvas = new PrintPreviewCanvas (book);
		
		LinePanel l = new LinePanel ();
		ColumnPanel c = new ColumnPanel ();
		l.add (canvas);
		l.addStrut0 ();
		c.add (l);
		c.addStrut0 ();
		
		getContentPane().add (c, BorderLayout.CENTER);
		
		// Control buttons
		//
		JPanel buttonPanel = new JPanel ();
		buttonPanel.setLayout (new BoxLayout (buttonPanel, BoxLayout.X_AXIS));
		
		if (numberOfPages > 1) {
			page = new JTextField (5);
			page.setEditable (false);
			prevButton = new JButton (Translator.swap ("Shared.back"));
			prevButton.addActionListener (this);
			nextButton = new JButton (Translator.swap ("Shared.forward"));
			nextButton.addActionListener (this);
			buttonPanel.add (prevButton);
			buttonPanel.add (nextButton);
			buttonPanel.add (page);
		}
		
		closeButton = new JButton (Translator.swap ("Shared.close"));
		closeButton.addActionListener (this);
		
		buttonPanel.add (closeButton);
		buttonPanel.add (Box.createHorizontalGlue ());
		getContentPane().add (buttonPanel, BorderLayout.NORTH);
		
		// sets closeButton as default (see AmapDialog)
		closeButton.setDefaultCapable (true);
		getRootPane ().setDefaultButton (closeButton);
		
		setTitle (Translator.swap ("DPrintPreview.printPreview"));
		
		setModal (true);
		setSize (500, 600);
		show ();
		
	}

	public void actionPerformed (ActionEvent event) {
		Object source = event.getSource ();
		if (source == prevButton) {
			canvas.flipPage (-1);
		} else if (source == nextButton) {
			canvas.flipPage (1);
		} else if (source == closeButton) {
			setVisible (false);
		}
	}

	private int numberOfPages;
	private JTextField page;
	private JButton prevButton;
	private JButton nextButton;
	private JButton closeButton;
	private PrintPreviewCanvas canvas;


	///////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////////////
	class PrintPreviewCanvas extends JPanel {
	
		private PageFormat pageFormat;
		private double px;
		private double py;
		
		public PrintPreviewCanvas (Book b) {
			super ();
			book = b;
			currentPage = 0;
		}
	
		public void paintComponent (Graphics g) {
			super.paintComponent (g);
			
			try {page.setText (Translator.swap ("Shared.page")+" "+(currentPage+1));} catch (Exception e) {}
			
			Graphics2D g2 = (Graphics2D) g;
			PageFormat pageFormat = book.getPageFormat (currentPage);
			
			double xoff; // x offset of page start in window
			double yoff; // y offset of page start in window
			double scale; // scale factor to fit page in window
			double px = pageFormat.getWidth ();
			double py = pageFormat.getHeight ();
			double sx = getWidth () - 1;
			double sy = getHeight () - 1;
			if (px / py < sx / sy) { // center horizontally
				scale = sy / py;
				xoff = 0.5 * (sx - scale * px);
				yoff = 0;
			} else { // center vertically
				scale = sx / px;
				xoff = 0;
				yoff = 0.5 * (sy - scale * py);
			}
			g2.translate ((float) xoff, (float) yoff);
			g2.scale ((float) scale, (float) scale);
			
			// draw page outline (ignoring margins)
			Rectangle2D page = new Rectangle2D.Double (0, 0, px, py);
			g2.setPaint (Color.white);
			g2.fill (page);				// white page
			g2.setPaint (Color.black);
			g2.draw (page);				// black border
			
			Printable printable = book.getPrintable (currentPage);
			try {
				printable.print (g2, pageFormat, currentPage);
			}
			catch (PrinterException exception) {
				g2.draw(new Line2D.Double (0, 0, px, py));
				g2.draw(new Line2D.Double (0, px, 0, py));	// 0, py, px, 0 ?
			}
		}
	
		public void flipPage (int by) {
			int newPage = currentPage + by;
			if (0 <= newPage && newPage < book.getNumberOfPages ()) { 
				currentPage = newPage;
				repaint();
			}
		}
	
		private Book book;
		private int currentPage;
		
	}
	
}
