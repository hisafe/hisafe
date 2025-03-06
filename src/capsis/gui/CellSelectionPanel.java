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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.plotofcells.Cell;
import capsis.kernel.Plot;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import capsis.util.Pilotable;

/**
 * A panel to select Capsis Cells (subclasses of GCells, ex : SquareCells, PolygonalCells in  a Collection).
 * The given cells must be in the same plot. 
 * If the given cells are embedded in an upper cell layer, all the given cells must have the same mother cell.
 * 
 * @author F. de Coligny - august 2003
 */
public class CellSelectionPanel extends JPanel implements Drawer, Pilotable, ActionListener {
	private static final int BUTTON_SIZE = 23;
	
	private Collection cells;
	private Collection initiallySelectedIds;
	
	private Panel2D panel2D;
	private Collection drawnCells;	// filled in draw, used in select
	private Collection selectedCellIds;
	
	private JButton helpButton;
	
	private boolean emptyPanel;

	/**
	 * Constructor.
	 */
	public CellSelectionPanel (Collection cells, Collection alreadySelectedIds) {
		
		super ();
		
		// Security test
		emptyPanel = false;
		if (cells == null || cells.isEmpty ()) {
			add (new JLabel (Translator.swap ("Shared.empty")), BorderLayout.NORTH);
			emptyPanel = true;
			return;	// can not work without cells in the collection -> whte panel with "empty" message
		}
		
		this.cells = cells;
		if (alreadySelectedIds == null) {alreadySelectedIds = new ArrayList ();}
		initiallySelectedIds = alreadySelectedIds;	// not null
		drawnCells = new ArrayList ();
		selectedCellIds = new ArrayList (alreadySelectedIds);
		
		// Get origin and extension of the whole scene
		Cell cell1 = (Cell) cells.iterator ().next ();
		
		// Default case
		Plot plot = cell1.getPlot ();
		Vertex3d origin = plot.getOrigin ();
		double width = plot.getXSize ();
		double height = plot.getYSize ();
		
		// If the cells are embedded in a mother cell
		if (cell1.getMother () != null) {
			Cell mother = cell1.getMother ();
			origin = cell1.getOrigin ();
			Rectangle2D boundingBox = cell1.getShape ().getBounds2D ();
			width = boundingBox.getWidth ();
			height = boundingBox.getHeight ();
		}
		
		Rectangle.Double r = new Rectangle.Double (
				origin.x, 
				origin.y, 
				width, 
				height);		
		panel2D = new Panel2D (this, r, 10, 10);
		JScrollPane scrollPane = new JScrollPane (panel2D);
		scrollPane.setPreferredSize (new Dimension (300, 300));
		
		setBackground (Color.white);
		setLayout (new BorderLayout ());
		add (scrollPane, BorderLayout.CENTER);
		add (getPilot (), BorderLayout.NORTH);
		
	}

	/**
	 * Main accessor, returns the collection of selected cells ids.
	 */
	public Collection getSelectedCellIds () {return selectedCellIds;}

	public Collection getInitiallySelectedIds () {return initiallySelectedIds;}
	
	public boolean isEmptyPanel () {return emptyPanel;}

	/**
	 * Draws the cells : selected ones are in "red".
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		if (emptyPanel) {return;}
		
		Graphics2D g2 = (Graphics2D) g;
		drawnCells.clear ();	// we're gonna fill it now
		
		for (Iterator i = cells.iterator (); i.hasNext ();) {
			Cell c = (Cell) i.next ();
			
			Shape shape = c.getShape ();
			
			// do not draw if invisible
			if (!shape.getBounds2D ().intersects (r)) {continue;}
			
			if (isSelected (c)) {
				g2.setColor (Color.RED);
				g2.fill (shape);
			}
			g2.setColor (Color.BLACK);
			g2.draw (shape);
			drawnCells.add (c);		// memorize what is really drawn (zoom active, maybe we draw only some cells)
		}
	}

	// Return true if the cell is currently selected
	//
	private boolean isSelected (Cell c) {
		if (selectedCellIds.contains (new Integer (c.getId ()))) {return true;} 
		return false;
	}

	/**
	 * Must return a JPanel to represent selection if selection was done, null if nothing selected
	 */
	public JPanel select (Rectangle.Double r, boolean isControlDown) {	// r is the selection rectangle
		if (emptyPanel) {return null;}
		
		Collection cellsToBeInspected = new ArrayList ();
		
		if (!isControlDown) {	
			// Cells selection
			for (Iterator i = drawnCells.iterator (); i.hasNext ();) {
				Cell c = (Cell) i.next ();
				
				Shape shape = c.getShape ();
				
				if (shape.intersects (r)) {
					Integer id = new Integer (c.getId ());
					if (!selectedCellIds.contains (id)) {
						selectedCellIds.add (id);
					} else {
						selectedCellIds.remove (id);
					}
				}
			}
			
		} else {	
			// Cells inspection : user wants information, show in an inspector
			if (drawnCells != null) {
				
				for (Iterator i = drawnCells.iterator (); i.hasNext ();) {
					Cell c = (Cell) i.next ();
					
					Shape shape = c.getShape ();
					
					// inspection requested ?
					if (shape.getBounds2D ().intersects (r)) {
						cellsToBeInspected.add (c);
					}
				}
			}
		} 
		
		panel2D.reset ();
		panel2D.repaint ();
		
		if (cellsToBeInspected.isEmpty ()) {	
			return null;	// no JPanel returned (no information requested)
		} else {
			return AmapTools.createInspectorPanel (cellsToBeInspected);	// an inspector for each concerned cell
		}
	}

	/**
	 * From Pilotable interface.
	 */
	public JComponent getPilot () {
		
		ImageIcon icon = IconLoader.getIcon ("help_16.png");
		helpButton = new JButton (icon);
		Tools.setSizeExactly (helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);
		
		JToolBar toolbar = new JToolBar ();
		toolbar.add (helpButton);
		toolbar.setVisible (true);
		
		return toolbar;
	}
		
	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
		}
	}
		
}

