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

package safe.extension.ioformat.safeExport;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import safe.model.SafeCell;
import safe.model.SafeCrop;
import safe.model.SafeStand;
import safe.model.SafeTree;
import capsis.commongui.util.Helper;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.util.Drawer;
import capsis.util.Panel2D;
/**
 * A panel to select Safe Cells (subclasses of GCells, ex : SquareCells, PolygonalCells in  a Collection).
 * The given cells must be in the same plot.
 * If the given cells are embedded in an upper cell layer, all the given cells must have the same mother cell.
 *
 * @author F. de Coligny / R. Tuquet Laburre - august 2003

 */
public class SafeExportCellSelectionPanel extends JPanel implements Drawer, ActionListener {

	private Color cellWithCropColor = new Color(0,150,0);
	private Color cellWithoutCropColor = new Color(255,255,255);
	private Color selectionColor = new Color(255,0,0);
	private Color treeTrunkColor = new Color(200,100,0);
	private Color treeCrownColor = new Color(100,255,150);

	private Collection trees;
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
	public SafeExportCellSelectionPanel (Collection cells, Collection alreadySelectedIds) {

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
		PlotOfCells plot = cell1.getPlot ();
		Vertex3d origin = plot.getOrigin ();
		double width = plot.getXSize ();
		double height = plot.getYSize ();

		//is there any tree ?
		trees = ((SafeStand) plot.getScene()).getTrees();

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
			SafeCell c = (SafeCell) i.next ();

			Shape shape = c.getShape ();

			// do not draw if invisible
			if (!shape.getBounds2D ().intersects (r)) {continue;}

			if (isSelected (c)) {
				g2.setColor (selectionColor);
				g2.fill (shape);
			}
			else {
				g2.setColor (cellWithoutCropColor);
				g2.fill (shape);
			}
			g2.setColor (Color.black);
			g2.draw (shape);
			drawnCells.add (c);		// memorize what is really drawn (zoom active, maybe we draw only some cells)
		}

		//draw trees
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			SafeTree t = (SafeTree) i.next ();

			double width = 0.1;		// 10 cm.
			double x = t.getX ();
			double y = t.getY ();

			// Draw the trunk
			width = t.getDbh ()/100;
			Shape sh = new Ellipse2D.Double (x-width/2, y-width/2, width, width);
			Rectangle2D bBox = sh.getBounds2D ();
			g2.setColor (treeTrunkColor);
			if (r.intersects (bBox)) {g2.fill (sh);}

			// Draw the crown
			width = 2*t.getCrownRadius ();
			Shape sh2 = new Ellipse2D.Double (x-width/2, y-width/2, width, width);
			Rectangle2D bBox2 = sh2.getBounds2D ();
			g2.setColor(treeCrownColor);
			if (r.intersects (bBox2)) {g2.draw (sh2);}
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
	 * From ActionListener interface.
	 */
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
		}
	}

}

