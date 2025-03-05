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

package capsis.extension.grouperdisplay;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import javax.swing.JPanel;

import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.extensiontype.GrouperDisplay;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import capsis.util.SmartFlowLayout;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;

/**
 * CellsDisplay is a display for groupers on plots divided in cells.
 * 
 * @author F. de Coligny - October 2004
 */
public class CellsDisplay extends JPanel implements GrouperDisplay, Drawer, ActionListener {

	static {
		Translator.addBundle("capsis.extension.grouperdisplay.CellsDisplay");
	} 

	// nb-07.08.2018
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR = "F. de Coligny";
	
	private Panel2D panel2D;
	private Rectangle.Double initialUserBounds;
	
	protected int labelCounter;		// for trees label drawing strategy
	protected int labelFrequency;
	
	private Collection drawnCells;	// filled in draw, used in select

	private GScene stand;
	private Grouper grouper;	
	
	/**	Phantom constructor. 
	*	Only to ask for extension properties (authorName, version...).
	*/
	public CellsDisplay () {}

	
	
	
	
	/**	Referent is a couple of objects : Object[] typeAndStand = (Objet[])  referent;
	*/
	static public boolean matchWith (Object referent) {
		try {
			Object[] typeAndStand = (Object[]) referent;
			
			// fc-29.8.2019 type is now a GroupableType instance
			GroupableType type = (GroupableType) typeAndStand[0];
//			String type = (String) typeAndStand[0];
			GScene stand = (GScene) typeAndStand[1];
			
			// Type is Group.CELL
			if (!type.equals (TreeList.GROUP_PLOT_CELL)) {return false;}
//			if (!type.equals (Group.CELL)) {return false;}
			
			// Stand is a non empty TreeCollection with Spatialized trees inside
			if (stand.getPlot () == null) {return false;}
			
			 // fc-26.10.2017
			if (!(stand.getPlot () instanceof PlotOfCells))
				return false;
			PlotOfCells plotc = stand.getPlot ();
			
			Collection cells = plotc.getCells ();
			if (cells == null || cells.isEmpty ()) {return false;}
			return true;
			
		} catch (Exception e) {
			return false;
		}
	}
	
	@Override
	public String getName() {
		return Translator.swap("CellsDisplay.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("CellsDisplay.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**	Update the display on given stand, according to the given grouper
	*/
	public void update (GScene stand, Grouper grouper){	// not ?
		if (this.stand == null || !this.stand.equals (stand)) {
			standChanged (stand);
		}
		this.grouper = grouper;
		
		panel2D.reset ();		// triggers display update
		panel2D.repaint ();
	}

	/**	Stand has changed : re initialize panel2D and trigger a repaint ().
	*/
	private void standChanged (GScene stand) {
		this.stand = stand;
		
		Vertex3d o = stand.getOrigin ();
		initialUserBounds = new Rectangle.Double (
				o.x, o.y, stand.getXSize (), stand.getYSize ());
		
		setLayout (new SmartFlowLayout ());
		panel2D = new Panel2D (this, initialUserBounds);
		panel2D.setBackground (new Color (255, 255, 200));
		drawnCells = new HashSet ();
		
		removeAll ();
		setLayout (new BorderLayout ());
		add (panel2D, BorderLayout.CENTER);	// we want to be at CENTER
		//~ add (getPilot (), BorderLayout.SOUTH);
	}
	
	/**	From Pilotable interface.
	*/
	//~ public JComponent getPilot () {
		//~ JToolBar toolbar = new JToolBar ();
		
		//~ fLabelNumber = new JTextField (2);
		//~ fLabelNumber.setText (""+labelNumber);
		//~ fLabelNumber.setToolTipText (Translator.swap ("Shared.labelNumberExplanation"));
		//~ fLabelNumber.addActionListener (this);
		
		//~ fMagnifyFactor = new JTextField (2);
		//~ fMagnifyFactor.setText (""+magnifyFactor);
		//~ fMagnifyFactor.setToolTipText (Translator.swap ("Shared.magnifyFactorExplanation"));
		//~ fMagnifyFactor.addActionListener (this);
		
		//~ showDiameter = new JCheckBox (Translator.swap ("Shared.diameter"), diameterMode);
		//~ showDiameter.addActionListener (this);
		
		//~ JLabel l = new JLabel (Translator.swap ("Shared.labelNumber")+": ");
		//~ l.setToolTipText (Translator.swap ("Shared.labelNumberExplanation"));
		//~ toolbar.add (l);
		//~ toolbar.add (fLabelNumber);
		
		//~ JLabel l2 = new JLabel (Translator.swap ("Shared.magnifyFactor")+": ");
		//~ l2.setToolTipText (Translator.swap ("Shared.magnifyFactorExplanation"));
		//~ toolbar.add (l2);
		//~ toolbar.add (fMagnifyFactor);
		
		//~ toolbar.add (showDiameter);
		
		//~ toolbar.setVisible (true);
		//~ return toolbar;
	//~ }
	
	/**	From Drawer interface.
	*/
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		
		Plot plot = stand.getPlot ();
		
		PlotOfCells plotc = (PlotOfCells) plot; // fc-30.10.2017
		
		Collection cells = plotc.getCells ();
		drawnCells.clear ();	// we're gonna fill it now
		Collection selectedCells = new HashSet ();
		if (grouper != null) {
			selectedCells = grouper.apply (cells);	// not = false
		}
		
		cells = plotc.getCellsAtLevel (1);
		drawCells (cells, selectedCells, g2, r);
		
	}
	
	// Draw cells level by level
	//
	private void drawCells (Collection cells, Collection selectedCells, Graphics2D g2, Rectangle.Double r) {
		if (cells == null || cells.isEmpty ()) {return;}
		
		for (Iterator i = cells.iterator (); i.hasNext ();) {
			Cell cell = (Cell) i.next ();
			drawCell (cell, selectedCells, g2, r);	// draw the cell before its daughters
			drawCells (cell.getCells (), selectedCells, g2, r);
		}
	}
	
	// Draw the given cell if needed
	//
	private void drawCell (Cell cell, Collection selectedCells, Graphics2D g2, Rectangle.Double r) {
		Shape shape = cell.getShape ();
		
		// do not draw if invisible
		if (!shape.getBounds2D ().intersects (r)) {return;}
		
		if (selectedCells.contains (cell)) {
			g2.setColor (Color.RED);
			g2.fill (shape);
		}
		g2.setColor (Color.BLACK);
		g2.draw (shape);
	}
	
	
	/**
	 * From Drawer interface.
	 * We may receive (from Panel2D) a selection rectangle (in user space i.e. meters)
	 * and return a JPanel containing information about the objects (trees) inside
	 * the rectangle.
	 * If no objects are found in the rectangle, return null.
	 */
	public JPanel select (Rectangle.Double r, boolean more) {
		
		if (drawnCells == null) {return null;}	// who knows...
		
		Collection cellsToBeInspected = new ArrayList ();
		
		for (Iterator i = drawnCells.iterator (); i.hasNext ();) {
			Cell c = (Cell) i.next ();
			
			Shape shape = c.getShape ();
			if (shape.intersects (r)) {
				cellsToBeInspected.add (c);
			}
		}
			
		if (cellsToBeInspected.isEmpty ()) {	
			return null;	// no JPanel returned
		} else {
			return AmapTools.createInspectorPanel (cellsToBeInspected);	// an inspector for the concerned trees
		}
	}
	
	// Deal with filtering dialog
	//
	public void actionPerformed (ActionEvent evt) {
		
	}
	
	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}



