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

package capsis.extension.filter.gcell;

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
import java.util.Set;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JToolBar;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.util.Drawer;
import capsis.util.History;
import capsis.util.Panel2D;
import capsis.util.Pilotable;
import capsis.util.Redoable;
import capsis.util.Undoable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for FCellGridSelector filter.
 * 
 * @author F. de Coligny - may 2002
 */
public class FCellGridSelectorConfigPanel extends ConfigurationPanel 
		implements Undoable, Redoable, Drawer, Pilotable, ActionListener {
	private static final int BUTTON_SIZE = 23;
	
	private FCellGridSelector mum;	// mummy is being configured
	
	private Plot plot;				// fc - 5.4.2004
	private Collection candidates;	// fc - 5.4.2004
	
	// Support cells are the cells among which the user can select.
	// First time : all the cells in the plot, then, only the cells selected 
	// at preceding filtration step: the cells supporting the selected trees.
	// NOTE: "First time" means if the filter is used directly on an original 
	// filtrable (a GPlot). "then" means if the filter is used on the result
	// of another filter.
	//~ private Set supportCellIds;

	private Panel2D panel2D;
	private Collection drawnCells;	// filled in draw, used in select
	
	private JButton helpButton;
	private JButton eraseAllButton;
	private JButton undoButton;
	private JButton redoButton;

	private boolean disabled;

	public void disablePanel () {disabled = true;}

	private History selectionHistory;	// for undo / redo - fc - 2.3.2005
	
	
	/**	Draw every cell in supportCellIds, 
	*	selecting those in selectedCellIds.
	*/
	protected FCellGridSelectorConfigPanel (Configurable configurable) {
		super (configurable);
		
		selectionHistory = new History ();	// for undo / redo - fc - 2.3.2005
		
		mum = (FCellGridSelector) configurable;
		
		// fc - 5.4.2004
		//
		//~ plot = (GPlot) mum.referent;
		plot = (Plot) ((GScene) mum.referent).getPlot ();	// fc - 21.9.2004
		candidates = mum.candidates;
		
		drawnCells = new Vector ();
		disabled = false;
		
		Vertex3d o = plot.getOrigin ();
		Rectangle.Double r = new Rectangle.Double (
				o.x, 
				o.y, 
				plot.getXSize (), 
				plot.getYSize ());		
		panel2D = new Panel2D (this, r, 10, 10);
		
		selectionHistory.add (new HashSet (mum.selectedCellIds));	// for undo /redo - fc - 2.3.2005
		
		setBackground (Color.WHITE);
		setLayout (new BorderLayout ());
		add (panel2D, BorderLayout.CENTER);
			
	}

	/**	Drawing delegation : should draw something in the given user space Graphics
	*	Restriction: we draw (selected or not) only the cells in support, result
	*	of optional previous selections.
	*/
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		drawnCells.clear ();	// we're gonna fill it now
		
		for (Iterator i = candidates.iterator (); i.hasNext ();) {
			Cell c = (Cell) i.next ();
			
			//~ if (!supportCellIds.contains (new Integer (c.getId ()))) {continue;}
			
			Shape shape = c.getShape ();
			
			// do not draw if invisible
			if (!shape.getBounds2D ().intersects (r)) {continue;}
			
			if (isSelected (c)) {
				g2.setColor (Color.red);
				g2.fill (shape);
			}
			g2.setColor (disabled?Color.gray:Color.black);
			g2.draw (shape);
			drawnCells.add (c);		// memo
		}
	}

	/**	Should return a JPanel if selection done, null if nothing selected
	*/
	public JPanel select (Rectangle.Double r, boolean isControlDown) {
		if (disabled) {return null;}	// panel may be disabled -> do no selection
		
		Collection cellsToBeInspected = new ArrayList ();
		
		if (!isControlDown) {	// cells selection
			
			//~ for (Iterator i = plot.getCells ().iterator (); i.hasNext ();) {
			for (Iterator i = candidates.iterator (); i.hasNext ();) {
				TreeListCell c = (TreeListCell) i.next ();
				if (!c.isTreeLevel ()) {continue;}	// we are interested in cells containing trees
				
				Shape shape = c.getShape ();
				
				if (shape.intersects (r)) {
					Integer id = new Integer (c.getId ());
					if (!mum.selectedCellIds.contains (id)) {
						mum.selectedCellIds.add (id);
					} else {
						mum.selectedCellIds.remove (id);
					}
				}
			}
			
			selectionHistory.add (new HashSet (mum.selectedCellIds));	// for undo /redo - fc - 2.3.2005
			
		} else {	// cells selection for inspector
			if (drawnCells != null) {
				for (Iterator i = drawnCells.iterator (); i.hasNext ();) {
					Cell c = (Cell) i.next ();
					Shape shape = c.getShape ();
					
					// inspection ?
					if (shape.getBounds2D ().intersects (r)) {
						cellsToBeInspected.add (c);
					}
				}
			}
		} 
		panel2D.reset ();
		panel2D.repaint ();
		
		if (cellsToBeInspected.isEmpty ()) {	
			return null;	// no JPanel returned
		} else {
			return AmapTools.createInspectorPanel (cellsToBeInspected);	// an inspector for the concerned cells
		}
	}
	
	public void undo () {
		if (selectionHistory != null && selectionHistory.canBack ()) {
			mum.selectedCellIds = new HashSet ((Set) selectionHistory.back ());
			panel2D.reset ();
			panel2D.repaint ();
		}
			
	}
	
	public void redo () {
		if (selectionHistory != null && selectionHistory.canNext ()) {
			mum.selectedCellIds = new HashSet ((Set) selectionHistory.next ());
			panel2D.reset ();
			panel2D.repaint ();
		}
			
	}
	
	// Tell if the given cell is currently selected
	//
	private boolean isSelected (Cell c) {
		if (mum.selectedCellIds.contains (new Integer (c.getId ()))) {return true;} 
		return false;
	}

	/**	From Pilotable interface
	 */
	public JComponent getPilot () {
		ImageIcon icon = IconLoader.getIcon ("help_16.png");
		helpButton = new JButton (icon);
		Tools.setSizeExactly (helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);
		
		icon = IconLoader.getIcon("cancel_16.png");
//		icon = IconLoader.getIcon ("stop_16.png");
		eraseAllButton = new JButton (icon);
		Tools.setSizeExactly (eraseAllButton, 23, 23);
		eraseAllButton.setToolTipText (Translator.swap ("Shared.unselectAll"));
		eraseAllButton.addActionListener (this);
		
		icon = IconLoader.getIcon ("edit-undo_16.png");
		undoButton = new JButton (icon);
		Tools.setSizeExactly (undoButton, 23, 23);
		undoButton.setToolTipText (Translator.swap ("Shared.undoCtrlZ"));
		undoButton.addActionListener (this);
		
		icon = IconLoader.getIcon ("edit-redo_16.png");
		redoButton = new JButton (icon);
		Tools.setSizeExactly (redoButton, 23, 23);
		redoButton.setToolTipText (Translator.swap ("Shared.redoCtrlR"));
		redoButton.addActionListener (this);
		
		JToolBar toolbar = new JToolBar ();
		toolbar.add (eraseAllButton);
		toolbar.add (undoButton);
		toolbar.add (redoButton);
		toolbar.add (helpButton);
		toolbar.setVisible (true);
		
		return toolbar;
	}
		
	/**	From ActionListener interface
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
			
		} else if (evt.getSource ().equals (eraseAllButton)) {
				
			if (Question.ask (MainFrame.getInstance (), 
					Translator.swap ("Shared.confirm"), Translator.swap ("Shared.unselectAll"))) {
				mum.selectedCellIds.clear ();
				
				selectionHistory.add (new HashSet (mum.selectedCellIds));	// for undo /redo - fc - 2.3.2005
				
				panel2D.reset ();
				panel2D.repaint ();
			}
			
		} else if (evt.getSource ().equals (undoButton)) {
			undo ();
			
		} else if (evt.getSource ().equals (redoButton)) {
			redo ();
			
		}
	}
		
	public boolean checksAreOk () {return true;}
	
}

