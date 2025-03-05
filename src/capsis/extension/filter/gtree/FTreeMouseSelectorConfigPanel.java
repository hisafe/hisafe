/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2014  Francois de Coligny
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

package capsis.extension.filter.gtree;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.io.Serializable;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToolBar;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;

import capsis.commongui.projectmanager.ColorManager;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.gui.DialogWithOkCancel;
import capsis.gui.FilteringPanelSettings;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import capsis.util.Drawer;
import capsis.util.History;
import capsis.util.Panel2D;
import capsis.util.Pilotable;
import capsis.util.Redoable;
import capsis.util.Undoable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for tree mouse selector filter.
 * 
 * @author F. de Coligny - May 2002
 */
public class FTreeMouseSelectorConfigPanel extends ConfigurationPanel implements Undoable, Redoable, Drawer,
		ActionListener, Pilotable {
	private final static Color GREEN = new Color(0, 153, 0);
	private final static Color SELECTION_COLOR = Color.WHITE;

	private FTreeMouseSelector mum; // mummy is being configured

	// The stand must be a GScene AND a TreeCollection
	private GScene stand;
	private TreeCollection treeCollection;

	private Collection candidates;

	// protected double visibleThreshold; // things under this user size should
	// not
	// be drawn (too small on screen)

	private FilteringPanelSettings filtration;

	// ~ private JTextField threshold;

	private JTextField fLabelNumber; // fc - 23.12.2003
	private JTextField fMagnifyFactor; // fc - 23.12.2003
	private JCheckBox showDiameter;

	private int labelNumber;
	private int magnifyFactor;
	private boolean diameterMode;

	protected int labelCounter; // for trees label drawing strategy
	protected int labelFrequency;

	private JButton settingsButton;
	private JButton eraseAllButton;
	private JButton undoButton;
	private JButton redoButton;
	private JButton helpButton;

	// ~ private int pixelThreshold;

	private double minDbh;
	private double maxDbh;

	private Map<Tree, Shape> drawnTrees; // filled in draw, used in select
	// private Collection drawnTrees; // filled in draw, used in select

	private Panel2D panel2D;
	private boolean disabled;

	private NumberFormat formater;

	private History selectionHistory; // for undo / redo - fc - 2.3.2005

	/**
	 * Constructor
	 */
	protected FTreeMouseSelectorConfigPanel(Configurable configurable) {
		super(configurable);

		selectionHistory = new History(); // for undo / redo - fc - 2.3.2005

		mum = (FTreeMouseSelector) configurable;

		formater = NumberFormat.getInstance(Locale.ENGLISH);
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(2);

		labelNumber = Settings.getProperty("extension.ftreemouseselector.label.number", 50);
		magnifyFactor = Settings.getProperty("extension.ftreemouseselector.magnify.factor", 10);

		diameterMode = Settings.getProperty("extension.ftreemouseselector.diameter.mode", false);

		disabled = false;

		stand = (GScene) mum.referent;
		treeCollection = (TreeCollection) mum.referent;

		candidates = mum.candidates;

		drawnTrees = new HashMap<Tree, Shape>();
		// drawnTrees = new Vector();

		filtration = new FilteringPanelSettings();
		initFiltrationExtremes();

		Vertex3d o = stand.getOrigin();
		Rectangle.Double r = new Rectangle.Double(o.x, o.y, stand.getXSize(), stand.getYSize());
		panel2D = new Panel2D(this, r, 10, 10);

		selectionHistory.add(new HashSet(mum.treeIds)); // for undo /redo - fc -
														// 2.3.2005

		setBackground(Color.WHITE);

		setLayout(new BorderLayout());
		add(panel2D, BorderLayout.CENTER); // we want to be at CENTER

	}

	public void disablePanel() {
		disabled = true;
	}

	// Look for extreme possible values
	//
	private void initFiltrationExtremes() {

		// 1. get min and max dbh
		double userMin = Double.MAX_VALUE;
		double userMax = Double.MIN_VALUE;
		for (Iterator i = candidates.iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			double dbh = t.getDbh(); // cm.
			userMin = Math.min(userMin, dbh);
			userMax = Math.max(userMax, dbh);
		}
		filtration.init((int) userMin, (int) (userMax + 1));
	}

	/**
	 * From Pilotable interface
	 */
	public JComponent getPilot() {

		ImageIcon icon = IconLoader.getIcon("configure-project_16.png");
		settingsButton = new JButton(icon);
		Tools.setSizeExactly(settingsButton, 23, 23);
		settingsButton.setToolTipText(Translator.swap("SVSimple.settings"));
		settingsButton.addActionListener(this);

		icon = IconLoader.getIcon("cancel_16.png");
		eraseAllButton = new JButton(icon);
		Tools.setSizeExactly(eraseAllButton, 23, 23);
		eraseAllButton.setToolTipText(Translator.swap("Shared.unselectAll"));
		eraseAllButton.addActionListener(this);

		icon = IconLoader.getIcon("edit-undo_16.png");
		undoButton = new JButton(icon);
		Tools.setSizeExactly(undoButton, 23, 23);
		undoButton.setToolTipText(Translator.swap("Shared.undoCtrlZ"));
		undoButton.addActionListener(this);

		icon = IconLoader.getIcon("edit-redo_16.png");
		redoButton = new JButton(icon);
		Tools.setSizeExactly(redoButton, 23, 23);
		redoButton.setToolTipText(Translator.swap("Shared.redoCtrlR"));
		redoButton.addActionListener(this);

		icon = IconLoader.getIcon("help_16.png");
		helpButton = new JButton(icon);
		Tools.setSizeExactly(helpButton, 23, 23);
		helpButton.setToolTipText(Translator.swap("Shared.help"));
		helpButton.addActionListener(this);

		fLabelNumber = new JTextField(2);
		fLabelNumber.setText("" + labelNumber);
		fLabelNumber.setToolTipText(Translator.swap("Shared.labelNumberExplanation"));
		fLabelNumber.addActionListener(this);

		fMagnifyFactor = new JTextField(2);
		fMagnifyFactor.setText("" + magnifyFactor);
		fMagnifyFactor.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
		fMagnifyFactor.addActionListener(this);

		showDiameter = new JCheckBox(Translator.swap("Shared.diameter"), diameterMode);
		showDiameter.addActionListener(this);

		JToolBar toolbar = new JToolBar();

		LinePanel line1 = new LinePanel();
		LinePanel line2 = new LinePanel();

		line1.add(settingsButton);
		line1.add(eraseAllButton);
		line1.add(undoButton);
		line1.add(redoButton);

		JLabel l = new JLabel(Translator.swap("Shared.labelNumber") + ": ");
		l.setToolTipText(Translator.swap("Shared.labelNumberExplanation"));
		line2.add(l);
		line2.add(fLabelNumber);

		line2.add(showDiameter);

		JLabel l2 = new JLabel(Translator.swap("Shared.magnifyFactor") + ": ");
		l2.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
		line2.add(l2);
		line2.add(fMagnifyFactor);

		line2.add(helpButton);

		ColumnPanel aux = new ColumnPanel();
		aux.add(line1);
		aux.add(line2);

		toolbar.add(aux);

		toolbar.setVisible(true);

		return toolbar;

	}

	/**
	 * Drawing delegation : draws the scene in the given user space Graphics.
	 */
	public void draw(Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Preset. Choose a pixel detailThreshold, compute it in meters with
		// current scale
		// if dbh in m. >= detailThreshold -> detailled level is reached
		// ~ double detailThreshold = ((double) pixelThreshold) /
		// panel2D.getCurrentScale ().x; // in meters

		// fc - 23.12.2003
		// visibleThreshold = 1.1 / panel2D.getCurrentScale().x; // 1 pixel in
		// in
		// meters

		// 1. Draw the cells

		// Memorize Stroke and desactivate it during cells drawing
		Stroke memoStroke = g2.getStroke();
		g2.setStroke(new BasicStroke(0));

		if (stand.hasPlot() && stand.getPlot () instanceof PlotOfCells) {
			
			PlotOfCells plotc = (PlotOfCells) stand.getPlot (); // fc-30.10.2017
			
			for (Iterator i = plotc.getCells().iterator(); i.hasNext();) {
				Cell c = (Cell) i.next();

				Shape shape = c.getShape();

				// do not draw if invisible
				if (!shape.getBounds2D().intersects(r)) {
					continue;
				}

				g.setColor(disabled ? Color.gray : Color.black);
				g2.draw(shape);
			}
		}
		g2.setStroke(memoStroke);

		// 2. Draw the trees and their labels
		drawnTrees.clear();

		List copy = new ArrayList();
		copy.addAll(candidates);

		Collections.sort(copy, new DbhThenIdComparator(false));

		// Set copy = new TreeSet(new TreeDbhThenIdComparator(false)); //
		// ascending
		// copy.addAll(candidates);

		// Sort: smallest trees visible on top of the largest ones
		// -> all the trees are visible and selection is made easier
		for (Iterator i = copy.iterator(); i.hasNext();) {

			Tree t = (Tree) i.next();
			Spatialized s = (Spatialized) t;

			// Marked trees are considered dead by generic tools -> don't draw
			if (t.isMarked())
				continue;

			// Preparation for Filtering dialog
			double dbh = t.getDbh(); // cm.

			// Tree is not in considered dbh interval -> don't draw
			if ((filtration.isMinimumSet && t.getDbh() < filtration.minimumValue)
					|| (filtration.isMaximumSet && t.getDbh() > filtration.maximumValue)) {
				continue; // not drawn
			}

			// fc - 25.3.2004 - magnify trees to see better (bc request)
			double diameter = dbh / 100 * magnifyFactor; // meters
			double radius = diameter / 2;

			Shape shape = new Ellipse2D.Double(s.getX() - radius, s.getY() - radius, diameter, diameter);

			// Do not draw tree if invisible
			if (shape.intersects(r) /*
									 * || r.contains(new Point.Double(s.getX(),
									 * s.getY()))
									 */) {

				// Get 2 colors: bright and dark based on the given color
				Color[] c_one = ColorManager.get2Colors(GREEN, 0.5f);

				Color treeColor = c_one[0];
				Color borderColor = c_one[1];

				if (t instanceof SimpleCrownDescription) {
					SimpleCrownDescription scd = (SimpleCrownDescription) t;

					Color[] c_two = ColorManager.get2Colors(scd.getCrownColor(), 0.5f);

					treeColor = c_two[0];
					borderColor = c_two[1];
				}

				// See if the tree is selected
				boolean selected = mum.treeIds.contains(new Integer(t.getId()));
				if (selected)
					treeColor = toGrayScale(treeColor);

				// Draw the tree
				g.setColor(treeColor);
				g2.fill(shape);

				// Draw a border
				g.setColor(borderColor);
				g2.draw(shape); // stroke sensitive (fill is not)

				drawnTrees.put(t, shape); // memo

			}

		}

		// Prepare label drawing strategy
		if (labelNumber <= 0) {
			labelCounter = 1;
			labelFrequency = Integer.MAX_VALUE;
		} else {
			labelCounter = 0;
			labelFrequency = Math.max(1, (int) drawnTrees.size() / Math.max(1, labelNumber));
		}

		// Draw some labels
		for (Iterator i = drawnTrees.keySet().iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			Spatialized s = (Spatialized) t;

			g2.setColor(Color.BLUE);

			String label = diameterMode ? formater.format(t.getDbh()) : "" + t.getId();

			drawLabel(g2, label, (float) s.getX(), (float) s.getY());
		}

		// Relative to filtering
		updateToolTipText();
	}

	// private Color darker (Color c) {
	// float[] hsb = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(),
	// null);
	//
	// int rgb = Color.HSBtoRGB(hsb[0], hsb[1], hsb[2] * 0.5f);
	//
	// return new Color (rgb);
	//
	// return DefaultColorProvider.getLighterColor (c);
	// }

	// Draws a label for the given tree
	// Implements a labels restriction strategy (see Draw (), very long to draw
	// if numerous)
	// fc - 23.12.2003
	//
	protected void drawLabel(Graphics2D g2, String label, float x, float y) {
		if (labelCounter % labelFrequency == 0) {
			labelCounter = 0;
			g2.drawString(label, x, y);
		}
		labelCounter++;
	}

	/**
	 * Returns true if the suer double-clicked for selection (false if the
	 * rectangle is bigger than the double-click selection rectangle).
	 */
	private boolean isDoubleClickSelection(Rectangle.Double r) {
		int size_pixel = panel2D.getSettings().getSelectionSquareSize();
		int rWidth_pixel = panel2D.getPixelWidth(r.width);

		return rWidth_pixel <= size_pixel; // ok, fc-31.10.2014

	}

	/**
	 * Turns the given color into a gray scale color.
	 */
	private Color toGrayScale(Color color) {

		// fc-28.4.2014
		float[] rgb = color.getRGBColorComponents(null);

		// Works with three colors only -> 3 very contrasted grays
		float gray = 0;
		if (rgb[0] > rgb[1] && rgb[0] > rgb[2]) { // more red, other species
			gray = 0.8f; // clear gray
		} else if (rgb[1] > rgb[0] && rgb[1] > rgb[2]) { // more green, spruce
			gray = 0.4f; // 0.0f; //mid gray
		} else { // more blue, fir
			gray = 0.0f; // 0.4f; // black
		}

		// // Comission internationale de l'éclairage, recommandation 709,
		// couleurs 'vraies' ou
		// // 'naturelles' (wikipedia)
		// float gray = (float) (0.2125 * rgb[0] + 0.7154 * rgb[1] + 0.0721 *
		// rgb[2]);

		// // Comission internationale de l'éclairage, recommandation 601,
		// couleurs 'non linéaires'
		// // (wikipedia)
		// float gray = (float) (0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 *
		// rgb[2]);

		return new Color(gray, gray, gray);

	}

	/**
	 * Adds / removes the ids of the selected / deselected trees in mum's
	 * selection list. Returns a JPanel if selection done with Ctrl-selection,
	 * null otherwise.
	 */
	public JPanel select(Rectangle.Double r, boolean ctrlIsDown) {
		// Panel may be disabled -> do no selection
		if (disabled)
			return null;

		if (drawnTrees == null)
			return null;

		Collection treesToBeInspected = new ArrayList();
		
		// Trees must be sorted from bigger dbh to lower for correct double
		// click selection

		List sortedDrawnTrees = new ArrayList();
		sortedDrawnTrees.addAll(drawnTrees.keySet());

		Collections.sort(sortedDrawnTrees, new DbhThenIdComparator(true));

		for (Iterator i = sortedDrawnTrees.iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
//			Spatialized s = (Spatialized) t;
			
			int selectedId = t.getId();
			Shape selectedShape = drawnTrees.get(t);
			
//			double w = t.getDbh() / 100;
//			Shape shape = new Ellipse2D.Double(s.getX() - w / 2, s.getY() - w / 2, w, w);


			// If selected -> deselection (& vice versa)
			if (selectedShape.intersects(r)) {
				// if (selectedShape.getBounds2D().intersects(r) ||
				// r.contains(new Point.Double(selectedX, selectedY))) {
				if (ctrlIsDown) {
					treesToBeInspected.add(t);
				} else {

					// swap selection status
					if (mum.treeIds.contains(new Integer(selectedId))) {
						mum.treeIds.remove(new Integer(selectedId));
					} else {
						mum.treeIds.add(new Integer(selectedId));
					}

				}
				if (isDoubleClickSelection(r)) {
					break; // single tree selected only on double click
				}
			}
			
//			// if selected -> deselection (& vice versa)
//			if (shape.getBounds2D().intersects(r) || r.contains(new Point.Double(s.getX(), s.getY()))) {
//				if (ctrlIsDown) {
//					treesToBeInspected.add(t);
//				} else {
//
//					// swap selection status
//					if (mum.treeIds.contains(new Integer(t.getId()))) {
//						mum.treeIds.remove(new Integer(t.getId()));
//					} else {
//						mum.treeIds.add(new Integer(t.getId()));
//					}
//
//				}
//			}
			
		}

		selectionHistory.add(new HashSet(mum.treeIds)); // for undo /redo - fc -
														// 2.3.2005

		panel2D.reset();
		panel2D.repaint();

		if (treesToBeInspected.isEmpty()) {
			return null; // no JPanel returned
		} else {
			return AmapTools.createInspectorPanel(treesToBeInspected); // an
																		// inspector
																		// for
																		// the
																		// concerned
																		// trees
		}
	}

	public void undo() {

		if (selectionHistory != null && selectionHistory.canBack()) {
			mum.treeIds = new HashSet((Set) selectionHistory.back());
			panel2D.reset();
			panel2D.repaint();
		}

	}

	public void redo() {

		if (selectionHistory != null && selectionHistory.canNext()) {
			mum.treeIds = new HashSet((Set) selectionHistory.next());
			panel2D.reset();
			panel2D.repaint();
		}

	}

	/**
	 * Events processing
	 */
	public void actionPerformed(ActionEvent evt) {
		// disabled panel -> no action
		if (disabled)
			return;

		if (evt.getSource().equals(settingsButton)) {

			ConfigurationPanel p = panel2D.getConfigPanel(null);
			DialogWithOkCancel dlg = new DialogWithOkCancel(p);
			if (dlg.isValidDialog()) {
				panel2D.applyConfig(p);
			}
			dlg.dispose();

		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);

		} else if (evt.getSource().equals(eraseAllButton)) {

			if (Question.ask(MainFrame.getInstance(), Translator.swap("Shared.confirm"),
					Translator.swap("Shared.unselectAll"))) {
				mum.treeIds.clear();

				selectionHistory.add(new HashSet(mum.treeIds)); // for undo
																// /redo - fc -
																// 2.3.2005

				panel2D.reset();
				panel2D.repaint();
			}

		} else if (evt.getSource().equals(undoButton)) {
			undo();

		} else if (evt.getSource().equals(redoButton)) {
			redo();

		} else if (evt.getSource().equals(fLabelNumber)) {
			try {
				String t = fLabelNumber.getText().trim();
				labelNumber = new Integer(t).intValue();
				Settings.setProperty("extension.ftreemouseselector.label.number", "" + labelNumber);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.labelNumberMustBeAnInteger"));
				return;
			}

		} else if (evt.getSource().equals(fMagnifyFactor)) {
			try {
				String t = fMagnifyFactor.getText().trim();
				magnifyFactor = new Integer(t).intValue();
				Settings.setProperty("extension.ftreemouseselector.magnify.factor", "" + magnifyFactor);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

		} else if (evt.getSource().equals(showDiameter)) {
			try {
				diameterMode = showDiameter.isSelected();
				Settings.setProperty("extension.ftreemouseselector.diameter.mode", "" + diameterMode);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

			// ~ } else if (evt.getSource ().equals (threshold)) {
			// ~ try {
			// ~ String t = threshold.getText ().trim ();
			// ~ pixelThreshold = new Integer (t).intValue ();
			// ~ Settings.setProperty
			// ("extension.ftreemouseselector.pixel.threshold",
			// ""+pixelThreshold);
			// ~ panel2D.reset ();
			// ~ panel2D.repaint ();

			// ~ } catch (Exception e) {
			// ~ MessageDialog.promptError (Translator.swap
			// ("FTreeMouseSelectorConfigPanel.pixelThresholdMustBeAnInteger"));
			// ~ return;
			// ~ }

		}
	}

	/**
	 * Creates a tool tip text on panel2D to inform user if some filtering is
	 * currently active.
	 */
	protected void updateToolTipText() {
		String toolTipText = "";

		if (filtration.isDetailSet) {
			toolTipText += "dDet=" + filtration.detailValue + "p. ";
		}
		if (filtration.isMinimumSet) {
			toolTipText += "dMin=" + filtration.minimumValue + "cm. ";
		}
		if (filtration.isMaximumSet) {
			toolTipText += "dMax=" + filtration.maximumValue + "cm. ";
		}
		if (toolTipText.trim().length() == 0) {
			toolTipText = Translator.swap("Shared.noActiveFilters");
		}

		panel2D.setToolTipText(toolTipText);
	}

	/**
	 * From ConfigurationPanel No possible errors (only clicks on trees)
	 */
	public boolean checksAreOk() {

		// fc - 21.5.2003 - this method is called if config panel is validated
		// by ok
		// -> consider the intersection of the selected treeIds and the
		// filtrable treeIds
		// because : filter may contain ids (chosen at creation time) that are
		// not in
		// the present filtrable (at modification time) -> remove them on ok
		// only
		//
		mum.treeIds.retainAll(treeCollection.getTreeIds());

		return true;
	}

	// Inner comparator, knows Samsa2Tree and Samsa2CedayedTree
	private static class DbhThenIdComparator implements Comparator, Serializable {

		boolean ascending; // fc - 23.9.2004 - added ascending / descending

		public DbhThenIdComparator(boolean a) {
			ascending = a;
		}

		public DbhThenIdComparator() {
			this(true);
		}

		public int compare(Object o1, Object o2) throws ClassCastException {
			double d1 = 0;
			double d2 = 0;
			int i1 = 0;
			int i2 = 0;

			if (o1 instanceof Tree) {
				d1 = ((Tree) o1).getDbh();
				i1 = ((Tree) o1).getId();

			} else {
				throw new ClassCastException("o1 should be a Samsa2Tree or a Samsa2DecayedTree");
			}

			if (o2 instanceof Tree) {
				d2 = ((Tree) o2).getDbh();
				i2 = ((Tree) o2).getId();

			} else {
				throw new ClassCastException("o2 should be a Samsa2Tree or a Samsa2DecayedTree");
			}

			if (d1 < d2) {
				return ascending ? -1 : 1; // t1 < t2
			} else if (d1 > d2) {
				return ascending ? 1 : -1; // t1 > t2
			} else {

				if (i1 < i2) {
					return ascending ? -1 : 1; // t1 < t2
				} else if (i1 > i2) {
					return ascending ? 1 : -1; // t1 > t2
				} else {
					return 0; // t1 == t2
				}
			}
		}

	}

}
