/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.objectviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColoredButton;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.SpatializedList;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.Neighbour;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.RoundMask;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.defaulttype.plotofcells.SquareCellHolder;
import capsis.extension.AbstractObjectViewer;
import capsis.kernel.GScene;
import capsis.util.Drawer;
import capsis.util.Panel2D;

/**
 * A viewer for testing the Capsis squareCell based masks, to get trees or cells around a point
 * / cell / tree with or without torus
 *
 * @author F. de Coligny - October 2008
 */
public class MaskTesterForCells extends AbstractObjectViewer implements Drawer, ActionListener,
// ~ SelectionListener, Disposable {
		Disposable {

	static {
		Translator.addBundle("capsis.extension.objectviewer.MaskTesterForCells");
	}
	
	// nb-07.08.2018
	//public static final String NAME = Translator.swap("MaskTesterForCells");
	//public static final String DESCRIPTION = Translator.swap("MaskTesterForCells.description");
	//static public final String AUTHOR = "F. de Coligny";
	//static public final String VERSION = "1.0";

	public static final int X_MARGIN_IN_PIXELS = 10;
	public static final int Y_MARGIN_IN_PIXELS = 10;

	public static final Color SELECTION_CIRCLE_COLOR = Color.BLACK;
	public static final Color CANDIDATE_CELL_COLOR = new Color(153, 255, 153); // ligh
																				// green
	public static final Color SELECTED_CELL_COLOR = Color.ORANGE;
	public static final Color SELECTED_TREE_COLOR = Color.BLUE;
	public static final Color NEUTRAL_COLOR = Color.GRAY;

	// ~ private Dimension colorButtonDimension = new Dimension (30, 10);
	private JButton help;

	private GScene stand;
	private Collection squareCells;

	private Collection selection; // one cell
	private Collection inMask; // cells or trees
	private Collection<Neighbour> inNeighbours; // cells matching all the mask
												// sites - fc - 10.10.2008

	private Panel2D panel2D;
	private JScrollPane scroll;

	private Container normalPanel; // fc - 11.2.2008
	private Container securityPanel; // fc - 11.2.2008

	private Vertex2d selectedVertex;
	private JTextField maskRadius; // m
	private JCheckBox torusEnabled;
	private JRadioButton cellMask;
	private JRadioButton partilallyIncluded;
	private JRadioButton centerIncluded;
	private JRadioButton completelyIncluded;
	private JRadioButton treeMask;
	private ButtonGroup group;
	private ButtonGroup group2;
	private JTextField numberInMask;

	/**
	 * Constructor.
	 */
	public MaskTesterForCells() {
	}

	@Override
	public void init(Collection s) throws Exception {

		try {
			// fc - 8.2.2008

			selection = new HashSet();
			inMask = new HashSet();
			inNeighbours = new ArrayList<Neighbour>();

			extractSquareCells(s);

			// ~ try {
			// ~ factor = Check.doubleValue (Settings.getProperty
			// ("parentage.viewer.magnify.factor"), null);
			// ~ } catch (Exception e) {} // may be not found

			// ~ setTitle (Translator.swap ("MaskTesterForCells.title")); // fc -
			// 13.6.2003
			// ~ traceFij (); // fc + som - 14.12.2006

			createUI();
			calculatePanel2D();

		} catch (Exception exc) {
			Log.println(Log.ERROR, "MaskTesterForCells.c ()", exc.toString(), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			// fc - 11.2.2008 - referent is now always a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty()) {
				return false;
			}

			// Find representative objects (ie with different classes)
			Collection reps = Tools.getRepresentatives(c);
			for (Iterator i = reps.iterator(); i.hasNext();) {
				Object candidate = i.next();
				if (candidate instanceof SquareCell) {
					return true;
				} // if at least one square cell, ok
			}
			return false;
		} catch (Exception e) {
			Log.println(Log.ERROR, "MaskTesterForCells.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("MaskTesterForCells.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MaskTesterForCells.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	// call extractSquareCells before this method
	private void calculatePanel2D() {
		Vertex3d origin = stand.getOrigin();
		double userWidth = stand.getXSize();
		double userHeight = stand.getYSize();

		Rectangle.Double r2 = new Rectangle.Double(origin.x, origin.y, userWidth, userHeight); // x,
																								// y,
																								// w,
																								// h

		panel2D = new Panel2D(this, // when repaint needed, panel2D will call
									// this.draw ()
				r2, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS, false);
		scroll.getViewport().setView(panel2D);
	}

	// fc - 8.2.2008 - OVs now always manage a Collection
	private Collection extractSquareCells(Collection subjects) throws Exception {
		squareCells = new ArrayList();
		if (subjects == null || subjects.isEmpty()) {
			return squareCells;
		}

		//
		for (Object o : subjects) {
			if (o instanceof Cell) {
				Cell c = (Cell) o;

				PlotOfCells plot = c.getPlot(); // fc-26.10.2017

				stand = plot.getScene();
				Collection cells = plot.getCells();
				for (Object o2 : cells) {
					if (o2 instanceof SquareCell) {
						squareCells.add(o2);
					}
				}
				break;
			}
		}
		return squareCells;
	}

	private void update() {

		if (panel2D != null) {
			panel2D.reset();
			panel2D.repaint();
		}
	}

	/**
	 * Disposable
	 */
	public void dispose() {
	}

	/**
	 * In case of trouble, show a security panel
	 */
	public void security() {
		securityPanel = new JPanel();
		securityPanel.add(new JLabel(Translator.swap("MaskTesterForCells.canNotUpdate")));

		removeAll();
		add(securityPanel); // fc - 11.2.2008

	}

	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed(ActionEvent evt) {
		synchro();

		update();

		// ~ if (evt.getSource ().equals (magnifyFactor)) {

		// ~ if (!Check.isDouble (magnifyFactor.getText ().trim ())) {
		// ~ MessageDialog.promptError (Translator.swap
		// ("MaskTesterForCells.magnifyFactorMustBeADouble"));
		// ~ return;
		// ~ }
		// ~ factor = Check.doubleValue (magnifyFactor.getText ().trim ());
		// ~ Settings.setProperty ("parentage.viewer.magnify.factor",
		// ""+factor);
		// ~ panel2D.reset ();
		// ~ panel2D.repaint ();

		// ~ } else if (evt.getSource ().equals (hideOtherProgeny)) {
		// ~ panel2D.reset ();
		// ~ panel2D.repaint ();

		// ~ } else if (evt.getSource ().equals (help)) {
		// ~ Helper.helpFor (this);
		// ~ }
	}

	/**
	 * From Drawer interface. This method draws in the Panel2D each time this
	 * one must be repainted. The given Rectangle is the sub-part of the object
	 * to draw (zoom) in user coordinates (i.e. meters...). It can be used in
	 * preprocesses to avoid drawing invisible parts.
	 */
	public void draw(Graphics g, Rectangle.Double r) {

		Graphics2D g2 = (Graphics2D) g;

		if (selectedVertex != null) {
			selection.clear();
			Vertex2d v2 = selectedVertex;
			for (Object o : squareCells) {
				SquareCell cell = (SquareCell) o;
				if (cell.getShape().contains(v2.x, v2.y)) {
					selection.add(cell); // only one cell in selection
					// Run a neighbourhood mask on the selected cell
					double maskRadius = Check.doubleValue(this.maskRadius.getText().trim());
					boolean torusEnabled = this.torusEnabled.isSelected();

					SquareCellHolder holder = null;
					if (cell.getPlot() instanceof SquareCellHolder) {
						holder = (SquareCellHolder) cell.getPlot();
					} else if (cell.getMother() != null && cell.getMother() instanceof SquareCellHolder) {
						holder = (SquareCellHolder) cell.getMother();
					}

					RoundMask mask = new RoundMask(holder, maskRadius, torusEnabled);

					// to show the neighbours (i.e. cells matching the mask
					// sites) - fc - 10.10.2008
					boolean onlyCellsWithTreesInside = false;
					inNeighbours = mask.getNeighbours(cell, onlyCellsWithTreesInside);

					if (cellMask.isSelected()) { // cells
						byte criterion = RoundMask.PARTIALLY_INCLUDED;
						if (centerIncluded.isSelected()) {
							criterion = RoundMask.CENTER_INCLUDED;
						} else if (completelyIncluded.isSelected()) {
							criterion = RoundMask.COMPLETELY_INCLUDED;
						}
						inMask = mask.getCellsNear(v2.x, v2.y, criterion);
						break;
					} else { // trees
						inMask = mask.getTreesNear(v2.x, v2.y);
						break;

					}
				}
			}
		}

		// fc - 12.10.2007 - if cells, draw them
		for (Iterator i = squareCells.iterator(); i.hasNext();) {
			Cell cell = (Cell) i.next();
			drawCell(g2, cell, r);
		}

		// If trees, draw them
		// -> check TreeList and SPatializedList
		List<Spatialized> trees = new ArrayList();

		if (stand instanceof TreeList) {
			for (Object o2 : ((TreeList) stand).getTrees()) {
				if (!(o2 instanceof Spatialized)) {
					continue;
				}
				trees.add((Spatialized) o2);
			}
		}

		if (stand instanceof SpatializedList) {
			for (Spatialized o3 : ((SpatializedList) stand).getSpatializeds()) {
				trees.add(o3);
			}
		}

		for (Spatialized t : trees) {
			drawTree(g2, t, r);
		}

		if (selectedVertex != null) {
			// Draw the selection circle
			double maskRadius = Check.doubleValue(this.maskRadius.getText().trim());
			Ellipse2D.Double sh = new Ellipse2D.Double(selectedVertex.x - maskRadius, selectedVertex.y - maskRadius,
					2 * maskRadius, 2 * maskRadius);
			Rectangle2D bBox = sh.getBounds2D();
			if (r.intersects(bBox)) {
				g2.setColor(SELECTION_CIRCLE_COLOR);
				g2.draw(sh);
			}

			// Draw the center of circle
			Vertex2d v2 = selectedVertex;
			double t = 1; // m.
			Line2D.Double sh3 = new Line2D.Double(v2.x - t, v2.y - t, v2.x + t, v2.y + t);
			Line2D.Double sh4 = new Line2D.Double(v2.x - t, v2.y + t, v2.x + t, v2.y - t);
			g2.setColor(SELECTION_CIRCLE_COLOR);
			g2.draw(sh3);
			g2.draw(sh4);
		}

		numberInMask.setText("" + inMask.size());
		
		
		Settings.setProperty("MaskTesterForCells.maskRadius", Check.doubleValue(this.maskRadius.getText().trim())); // fc-19.3.2018
		Settings.setProperty("MaskTesterForCells.torusEnabled", torusEnabled.isSelected ()); // fc-19.3.2018
		Settings.setProperty("MaskTesterForCells.treeMask", treeMask.isSelected ()); // fc-19.3.2018

	}

	/**
	 * Method to draw a Spatialized GTree within this viewer.
	 */
	public void drawTree(Graphics2D g2, Spatialized spa, Rectangle.Double r) {
		double dbh = 20; // cm
		if (spa instanceof Tree) {
			Tree t = (Tree) spa;
			dbh = t.getDbh();
			if (dbh < 20) {
				dbh = 20;
			} // at least 20 cm for drawing
		}
		double radius = dbh / 100 / 2;
		Ellipse2D.Double sh = new Ellipse2D.Double(spa.getX() - radius, spa.getY() - radius, 2 * radius, 2 * radius);
		Rectangle2D bBox = sh.getBounds2D();
		if (r.intersects(bBox)) {
			if (inMask.contains(spa)) {
				g2.setColor(SELECTED_TREE_COLOR);
				g2.fill(sh);

				double r2 = radius * 2;
				Ellipse2D.Double sh2 = new Ellipse2D.Double(spa.getX() - r2, spa.getY() - r2, 2 * r2, 2 * r2);
				g2.setColor(SELECTED_TREE_COLOR);
				g2.draw(sh2);

			} else {
				g2.setColor(NEUTRAL_COLOR);
				g2.draw(sh);
			}
		}

	}

	/**
	 * Method to draw a GCell within this viewer.
	 */
	public void drawCell(Graphics2D g2, Cell cell, Rectangle.Double r) {

		Shape sh = cell.getShape();
		Rectangle2D bBox = sh.getBounds2D();
		if (r.intersects(bBox)) {
			for (Neighbour n : inNeighbours) {
				if (n.getCell().equals(cell)) {
					g2.setColor(CANDIDATE_CELL_COLOR); // ligh green
					g2.fill(sh);
					break;
				}
			}
			if (inMask.contains(cell)) {
				g2.setColor(SELECTED_CELL_COLOR);
				g2.fill(sh);
			}
			g2.setColor(NEUTRAL_COLOR);
			g2.draw(sh);
		}

	}

	private boolean check() {
		if (!Check.isDouble(maskRadius.getText().trim())) {
			MessageDialog.print(this, Translator.swap("MaskTesterForCells.maskRadiusMustBeADouble"));
			return false;
		}

		return true;
	}

	/**
	 * From Drawer interface. We may receive (from Panel2D) a selection
	 * rectangle (in user space i.e. meters) and return a JPanel containing
	 * information about the objects (trees) inside the rectangle. If no objects
	 * are found in the rectangle, return null.
	 */
	public JPanel select(Rectangle.Double r, boolean ctrlIsDown) {
		if (!check()) {
			return null;
		}

		selectedVertex = new Vertex2d(r.getCenterX(), r.getCenterY());

		panel2D.reset();
		panel2D.repaint();

		return null;
	}

	@Override
	public Collection show(Collection candidateSelection, boolean thisIsAReselection) {
		try {
			realSelection = extractSquareCells(candidateSelection);

			calculatePanel2D();
			update();
			return realSelection;
		} catch (Exception e) {
			Log.println(Log.ERROR, "MaskTesterForCells.show ()", "Could not show candidate selection", e);
			return Collections.EMPTY_LIST;
		}
	}

	/**
	 * Synchronize gui components according to radio buttons selections.
	 */
	private void synchro() {
		partilallyIncluded.setEnabled(cellMask.isSelected());
		centerIncluded.setEnabled(cellMask.isSelected());
		completelyIncluded.setEnabled(cellMask.isSelected());
	}

	/**
	 * User interface definition.
	 */
	private void createUI() {
		this.setLayout(new BorderLayout());

		JPanel part1 = new JPanel(new BorderLayout());
		scroll = new JScrollPane();

		// Do not set sizes explicitly inside object viewers
		// ~ scroll.setPreferredSize (new Dimension (500, 400));
		scroll.setMinimumSize(new Dimension(100, 100));

		part1.add(scroll, BorderLayout.CENTER);

		ColumnPanel lateral = new ColumnPanel(0, 0);

		LinePanel l5 = new LinePanel();
		l5.add(new JLabel(Translator.swap("MaskTesterForCells.selectInTheSceneWithTheMouse")));
		l5.addGlue();
		lateral.add(l5);

		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("MaskTesterForCells.maskRadius") + " : ", 80));
		maskRadius = new JTextField();
		maskRadius.setText(""+Settings.getProperty("MaskTesterForCells.maskRadius", 3d)); // fc-19.3.2018
		maskRadius.addActionListener(this);
		l1.add(maskRadius);
		l1.addStrut0();
		lateral.add(l1);

		LinePanel l2 = new LinePanel();
		torusEnabled = new JCheckBox(Translator.swap("MaskTesterForCells.torusEnabled"), true);
		torusEnabled.setSelected(Settings.getProperty("MaskTesterForCells.torusEnabled", true)); // fc-19.3.2018
		torusEnabled.addActionListener(this);
		l2.add(torusEnabled);
		l2.addGlue();
		lateral.add(l2);

		LinePanel l3 = new LinePanel();
		l3.add(new JWidthLabel(Translator.swap("MaskTesterForCells.select") + " ", 110));
		treeMask = new JRadioButton(Translator.swap("MaskTesterForCells.treeMask"));
		treeMask.addActionListener(this);
		l3.add(treeMask);
		l3.addGlue();
		lateral.add(l3);

		LinePanel l4 = new LinePanel();
		l4.add(new JWidthLabel("", 110));
		cellMask = new JRadioButton(Translator.swap("MaskTesterForCells.cellMask"));
		cellMask.addActionListener(this);
		l4.add(cellMask);
		l4.addGlue();
		lateral.add(l4);

		group = new ButtonGroup();
		group.add(cellMask);
		group.add(treeMask);
		cellMask.setSelected(true);
		
		treeMask.setSelected(Settings.getProperty("MaskTesterForCells.treeMask", true)); // fc-19.3.2018
		cellMask.setSelected(!treeMask.isSelected ()); // fc-19.3.2018

		LinePanel l7 = new LinePanel();
		l7.add(new JWidthLabel("", 50));
		partilallyIncluded = new JRadioButton(Translator.swap("MaskTesterForCells.partilallyIncluded"));
		partilallyIncluded.addActionListener(this);
		l7.add(partilallyIncluded);
		l7.addGlue();
		lateral.add(l7);

		LinePanel l8 = new LinePanel();
		l8.add(new JWidthLabel("", 50));
		centerIncluded = new JRadioButton(Translator.swap("MaskTesterForCells.centerIncluded"));
		centerIncluded.addActionListener(this);
		l8.add(centerIncluded);
		l8.addGlue();
		lateral.add(l8);

		LinePanel l9 = new LinePanel();
		l9.add(new JWidthLabel("", 50));
		completelyIncluded = new JRadioButton(Translator.swap("MaskTesterForCells.completelyIncluded"));
		completelyIncluded.addActionListener(this);
		l9.add(completelyIncluded);
		l9.addGlue();
		lateral.add(l9);

		group2 = new ButtonGroup();
		group2.add(partilallyIncluded);
		group2.add(centerIncluded);
		group2.add(completelyIncluded);
		partilallyIncluded.setSelected(true);

		LinePanel l10 = new LinePanel();
		JButton selectionCircle = new ColoredButton(SELECTION_CIRCLE_COLOR);
		l10.add(selectionCircle);
		l10.add(new JLabel(Translator.swap("MaskTesterForCells.selectionCircleColor")));
		l10.addGlue();
		lateral.add(l10);

		LinePanel l11 = new LinePanel();
		JButton cellButton1 = new ColoredButton(CANDIDATE_CELL_COLOR);
		l11.add(cellButton1);
		l11.add(new JLabel(Translator.swap("MaskTesterForCells.candidateCellColor")));
		l11.addGlue();
		lateral.add(l11);

		LinePanel l12 = new LinePanel();
		JButton cellButton2 = new ColoredButton(SELECTED_CELL_COLOR);
		l12.add(cellButton2);
		l12.add(new JLabel(Translator.swap("MaskTesterForCells.selectedCellColor")));
		l12.addGlue();
		lateral.add(l12);

		LinePanel l13 = new LinePanel();
		JButton treeButton = new ColoredButton(SELECTED_TREE_COLOR);
		l13.add(treeButton);
		l13.add(new JLabel(Translator.swap("MaskTesterForCells.selectedTreeColor")));
		l13.addGlue();
		lateral.add(l13);

		LinePanel l14 = new LinePanel();
		l14.add(new JLabel(Translator.swap("MaskTesterForCells.numberInMask") + " : "));
		numberInMask = new JTextField(5);
		l14.add(numberInMask);
		l14.addStrut0();
		lateral.add(l14);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(lateral, BorderLayout.NORTH);

		// 2. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		help = new JButton(Translator.swap("MaskTesterForCells.help"));
		help.addActionListener(this);
		pControl.add(help);

		// Layout parts
		normalPanel = new JPanel(new BorderLayout());
		normalPanel.add(part1, BorderLayout.CENTER);
		normalPanel.add(aux, BorderLayout.EAST);
		normalPanel.add(pControl, BorderLayout.SOUTH);

		setLayout(new GridLayout(1, 1));
		add(normalPanel); // fc - 11.2.2008

		synchro();

	}

}
