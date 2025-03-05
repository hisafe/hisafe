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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JToolBar;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.extensiontype.GrouperDisplay;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import capsis.util.Pilotable;
import capsis.util.SmartFlowLayout;
import capsis.util.TreeDbhThenIdComparator;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;

/**
 * MaddDisplay is a display for groupers on spatially explicit stands.
 * 
 * @author F. de Coligny - October 2004
 */
public class MaddDisplay extends JPanel implements GrouperDisplay, Drawer, ActionListener, Pilotable {

	static {
		Translator.addBundle("capsis.extension.grouperdisplay.MaddDisplay");
	}
	
	// nb-07.08.2018
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR = "F. de Coligny";

	private final Color GREEN = new Color(0, 153, 0);
	private final Color RED = Color.RED;
	private final Color YELLOW = new Color(255, 255, 200);
	private NumberFormat formater;

	private Panel2D panel2D;
	private Rectangle.Double initialUserBounds;

	private JTextField fLabelNumber;
	private int labelNumber;
	protected int labelCounter; // for trees label drawing strategy
	protected int labelFrequency;

	private Collection drawnTrees; // filled in draw, used in select

	private JTextField fMagnifyFactor; // fc - 23.12.2003
	private JCheckBox showDiameter;
	private int magnifyFactor;
	private boolean diameterMode;
	
	private JLabel selectionNumber;

	private GScene stand;
	private Grouper grouper;

	/**
	 * Default constructor.
	 */
	public MaddDisplay() {

		labelNumber = Settings.getProperty(
				"extension.madd.display.label.number", 20);
		magnifyFactor = Settings.getProperty(
				"extension.madd.display.magnify.factor", 10);
		diameterMode = Settings.getProperty(
				"extension.madd.display.diameter.mode", false);

		formater = NumberFormat.getInstance(Locale.ENGLISH);
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(2);
	}

	/**
	 * Referent is a couple of objects : Object[] typeAndStand = (Objet[])
	 * referent;
	 */
	static public boolean matchWith(Object referent) {
		try {
			Object[] typeAndStand = (Object[]) referent;
			
			// fc-29.8.2019 type is now a GroupableType instance
			GroupableType type = (GroupableType) typeAndStand[0];
//			String type = (String) typeAndStand[0];
			GScene stand = (GScene) typeAndStand[1];

			// Type is TreeList.GROUP_ALIVE_TREE
			if (!type.equals(TreeList.GROUP_ALIVE_TREE)) {
//			if (!type.equals(TreeList.GROUP_ALIVE_TREE)) {
				return false;
			}

			// Stand is a non empty TreeCollection with Spatialized trees inside
			if (!(stand instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) stand;
			if (tc.getTrees().isEmpty()) {
				return false;
			}
			Collection reps = Tools.getRepresentatives(tc.getTrees()); // one
																		// instance
																		// of
																		// each
																		// class
			for (Iterator i = reps.iterator(); i.hasNext();) {
				if (!(i.next() instanceof Spatialized)) {
					return false;
				}
			}
			return true;

		} catch (Exception e) {
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("MaddDisplay.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MaddDisplay.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**
	 * Update the display on given stand, according to the given grouper
	 */
	public void update(GScene stand, Grouper grouper) {
		if (this.stand == null || !this.stand.equals(stand)) {
			standChanged(stand);
		}
		this.grouper = grouper;

		panel2D.reset(); // triggers display update
		panel2D.repaint();
	}

	/**
	 * Stand has changed : re initialize panel2D and trigger a repaint ().
	 */
	private void standChanged(GScene stand) {
		this.stand = stand;

		Vertex3d o = stand.getOrigin();
		initialUserBounds = new Rectangle.Double(o.x, o.y, stand.getXSize(),
				stand.getYSize());

		setLayout(new SmartFlowLayout());
		panel2D = new Panel2D(this, initialUserBounds);
		panel2D.setBackground(YELLOW);
		drawnTrees = new HashSet();

		// ~ panel2D.setPreferredSize (new Dimension (DISPLAY_WIDTH,
		// DISPLAY_WIDTH));

		removeAll();
		setLayout(new BorderLayout());
		add(panel2D, BorderLayout.CENTER); // we want to be at CENTER
		add(getPilot(), BorderLayout.SOUTH);
	}

	/**
	 * From Pilotable interface.
	 */
	public JComponent getPilot() {
		JToolBar toolbar = new JToolBar();

		// ~ ImageIcon icon = new IconLoader ().getIcon ("zoom-out_16.png");
		// ~ filteringButton = new JButton (icon);
		// ~ Tools.setSizeExactly (filteringButton, 23, 23);
		// ~ filteringButton.setToolTipText (Translator.swap
		// ("Shared.filtering"));
		// ~ filteringButton.addActionListener (this);
		// ~ toolbar.add (filteringButton);

		fLabelNumber = new JTextField(2);
		fLabelNumber.setText("" + labelNumber);
		fLabelNumber.setToolTipText(Translator
				.swap("Shared.labelNumberExplanation"));
		fLabelNumber.addActionListener(this);

		fMagnifyFactor = new JTextField(2);
		fMagnifyFactor.setText("" + magnifyFactor);
		fMagnifyFactor.setToolTipText(Translator
				.swap("Shared.magnifyFactorExplanation"));
		fMagnifyFactor.addActionListener(this);

		showDiameter = new JCheckBox(Translator.swap("Shared.diameter"),
				diameterMode);
		showDiameter.addActionListener(this);

		JLabel l = new JLabel(Translator.swap("Shared.labelNumber") + ": ");
		l.setToolTipText(Translator.swap("Shared.labelNumberExplanation"));
		toolbar.add(l);
		toolbar.add(fLabelNumber);
		toolbar.addSeparator();
		
		toolbar.add(showDiameter);
		toolbar.addSeparator();
		
		JLabel l2 = new JLabel(Translator.swap("Shared.magnifyFactor") + ": ");
		l2.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
		toolbar.add(l2);
		toolbar.add(fMagnifyFactor);
		toolbar.addSeparator();
		
		selectionNumber = new JLabel ();
		toolbar.add(selectionNumber);

		toolbar.setVisible(true);
		toolbar.setFloatable (false);
		return toolbar;
	}

	/**
	 * From Drawer interface.
	 */
	public void draw(Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		TreeCollection tc = (TreeCollection) stand; // fc - 17.9.2004

		Collection trees = tc.getTrees();
		drawnTrees.clear(); // we're gonna fill it now
		Collection selectedTrees = new HashSet();
		if (grouper != null) {
			selectedTrees = grouper.apply(trees); // not = false
			// ~ selectedTrees = new HashSet (selectedTrees); // fast on
			// contains ()
		}
		// ~ Vertex3d o = s.getOrigin (); // origin (bottom left)

		// Draw the cells
		Plot p = stand.getPlot();
		if (p != null && p instanceof PlotOfCells) { // there may be no plot

			PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017
			
			for (Iterator i = plotc.getCells().iterator(); i.hasNext();) {
				Cell cell = (Cell) i.next();
				Shape shape = cell.getShape();

				// do not draw if invisible
				if (!shape.getBounds2D().intersects(r)) {
					continue;
				}
				g.setColor(Color.BLACK);
				g2.draw(shape);
			}
		}

		// Draw the trees
		boolean selected = false;

		Set copy = new TreeSet (new TreeDbhThenIdComparator(false)); // ascending = false
		copy.addAll (trees);
	
		for (Iterator i = copy.iterator(); i.hasNext();) { // sort: smallest trees visible on top of the largest ones
			Tree t = (Tree) i.next();
			Spatialized s = (Spatialized) t;

			// Marked trees are considered dead by generic tools -> don't draw
			if (t.isMarked()) {
				continue;
			}

			selected = false;

			// Preparation for Filtering dialog
			double dbh = t.getDbh(); // cm.
			if (dbh <= 0) {
				dbh = 1;
			} // fc - 5.10.2007 - Prunus dbh is 0, bsd used instead

			if (selectedTrees.contains(t)) {
				selected = true;
			}

			// fc - 25.3.2004 - magnify trees to see better (bc request)
			//
			double diameter = dbh / 100 * magnifyFactor; // meters *
															// magnifyFactor
			double radius = diameter / 2;
			Shape shape = new Ellipse2D.Double(s.getX() - radius, s.getY()
					- radius, diameter, diameter);

			// Do not draw tree if invisible
			if (shape.getBounds2D().intersects(r)
					|| r.contains(new Point.Double(s.getX(), s.getY()))) {
				
				g.setColor (YELLOW);
				g2.fill(shape);
				
				if (selected) {
					// ~ selectedTrees.add (t);
					g.setColor(RED);
				} else {
					g.setColor(GREEN);
				}

				g2.draw(shape);
				drawnTrees.add(t); // memo
				
			}
		}
		
		try {
			selectionNumber.setText("#"+selectedTrees.size ());
		} catch (Exception e) {} // maybe selectedTrees is null -> show nothing
		
		// Prepare label drawing strategy
		Collection labelledTrees = drawnTrees; // fc - 25.3.2004

		if (labelNumber <= 0) {
			labelCounter = 1;
			labelFrequency = Integer.MAX_VALUE;
		} else {
			labelCounter = 0;
			labelFrequency = Math.max(1,
					(int) labelledTrees.size() / Math.max(1, labelNumber));
		}

		// Draw some labels
		for (Iterator i = labelledTrees.iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			Spatialized s = (Spatialized) t;

			g2.setColor(Color.BLUE);
			String label = null;
			if (diameterMode) { // fc - 25.3.2004
				label = formater.format(t.getDbh());
			} else {
				label = "" + t.getId();
			}

			drawLabel(g2, label, (float) s.getX(), (float) s.getY());
		}

	}

	// Draws a label for the given tree
	// Implements a labels restriction strategy (see Draw (), very long to draw
	// if numerous)
	// fc - 23.12.2003
	//
	private void drawLabel(Graphics2D g2, String label, float x, float y) {
		if (labelCounter % labelFrequency == 0) {
			labelCounter = 0;
			g2.drawString(label, x, y);
		}
		labelCounter++;
	}

	/**
	 * From Drawer interface. We may receive (from Panel2D) a selection
	 * rectangle (in user space i.e. meters) and return a JPanel containing
	 * information about the objects (trees) inside the rectangle. If no objects
	 * are found in the rectangle, return null.
	 */
	public JPanel select(Rectangle.Double r, boolean more) {

		if (drawnTrees == null) {
			return null;
		} // who knows...

		Collection treesToBeInspected = new ArrayList();

		for (Iterator i = drawnTrees.iterator(); i.hasNext();) {
			Spatialized s = (Spatialized) i.next();

			Point.Double p = new Point.Double(s.getX(), s.getY());
			if (r.contains(p)) {
				treesToBeInspected.add(s);
			}
		}

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

	// ~ protected double visibleThreshold; // things under this user size
	// should not be drawn (too small on screen)
	// ~ protected int labelCounter; // for trees label drawing strategy
	// ~ protected int labelFrequency;

	// ~ private FilteringPanelSettings filtration;

	// ~ private JButton filteringButton;
	// ~ private double minDbh;
	// ~ private double maxDbh;

	// ~ public Dimension getPreferredSize () {return panel2D.getPreferredSize
	// ();}

	//
	// Deal with filtering dialog
	//
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(fLabelNumber)) {
			try {
				String t = fLabelNumber.getText().trim();
				labelNumber = new Integer(t).intValue();
				Settings.setProperty("extension.madd.display.label.number", ""
						+ labelNumber);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog
						.print(this,
								Translator
										.swap("FTreeGridSelectorConfigPanel.labelNumberMustBeAnInteger"));
				return;
			}

		} else if (evt.getSource().equals(fMagnifyFactor)) {
			try {
				String t = fMagnifyFactor.getText().trim();
				magnifyFactor = new Integer(t).intValue();
				Settings.setProperty("extension.madd.display.magnify.factor",
						"" + magnifyFactor);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this,
						Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

		} else if (evt.getSource().equals(showDiameter)) {
			try {
				diameterMode = showDiameter.isSelected();
				Settings.setProperty("extension.madd.display.diameter.mode", ""
						+ diameterMode);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this,
						Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

		}
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
