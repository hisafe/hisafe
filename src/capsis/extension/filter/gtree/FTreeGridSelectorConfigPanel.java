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

package capsis.extension.filter.gtree;

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
import java.util.Vector;

import javax.swing.DefaultComboBoxModel;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.gui.DGrouperDefiner;
import capsis.gui.MainFrame;
import capsis.gui.command.BuildGrouper;
import capsis.util.Drawer;
import capsis.util.History;
import capsis.util.Panel2D;
import capsis.util.Pilotable;
import capsis.util.Redoable;
import capsis.util.Undoable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;

/**
 * Configuration panel for FTreeGridSelector.
 * 
 * @author F. de Coligny - may 2002
 */
public class FTreeGridSelectorConfigPanel extends ConfigurationPanel implements Undoable, Redoable, Drawer, Pilotable,
		ActionListener, ChangeListener {
	private final static Color GREEN = new Color(0, 153, 0);
	private final static Color RED = Color.RED;

	private static final int BUTTON_SIZE = 23;
	private FTreeGridSelector mum; // mummy is being configured
	protected double visibleThreshold; // things under this user size should not
										// be drawn (too small on screen)

	// Support cells are the cells among which the user can select trees.
	// First time : all the cells in the plot, then, only the cells supporting
	// the trees selected at previous filtration step.
	// NOTE: "First time" means if the filter is used directly on an original
	// filtrable (a GStand). "then" means if the filter is used on the result
	// of another filter.
	private Set supportCellIds;

	// fc - 2.4.2004
	//
	private PlotOfCells plot; // fc-26.10.2017
	private TreeList stand;
	private Collection candidates;

	private Panel2D panel2D;
	private boolean disabled;

	private JTextField fLabelNumber; // fc - 23.12.2003
	private JTextField fMagnifyFactor; // fc - 23.12.2003
	private JCheckBox showDiameter;

	private int labelNumber;
	private int magnifyFactor;
	private boolean diameterMode;

	protected int labelCounter; // for trees label drawing strategy
	protected int labelFrequency;

	private JButton eraseAllButton;
	private JButton undoButton;
	private JButton redoButton;

	private JSpinner cellLevel;
	private int maxLevel;
	private int currentCellLevel;

	private Collection drawnTrees; // filled in draw, used in select

	private JButton helpButton;

	public void disablePanel() {
		disabled = true;
	}

	private NumberFormat formater;

	private History selectionHistory; // for undo / redo - fc - 2.3.2005

	// fc - 4.12.2007
	private JCheckBox ckNot;
	private JComboBox chooseGrouper;
	private JButton grouperCatalog;
	private JButton addCells;

	// fc - 4.12.2007

	/**
	 * Constructor
	 */
	protected FTreeGridSelectorConfigPanel(Configurable configurable) {
		super(configurable);

		selectionHistory = new History(); // for undo / redo - fc - 2.3.2005

		mum = (FTreeGridSelector) configurable;

		formater = NumberFormat.getInstance(Locale.ENGLISH);
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(2);

		labelNumber = Settings.getProperty("extension.ftreegridselector.label.number", 50);

		magnifyFactor = Settings.getProperty("extension.ftreegridselector.magnify.factor", 10);

		diameterMode = Settings.getProperty("extension.ftreegridselector.diameter.mode", false);
		drawnTrees = new Vector();
		disabled = false;

		stand = (TreeList) mum.referent; // fc - 2.4.2004
		plot = stand.getPlot();
		candidates = mum.candidates; // trees

		// Retrieve support cells ids
		supportCellIds = new HashSet();
		for (Iterator i = candidates.iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			Integer cellId = new Integer(t.getCell().getId());
			supportCellIds.add(cellId);
		}

		// Find max cell nesting levels if nested cells in module
		// fc- 15.12.2003
		boolean found = false;
		int i = 1;
		while (!found) {
			Collection c = plot.getCellsAtLevel(i);
			if (c == null || c.isEmpty()) {
				found = true;
				maxLevel = i - 1;
			} else {
				i++;
			}
		}
		currentCellLevel = maxLevel;

		Vertex3d o = plot.getOrigin();
		Rectangle.Double r = new Rectangle.Double(o.x, o.y, plot.getXSize(), plot.getYSize());
		panel2D = new Panel2D(this, r, 10, 10);

		selectionHistory.add(new HashSet(mum.selectedCellIds)); // for undo
																// /redo - fc -
																// 2.3.2005

		setBackground(Color.WHITE);
		setLayout(new BorderLayout());

		add(panel2D, BorderLayout.CENTER);

	}

	private JPanel makeGroupAdder() {
		LinePanel l0 = new LinePanel();

		boolean not = false;
		ckNot = new JCheckBox(Translator.swap("Shared.NOT"), not);
		// ~ ckNot.addActionListener (this);

		// Grouper names combo box.
		// Ask GrouperManager for groupers matching with target. Retrieve
		// grouper names.
		GroupableType type = TreeList.GROUP_PLOT_CELL;
//		String type = Group.CELL;

		Collection c = Group.whichCollection(stand, type);
		TreeSet grouperNames = new TreeSet(GrouperManager.getInstance().getGrouperNames(c)); // if
																								// no
																								// groups,
																								// empty
																								// Collection
		chooseGrouper = new JComboBox(new Vector(grouperNames));
		// ~ chooseGrouper.addItemListener (this);
		// ~ chooseGrouper.setSelectedItem (lastGrouperName);
		chooseGrouper.addActionListener(this);
		// ~ ckGrouperModeAction (); // enables or not chooseGrouper

		// ~ lastSelectedOption = (String) chooseGrouper.getSelectedItem ();

		Icon icon = IconLoader.getIcon("group_16.png");
		grouperCatalog = new JButton(icon);
		grouperCatalog.setToolTipText(Translator.swap("FTreeGridSelectorConfigPanel.buildGrouperToolTip"));
		grouperCatalog.addActionListener(this);
		Tools.setSizeExactly(grouperCatalog);

		addCells = new JButton("+");
		addCells.addActionListener(this);
		addCells.setToolTipText(Translator.swap("FTreeGridSelectorConfigPanel.addTheCellsInThisGroup"));

		// ~ if (showGrouperMode) {add (ckGrouperMode);} // fc - 16.4.2007
		l0.add(ckNot);
		l0.add(chooseGrouper);
		l0.add(grouperCatalog);
		l0.add(addCells);
		l0.addStrut0();

		return l0;
	}

	private void addCellsAction() {
		String grouperName = getGrouperName();
		System.out.println("FTreeGridSelectorConfigPanel, grouperName=" + grouperName);

		PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017

		Collection cells = plotc.getCells();

		GrouperManager gm = GrouperManager.getInstance();
		Grouper gr = gm.getGrouper(grouperName); // return null if not found
		Collection aux = gr.apply(cells, grouperName.toLowerCase().startsWith("not "));
		Collection cellsInGroup = new HashSet(aux);

		for (Iterator i = cellsInGroup.iterator(); i.hasNext();) {
			TreeListCell c = (TreeListCell) i.next();
			selectCell(c, mum.selectedCellIds, null); // rectangle == null
		}

		selectionHistory.add(new HashSet(mum.selectedCellIds)); // for undo
																// /redo

		panel2D.reset();
		panel2D.repaint();

	}

	// Update grouper list in combo box after grouper management.
	//
	private void updateCombo() {
		
		GroupableType type = TreeList.GROUP_PLOT_CELL;
//		String type = Group.CELL;
		Collection c = Group.whichCollection(stand, type);
		TreeSet grouperNames = new TreeSet(GrouperManager.getInstance().getGrouperNames(c)); // if
																								// no
																								// groupers,
																								// empty
																								// Collection
		chooseGrouper.setModel(new DefaultComboBoxModel(new Vector(grouperNames)));
		// ~ chooseGrouper.setSelectedItem (lastSelectedOption);
	}

	/**
	 * Return answer (chosen grouper name), empty string if no selection.
	 *
	 * <PRE>
	 * Example: if (gc.isGrouperSelected()) {
	 * 	grouperName = gc.getGrouperName();
	 * }
	 * </PRE>
	 */
	public String getGrouperName() {
		String not = ckNot.isSelected() ? "Not " : "";
		return (not + (String) chooseGrouper.getSelectedItem());
	}

	/**
	 * Drawing delegation : should draw something in the given user space
	 * Graphics
	 */
	public void draw(Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Preset. Choose a pixel detailThreshold, compute it in meters with
		// current scale
		// if dbh in m. >= detailThreshold -> detailled level is reached
		// ~ double detailThreshold = ((double) pixelThreshold) /
		// panel2D.getCurrentScale ().x; // in meters

		// fc - 23.12.2003
		visibleThreshold = 1.1 / panel2D.getCurrentScale().x; // 1 pixel in in
																// meters

		// 1. Cells
		for (Iterator i = plot.getCells().iterator(); i.hasNext();) {
			Cell c = (Cell) i.next();

			Shape shape = c.getShape();

			// Do not draw cell if invisible
			if (!shape.getBounds2D().intersects(r)) {
				continue;
			}

			if (mum.selectedCellIds.contains(new Integer(c.getId()))) {
				g.setColor(RED);
				g2.fill(shape);
			}

			Color normalColor;
			if (c.getLevel() == currentCellLevel) {
				normalColor = Color.BLACK;
			} else {
				normalColor = Color.LIGHT_GRAY;
			}

			g.setColor(disabled ? Color.GRAY : normalColor);
			g2.draw(shape);
		}

		// 2. Trees
		drawnTrees.clear(); // We're gonna fill it now
		for (Iterator i = candidates.iterator(); i.hasNext();) {
			SpatializedTree t = (SpatializedTree) i.next();

			// Marked trees are considered dead by generic tools -> don't draw
			if (t.isMarked()) {
				continue;
			}

			double dbh = t.getDbh(); // cm.

			// fc - 26.3.2004 - magnify trees to see better (bc request)
			//
			double diameter = dbh / 100 * magnifyFactor; // meters *
															// magnifyFactor
			double radius = diameter / 2;
			Shape shape = new Ellipse2D.Double(t.getX() - radius, t.getY() - radius, diameter, diameter);

			// Do not draw tree if invisible
			if (shape.intersects(r) || r.contains(new Point.Double(t.getX(), t.getY()))) {
				g.setColor(GREEN);
				g2.fill(shape);
				drawnTrees.add(t); // Memo
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
		for (Iterator i = drawnTrees.iterator(); i.hasNext();) {
			SpatializedTree t = (SpatializedTree) i.next();
			String label = null;
			if (diameterMode) { // fc - 25.3.2004
				label = formater.format(t.getDbh());
			} else {
				label = "" + t.getId();
			}
			drawLabel(g2, label, (float) t.getX(), (float) t.getY());
		}

	}

	// Draw a label for the given tree
	// Implements a labels restriction strategy (see Draw (), very long to draw
	// if numerous)
	// fc - 23.12.2003
	//
	protected void drawLabel(Graphics2D g2, String label, float x, float y) {
		if (labelCounter % labelFrequency == 0) {
			labelCounter = 0;
			g2.setColor(Color.BLUE);
			g2.drawString(label, x, y);
		}
		labelCounter++;
	}

	/**
	 * Should return a JPanel if tree selection done, manage selectedCellIds if
	 * cells selection
	 */
	public JPanel select(Rectangle.Double r, boolean isControlDown) {
		if (disabled) {
			return null;
		} // Panel may be disabled -> do no selection

		Collection treesToBeInspected = new ArrayList();

		if (!isControlDown) { // Cells selection

			// Search lower level from spinner cell level up
			// fc - 15.12.2003
			//
			Collection cells = null;
			int level = currentCellLevel; // Chosen by user in spinner
			while ((cells == null || cells.isEmpty()) && level >= 1) {
				cells = plot.getCellsAtLevel(level--);

				// Check if at least one cell is in the rectangle. If not, get
				// up one level
				if (cells != null) {
					boolean good = false;
					for (Iterator i = cells.iterator(); i.hasNext();) {
						Cell c = (Cell) i.next();
						if (c.getShape().intersects(r)) {
							good = true;
							break;
						}
					}
					if (!good) {
						cells = null;
					}
				}
			}

			for (Iterator i = cells.iterator(); i.hasNext();) {
				TreeListCell c = (TreeListCell) i.next();
				selectCell(c, mum.selectedCellIds, r);
			}

			selectionHistory.add(new HashSet(mum.selectedCellIds)); // for undo
																	// /redo -
																	// fc -
																	// 2.3.2005

		} else { // Trees selection for inspector

			if (drawnTrees != null) {

				for (Iterator i = drawnTrees.iterator(); i.hasNext();) {
					SpatializedTree t = (SpatializedTree) i.next();

					double w = t.getDbh() / 100;
					Shape shape = new Ellipse2D.Double(t.getX() - w / 2, t.getY() - w / 2, w, w);

					// Inspection ?
					if (shape.getBounds2D().intersects(r) || r.contains(new Point.Double(t.getX(), t.getY()))) {
						treesToBeInspected.add(t);
					}
				}

			}

		}

		panel2D.reset();
		panel2D.repaint();

		if (treesToBeInspected.isEmpty()) {
			return null; // no JPanel returned
		} else {
			return AmapTools.createInspectorPanel(treesToBeInspected); // An
																		// inspector
																		// for
																		// the
																		// concerned
																		// trees
		}

	}

	public void undo() {

		if (selectionHistory != null && selectionHistory.canBack()) {
			mum.selectedCellIds = new HashSet((Set) selectionHistory.back());
			panel2D.reset();
			panel2D.repaint();
		}

	}

	public void redo() {

		if (selectionHistory != null && selectionHistory.canNext()) {
			mum.selectedCellIds = new HashSet((Set) selectionHistory.next());
			panel2D.reset();
			panel2D.repaint();
		}

	}

	/**
	 * Select cell if in selection rectangle. If cell is not tree level, select
	 * all cells inside the cell. Recursive. fc - 15.12.2003
	 */
	public void selectCell(TreeListCell cell, Collection selectedCells, Rectangle.Double r) {
		if (!cell.isTreeLevel()) {
			Shape shape = cell.getShape();
			if ((r == null || shape.intersects(r)) && cell.isMother()) {
				for (Iterator cells = cell.getCells().iterator(); cells.hasNext();) {
					selectCell((TreeListCell) cells.next(), selectedCells, null);
				}
			}
		} else {
			Shape shape = cell.getShape();

			if (r == null || shape.intersects(r)) { // r is null if we try to
													// select nested cells
				Integer id = new Integer(cell.getId());
				if (!selectedCells.contains(id)) {
					selectedCells.add(id);
				} else {
					selectedCells.remove(id);
				}
			}
		}

	}

	/**
	 * From Pilotable interface.
	 */
	public JComponent getPilot() {

		ImageIcon icon = IconLoader.getIcon("help_16.png");
		helpButton = new JButton(icon);
		Tools.setSizeExactly(helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText(Translator.swap("Shared.help"));
		helpButton.addActionListener(this);

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

		Integer min = new Integer(1);
		Integer max = new Integer(maxLevel);
		Integer step = new Integer(1);
		Integer value = new Integer(currentCellLevel);
		SpinnerNumberModel model = new SpinnerNumberModel(value, min, max, step);
		cellLevel = new JSpinner(model);
		((JSpinner.NumberEditor) cellLevel.getEditor()).getTextField().setEditable(false);
		cellLevel.setToolTipText(Translator.swap("FTreeGridSelectorConfigPanel.cellLevel"));
		cellLevel.addChangeListener(this);

		ColumnPanel toolbar = new ColumnPanel();

		LinePanel line1 = new LinePanel();
		line1.add(eraseAllButton);
		line1.add(undoButton);
		line1.add(redoButton);

		JLabel l = new JLabel(Translator.swap("Shared.labelNumber") + ": ");
		l.setToolTipText(Translator.swap("Shared.labelNumberExplanation"));
		line1.add(l);
		line1.add(fLabelNumber);

		JLabel l2 = new JLabel(Translator.swap("Shared.magnifyFactor") + ": ");
		l2.setToolTipText(Translator.swap("Shared.magnifyFactorExplanation"));
		line1.add(l2);
		line1.add(fMagnifyFactor);

		toolbar.add(line1);

		LinePanel line2 = new LinePanel();
		line2.add(showDiameter);

		JLabel cellLevelLabel = new JLabel(Translator.swap("FTreeGridSelectorConfigPanel.cellLevelAbbreviation") + ": ");
		cellLevelLabel.setToolTipText(Translator.swap("FTreeGridSelectorConfigPanel.cellLevel"));
		line2.add(cellLevelLabel);
		line2.add(cellLevel);
		line2.add(helpButton);
		toolbar.add(line2);

		toolbar.add(makeGroupAdder()); // fc - 4.12.2007

		// ~ toolbar.setPreferredSize (new Dimension (200, 200));

		toolbar.setVisible(true);

		return toolbar;
	}

	/**
	 * Events processing
	 */
	public void stateChanged(ChangeEvent evt) {
		if (evt.getSource().equals(cellLevel)) {
			currentCellLevel = ((Integer) cellLevel.getValue()).intValue();
			panel2D.reset();
			panel2D.repaint();
		}
	}

	/**
	 * Events processing
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(addCells)) { // fc - 4.12.2007
			addCellsAction();

		} else if (evt.getSource().equals(grouperCatalog)) { // fc - 4.12.2007

			// if !ctrl is hit, open directly group definer
			// ~ if ((evt.getModifiers () & Tools.getCtrlMask ()) != 0) {
			if ((evt.getModifiers() & Tools.getCtrlMask()) == 0) {
				DGrouperDefiner dlg = new DGrouperDefiner(Current.getInstance().getStep(), 
						TreeList.GROUP_PLOT_CELL, null);
				updateCombo();

				// else open group catalog
			} else {
				new BuildGrouper(MainFrame.getInstance()).execute();
				updateCombo();
			}

		} else if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);

		} else if (evt.getSource().equals(eraseAllButton)) {

			if (Question.ask(MainFrame.getInstance(), Translator.swap("Shared.confirm"),
					Translator.swap("Shared.unselectAll"))) {
				mum.selectedCellIds.clear();

				selectionHistory.add(new HashSet(mum.selectedCellIds)); // for
																		// undo
																		// /redo
																		// - fc
																		// -
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
				Settings.setProperty("extension.ftreegridselector.label.number", "" + labelNumber);

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
				Settings.setProperty("extension.ftreegridselector.magnify.factor", "" + magnifyFactor);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

		} else if (evt.getSource().equals(showDiameter)) {
			try {
				diameterMode = showDiameter.isSelected();
				Settings.setProperty("extension.ftreegridselector.diameter.mode", "" + diameterMode);

				panel2D.reset();
				panel2D.repaint();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Shared.magnifyFactorMustBeAnInteger"));
				return;
			}

		}
	}

	/**
	 * Possible error : empty selection
	 */
	public boolean checksAreOk() {
		if (mum.selectedCellIds == null || mum.selectedCellIds.isEmpty()) {
			MessageDialog.print(this, Translator.swap("FTreeGridSelectorConfigPanel.selectionIsNeeded"));
			return false;
		}
		return true;
	}

}
