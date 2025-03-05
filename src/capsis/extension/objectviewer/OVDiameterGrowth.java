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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.geom.Arc2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;
import capsis.commongui.util.Tools;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.extension.AbstractObjectViewer;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.CheckListRenderer;
import capsis.util.CheckableItem;
import capsis.util.Drawer;
import capsis.util.Panel2D;

/**
 * A viewer to check diameter growth.
 * 
 * @author Sandrine Chalon - April 2003
 */
public class OVDiameterGrowth extends AbstractObjectViewer implements Drawer, ActionListener,
// ~ SelectionListener, Disposable {
		Disposable {

	static {
		Translator.addBundle("capsis.extension.objectviewer.OVDiameterGrowth");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap("OVDiameterGrowth");
	//static public final String DESCRIPTION = Translator.swap("OVDiameterGrowth.description");
	//static public final String AUTHOR = "Sandrine Chalon";
	//static public final String VERSION = "1.3";

	public static final int X_MARGIN_IN_PIXELS = 20;
	public static final int Y_MARGIN_IN_PIXELS = 20;

	private TreeListCell gcell; // target GCell

	private double x0;
	private double x1;
	private double y0;
	private double y1;
	// ~ private Rectangle.Double r2;

	private JPanel lateral;

	private ColumnPanel caption; // fc - 12.12.2007 - JPanel -> ColumnPanel
	private Map<CheckableItem, Step> checkItems;
	private Color[] color;

	private JScrollPane scroll; // contains panel2D
	private Panel2D panel2D;

	private ColumnPanel controlPanel; // fc - 12.12.2007 - JPanel -> ColumnPanel
	private JRadioButton allSteps;
	private JRadioButton frequencySteps;
	private JRadioButton listedSteps;
	private ButtonGroup group1;
	private JTextField frequency;
	private JScrollPane scroll2;
	private JList outputList;
	private Collection<String> selectedDates;

	private Collection selectedSteps;

	// ~ private SelectionSource source; // fc - 12.12.2007

	/**
	 * Default constructor.
	 */
	public OVDiameterGrowth() {
	}

	@Override
	public void init(Collection s) throws Exception {

		try {
			// fc - 12.12.2007 - Selection listeners
			// ~ source = s.getSelectionSource ();
			// ~ source.addSelectionListener (this);
			// fc - 12.12.2007 - Selection listeners

			selectedDates = new HashSet<String>();
			checkItems = new HashMap<CheckableItem, Step>(); // item -> step

			// Cr�ation du tableau comportant une couleur pour chaque �tape
			color = new Color[10];
			color[0] = Color.BLACK;
			color[1] = Color.BLUE;
			color[2] = Color.RED;
			color[3] = Color.GREEN;
			color[4] = Color.PINK;
			color[5] = Color.YELLOW;
			color[6] = Color.ORANGE;
			color[7] = Color.MAGENTA;
			color[8] = Color.CYAN;
			color[9] = Color.BLACK;

			createUI();

			gcell = extractCell(s);

			calculatePanel2D(gcell); // fc - 13.12.2007

			update(); // fc - 12.12.2007

		} catch (Exception exc) {
			Log.println(Log.ERROR, "OVDiameterGrowth.c ()", exc.toString(), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}
	}

	// Get the first GCell in the candidate selection.
	// If not found, return null.
	private TreeListCell extractCell(Collection candidateSelection) {
		gcell = null;
		if (candidateSelection != null) {
			for (Iterator i = candidateSelection.iterator(); i.hasNext();) {
				Object candidate = i.next();
				if (candidate instanceof Cell) {
					gcell = (TreeListCell) candidate;
					break;
				}
			}
		}
		return gcell;
	}

	// fc - 2.6.2003
	// was a AmapDialog, now a JPanel : no content pane any more ->
	// getContentPane redefinition
	public Container getContentPane() {
		return this;
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			// BEFORE:
			// ~ if (!(referent instanceof GCell)) {return false;} // si ce
			// n'est pas une cellule : false

			// fc - 12.12.2007 - referent is now always a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty()) {
				return false;
			}

			// Possibly several subclasses in the collection
			Collection reps = Tools.getRepresentatives(c); // one instance of
															// each class
			for (Iterator i = reps.iterator(); i.hasNext();) {
				Object e = i.next();
				if (e instanceof Cell) {
					return true;
				} // we need one GCell
			}
			return false;
		} catch (Exception e) {
			Log.println(Log.ERROR, "OVDiameterGrowth.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}
	
	@Override
	public String getName() {
		return Translator.swap("OVDiameterGrowth.name");
	}
	
	@Override
	public String getAuthor() {
		return "Sandrine Chalon";
	}

	@Override
	public String getDescription() {
		return Translator.swap("OVDiameterGrowth.description");
	}

	@Override
	public String getVersion() {
		return "1.3";
	}

	
	private void synchro() {
		frequency.setEnabled(frequencySteps.isSelected());
		try {
			outputList.setEnabled(listedSteps.isSelected());
		} catch (Exception e) {
		}
	}

	/**
	 * ActionListener.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(allSteps)) {
			synchro();
		} else if (evt.getSource().equals(frequencySteps)) {
			synchro();
		} else if (evt.getSource().equals(frequency)) {
			if (!Check.isInt(frequency.getText().trim())) {
				MessageDialog
						.print(this, Translator.swap("OVDiameterGrowth.frequencyShouldBeAnIntegerGreaterThanZero"));
				return;
			}
			int v = Check.intValue(frequency.getText().trim());
			if (v < 0) {
				MessageDialog
						.print(this, Translator.swap("OVDiameterGrowth.frequencyShouldBeAnIntegerGreaterThanZero"));
				return;
			}
		} else if (evt.getSource().equals(listedSteps)) {
			synchro();
		}
		update();
	}

	/**
	 * We have to redraw the subscene.
	 */
	public void draw(Graphics g, Rectangle.Double r) {
		if (gcell == null) {
			return;
		}
		if (selectedSteps == null) {
			return;
		}

		Graphics2D g2 = (Graphics2D) g;

		int cpt = 0;
		int cellId = gcell.getId();
		for (Iterator i = selectedSteps.iterator(); i.hasNext();) {
			Step stp = (Step) i.next();

			Color stepColor = color[cpt % color.length];

			TreeList stand = (TreeList) stp.getScene();

			// MatchWith requires a cell, means we have a PlotOfCells
			PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017

			TreeListCell cell = (TreeListCell) plotc.getCell(cellId);
			Collection trees = new ArrayList();
			if (cell.isTreeLevel()) {
				trees.addAll(cell.getTrees());
			} else {
				Collection cells = cell.getCells();
				if (cells != null) {
					for (Iterator j = cells.iterator(); j.hasNext();) {
						TreeListCell c = (TreeListCell) j.next();
						trees.addAll(c.getTrees());
					}
				}
			}

			for (Iterator k = trees.iterator(); k.hasNext();) {
				SpatializedTree tree = (SpatializedTree) k.next();
				double radius = tree.getDbh() / 10d / 2d;
				Arc2D arc = new Arc2D.Double();
				arc.setArcByCenter(tree.getX(), tree.getY(), radius, 180, 360, Arc2D.OPEN);

				g2.setColor(stepColor);
				g2.draw(arc);
			}

			cpt++;
		}
	}

	/**
	 * Disposable
	 */
	public void dispose() {
		// ~ System.out.println ("OVDiameterGrowth.dispose ()...");
		// ~ try {
		// ~ source.removeSelectionListener (this);
		// ~ panel2D.dispose ();
		// ~ } catch (Exception e) {} // does not matter very much
	}

	/**
	 * SelectionListener
	 */
	// ~ public void sourceSelectionChanged (SelectionEvent e) {
	// ~ SelectionSource source = e.getSource ();
	// ~ Collection newSelection = source.getSelection ();
	// ~ boolean selectionActuallyChanged = e.hasSelectionActuallyChanged (); //
	// fc - 13.12.2007
	// ~ System.out.println
	// ("OVDiameterGrowth, sourceSelectionChanged, selectionActuallyChanged="+selectionActuallyChanged);

	// ~ // fc - 12.12.2007 - OjectViewers now get a Collection
	// ~ gcell = null;
	// ~ for (Iterator i = newSelection.iterator (); i.hasNext ();) {
	// ~ Object candidate = i.next ();
	// ~ if (candidate instanceof GCell) {
	// ~ gcell = (GCell) candidate;
	// ~ break;
	// ~ }
	// ~ }
	// ~ Collection listenerEffectiveSelection = Tools.intoCollection (gcell);
	// // we select one GCell only

	// ~ if (panel2D == null || selectionActuallyChanged) {calculatePanel2D
	// (gcell);} // fc - 13.12.2007

	// ~ // Tell the source what we've selected effectively - fc - 12.12.2007
	// ~ e.setListenerEffectiveSelection (listenerEffectiveSelection);

	// ~ update ();
	// ~ }

	// Calculate the size extension of the panel2D to view the complete
	// selected scene
	//
	private void calculatePanel2D(TreeListCell cell) {
		if (cell == null) {
			panel2D = null;
			return;
		}

		// pour r�cup�rer les arbres contenus dans la GCell s�lectionn�e ou dans
		// ses sous-cellules
		Collection trees = new ArrayList();
		if (gcell.isTreeLevel()) {
			trees.addAll(gcell.getTrees());
		} else {
			Collection cells = gcell.getCells();
			if (cells != null) {
				for (Iterator i = cells.iterator(); i.hasNext();) {
					TreeListCell c = (TreeListCell) i.next();
					trees.addAll(c.getTrees());
				}
			}
		}
		if (trees == null || trees.isEmpty()) {
			panel2D = null;
			return;
		}

		x0 = Double.MAX_VALUE;
		x1 = Double.MIN_VALUE;
		y0 = Double.MAX_VALUE;
		y1 = Double.MIN_VALUE;
		// ~ for (int i = 0; i < trees.size (); i++) {
		// ~ GMaddTree t = (GMaddTree ) trees.elementAt (i);
		for (Iterator i = trees.iterator(); i.hasNext();) {
			SpatializedTree t = (SpatializedTree) i.next();
			double radius = t.getDbh() / 10 / 2;
			double x = t.getX();
			double y = t.getY();

			x0 = Math.min(x0, x - radius);
			x1 = Math.max(x1, x + radius);
			y0 = Math.min(y0, y - radius);
			y1 = Math.max(y1, y + radius);
		}

		Rectangle.Double r2 = new Rectangle.Double(x0, y0, x1 - x0, y1 - y0);
		panel2D = new Panel2D(this, r2, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS);
		scroll.getViewport().setView(panel2D);
	}

	private void resetPanel2D() {
		if (panel2D != null) {
			panel2D.reset();
			panel2D.repaint();
		}
	}

	// fc - 12.12.2007 - update
	// At construction time, referent gcell can be null, we need to be able
	// to rebuild the gui afterwards and to refresh on steps changes
	private void update() {
		if (gcell == null) {
			return;
		} // fc - 12.12.2007

		Step refStep = gcell.getPlot().getScene().getStep();
		Collection steps = refStep.getProject().getStepsFromRoot(refStep);

		// ~ listedDates.clear ();
		checkItems.clear();
		for (Iterator i = steps.iterator(); i.hasNext();) {
			Step stp = (Step) i.next();
			GScene std = stp.getScene();
			int date = std.getDate();
			// ~ listedDates.add (""+date);

			boolean selected = selectedDates.contains("" + date);
			CheckableItem item = new CheckableItem("" + date, selected);
			checkItems.put(item, stp);

		}

		outputList = new JList(new Vector(new TreeSet(checkItems.keySet())));
		outputList.setCellRenderer(new CheckListRenderer());
		outputList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		outputList.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				int index = outputList.locationToIndex(e.getPoint());
				CheckableItem item = (CheckableItem) outputList.getModel().getElementAt(index);
				item.setSelected(!item.isSelected());
				String date = "" + item.getCaption();
				if (item.isSelected()) {
					selectedDates.add(date);
				} else {
					selectedDates.remove(date);
				}
				Rectangle rect = outputList.getCellBounds(index, index);
				outputList.repaint(rect);
				update();
			}
		});
		scroll2.getViewport().setView(outputList);

		// calculate selectedSteps
		if (allSteps.isSelected()) {
			selectedSteps = steps;
		} else if (frequencySteps.isSelected()) {
			selectedSteps = new ArrayList();
			int freq = Check.intValue(frequency.getText().trim());
			int cpt = 0;
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step stp = (Step) i.next();
				if (cpt % freq == 0) {
					selectedSteps.add(stp);
				}
				cpt++;
			}
		} else if (listedSteps.isSelected()) {
			selectedSteps = new ArrayList();

			for (Iterator i = checkItems.keySet().iterator(); i.hasNext();) {
				CheckableItem item = (CheckableItem) i.next();
				String date = item.getCaption();
				if (selectedDates.contains(date)) {
					Step stp = checkItems.get(item);
					selectedSteps.add(stp);
				}
			}
		}

		// prepare caption
		updateCaption();

		resetPanel2D();

		lateral.validate();
		lateral.repaint();

	}

	/**
	 * From Drawer interface. We may receive (from Panel2D) a selection
	 * rectangle (in user space i.e. meters) and return a JPanel containing
	 * information about the objects (trees) inside the rectangle. If no objects
	 * are found in the rectangle, return null.
	 */
	public JPanel select(Rectangle.Double r, boolean ctrlIsDown) {
		return null;
	}

	private void updateCaption() {
		caption.removeAll();
		int cpt = 0;
		for (Iterator i = selectedSteps.iterator(); i.hasNext();) {
			Step stp = (Step) i.next();

			JLabel label = new JLabel(" ___" + stp.getCaption());
			label.setForeground(color[cpt % color.length]);
			caption.add(label);
			cpt++;
		}
		caption.validate();
		caption.repaint();
	}

	@Override
	public Collection show(Collection candidateSelection, boolean thisIsAReselection) {
		gcell = extractCell(candidateSelection);
		realSelection = new ArrayList();
		realSelection.add(gcell);
		calculatePanel2D(gcell);
		update();

		// ~ calculatePanel2D (new ArrayList (realSelection));
		// ~ resetPanel2D ();

		System.out.println("" + getName() + ".select candidateSelection " + candidateSelection.size()
				+ " realSelection " + realSelection.size());
		return realSelection;
	}

	// User interface definition
	private void createUI() {
		setLayout(new BorderLayout());

		lateral = new JPanel(new BorderLayout());

		// 1. Caption
		caption = new ColumnPanel(Translator.swap("OVDiameterGrowth.caption"));
		lateral.add(caption, BorderLayout.NORTH);

		// 2. Drawing
		scroll = new JScrollPane();

		// 3. Control panel
		controlPanel = new ColumnPanel(Translator.swap("OVDiameterGrowth.view"));
		lateral.add(controlPanel, BorderLayout.CENTER);

		LinePanel l0 = new LinePanel();
		allSteps = new JRadioButton(Translator.swap("OVDiameterGrowth.All"));
		allSteps.addActionListener(this);
		l0.add(allSteps);
		l0.addGlue();
		controlPanel.add(l0);

		LinePanel l1 = new LinePanel();
		frequencySteps = new JRadioButton(Translator.swap("OVDiameterGrowth.n"));
		frequencySteps.addActionListener(this);
		l1.add(frequencySteps);
		frequency = new JTextField("1", 10);
		frequency.addActionListener(this);
		frequency.addActionListener(this);
		l1.add(frequency);
		l1.addGlue();
		controlPanel.add(l1);

		LinePanel l2 = new LinePanel();
		listedSteps = new JRadioButton(Translator.swap("OVDiameterGrowth.Choice"));
		listedSteps.addActionListener(this);
		l2.add(listedSteps);
		l2.addGlue();
		controlPanel.add(l2);

		scroll2 = new JScrollPane();

		// Do not set sizes explicitly inside object viewers
		// ~ scroll2.setPreferredSize (new Dimension (120, 200));

		controlPanel.add(scroll2);
		controlPanel.addGlue();

		group1 = new ButtonGroup();
		group1.add(allSteps);
		group1.add(frequencySteps);
		group1.add(listedSteps);
		group1.setSelected(allSteps.getModel(), true);

		synchro();

		// Layout parts
		add(scroll, BorderLayout.CENTER);
		add(new NorthPanel(lateral), BorderLayout.EAST);
	}

}
