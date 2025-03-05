/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2003  Francois de Coligny
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

package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.EventQueue;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JToolBar;
import javax.swing.JViewport;

import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.GTreeIdComparator;
import capsis.util.Pilotable;

/**
 * SVText is a cartography simple viewer for trees with coordinates. It gives a
 * text representation of the Step, Scene and Trees.
 * 
 * @author F. de Coligny - July 1999 / February 2006
 */
public class SVText extends AbstractStandViewer implements ActionListener, Pilotable {
	// fc-11.1.2017 code review

	static {
		Translator.addBundle("capsis.extension.standviewer.SVText");
	}

	// nb-07.08.2018
	//static public String NAME = Translator.swap("SVText");
	//static public String DESCRIPTION = Translator.swap("SVText.description");
	//static public String AUTHOR = "F. de Coligny";
	//static public String VERSION = "1.2";

	private JButton helpButton;
	private int maxNumberOfTrees = 100;
	private JScrollPane scrollpane;
	private JViewport viewport;

	/**
	 * Opens and inits the tool on the given stepButton
	 */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {

		super.init(model, s, but);
		setLayout(new GridLayout(1, 1)); // important for viewer appearance

		try {

			createUI();
			update();
		} catch (Exception e) {
			Log.println(Log.ERROR, "SVText.c ()", "Error in constructor", e);
			throw e; // propagate
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;

		} catch (Exception e) {
			Log.println(Log.ERROR, "SVText.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("SVText.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVText.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**
	 * Update the viewer on a given stepButton
	 */
	public void update(StepButton sb) {
		super.update(sb); // computes boolean sameStep
		update(); // for example
	}

	/**
	 * Update the viewer with the current stepButton
	 */
	public void update() {
		super.update();
		if (sameStep) {
			return;
		}

		final JScrollBar vBar = scrollpane.getVerticalScrollBar();
		final JScrollBar hBar = scrollpane.getHorizontalScrollBar();
		final int vBarPosition = vBar.getValue();
		final int hBarPosition = hBar.getValue();

		StringBuffer sb = new StringBuffer();

		// GScene:
		String filler = "    ";

		Collection data = new ArrayList(); // Strings

		GScene scene = stepButton.getStep().getScene();

		// Scene title line
		sb.append("Scene " + scene.getDate());

		data.add(sb.toString());
		sb.delete(0, sb.length());

		sb.append(filler);
		sb.append("GScene ");
		sb.append("date[" + scene.getDate() + "] ");
		sb.append("area[" + scene.getArea() + "] ");
		sb.append("initialScene[" + scene.isInitialScene() + "] ");
		sb.append("interventionResult[" + scene.isInterventionResult() + "] ");

		data.add(sb.toString());
		sb.delete(0, sb.length());

		sb.append(filler);
		sb.append(filler);
		sb.append("origin[" + scene.getOrigin() + "] ");
		sb.append("width[" + scene.getXSize() + "] ");
		sb.append("height[" + scene.getYSize() + "] ");

		data.add(sb.toString());
		sb.delete(0, sb.length());

		sb.append(filler);
		sb.append(filler);
		sb.append("sourceName[" + Translator.swap(scene.getSourceName()) + "] ");

		data.add(sb.toString());
		sb.delete(0, sb.length());

		// fc-17.2.2012 added the toString () result (convenient for tests)
		data.add("-> " + scene.toString());

		if (scene instanceof TreeList) {
			TreeList treeList = (TreeList) scene;
			sb.append(filler);
			sb.append("TreeList ");
			sb.append("trees[List:" + treeList.getTrees().size() + "] ");

			// ~ sb.append ("\n");
			data.add(sb.toString());
			sb.delete(0, sb.length());

			if (!treeList.getTrees().isEmpty()) {
				int cpt = 0;

				// Sort the trees on their ascending ids
				Collection sortedTrees = new TreeSet(new GTreeIdComparator());
				sortedTrees.addAll(treeList.getTrees());

				for (Iterator i = sortedTrees.iterator(); i.hasNext();) {
					if (cpt++ > maxNumberOfTrees) {
						data.add("Restricted view to " + maxNumberOfTrees
								+ " trees");
						break;
					}
					Tree gtree = (Tree) i.next();

					// Tree title line
					sb.append("Tree " + gtree.getId());
					// ~ sb.append ("\n");
					data.add(sb.toString());
					sb.delete(0, sb.length());

					sb.append(filler);
					sb.append("GTree id[" + gtree.getId() + "] ");
					sb.append("age[" + gtree.getAge() + "] ");
					sb.append("dbh[" + gtree.getDbh() + "] ");
					sb.append("height[" + gtree.getHeight() + "] ");
					// ~ sb.append ("\n");
					data.add(sb.toString());
					sb.delete(0, sb.length());
					if (gtree instanceof Numberable) {
						Numberable n = (Numberable) gtree;
						sb.append(filler);
						sb.append("Numberable number[" + n.getNumber() + "] ");
						// ~ sb.append ("\n");
						data.add(sb.toString());
						sb.delete(0, sb.length());
					}
					if (gtree instanceof Spatialized) {
						Spatialized s = (Spatialized) gtree;
						sb.append(filler);
						sb.append("Spatialized x[" + s.getX() + "] y["
								+ s.getY() + "] z[" + s.getZ() + "] ");
						// ~ sb.append ("\n");
						data.add(sb.toString());
						sb.delete(0, sb.length());
					}
					if (gtree instanceof Speciable) {
						Speciable s = (Speciable) gtree;
						sb.append(filler);
						sb.append("Speciable species[" + s.getSpecies() + "] ");
						// ~ sb.append ("\n");
						data.add(sb.toString());
						sb.delete(0, sb.length());
					}

					// Real tree type
					sb.append(filler);
					sb.append(gtree.getClass().getName());
					sb.append(" toString["
							+ gtree.toString().replace('\t', ' ') + "] ");
					// ~ sb.append ("\n");
					data.add(sb.toString());
					sb.delete(0, sb.length());
				}

			}

		}

		// sb.append ("Stand:\n"+scene.bigString ()); // true: with trees
		// details

		// ...
		// ~ sb.append ("\n");
		// ~ GPlot p = stepButton.getStep ().getStand ().getPlot ();
		// ~ if (p != null) {
		// ~ sb.append ("\n6. Plot:\n"+p.bigString ());
		// ~ }
		// ~ buffer = sb.toString ();

		// ~ Object[][] rowData = new Object[data.size ()][1]; // 1 column
		// ~ Iterator it = data.iterator ();
		// ~ for (int i = 0; i < data.size (); i++) {
		// ~ rowData[i][0] = it.next ();
		// ~ }
		// ~ Object[] columnNames = {"Stand "+scene.getDate ()};

		// ~ int fontHeight = getFontMetrics (font).getHeight ();

		// ~ DefaultTableModel model = new DefaultTableModel (rowData,
		// columnNames);
		// ~ JTable table = new JTable (model) {
		// ~ public boolean isCellEditable (int row, int col) {return false;}
		// ~ };
		// ~ table.sizeColumnsToFit (1000); // manage resizing

		// ~ table.setDefaultRenderer (Object.class, new
		// MultiLineTableCellRenderer (
		// ~ font, tabSize));
		// ~ table.setShowHorizontalLines (false);
		// ~ table.setRowHeight (1*fontHeight); // 1 line

		// ~ viewport.setView (table);

		// Compact all data in a StringBuffer
		StringBuffer b = new StringBuffer();
		for (Iterator i = data.iterator(); i.hasNext();) {
			b.append((String) i.next());
			b.append("\n");
		}

		JTextArea view = new JTextArea(b.toString());
		viewport.setView(view);

		// restore scrollbar position
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				vBar.setValue(vBarPosition);
				hBar.setValue(hBarPosition);
			}
		});

	}

	/**
	 * Create the user interface
	 */
	public void createUI() {
		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(getPilot(), BorderLayout.NORTH);

		scrollpane = new JScrollPane(new JTextArea());
		viewport = scrollpane.getViewport();

		getContentPane().add(scrollpane, BorderLayout.CENTER);
	}

	/**
	 * Used for the settings buttons.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(helpButton)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * From Pilotable interface.
	 */
	public JComponent getPilot() {
		ImageIcon icon = IconLoader.getIcon("help_16.png");
		helpButton = new JButton(icon);
		Tools.setSizeExactly(helpButton, 23, 23);
		helpButton.setToolTipText(Translator.swap("Shared.help"));
		helpButton.addActionListener(this);

		JToolBar toolbar = new JToolBar();
		toolbar.add(helpButton);
		toolbar.setVisible(true);

		return toolbar;
	}

}
