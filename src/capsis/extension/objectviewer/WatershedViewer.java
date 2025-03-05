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

package capsis.extension.objectviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.io.File;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Node;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.extension.AbstractObjectViewer;
import capsis.extension.modeltool.rivermaker.NodesWriter;
import capsis.extension.modeltool.rivermaker.Reach;
import capsis.extension.modeltool.rivermaker.Weir;
import capsis.gui.MainFrame;
import jeeb.lib.util.PathManager;
import capsis.util.Drawer;
import capsis.util.JSmartFileChooser;
import capsis.util.NeighboursCounter;
import capsis.util.Panel2D;
import capsis.util.WatershedNode;

/**
 * A viewer for watersheds Possible to use directly this ov in a DUser2 dialog
 * (without control panel)
 *
 * @author B. Parisi - April 2006
 */
public class WatershedViewer extends AbstractObjectViewer implements Drawer, ActionListener {

	static {
		Translator.addBundle ("capsis.extension.objectviewer.WatershedViewer");
	}
	
	// nb-07.08.2018
	//static public final String NAME = Translator.swap ("WatershedViewer");
	//static public final String DESCRIPTION = Translator.swap ("WatershedViewer.description");
	//static public final String AUTHOR = "B. Parisi";
	//static public final String VERSION = "1.0";

	private Collection nodes;
	private float[] ratios;
	// private float [] mRatios;
	private double[][] connectivity;
	private Panel2D panel2D;

	private JButton save;
	private JButton close;
	private JButton help;

	/**
	 * Default constructor.
	 */
	public WatershedViewer () {
	}

	public void init (Collection s) throws Exception {

		try {

			nodes = (Collection) s;

			panel2D = calculatePanel2D (nodes);

			int[][] branches = calculateNetworkBranches (nodes);
			ratios = calculateRatios (networkQuantities (branches));
			NeighboursCounter NC = new NeighboursCounter ();

			connectivity = NC.connectivityStats (nodes);
			createUI ();

		} catch (Exception exc) {
			Log.println (Log.ERROR, "WatershedViewer.c ()", exc.toString (), exc);
			throw exc; // fc - 4.11.2003 - object viewers may throw exception
		}

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			// A spatialized GTree or a collection of some : true
			if (referent instanceof WatershedNode) { return true; }

			if (referent instanceof Collection) {
				Collection c = (Collection) referent;
				if (c.isEmpty ()) { return false; }

				// Possibly several subclasses in the collection
				// if there is at least one spatialized tree in the collection,
				// ok
				Collection reps = Tools.getRepresentatives (c); // one instance
																// of each class
				for (Iterator i = reps.iterator (); i.hasNext ();) {
					Object e = i.next ();
					if (!(e instanceof WatershedNode)) { return false; }
				}
				return true;
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "WatershedViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("WatershedViewer.name");
	}

	@Override
	public String getAuthor() {
		return "B. Parisi";
	}

	@Override
	public String getDescription() {
		return Translator.swap("WatershedViewer.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public Collection show(Collection candidateSelection, boolean thisIsAReselection) {
		// fc-5.7.2018 WatershedViewer seems to not override show()...
		return Collections.EMPTY_LIST;
	}
	
	/**
	 * User interface definition.
	 */
	private void createUI () {
// comment specifier une taille par d�faut pour l'element central du borderlayout ? - jl 07.12
		// Layout parts
		setLayout (new BorderLayout ());

		JPanel indices = new JPanel (new BorderLayout ());
		JPanel R = new JPanel (new FlowLayout (FlowLayout.CENTER));
		JPanel cbar = new JPanel (new FlowLayout (FlowLayout.CENTER));
		JPanel cvar = new JPanel (new FlowLayout (FlowLayout.CENTER));
		NumberFormat NF = NumberFormat.getInstance ();
		NF.setMaximumFractionDigits (3);
		JLabel nodesNbr = new JLabel ("Nodes number = " + nodes.size ());
		JLabel rn = new JLabel ("Rn ratio = " + NF.format (ratios[0]));
		JLabel rl = new JLabel ("Rl ratio = " + NF.format (ratios[1]));
		JLabel cbar1 = new JLabel ("cbar1 = " + NF.format (connectivity[0][0]));
		JLabel cvar1 = new JLabel ("cvar1 = " + NF.format (connectivity[0][1]));
		JLabel cbar2 = new JLabel ("cbar2 = " + NF.format (connectivity[1][0]));
		JLabel cvar2 = new JLabel ("cvar2 = " + NF.format (connectivity[1][1]));
		JLabel cbar3 = new JLabel ("cbar3 = " + NF.format (connectivity[2][0]));
		JLabel cvar3 = new JLabel ("cvar3 = " + NF.format (connectivity[2][1]));
		JLabel cbar4 = new JLabel ("cbar4 = " + NF.format (connectivity[3][0]));
		JLabel cvar4 = new JLabel ("cvar4 = " + NF.format (connectivity[3][1]));
		JLabel cbar5 = new JLabel ("cbar5 = " + NF.format (connectivity[4][0]));
		JLabel cvar5 = new JLabel ("cvar5 = " + NF.format (connectivity[4][1]));

		R.add (nodesNbr);
		R.add (rn);
		R.add (rl);
		cbar.add (cbar1);
		cvar.add (cvar1);
		cbar.add (cbar2);
		cvar.add (cvar2);
		cbar.add (cbar3);
		cvar.add (cvar3);
		cbar.add (cbar4);
		cvar.add (cvar4);
		cbar.add (cbar5);
		cvar.add (cvar5);

		indices.add (R, BorderLayout.NORTH);
		indices.add (cbar, BorderLayout.CENTER);
		indices.add (cvar, BorderLayout.SOUTH);

		add (indices, BorderLayout.NORTH);

		add (new JScrollPane (panel2D), BorderLayout.CENTER);

		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		save = new JButton (Translator.swap ("Shared.save"));
		save.addActionListener (this);
		pControl.add (save);
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
//		pControl.add (close); // removed fc-1.8.2012, see below
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (help);

		add (pControl, BorderLayout.SOUTH);

		// Do not set sizes explicitly inside object viewers
		// ~ this.setPreferredSize (new Dimension (500, 450));

	}

	/**
	 * Save
	 */
	public void saveAction () {
// here I have a problem with the path to save the network file. - jl 07.12
		
// I rewrote the code below fc-21.8.2012
		boolean trouble = false;
		JFileChooser chooser = null;
		int returnVal = 0;

		int RnInt = (int) (1000 * ratios[0] + 0.5);
		int RlInt = (int) (1000 * ratios[1] + 0.5);
		String Rn = Integer.toString (RnInt);
		String Rl = Integer.toString (RlInt);
		String candidateName = (Rn + "_" + Rl);

		do {
			trouble = false;

			String candidateDir = Settings.getProperty(
					"capsis.data.dynet", PathManager.getInstallDir());
			
			chooser = new JFileChooser(candidateDir);
			chooser.setDialogType(JFileChooser.SAVE_DIALOG);

			// Try to set the file name with a default proposal
			chooser.setSelectedFile(new File(candidateDir + File.separator + candidateName));

			returnVal = chooser.showDialog(this, null); // null: approveButton

			if (returnVal == JFileChooser.APPROVE_OPTION
					&& chooser.getSelectedFile().exists()) {
				if (!Question
						.ask(this,
								Translator.swap ("Shared.confirm"), ""
										+ chooser.getSelectedFile ().getPath () + "\n"
										+ Translator.swap ("Shared.fileExistsPleaseConfirmOverwrite"))) {
					trouble = true;
				}
			}

		} while (trouble);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			File file = chooser.getSelectedFile();
			Settings.setProperty("capsis.data.dynet", file.getParent());

			String fileName = file.getAbsolutePath();

			try {
				// float [][] indices =
				// {{ratios[ratios.length-1][0],ratios[ratios.length-1][1]};
				NodesWriter w = new NodesWriter (nodes, ratios);
				w.setHeaderEnabled (false);
				w.save (fileName);

			} catch (Exception e) {
				Log.println (Log.ERROR, "WatershedViewer.saveAction ()", "Unable to write tree to disk."
						+ " Target file=" + fileName + " " + e.toString (), e);
				MessageDialog.print (this, Translator.swap ("WatershedViewer.saveErrorSeeLog"));
			}
			
		}
		
	}

	/**
	 * Used for the settings and filtering buttons.
	 */
	public void actionPerformed (ActionEvent evt) {

		if (evt.getSource ().equals (close)) {
// here I do not know how to close the window with this close button ! help ! - jl 07.12
			
// fc-1.8.2012 this object is a JPanel, not directly a JDialog -> a JPanel cannot be closed
// This panel is supposed to be embedded in a dialog at some time -> the dialog will have its own 'close' feature
// -> I removed the close button
			
		} else if (evt.getSource ().equals (save)) {
			saveAction ();

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**
	 * We have to redraw the subscene
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		Map drawnNodes = new HashMap ();
		// ~ drawnNodesPositions = new HashMap ();

		// 2. Choose a pixel detailThreshold, compute it in meters with current
		// scale
		// ~ visibleThreshold = 1.1 / panel2D.getCurrentScale ().x; // 1 pixel
		// in in meters

		for (Iterator i = nodes.iterator (); i.hasNext ();) {
			WatershedNode n = (WatershedNode) i.next ();
			drawNode (g2, n, r, drawnNodes);
		}

		// 7. draw information on the panel2D
		// ~ StringBuffer label = new StringBuffer ();
		// ~ label.append (Translator.swap ("WatershedViewer.numberOfTrees"));
		// ~ label.append (": ");
		// ~ label.append (locations.size ());
		// ~ if (drawnCells != null && !drawnCells.isEmpty ()) {
		// ~ label.append (" ");
		// ~ label.append (Translator.swap ("WatershedViewer.numberOfCells"));
		// ~ label.append (": ");
		// ~ label.append (drawnCells.size ());
		// ~ }
		// ~ //java.awt.geom.Point2D.Double o = panel2D.getUserPoint (new Point
		// (0, y));
		// FontMetrics fm = g.getFontMetrics ();
		// ~ int fontHeight = 10;
		// ~ java.awt.geom.Point2D.Double up = panel2D.getUserPoint (new Point
		// (0, fontHeight)); // Rectangle2D.Double
		// ~ float x = (float) up.x;
		// ~ float y = (float) up.y;
		// ~ g2.setColor (Color.BLACK);
		// ~ g2.drawString (label.toString (), x, y);

	}

	// Draw a node
	//
	private void drawNode (Graphics2D g2, WatershedNode n, Rectangle.Double r, Map drawnNodes) {

		Vertex3d origin = n.getOrigin ();
		Vertex3d end = n.getEnd ();

		if (end == null) { // circle
			double radius = 0.1;
			Shape shape = new Ellipse2D.Double (origin.x - radius, origin.y - radius, radius * 2, radius * 2);
			// ~ if (selectedNodes != null && selectedNodes.contains (n)) {
			// ~ g2.setColor (Color.RED);
			// ~ } else {
			g2.setColor (Color.BLACK);
			// ~ }
			g2.draw (shape);

		} else { // line
			Shape shape = new Line2D.Double (origin.x, origin.y, end.x, end.y);
			g2.setColor (Color.BLACK);
			g2.draw (shape);

		}

		// ~ drawnNodes.put (shape, node);

		// ~ drawnNodesPositions.put (node, end);
		// ~ return end;
	}

	/**
	 * Some selection was done
	 */
	public JPanel select (Rectangle.Double r, boolean more) {
		return null; // desactivated
	}

	// Calculate the size extension of the panel2D to view the complete selected
	// scene
	//
	private Panel2D calculatePanel2D (Collection nodes) {
		double x0 = Double.MAX_VALUE;
		double x1 = Double.MIN_VALUE;
		double y0 = Double.MAX_VALUE;
		double y1 = Double.MIN_VALUE;

		for (Iterator i = nodes.iterator (); i.hasNext ();) {

			WatershedNode n = (WatershedNode) i.next ();

			// ~ // fc - 5.2.2004 - allow not spatialized trees
			Vertex3d origin = n.getOrigin ();
			Vertex3d end = n.getEnd ();
			x0 = Math.min (x0, origin.x);
			x1 = Math.max (x1, origin.x);
			y0 = Math.min (y0, origin.y);
			y1 = Math.max (y1, origin.y);
			if (end != null) {
				x0 = Math.min (x0, end.x);
				x1 = Math.max (x1, end.x);
				y0 = Math.min (y0, origin.y);
				y1 = Math.max (y1, origin.y);
			}
		}

		Rectangle.Double r2 = new Rectangle.Double (x0, y0, x1 - x0, y1 - y0);
System.out.println ("Panel2D coordinates are "+x0+" "+ y0+" "+ x1+" "+ y1+" "+Panel2D.X_MARGIN_IN_PIXELS+" "+ Panel2D.Y_MARGIN_IN_PIXELS);
		Panel2D panel2D = new Panel2D (this, r2, Panel2D.X_MARGIN_IN_PIXELS, Panel2D.Y_MARGIN_IN_PIXELS);
		return panel2D;
	}

	// process to record a matrice of order and length of all branches of the
	// network
	// process to record a matrice of order and length of all branches of the
	// network
	public int[][] calculateNetworkBranches (Collection nodes) {

		Collection orderTree = new ArrayList ();
		orderTree.addAll (nodes);
		int reachCount = 0;
		for (Iterator j = orderTree.iterator (); j.hasNext ();) {
			Node n = (Node) j.next ();
			n.setVisible (false);
			if (!(n instanceof Weir)) {
				reachCount++;
			}
		}

		int[][] resultMatrix = new int[reachCount][2]; // temporary Matrice
														// recording the order
														// and length of each
														// segment --

		int visitedBranches = 0; // to count the remaining branches
		int visitedReaches = 0; // to count the remaining reaches

		method: do {
			for (Iterator j = orderTree.iterator (); j.hasNext ();) {
				Node n = (Node) j.next ();

				if (n instanceof Weir) { // not considered
					continue;
				} else if (n.isVisible () == false && ((Reach) n).getUpstream (1).isEmpty ()) { // for
																								// "leaf"
																								// reaches.
					Reach r = (Reach) n;
					Collection tempReaches = new ArrayList ();
					if (!(r.getDownstreamOrder (r.getOrder ()).isEmpty ())) { // get
																				// the
																				// collection
																				// of
																				// reaches
																				// downstream
																				// having
																				// the
																				// same
																				// order
																				// (for
																				// the
																				// moment
																				// this
																				// method
																				// is
																				// only
																				// in
																				// reach
																				// from
																				// rivermaker)
						tempReaches = r.getDownstreamOrder (r.getOrder ());
						for (Iterator k = tempReaches.iterator (); k.hasNext ();) {
							Reach f = (Reach) k.next ();
							// (f.getFather()).remove();
							f.setVisible (true); // check boolean in order not
													// to consider again later
							visitedReaches++;
							// System.out.println("removed a downstream reach"
							// );
						}
					}
					int length = tempReaches.size () + 1; // the size of
															// collection may be
															// zero.
					int order = r.getOrder ();
					// System.out.println("real order = " + order );
					// System.out.println("real length = " + length );
					resultMatrix[visitedBranches][0] = order;
					resultMatrix[visitedBranches][1] = length;
					System.out.println ("order = " + resultMatrix[visitedBranches][0]);
					System.out.println ("length = " + resultMatrix[visitedBranches][1]);
					r.setVisible (true);
					visitedBranches++;
					visitedReaches++;

					// System.out.println("visitedReaches is "+ visitedReaches
					// );
				} else if (n.isVisible () == false) { // other possible case:
														// the reach is not a
														// "leaf", and may have
														// upstream sons.
					Reach r = (Reach) n;
					int oup = 0;
					Collection up = r.getUpstream (1);
					for (Iterator h = up.iterator (); h.hasNext ();) {
						Reach u = (Reach) h.next ();
						oup = Math.max (oup, u.getOrder ());
					}

					if (r.getOrder () > oup) { // order of reach must be greater
												// than orders of sons.

						Collection tempReaches = new ArrayList ();
						if (!(r.getDownstreamOrder (r.getOrder ()).isEmpty ())) {
							tempReaches = r.getDownstreamOrder (r.getOrder ());
							for (Iterator k = tempReaches.iterator (); k.hasNext ();) {
								Reach f = (Reach) k.next ();
								f.setVisible (true);
								visitedReaches++;

								// System.out.println("removed a downstream reach"
								// );
							}
						}
						int length = tempReaches.size () + 1;
						int order = r.getOrder ();
						// System.out.println("real order = " + order );
						// System.out.println("real length = " + length );
						resultMatrix[visitedBranches][0] = order;
						resultMatrix[visitedBranches][1] = length;
						// System.out.println("order = " +
						// resultMatrix[visitedBranches][0] );
						// System.out.println("length = " +
						// resultMatrix[visitedBranches][1] );
						r.setVisible (true);
						visitedBranches++;
						visitedReaches++;
						// System.out.println("removed a reach" );
						// System.out.println("visitedReaches is "+
						// visitedReaches );
					}
				}

			}

		} while (visitedReaches < reachCount);
		// here we just create a new table with the right size and only the data
		// of interest.
		int matrixRealLength = 0;
		for (int a = 0; a < resultMatrix.length; a++) {
			if (resultMatrix[a][0] != 0) {
				matrixRealLength++;
			}
			// System.out.println("RMorder = " + resultMatrix[a][0] );
			// System.out.println("RMlength = " + resultMatrix[a][1] );
		}
		int[][] finalMatrix = new int[matrixRealLength][2];
		for (int a = 0; a < resultMatrix.length; a++) {
			if (resultMatrix[a][0] != 0) {
				finalMatrix[a][0] = resultMatrix[a][0];
				finalMatrix[a][1] = resultMatrix[a][1];
				// System.out.println("order = " + finalMatrix[a][0] );
				// System.out.println("length = " + finalMatrix[a][1] );
			}

		}

		return finalMatrix;

	}

	/**
	 * Method that computes the table order versus reach numbers. used in the
	 * calculation of regression slope (that will estimate Rn and Rl). jl -
	 * 01.09.2006
	 */
	public float[][] networkQuantities (int[][] branches) {

		int wMax = 0;
		for (int i = 0; i < branches.length; i++) { // check to find order max
			if (branches[i][0] > wMax) {
				wMax = branches[i][0];
			}
		}

		float[][] dataReg = new float[wMax][2];

		for (int i = 0; i < branches.length; i++) {
			for (int j = 1; j < (wMax + 1); j++) {
				if (branches[i][0] == j) {
					dataReg[j - 1][0] = dataReg[j - 1][0] + 1;
					dataReg[j - 1][1] = dataReg[j - 1][1] + branches[i][1];
				}
			}
		}

		for (int i = 0; i < dataReg.length; i++) {
			dataReg[i][1] = dataReg[i][1] / dataReg[i][0];
			dataReg[i][0] = (float) Math.log (dataReg[i][0]);

			dataReg[i][1] = (float) Math.log (dataReg[i][1]);
		}
		for (int i = 0; i < dataReg.length; i++) {
			System.out.println ("order " + (i + 1) + " number is " + dataReg[i][0]);
			System.out.println ("order " + (i + 1) + " mean length is " + dataReg[i][1]);
		}

		return dataReg;
	}

	/**
	 * Method that computes the estimates of slopes and deduces the ratios Rn
	 * and Rl. used in the calculation of regression slope (that will estimate
	 * Rn and Rl). jl - 01.09.2006
	 */
	public float[] calculateRatios (float[][] dataReg) {

		int wMax = dataReg.length;
		float sumXn = 0;
		float sumXl = 0;
		float sumX2n = 0;
		float sumX2l = 0;
		float sumYn = 0;
		float sumYl = 0;
		float sumXYn = 0;
		float sumXYl = 0;
		float meanXn = 0;
		float meanXl = 0;
		float meanYn = 0;
		float meanYl = 0;

		for (int i = 0; i < wMax - 1; i++) {
			sumXn = sumXn + i + 1;
			sumYn = sumYn + dataReg[i][0];
			sumXYn = sumXYn + (i + 1) * dataReg[i][0];
		}
		for (int i = 0; i < wMax; i++) {

			sumXl = sumXl + i + 1;
			sumYl = sumYl + dataReg[i][1];
			sumXYl = sumXYl + (i + 1) * dataReg[i][1];
		}
		meanXn = sumXn / (wMax - 1);
		meanXl = sumXl / (wMax);
		meanYn = sumYn / (wMax - 1);
		meanYl = sumYl / wMax;

		for (int i = 0; i < wMax; i++) {
			sumX2l = sumX2l + (float) Math.pow ((i + 1), 2);
		}
		for (int i = 0; i < wMax - 1; i++) {
			sumX2n = sumX2n + (float) Math.pow ((i + 1), 2);
		}
		float Rn = (float) Math.exp (-(((wMax - 1) * sumXYn) - sumXn * sumYn)
				/ (float) ((wMax - 1) * sumX2n - Math.pow (sumXn, 2)));
		float Rl = (float) Math.exp (((wMax * sumXYl) - sumXl * sumYl) / (float) (wMax * sumX2l - Math.pow (sumXl, 2)));
		System.out.println ("slope is " + (((wMax - 1) * sumXYn) - sumXn * sumYn)
				/ (float) ((wMax - 1) * sumX2n - Math.pow (sumXn, 2)) + "Rn is " + Rn);
		System.out.println ("slope is " + ((wMax * sumXYl) - sumXl * sumYl)
				/ (float) (wMax * sumX2l - Math.pow (sumXl, 2)) + "Rl is " + Rl);
		float[] ratios = { Rn, Rl };
		return ratios;

	}

	/**
	 * process for calculate the mean fork ratio Rn and the mean length ratio Rl
	 * of the network //process for calculate the mean fork ratio Rn and the
	 * mean length ratio Rl of the network
	 *
	 * public float [][] networkRatios ( int[][] branches ) {
	 *
	 * int wMax=0; for (int i=0; i<branches.length; i++) {if
	 * (branches[i][0]>wMax) {wMax=branches[i][0];}} // check to find order max
	 *
	 * int [] nbrBranchesPerOrder = new int [wMax]; // matrice containing the
	 * number of branches per order **i.e line x contains the number of order
	 * x+1's branches int [] sumLengthPerOrder = new int [wMax]; // matrice
	 * containing the sum of the length of branches per order **i.e line x
	 * contains the sum of order x+1's length of branches for (int j=0;
	 * j<branches.length; j++) { int w = branches[j][0];
	 * nbrBranchesPerOrder[w-1]++; sumLengthPerOrder[w-1] += branches[j][1]; }
	 *
	 * float meanLengthPerOrder[] = new float [wMax]; // matrice containing the
	 * mean length of branches per order **i.e line x contains the mean of order
	 * x+1's length of branches for (int w=1; w <= wMax ; w++) {
	 * meanLengthPerOrder[w-1] =
	 * ((float)sumLengthPerOrder[w-1]/(float)nbrBranchesPerOrder[w-1]);
	 * System.out.println("meanLengthPerOrder "+w+"  = " +
	 * meanLengthPerOrder[w-1] ); }
	 *
	 * float [] Rn = new float [wMax-1]; // matrice of Rn values float [] Rl =
	 * new float [wMax-1]; // matrice of Rl values
	 *
	 * for (int w=1; w<wMax; w++) { Rn[w-1] =
	 * ((float)nbrBranchesPerOrder[w-1]/(float)nbrBranchesPerOrder[w]); Rl[w-1]
	 * = (meanLengthPerOrder[w]/meanLengthPerOrder[w-1]);
	 * System.out.println("Rn order "+w+"  = " + Rn[w-1] );
	 * System.out.println("Rl order "+w+"  = " + Rl[w-1] ); }
	 *
	 * float [][] ratios = new float[Rn.length][2];
	 *
	 * for (int k=0; k<Rn.length; k++) { ratios[k][0] = Rn[k]; ratios[k][1] =
	 * Rl[k]; } return ratios; }
	 */

	/**
	 * public float [] meanNetworkRatios ( float [][] networkRatios ) {
	 *
	 *
	 * float [] mRatios = new float [2];
	 *
	 * float sumRn =0f; float sumRl =0f; for (int k=0; k<networkRatios.length;
	 * k++) { sumRn += networkRatios[k][0]; sumRl += networkRatios[k][1]; }
	 *
	 * float meanRn = sumRn/networkRatios.length; //mean fork ratio float meanRl
	 * = sumRl/networkRatios.length; //mean length ratio mRatios [0] = meanRn;
	 * //line Rn.length contains the mean of each ratio mRatios [1] = meanRl;
	 * return mRatios; }
	 */
	/**
	 * check if each ratio's value is between extrem values
	 */
	public boolean areRatiosBetweenExtremValues (Collection nodes) {

		float[] ratios = calculateRatios (networkQuantities (calculateNetworkBranches (nodes)));

		boolean respected = false;
		if ((ratios[0] <= 5) && (3 <= ratios[0]) && (ratios[1] <= 3) && (1.5 <= ratios[1])) {
			respected = true;
		}
		return respected;

	}

}
