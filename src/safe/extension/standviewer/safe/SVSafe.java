/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2001 Francois de Coligny
 * 
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with this program; if
 * not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

package safe.extension.standviewer.safe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import safe.model.SafeCell;
import safe.model.SafeModel;
import safe.model.SafePlot;
import safe.model.SafePlotSettings;
import safe.model.SafeTree;
import safe.model.SafeVoxel;
import capsis.commongui.projectmanager.StepButton;
import capsis.defaulttype.Tree;
import capsis.defaulttype.plotofcells.Cell;
import capsis.extension.standviewer.SVSimple;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * SVSafe is a cartography viewer for trees with coordinates. It draws the trees within the cells.
 * Cells are in different colors attached to the crop status It's based on SVSimple.
 * 
 * @author Isabelle LECOMTE - August 2002
 */
public class SVSafe extends SVSimple {

	static {
		Translator.addBundle ("safe.extension.standviewer.safe.SVSafe");
	}

	// nb-22.08.2018
	//static public String AUTHOR = "Isabelle LECOMTE";
	//static public String VERSION = "1.1";

	// fc - 8.3.2004 - Disposition changes
	private LinePanel mainPanel;
	private JPanel leftPart; // contains subOptionPanel (changed by displayPanel)
	private JPanel rightPart; // contains panel2D, unserPanel2D and rightToPanel2D

	private JPanel rightToPanel2D;
	private JPanel underPanel2D;
	private DisplayChoicePanel displayPanel; // placed in underPanel2D
	private JScrollPane subOptionPanel;
	private ColumnPanel voxelPanel;

	// Voxel seletor
	public int nVoxels;
	public int voxelSelected;
	// public int nbTimeStep; //greg0807

	private JLabel labelList;
	private JLabel labelVoxelDepthTop;
	private JLabel labelVoxelDepthBottom;
	private String[] voxelsDepthTop;
	private String[] voxelsDepthBottom;

	// To choose color adjustement options (auto/user defined)
	private ButtonGroup rdGroup1;
	private JRadioButton rdAutoAdjust, rdUserAdjust;
	private JTextField fldLightMin, fldLightMax;
	private JTextField fldWaterMin, fldWaterMax;
	private JTextField fldNitrogenMin, fldNitrogenMax;
	private JTextField fldTreeRootMin, fldTreeRootMax;
	private JTextField fldCropRootMin, fldCropRootMax;
	private JTextField fldLaiMin, fldLaiMax;
	private JTextField fldYieldMin, fldYieldMax;
	private JTextField fldTemperatureMin, fldTemperatureMax;
	private JTextField fldRootDepthMin, fldRootDepthMax;

	/** Init function */
	@Override
	public void init (GModel model, Step s, StepButton but) throws Exception {
		super.init (model, s, but);

		createOptionPanel (); // create special option panel
		updateLateralPanel ();
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if the extension can
	 * deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!SVSimple.matchWith (referent)) { return false; }
			GModel m = (GModel) referent;
			if (!(m instanceof SafeModel)) { return false; } // fc : not necessary to restrict to
																// SafeModel

		} catch (Exception e) {
			Log.println (Log.ERROR, "SVSafe.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("SVSafe.name");
	}

	@Override
	public String getAuthor() {
		return "Isabelle LECOMTE";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVSafe.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}
	
	/**
	 * Retrieve or create settings for the viewer
	 */
	protected void retrieveSettings () {
		settings = new SVSafeSettings ();

	}

	/**
	 * Settings accessor
	 */
	private SVSafeSettings getVisuSettings () {
		return (SVSafeSettings) settings;
	}

	/**
	 * Called when option panel is validated by user
	 */
	public void optionAction () {
		ExtensionManager.recordSettings (this);
	}

	/**
	 * Action on rdAutoAdjust / rdUserAdjust
	 */
	public void actionPerformed (ActionEvent evt) {
		super.actionPerformed (evt);

		// Adjustment of color scale with user defined values
		getVisuSettings ().colorAutoAjust = false;
		getVisuSettings ().laiValueMin = Check.doubleValue (fldLaiMin.getText ());
		getVisuSettings ().laiValueMax = Check.doubleValue (fldLaiMax.getText ());
		getVisuSettings ().yieldValueMin = Check.doubleValue (fldYieldMin.getText ());
		getVisuSettings ().yieldValueMax = Check.doubleValue (fldYieldMax.getText ());
		getVisuSettings ().temperatureValueMin = Check.doubleValue (fldTemperatureMin.getText ());
		getVisuSettings ().temperatureValueMax = Check.doubleValue (fldTemperatureMax.getText ());
		getVisuSettings ().rootDepthValueMin = Check.doubleValue (fldRootDepthMin.getText ());
		getVisuSettings ().rootDepthValueMax = Check.doubleValue (fldRootDepthMax.getText ());
		getVisuSettings ().lightValueMin = Check.doubleValue (fldLightMin.getText ());
		getVisuSettings ().lightValueMax = Check.doubleValue (fldLightMax.getText ());
		getVisuSettings ().waterValueMin = Check.doubleValue (fldWaterMin.getText ());
		getVisuSettings ().waterValueMax = Check.doubleValue (fldWaterMax.getText ());
		getVisuSettings ().nitrogenValueMin = Check.doubleValue (fldNitrogenMin.getText ());
		getVisuSettings ().nitrogenValueMax = Check.doubleValue (fldNitrogenMax.getText ());
		getVisuSettings ().treeRootValueMin = Check.doubleValue (fldTreeRootMin.getText ());
		getVisuSettings ().treeRootValueMax = Check.doubleValue (fldTreeRootMax.getText ());
		getVisuSettings ().cropRootValueMin = Check.doubleValue (fldCropRootMin.getText ());
		getVisuSettings ().cropRootValueMax = Check.doubleValue (fldCropRootMax.getText ());

		updateLegend ();
	}

	/**
	 * Method to draw a SafeCell within this viewer. Only rectangle r is visible (user coordinates)
	 * -> do not draw if outside. The color of the cell depend of the display option choice (light /
	 * crop / soil)
	 */
	public void drawCell (Graphics2D g2, Cell gcell, Rectangle.Double r) {

		SafeCell cell = (SafeCell) gcell;

		// CROP DISPLAY
		if ((getVisuSettings ().cellOption == 1) && (cell.getCrop () != null)) {

			if (getVisuSettings ().cropChoice == 1) { // LAI
				double lai = cell.getCrop ().getLai ();

				double laiMin = 0;
				double laiMax = 0;
				// adjustment of color scale
				laiMin = getVisuSettings ().laiValueMin;
				laiMax = getVisuSettings ().laiValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (lai == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((lai >= laiMin) && (lai < laiMax)) {
						double laiStep = (laiMax - laiMin) / 15;
						double laiCompare = laiMax;
						int i = 0;
						while ((lai <= laiCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().cropColors[i]);
							i = i + 1;
							laiCompare -= laiStep;
						}
					}
					else if  (lai >= laiMax) {
						g2.setColor (getVisuSettings ().cropColors[0]);
					}
				}
			}

			if (getVisuSettings ().cropChoice == 2) { // YIELD
				double yield = cell.getCrop ().getYield ();
				double yieldMin = 0;
				double yieldMax = 0;
				// adjustment of color scale
				yieldMin = getVisuSettings ().yieldValueMin;
				yieldMax = getVisuSettings ().yieldValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (yield == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((yield >= yieldMin) && (yield < yieldMax)) {
						double yieldStep = (yieldMax - yieldMin) / 15;
						double yieldCompare = yieldMax;
						int i = 0;
						while ((yield <= yieldCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().cropColors[i]);
							i = i + 1;
							yieldCompare -= yieldStep;
						}
					}
					else if  (yield >= yieldMax) {
						g2.setColor (getVisuSettings ().cropColors[0]);
					}					
				}
			}

			if (getVisuSettings ().cropChoice == 3) { // Temperature
				double temperature = cell.getCrop ().getCropTemperature ();
				double temperatureMin = 0;
				double temperatureMax = 0;
				// adjustment of color scale with user defined values
				temperatureMin = getVisuSettings ().temperatureValueMin;
				temperatureMax = getVisuSettings ().temperatureValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (temperature == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((temperature >= temperatureMin) && (temperature < temperatureMax)) {
						double temperatureStep = (temperatureMax - temperatureMin) / 15;
						double temperatureCompare = temperatureMax;
						int i = 0;
						while ((temperature <= temperatureCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().cropColors[i]);
							i = i + 1;
							temperatureCompare -= temperatureStep;
						}
					}
					else if  (temperature >= temperatureMax) {
						g2.setColor (getVisuSettings ().cropColors[0]);
					}	
				}

			}

			if (getVisuSettings ().cropChoice == 4) { // ROOT DEPTH
				double rootDepth = cell.getCrop ().getRootsDepth ();
				double rootDepthMin = 0;
				double rootDepthMax = 0;
				// adjustment of color scale with user defined values
				rootDepthMin = getVisuSettings ().rootDepthValueMin;
				rootDepthMax = getVisuSettings ().rootDepthValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (rootDepth == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((rootDepth >= rootDepthMin) && (rootDepth < rootDepthMax)) {
						double rootDepthStep = (rootDepthMax - rootDepthMin) / 15;
						double rootDepthCompare = rootDepthMax;
						int i = 0;
						while ((rootDepth <= rootDepthCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().cropColors[i]);
							i = i + 1;
							rootDepthCompare -= rootDepthStep;
						}
					}
					else if  (rootDepth >= rootDepthMax) {
						g2.setColor (getVisuSettings ().cropColors[0]);
					}
				}
			}

			// LIGHT DISPLAY
		} else if (getVisuSettings ().cellOption == 2) {

			// float[] energy = new float[nbTimeStep]; //greg0807

			double relEnergy = 0;
			if (getVisuSettings ().lightChoice == 1) {
				relEnergy = cell.getRelativeTotalParIncident ();
			} else if (getVisuSettings ().lightChoice == 2) {
				relEnergy = cell.getRelativeDiffuseParIncident ();
			} else if (getVisuSettings ().lightChoice == 3) {
				relEnergy = cell.getRelativeDirectParIncident ();
			}
			/*
			 * for (int i=0; i<nbTimeStep; i++) { if (getVisuSettings ().lightChoice == 4+i) {
			 * relEnergy = energy[i]; } }
			 */// greg0807

			double lightMin = 0;
			double lightMax = 0;
			// adjustment of color scale with user defined values
			lightMin = (getVisuSettings ().lightValueMin) / 100;
			lightMax = (getVisuSettings ().lightValueMax) / 100;

			// Color gradient (15 different colors)
			g2.setColor (getVisuSettings ().colorNull);
			if ((relEnergy >= lightMin) && (relEnergy < lightMax)) {
				double lightStep = (lightMax - lightMin) / 15;
				double lightCompare = lightMax;
				int i = 0;
				while ((relEnergy <= lightCompare) && (i < 15)) {
					g2.setColor (getVisuSettings ().lightColor[i]);
					i = i + 1;
					lightCompare -= lightStep;
				}
			}
			else if  (relEnergy >= lightMax) {
				g2.setColor (getVisuSettings ().lightColor[0]);
			}

			// BELOW GROUND DISPLAY
		} else if (getVisuSettings ().cellOption == 3) {

			if (getVisuSettings ().groundChoice == 1) { // WATER CONTENT
				double water = (cell.getVoxels ())[voxelSelected].getTheta ();

				double waterMin = 0;
				double waterMax = 0;
				// adjustment of color scale with user defined values
				waterMin = getVisuSettings ().waterValueMin;
				waterMax = getVisuSettings ().waterValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if ((water >= waterMin) && (water < waterMax)) {
					double waterStep = (waterMax - waterMin) / 15;
					double waterCompare = waterMax;
					int i = 0;
					while ((water <= waterCompare) && (i < 15)) {
						g2.setColor (getVisuSettings ().waterColor[i]);
						i = i + 1;
						waterCompare -= waterStep;
					}
				}
				else if  (water >= waterMax) {
					g2.setColor (getVisuSettings ().waterColor[0]);
				}
			}

			if (getVisuSettings ().groundChoice == 2) { // NITROGEN STOCK
				double nitrogen = (cell.getVoxels ())[voxelSelected].getNitrogenNo3Stock ();
				nitrogen += (cell.getVoxels ())[voxelSelected].getNitrogenNh4Stock ();
				nitrogen = nitrogen / 10 / (cell.getVoxels ())[voxelSelected].getVolume (); // convert
																							// g to
																							// kg
																							// ha-1

				double nitrogenMin = 0;
				double nitrogenMax = 0;
				// adjustment of color scale with user defined values
				nitrogenMin = getVisuSettings ().nitrogenValueMin;
				nitrogenMax = getVisuSettings ().nitrogenValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if ((nitrogen >= nitrogenMin) && (nitrogen < nitrogenMax)) {
					double nitrogenStep = (nitrogenMax - nitrogenMin) / 15;
					double nitrogenCompare = nitrogenMax;
					int i = 0;
					while ((nitrogen <= nitrogenCompare) && (i < 15)) {
						g2.setColor (getVisuSettings ().nitrogenColor[i]);
						i = i + 1;
						nitrogenCompare -= nitrogenStep;
					}
				}
				else if  (nitrogen >= nitrogenMax) {
					g2.setColor (getVisuSettings ().nitrogenColor[0]);
				}		

			}

			if (getVisuSettings ().groundChoice == 3) { // TREE ROOT DENSITY
				double treeRoot = ((cell.getVoxels ())[voxelSelected].getTotalTreeRootsDensity ());
				double treeRootMin = 0;
				double treeRootMax = 0;
				// adjustment of color scale with user defined values
				treeRootMin = getVisuSettings ().treeRootValueMin;
				treeRootMax = getVisuSettings ().treeRootValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (treeRoot == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((treeRoot >= treeRootMin) && (treeRoot < treeRootMax)) {
						double treeRootStep = (treeRootMax - treeRootMin) / 15;
						double treeRootCompare = treeRootMax;
						int i = 0;
						while ((treeRoot <= treeRootCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().rootColor[i]);
							i = i + 1;
							treeRootCompare -= treeRootStep;
						}
					}
					else if  (treeRoot >= treeRootMax) {
						g2.setColor (getVisuSettings ().rootColor[0]);
					}	
				}
			}

			if (getVisuSettings ().groundChoice == 4) { // CROP ROOT DENSITY
				double cropRoot = ((cell.getVoxels ())[voxelSelected].getCropRootsDensity ());
				double cropRootMin = 0;
				double cropRootMax = 0;
				// adjustment of color scale with user defined values
				cropRootMin = getVisuSettings ().cropRootValueMin;
				cropRootMax = getVisuSettings ().cropRootValueMax;

				g2.setColor (getVisuSettings ().colorNull);
				if (cropRoot == 0)
					g2.setColor (getVisuSettings ().colorNull);
				else {
					if ((cropRoot >= cropRootMin) && (cropRoot < cropRootMax)) {
						double cropRootStep = (cropRootMax - cropRootMin) / 15;
						double cropRootCompare = cropRootMax;
						int i = 0;
						while ((cropRoot <= cropRootCompare) && (i < 15)) {
							g2.setColor (getVisuSettings ().rootColor[i]);
							i = i + 1;
							cropRootCompare -= cropRootStep;
						}
					}
					else if  (cropRoot >= cropRootMax) {
						g2.setColor (getVisuSettings ().rootColor[0]);
					}	
				}
			}
		}

		// Fill cells with crop or not
		Shape sh = gcell.getShape ();
		Rectangle2D bBox = sh.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.fill (sh);
		}

		// Draw cell lines if required
		g2.setColor (getCellColor ());
		if (r.intersects (bBox)) {
			g2.draw (sh);
		}

	}

	/**
	 * Method to draw a SafeTree within this viewer.
	 */
	public void drawTree (Graphics2D g2, Tree t, Rectangle.Double r) {

		SafeTree tree = (SafeTree) t;

		// Marked trees are considered dead or prunned -> don't draw
		if (tree.isMarked ()) { return; }

		double width = 0.1; // 10 cm.
		double x = tree.getX ();
		double y = tree.getY ();

		// 1. Draw the tree
		// if (getVisuSettings ().showDiameters) {

		g2.setColor (getTreeColor ());

		// Draw the trunk
		width = tree.getDbhMeters ();
		Shape sh = new Ellipse2D.Double (x - width / 2, y - width / 2, width, width);
		Rectangle2D bBox = sh.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.fill (sh);
		}

		// Draw the crown
		double widthTreeLine = 2 * tree.getCrownRadiusTreeLine ();
		double widthInterRow = 2 * tree.getCrownRadiusInterRow ();
		Shape sh2 = new Ellipse2D.Double (x - widthInterRow / 2, y - widthTreeLine / 2, widthInterRow, widthTreeLine);
		Rectangle2D bBox2 = sh2.getBounds2D ();
		if (r.intersects (bBox2)) {
			g2.draw (sh2);
		}

		// }

		// 2. A label for each tree
		// if (getVisuSettings ().showLabels) {
		g2.setColor (getLabelColor ());
		if (r.contains (new Point.Double (x, y))) {
			g2.drawString (String.valueOf (tree.getId ()), (float) x, (float) y);
		}
		// }
	}

	// Utility method
	//
	private double rounding (double x, int n) {
		double r = (Math.round (x * Math.pow (10, n))) / (Math.pow (10, n));
		return r;
	}

	// Option panel creation, called in constructor
	// fc - 11.3.2004
	//
	private void createOptionPanel () throws Exception {
		try {
			// Option panel definition
			// SVSafe has a special option panel in addition of the
			// SVSimple general option panel
			//
			optionPanel.setLayout (new BoxLayout (optionPanel, BoxLayout.Y_AXIS));
			optionPanel.setAlignmentX (Component.LEFT_ALIGNMENT);
			Border etched = BorderFactory.createEtchedBorder ();

			// Choice of color adjustment (user defined)
			JPanel adjustPanel = new JPanel (new FlowLayout (FlowLayout.LEFT));
			adjustPanel.setLayout (new BoxLayout (adjustPanel, BoxLayout.Y_AXIS));
			Border adjustBorder = BorderFactory.createTitledBorder (etched, Translator
					.swap ("SVSafe.OptionPanel.colorAdjust"));
			adjustPanel.setBorder (adjustBorder);

			JPanel w1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w7 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			JPanel w9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
			fldLightMin = new JTextField (5);
			fldLightMax = new JTextField (5);
			fldWaterMin = new JTextField (5);
			fldWaterMax = new JTextField (5);
			fldNitrogenMin = new JTextField (5);
			fldNitrogenMax = new JTextField (5);
			fldTreeRootMin = new JTextField (5);
			fldTreeRootMax = new JTextField (5);
			fldCropRootMin = new JTextField (5);
			fldCropRootMax = new JTextField (5);
			fldLaiMin = new JTextField (5);
			fldLaiMax = new JTextField (5);
			fldYieldMin = new JTextField (5);
			fldYieldMax = new JTextField (5);
			fldTemperatureMin = new JTextField (5);
			fldTemperatureMax = new JTextField (5);
			fldRootDepthMin = new JTextField (5);
			fldRootDepthMax = new JTextField (5);
			fldLightMin.setText ("" + getVisuSettings ().lightValueMin);
			fldLightMax.setText ("" + getVisuSettings ().lightValueMax);
			fldWaterMin.setText ("" + getVisuSettings ().waterValueMin);
			fldWaterMax.setText ("" + getVisuSettings ().waterValueMax);
			fldNitrogenMin.setText ("" + getVisuSettings ().nitrogenValueMin);
			fldNitrogenMax.setText ("" + getVisuSettings ().nitrogenValueMax);
			fldTreeRootMin.setText ("" + getVisuSettings ().treeRootValueMin);
			fldTreeRootMax.setText ("" + getVisuSettings ().treeRootValueMax);
			fldCropRootMin.setText ("" + getVisuSettings ().cropRootValueMin);
			fldCropRootMax.setText ("" + getVisuSettings ().cropRootValueMax);
			fldLaiMin.setText ("" + getVisuSettings ().laiValueMin);
			fldLaiMax.setText ("" + getVisuSettings ().laiValueMax);
			fldYieldMin.setText ("" + getVisuSettings ().yieldValueMin);
			fldYieldMax.setText ("" + getVisuSettings ().yieldValueMax);
			fldTemperatureMin.setText ("" + getVisuSettings ().temperatureValueMin);
			fldTemperatureMax.setText ("" + getVisuSettings ().temperatureValueMax);
			fldRootDepthMin.setText ("" + getVisuSettings ().rootDepthValueMin);
			fldRootDepthMax.setText ("" + getVisuSettings ().rootDepthValueMax);
			w1.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.cellOption2") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.lightScale") + ")", 170));
			w1.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w1.add (fldLightMin);
			w1.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w1.add (fldLightMax);
			w2.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.groundOption1") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.groundScale1") + ")", 170));
			w2.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w2.add (fldWaterMin);
			w2.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w2.add (fldWaterMax);
			w3.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.groundOption2") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.groundScale2") + ")", 170));
			w3.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w3.add (fldNitrogenMin);
			w3.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w3.add (fldNitrogenMax);
			w4.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.groundOption3") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.groundScale3") + ")", 170));
			w4.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w4.add (fldTreeRootMin);
			w4.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w4.add (fldTreeRootMax);
			w5.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.groundOption4") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.groundScale4") + ")", 170));
			w5.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w5.add (fldCropRootMin);
			w5.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w5.add (fldCropRootMax);
			w6.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.cropOption1") + " "
					+ Translator.swap ("SVSafe.OptionPanel.cropScale1"), 170));
			w6.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w6.add (fldLaiMin);
			w6.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w6.add (fldLaiMax);
			w7.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.cropOption2") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.cropScale2") + ")", 170));
			w7.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w7.add (fldYieldMin);
			w7.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w7.add (fldYieldMax);
			w8.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.cropOption3") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.cropScale3") + ")", 170));
			w8.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w8.add (fldTemperatureMin);
			w8.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w8.add (fldTemperatureMax);
			w9.add (new JWidthLabel (Translator.swap ("SVSafe.OptionPanel.cropOption4") + " ("
					+ Translator.swap ("SVSafe.OptionPanel.cropScale4") + ")", 170));
			w9.add (new JWidthLabel (Translator.swap ("SVSafe.min") + " :", 20));
			w9.add (fldRootDepthMin);
			w9.add (new JWidthLabel (Translator.swap ("SVSafe.max") + " :", 20));
			w9.add (fldRootDepthMax);

			adjustPanel.add (w1);
			adjustPanel.add (w2);
			adjustPanel.add (w3);
			adjustPanel.add (w4);
			adjustPanel.add (w5);
			adjustPanel.add (w6);
			adjustPanel.add (w7);
			adjustPanel.add (w8);
			adjustPanel.add (w9);
			optionPanel.add (adjustPanel);

		} catch (Exception e) {
			Log.println (Log.ERROR, "SVSafe.createOptionPanel ()", "Exception", e);
			throw e; // propagate
		}

	}

	// Panel for legend
	//
	private void updateLegend () {

		SafeModel model = (SafeModel) (stepButton.getStep ().getProject ().getModel ());
		SafePlotSettings plotSettings = (SafePlotSettings) model.getPlotSettings ();

		// Legend panel will create crop control cell
		// this cell needs to get a reference to SVSafe to open its inspector
		//
		LegendPanel legendPanel = new LegendPanel (plotSettings, getVisuSettings (), this);
		JPanel aux0 = new JPanel (new BorderLayout ());
		aux0.add (legendPanel, BorderLayout.WEST);
		aux0.setBackground (Color.WHITE);

		JPanel aux = new JPanel (new BorderLayout ());
		aux.add (aux0, BorderLayout.NORTH);
		aux.setBackground (Color.WHITE);

		// Place it well, replace it if already exists
		setLegend (aux);
	}

	// Create panel for voxel selection
	// fc - 11.3.2004
	//
	private void createVoxelPanel () {
		voxelSelected = 0;

		Collection cells = ((SafePlot) stand.getPlot ()).getCells ();
		Iterator it = cells.iterator ();
		SafeCell cell = (SafeCell) it.next ();
		nVoxels = (cell.getVoxels ()).length; // voxel number
		String[] list = new String[nVoxels];
		voxelsDepthTop = new String[nVoxels];
		voxelsDepthBottom = new String[nVoxels];

		// fill temporary tables with voxels depths
		for (int i = 0; i < nVoxels; i++) {
			SafeVoxel voxel = (cell.getVoxels ())[i];
			// to avoid rouding problems
			double x = voxel.getSurfaceDepth () + voxel.getThickness ();
			x = (Math.round (x * Math.pow (10, 2))) / (Math.pow (10, 2));
			list[i] = "" + voxel.getSurfaceDepth () + " - " + x;
		}

		// fill combo box with voxels depths

		final JComboBox voxelsList = new JComboBox (list);
		voxelsList.setMaximumRowCount (5);
		voxelsList.addItemListener (new ItemListener () {

			public void itemStateChanged (ItemEvent e) {
				if ((getVisuSettings ().cellOption != 1) && (getVisuSettings ().cellOption != 2)) {
					voxelSelected = voxelsList.getSelectedIndex ();
					update (); // see SVSimple
				}
			}
		});

		// panel creation
		voxelPanel = new ColumnPanel ();
		Border etched1 = BorderFactory.createEtchedBorder ();
		Border voxelsListBorder = BorderFactory.createTitledBorder (etched1, Translator
				.swap ("SVSafe.OptionPanel.voxelChoiceTitle"));
		voxelPanel.setBorder (voxelsListBorder);
		labelList = new JLabel (Translator.swap ("SVSafe.OptionPanel.voxelChoice"));
		LinePanel l1 = new LinePanel ();
		l1.add (labelList);
		l1.addGlue ();
		voxelPanel.add (l1);
		voxelPanel.add (voxelsList);
	}

	// fc - 11.3.2004
	// when called, this method changes the lateral options according
	// to visu settings, then updates the legend, then update the
	// viewer, then memorizes the settings for further reopening
	//
	protected void updateLateralPanel () {
		// nbTimeStep = 0; //greg0807
		ColumnPanel lateralPanel = new ColumnPanel (); // fc - 9.3.2004
		SVSafeSettings visuSettings = getVisuSettings ();

		if (visuSettings.cellOption == 1) {
			lateralPanel.add (new CropOptionPanel (visuSettings, this)); // fc - 9.3.2004
			voxelSelected = 0;
		} else if (visuSettings.cellOption == 2) {
			LightOptionPanel light = new LightOptionPanel (visuSettings, step, this);
			lateralPanel.add (light); // fc - 9.3.2004
			voxelSelected = 0;
			// nbTimeStep = light.getNbTimeStep(); //greg0807
		} else if (visuSettings.cellOption == 3) {
			LinePanel aux1 = new LinePanel (0, 0);
			aux1.add (new GroundOptionPanel (visuSettings, this));
			aux1.addStrut0 ();

			LinePanel aux2 = new LinePanel (0, 0);
			aux2.add (voxelPanel);
			aux2.addStrut0 ();

			lateralPanel.add (aux1); // fc - 9.3.2004
			lateralPanel.add (aux2);
			lateralPanel.addGlue ();

		}

		JPanel aux3 = new JPanel (new BorderLayout (0, 0));
		aux3.add (lateralPanel, BorderLayout.NORTH);
		subOptionPanel.getViewport ().setView (aux3);
		updateLegend ();
		update (); // see SVSimple
		optionAction (); // memo current settings

	}

	// fc - 8.3.2004 - createUI redefinition, called in superclass constructor
	// Main viewer interface disposition
	//
	protected void createUI () {
		try {
			
			// fc-23.12.2020
//			defineTitle ();
			
			getContentPane ().setLayout (new BorderLayout ()); // mainBox in the internalFrame

			mainPanel = new LinePanel (); // subOptionPanel + rightPart

			// 1. left part
			//
			// Voxel selection panel
			createVoxelPanel ();

			// Data choice panel (crop, light or soil)
			GenericOptionPanel lateralOptionPanel = null;
			subOptionPanel = new JScrollPane (lateralOptionPanel);

			mainPanel.add (subOptionPanel); // leftPart unused

			// 2. right part
			//
			underPanel2D = new JPanel (new GridLayout (1, 1));
			rightToPanel2D = new JPanel (new GridLayout (1, 1));

			// Choice: crop / light / ground
			displayPanel = new DisplayChoicePanel (getVisuSettings (), this);
			underPanel2D.add (displayPanel);

			scrollPane = new JScrollPane (panel2D);
			scrollPane.getViewport ().putClientProperty ("EnableWindowBlit", Boolean.TRUE); // faster

			// fc - 8.3.2004 - Reserve some place under panel2D
			// to place display options

			// fc - 7.4.2006
			splitPane = new JSplitPane ();
			splitPane.setLeftComponent (scrollPane);
			splitPane.setRightComponent (null);
			splitPane.setResizeWeight (1); // when resizing, the extra space goes to the left
											// component

			JPanel rightPart = new JPanel (new BorderLayout ());
			rightPart.add (splitPane, BorderLayout.CENTER);
			rightPart.add (underPanel2D, BorderLayout.SOUTH);

			mainPanel.add (rightPart);

			updateLateralPanel (); // fc - 15.3.2004

			// create and add legend Panel (right to panel2D)
			updateLegend ();

			getContentPane ().add (getPilot (), BorderLayout.NORTH);
			getContentPane ().add (mainPanel, BorderLayout.CENTER);

		} catch (Exception e) {
			Log.println (Log.ERROR, "SVSafe.createUI", "exception ", e);
		}

	}

	/**
	 * Refresh the GUI with another Step.
	 */
	public void update (StepButton sb) {
		super.update (sb);
		updateLateralPanel ();
	}

}
