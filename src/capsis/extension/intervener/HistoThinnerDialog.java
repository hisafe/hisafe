/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003 Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.extension.intervener;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.DefaultTableModel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.SwingWorker3;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Relay;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import capsis.util.diagram2d.MaidHisto;
import capsis.util.equalizer.Equalizer;
import capsis.util.group.GroupCollection;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.StandDensityIndexInterface;
import capsis.util.methodprovider.StumpTreatmentAvailabilityProvider;

/**
 * This dialog box is used to setHistoThinner parameters in interactive context.
 * 
 * @author F. de Coligny - april 2003
 */
public class HistoThinnerDialog extends AmapDialog implements ActionListener, ChangeListener, ComponentListener {

	/**
	 * A cell renderer with gray font when disabled.
	 */
	private class StringRenderer extends DefaultTableCellRenderer {

		private Color normalForeground;
		private Color normalBackground;

		public StringRenderer() {
			super();
		}

		public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected,
				boolean hasFocus, int row, int column) {
			Component c = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

			if (normalForeground == null || normalBackground == null) {
				normalForeground = c.getForeground();
				normalBackground = c.getBackground();
			}

			JLabel l = (JLabel) c;
			l.setOpaque(true);
			if (value instanceof Number) {
				l.setHorizontalAlignment(JLabel.RIGHT);
			} else {
				l.setHorizontalAlignment(JLabel.LEFT);
			}

			if (table.isEnabled()) {
				if (isSelected) {
					l.setForeground(table.getSelectionForeground());
					l.setBackground(table.getSelectionBackground());
				} else {
					l.setForeground(normalForeground);
					l.setBackground(normalBackground);
				}

			} else {
				l.setForeground(Color.GRAY); // "pending update..."
				l.setBackground(normalBackground);
			}

			return c;
		}
	}

	private int numberOfThreads;
	private HistoThinner intervener;

	private GScene stand1; // we accept Tree : Numberable or not (NumberableTree
							// & SpatializedTree...)
	private TreeCollection stand2;

	private Collection concernedTrees; // restriction on some group
	private boolean groupInUse;

	private MaidHisto histo;
	private Equalizer equalizer;
	private JScrollPane scrollpane;
	private JPanel equalizerPanel;

	// If the connected model methodProvider knows how to calculate Hdom,
	// hdProvider is not null
	private HdomProvider hdProvider;
	private StandDensityIndexInterface sdiInterface;
	private GProvider basalAreaProvider;
	private StumpTreatmentAvailabilityProvider stumpTreatmentAvailabilityProvider;

	private int h0 = 0; // height of component at beginning
	private int eh0 = 0; // height of equalizer panel at beginning

	// This value is the sum of trees in concernedTrees: N before thinning
	private int nBefore;

	// Values in the equalizer
	// Note: the sum of initialValues can be slightly different from nBefore
	// because the equalizer manages only int values
	private int[] initialValues;
	private int[] values;

	private JTextField classWidth;
	private JTextField minSize;

	private JCheckBox girthMode;
	private JCheckBox perHectare;
	private double hectareCoefficient; // coefficient to consider an perHectare
	private JCheckBox centerClasses;

	private JTextField alderValue; // number of trees to be cut with Alder's
									// algorithm
	private JTextField alderWilsonFactor; // this factor will be turned into a
											// number of trees to be
											// cut
	private JTextField alderStandDensityIndex; // this stand density index will
												// be turned into a
												// number of trees to be cut

	private JRadioButton useAlderValue;
	private JRadioButton useWilsonFactor;
	private JRadioButton useStandDensityIndex;
	private ButtonGroup group;

	private JButton alderApply;
	private String applyMemo = ""; // to detect if 'Apply' was forgotten when
									// pressing 'Ok'

	public JCheckBox stumpTreatment; // to add stump treatment against
										// heterobasidion cm tl 02 05 2013
	public JTextField treatedFailure;
	// private double treatedFailure;
	public JTextField probSporeInfection;
	// private double probSporeInfection;

	// Before intervention, after and cut values for N, G and Dg
	final int[] N = new int[3];
	final double[] G = new double[3]; // may be updated in another thread
	final double[] Dg = new double[3]; // may be updated in another thread

	private NumberFormat formater;

	private JTable resultTable;
	private DefaultTableModel resultModel;

	private HistoThinnerCutJob[] cutJobs;

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	/**
	 * Constructor.
	 */
	public HistoThinnerDialog(HistoThinner intervener, GModel model, GScene stand1, Collection concernedTrees) {
		super();

		this.intervener = intervener;

		numberOfThreads = 0;

		this.stand1 = stand1;
		this.stand2 = (TreeCollection) stand1;

		this.concernedTrees = concernedTrees;
		groupInUse = false;
		if (concernedTrees != null && concernedTrees instanceof GroupCollection) {
			groupInUse = true;
		}

		// Calculate nBefore thinning
		double n = 0d;
		double maxDbh = 0d;
		for (Object o : concernedTrees) {
			Tree tree = (Tree) o;
			if (tree.getDbh() > maxDbh) {
				maxDbh = tree.getDbh();
			}
			if (o instanceof Numberable) {
				Numberable t = (Numberable) o;
				n += t.getNumber();
			} else {
				n++;
			}
		}
		nBefore = (int) Math.round(n);

		histo = new MaidHisto(concernedTrees); // fc - 22.9.2004 - maybe a group

		histo.setGirthMode(Settings.getProperty("histo.thinner.girth.mode", false));
		histo.setClassWidth(Settings.getProperty("histo.thinner.class.width", 5));
		if (Settings.getProperty("histo.thinner.min.threshold", 0d) > maxDbh) { // tl
																				// -
																				// 03-04-2014
																				// -
																				// reset
																				// threshold
																				// to
																				// 0
																				// if
																				// there
																				// are
																				// no
																				// trees
																				// to
																				// display
			Settings.setProperty("histo.thinner.min.threshold", "0.0");
		}
		histo.setForcedXMin(Settings.getProperty("histo.thinner.min.threshold", 0d));

		hectareCoefficient = 10000 / intervener.getSceneArea(stand1); // fc-8.6.2020
//		hectareCoefficient = 10000 / stand1.getArea();

		if (Settings.getProperty("histo.thinner.per.hectare", false)) {
			histo.setHectareCoefficient(hectareCoefficient);
		} else {
			histo.setHectareCoefficient(1);
		}

		formater = NumberFormat.getInstance();
		formater.setGroupingUsed(false);
		formater.setMaximumFractionDigits(2);

		// Check the model methodProvider
		checkModelAvailableMethods(model);

		createUI();
		update();

		addComponentListener(this);

		// fc-31.8.2018
		setTitle (intervener.getName());
//		setTitle(Translator.swap("HistoThinnerDialog"));

		setModal(true);

		// Location is set by AmapDialog
		pack(); // uses component's preferredSize
		show();

	}

	private void checkModelAvailableMethods(GModel model) {
		MethodProvider mp = model.getMethodProvider();
		if (mp instanceof HdomProvider)
			hdProvider = (HdomProvider) mp;
		if (mp instanceof StandDensityIndexInterface)
			sdiInterface = (StandDensityIndexInterface) mp;
		if (mp instanceof GProvider)
			basalAreaProvider = (GProvider) mp;

		// add cm tl 02 05 2013 availability of check box for FomPine model
		if (mp instanceof StumpTreatmentAvailabilityProvider)
			stumpTreatmentAvailabilityProvider = (StumpTreatmentAvailabilityProvider) mp;
	}

	private boolean applyWasForgotten() {

		if (useAlderValue.isSelected()) {
			// if (!checkNToCut (nBefore)) return;

			String v = applyMemo = alderValue.getText().trim();
			if (!applyMemo.equals(v))
				return true;

		} else if (useWilsonFactor.isSelected() && hdProvider != null) {
			// if (!checkWilsonFactor ()) return;

			String v = alderWilsonFactor.getText().trim();
			if (!applyMemo.equals(v))
				return true;

		} else if (useStandDensityIndex.isSelected() && sdiInterface != null) {
			// if (!checkStandDensityIndex ()) return;

			String v = alderStandDensityIndex.getText().trim();
			if (!applyMemo.equals(v))
				return true;

		}

		return false;
	}

	/**
	 * Action on ok button.
	 */
	private void okAction() {

		if (applyWasForgotten()) {
			MessageDialog.print(this, Translator.swap("HistoThinnerDialog.aValueWasChangedAndApplyWasNotHit"));
			return;

		}

		// Checks...
		// ~ if (min.getText ().length () == 0 && max.getText ().length () == 0)
		// {
		// ~ MessageDialog.promptError (Translator.swap
		// ("HistoThinnerDialog.someValueIsNeeded"));
		// ~ return;
		// ~ }

		// Create the cut jobs to be processed in HistoThinner
		// They will be retrieved by getCutJobs ();
		//
		try {
			cutJobs = intervener.buildCutJobs(equalizer.getInitialValues(), equalizer.getValues(), histo,
					perHectare.isSelected(), hectareCoefficient);
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("HistoThinnerDialog.exceptionOccuredSeeLog") + "\n" + e);
			return;
		}

		if (HistoThinnerCutJob.sumOfTreesToBeCut(cutJobs) <= 0) {
			MessageDialog.print(this, Translator.swap("HistoThinnerDialog.noTreesToBeCutPleaseCheckConfiguration"));
			return;
		}

		Settings.setProperty("histo.thinner.girth.mode", "" + girthMode.isSelected());
		Settings.setProperty("histo.thinner.class.width", classWidth.getText().trim());
		Settings.setProperty("histo.thinner.min.threshold", minSize.getText().trim());
		Settings.setProperty("histo.thinner.per.hectare", "" + perHectare.isSelected());
		Settings.setProperty("histo.thinner.center.classes", "" + centerClasses.isSelected());

		setValidDialog(true);
	}

	// MOVED to HistoThinner
	// /**
	// * Build the cut jobs from the equalizer
	// */
	// private HistoThinnerCutJob[] buildCutJobs (Equalizer equalizer) throws
	// Exception {
	//
	// int n = equalizer.getN ();
	// int[] values = equalizer.getValues ();
	// int[] initialValues = equalizer.getInitialValues ();
	//
	// HistoThinnerCutJob[] cutJobs = new HistoThinnerCutJob[n];
	//
	// for (int i = 0; i < n; i++) {
	// int targetValue = values[i];
	// int initialValue = initialValues[i];
	// if (targetValue != initialValue) {
	// Collection trees = histo.getUTrees (i);
	//
	// // If we specified thinning per hectare : reset to stand area
	// if (perHectare.isSelected ()) {
	// initialValue /= hectareCoefficient;
	// targetValue /= hectareCoefficient;
	// }
	// HistoThinnerCutJob job = new HistoThinnerCutJob (initialValue,
	// targetValue, trees);
	// cutJobs[i] = job;
	// }
	// }
	// return cutJobs;
	// }

	/**
	 * Action on cancel button.
	 */
	private void cancelAction() {
		setValidDialog(false);
	}

	/**
	 * Someone hit a button.
	 */
	public void actionPerformed(ActionEvent evt) {

		// User interface synchronization (according on JRadioButtons state)
		if (evt.getSource() instanceof JRadioButton)
			synchro();

		// Shortcut: hitting enter on the textfields applies (Ok must be hit to
		// actually thin)
		if (evt.getSource().equals(alderValue) || evt.getSource().equals(alderWilsonFactor)
				|| evt.getSource().equals(alderStandDensityIndex)) {
			alderApplyAction();
		}

		if (evt.getSource().equals(alderApply)) {
			alderApplyAction();

		} else if (evt.getSource().equals(classWidth)) {
			// ~ System.out.println ("action performed on classWidth");
			double width = 0;
			try {
				width = new Double(classWidth.getText().trim()).doubleValue();
				if (width < 1d) {
					throw new Exception();
				}
			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("HistoThinnerDialog.classWidthMustBeDecimalGreaterThan1"));
				return;
			}

			histo.setClassWidth(width);
			// Center classes ?
			if (centerClasses.isSelected()) {
				double xMin = width / 2;
				minSize.setText("" + xMin);
				histo.setForcedXMin(xMin);
			}
			update();

		} else if (evt.getSource().equals(minSize)) {
			// ~ System.out.println ("action performed on minSize");
			double xMin = 0;
			try {
				xMin = new Double(minSize.getText().trim()).doubleValue();
				// NOPE : foresters may try -2.5 with classwidth 5... - fc -
				// 9.4.2003 - if (xMin <
				// 0d) {xMin = 0;}
			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("HistoThinnerDialog.minSizeMustBeDecimal"));
				return;
			}

			histo.setForcedXMin(xMin);
			centerClasses.setSelected(false);
			update();

		} else if (evt.getSource().equals(perHectare)) {
			if (perHectare.isSelected()) {
				histo.setHectareCoefficient(hectareCoefficient);
			} else {
				histo.setHectareCoefficient(1);
			}
			update();

		} else if (evt.getSource().equals(girthMode)) {
			histo.setGirthMode(girthMode.isSelected());
			update();

		} else if (evt.getSource().equals(centerClasses)) {
			// Center classes
			if (centerClasses.isSelected()) {
				double width = 0;
				try {
					width = new Double(classWidth.getText().trim()).doubleValue();
					if (width < 1d) {
						throw new Exception();
					}

					double xMin = width / 2;
					minSize.setText("" + xMin);
					histo.setForcedXMin(xMin);

				} catch (Exception e) {
					MessageDialog
							.print(this, Translator.swap("HistoThinnerDialog.classWidthMustBeDecimalGreaterThan1"));
					return;
				}
				update();

				// Or reset centering
			} else {
				minSize.setText("" + 0);
				histo.setForcedXMin(0);
				update();
			}
		} else if (evt.getSource().equals(stumpTreatment)) {
			treatedFailure.setEditable(stumpTreatment.isSelected());
		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			cancelAction();
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	private void updateWilsonFactor(int nBefore, int nToCut) {
		if (hdProvider != null) {
			// Wilson can be updated only if Dominant height is available in the
			// model
			double hd = hdProvider.getHdom(stand1, concernedTrees);

			// checked
			double t1 = Math.sqrt(nBefore - nToCut) * hd;
			double wf = 100d / t1;

			alderWilsonFactor.setText(formater.format(wf));
		}
	}

	private void updateSDI(int nBefore, int nToCut) {
		if (sdiInterface != null && basalAreaProvider != null) {
			int nAfter = nBefore - nToCut;

			// checked
			double Gbefore = basalAreaProvider.getG(stand1, concernedTrees);

			double Gafter = sdiInterface.getGafter(Gbefore, nBefore, nAfter);

			double dgAfter = 100d * Math.sqrt(4d * Gafter / (Math.PI * nAfter));

			double b = sdiInterface.getSelfThinningSlopeCoefficient();
			int maxN = sdiInterface.getMaxNumberOfTreesOn1HaForDg25cm();

			double sdi = 100d * nAfter * Math.pow(dgAfter / 25d, b) / maxN;

			alderStandDensityIndex.setText(formater.format(sdi));

		}

	}

	/**
	 * Apply Alder's algorithm to the equalizer
	 */
	private void alderApplyAction() {

		int[] initialValues = equalizer.getInitialValues();
		int n = initialValues.length;

		// // TO BE REMOVED
		// int nBefore = 0;
		// for (int i = 0; i < n; i++) {
		// nBefore += initialValues[i];
		// }

		if (useAlderValue.isSelected()) {
			if (!checkNToCut(nBefore))
				return;

			applyMemo = alderValue.getText().trim(); // the user changed this
														// value

			int nToCut = Check.intValue(alderValue.getText().trim());

			updateWilsonFactor(nBefore, nToCut);

			updateSDI(nBefore, nToCut);

		} else if (useWilsonFactor.isSelected() && hdProvider != null) {
			if (!checkWilsonFactor())
				return;

			applyMemo = alderWilsonFactor.getText().trim(); // the user changed
															// this value

			// Wilson can be selected only if Dominant height is available in
			// the model
			double hd = hdProvider.getHdom(stand1, concernedTrees);
			double wf = Check.doubleValue(alderWilsonFactor.getText().trim());

			// Checked
			double t1 = 100d / (wf * hd);
			int nToCut = (int) Math.round(nBefore - t1 * t1);

			alderValue.setText("" + nToCut);

			updateSDI(nBefore, nToCut);

		} else if (useStandDensityIndex.isSelected() && sdiInterface != null) {
			if (!checkStandDensityIndex())
				return;

			applyMemo = alderStandDensityIndex.getText().trim(); // the user
																	// changed
																	// this
																	// value

			double sdi = Check.doubleValue(alderStandDensityIndex.getText().trim());

			double Gbefore = basalAreaProvider.getG(stand1, concernedTrees);

			int Nafter = sdiInterface.getNafter(sdi, Gbefore, nBefore);

			int nToCut = nBefore - Nafter;

			alderValue.setText("" + nToCut);

			updateWilsonFactor(nBefore, nToCut);

		}

		if (!checkNToCut(nBefore))
			return;

		int nToCut = Check.intValue(alderValue.getText().trim());

		int[] newValues = intervener.getAlderValues(initialValues, nToCut);
		equalizer.setValues(newValues);
	}

	private boolean checkNToCut(int nBefore) {
		// Check the number of trees to cut
		if (!Check.isInt(alderValue.getText().trim())) {
			MessageDialog.print(this, Translator.swap("HistoThinnerDialog.alderValueMustBeAnIntBetWeen1and") + " "
					+ nBefore);
			return false;
		}
		int nToCut = Check.intValue(alderValue.getText().trim());
		if (nToCut < 1 || nToCut > nBefore) {
			MessageDialog.print(this, Translator.swap("HistoThinnerDialog.alderValueMustBeAnIntBetWeen1and") + " "
					+ nBefore);
			return false;
		}
		return true;
	}

	private boolean checkWilsonFactor() {
		// Check the Wilson factor
		if (!Check.isDouble(alderWilsonFactor.getText().trim())) {
			MessageDialog.print(this,
					Translator.swap("HistoThinnerDialog.alderWilsonFactorMustBeANumberBetween0.10and0.55"));
			return false;
		}
		double v = Check.doubleValue(alderWilsonFactor.getText().trim());
		if (v < 0.10 || v > 0.55) {
			MessageDialog.print(this,
					Translator.swap("HistoThinnerDialog.alderWilsonFactorMustBeANumberBetween0.10and0.55"));
			return false;
		}
		return true;
	}

	private boolean checkStandDensityIndex() {
		// Check the stand density index
		if (!Check.isDouble(alderStandDensityIndex.getText().trim())) {
			MessageDialog.print(this,
					Translator.swap("HistoThinnerDialog.alderStandDensityIndexMustBeANumberBetween0and100"));
			return false;
		}
		double v = Check.doubleValue(alderStandDensityIndex.getText().trim());
		if (v < 0 || v > 100) {
			MessageDialog.print(this,
					Translator.swap("HistoThinnerDialog.alderStandDensityIndexMustBeANumberBetween0and100"));
			return false;
		}
		return true;
	}

	// MOVED TO HISTOTHINNER
	// /**
	// * Alder's thinning algo: given the initial numbers in the classes and the
	// number of trees to
	// * cut, calculate the new class numbers.
	// */
	// private int[] getAlderValues (int[] initialValues, int nToCut) {
	// int n = initialValues.length;
	// int[] cumNj = new int[n];
	//
	// // Note: nBefore_equ is the sum of the initialValues in the equalizer
	// // these values are integers -> nBefore_equ may be slightly different
	// than the accurate
	// // nBefore
	//
	// int nBefore_equ = 0;
	// for (int i = 0; i < n; i++) {
	// nBefore_equ += initialValues[i];
	// int prevValue = (i == 0) ? 0 : cumNj[i - 1];
	// cumNj[i] = prevValue + initialValues[i];
	// }
	//
	// double L = ((double) nBefore_equ - nToCut) / nBefore_equ;
	//
	// int[] Nafter = new int[n];
	// for (int i = 0; i < n; i++) {
	//
	// double prevCumNj = (i == 0) ? 0d : (double) cumNj[i - 1];
	//
	// double newValue = nBefore_equ * L * (Math.pow (((double) cumNj[i]) /
	// nBefore_equ, 1d / L) - Math.pow (prevCumNj / nBefore_equ, 1d / L));
	//
	// Nafter[i] = (int) Math.round (newValue);
	// }
	//
	// return Nafter;
	// }

	/**
	 * Update histo, then equalizer, then revalidate To be called when changing
	 * histo options (classWidth, girth, perHectare...)
	 */
	private void update() {
		histo.update();

		Collection bars = histo.getNBars();
		String[] labels = histo.getXLabels();
		int[] values = new int[bars.size()];

		int n = 0;
		for (Iterator i = bars.iterator(); i.hasNext();) {
			Vertex2d dn = (Vertex2d) i.next();

			// fc - 1.9.2005 - allow to cut trees when per hectare and less than
			// 1 tree in the bar
			int number = (int) dn.y;
			if (histo.isPerHectare() && dn.y > 0 && dn.y < 1) {
				number = 1;
			}

			values[n++] = number;
		}

		equalizer = new Equalizer(values, labels, Equalizer.NOT_MORE_THAN_INITIAL);
		equalizer.addChangeListener(this);

		int hMemo = 200; // initial equalizer height
		if (equalizerPanel != null) {
			hMemo = equalizerPanel.getPreferredSize().height;
		}

		equalizerPanel = new JPanel(new BorderLayout());

		equalizerPanel.setOpaque(true);
		equalizerPanel.setBackground(Color.white);
		equalizerPanel.add(equalizer, BorderLayout.WEST);
		equalizerPanel.add(new JLabel(" "), BorderLayout.SOUTH);

		if (hMemo != 0) {
			int w = equalizerPanel.getPreferredSize().width;
			equalizerPanel.setPreferredSize(new Dimension(w, hMemo)); // keep
																		// same
																		// height
		}

		scrollpane.getViewport().setView(equalizerPanel);

		// fc - 19.9.2005 - max width = 500 pixels
		int w = Math.min(500, equalizerPanel.getPreferredSize().width);
		int h = equalizerPanel.getPreferredSize().height + 5;
		scrollpane.setPreferredSize(new Dimension(w, h));

		// validate ();
		equalizerPanel.validate();
		equalizerPanel.repaint();

		updateResults();

	}

	/**
	 * This is triggered when the equalizer is moved (when a slider is released)
	 */
	public void stateChanged(ChangeEvent evt) {
		// ~ System.out.println (" - Equalizer was moved - ");
		updateResults();
	}

	// Enable / disable the gui during refresh thread work
	// Must be called only from Swing thread (=> not synchronized)
	//
	private void enableResultTable(boolean b) {
		resultTable.setEnabled(b);
		resultTable.getTableHeader().revalidate();
	}

	/**
	 * Helper method to get nToCut during the preview update.
	 */
	private int getNtoCut() {
		try {
			return Check.intValue(alderValue.getText().trim());
		} catch (Exception e) {
			return 0;
		}
	}

	/**
	 * Update the numbers, G and Dg when a slider was moved
	 */
	private void updateResults() {

		initialValues = equalizer.getInitialValues();
		values = equalizer.getValues();

		// REMOVED this, get back to the previous version based on sums in the
		// histogram
		// -> there was a bug when moving only the sliders -> the n cut was 0
		// (nothing changed in the Alder textfield)

		// REMOVED We now use accurate nBefore thinning, calculated in the
		// constructor
		// REMOVED and number of trees to cut read in the nToCut textfield
		// int nToCut = getNtoCut ();

		int total = 0;
		for (int i = 0; i < initialValues.length; i++) {
			total += initialValues[i];
		}

		// left
		int left = 0;
		for (int i = 0; i < values.length; i++) {
			left += values[i];
		}

		// cut
		int nToCut = total - left;

		// fc+rs-5.7.2012, haCoeff was missing for N
		double haCoeff = histo.getHectareCoefficient();

		N[0] = (int) Math.round(nBefore * haCoeff);
		N[1] = (int) Math.round(nBefore * haCoeff - nToCut);
		N[2] = (int) Math.round(nToCut);

		// Try to calculate G, Dg...
		additionalUpdate();

	}

	/**
	 * Try to update additional fields on the gui : G and Dg. May fail for
	 * several reasons.
	 */
	private void additionalUpdate() {
		try {

			enableResultTable(false);

			SwingWorker3 worker = new SwingWorker3() {

				// Runs in new Thread
				public Object construct() {
					try {
						incrementNumberOfThreads();

						// Cut trees in an auxiliary stand to simulate the
						// thinning

						// This object is a copy of fromStand which 'step'
						// instance variable is null
						GScene aux1 = (GScene) stand1.getInterventionBase();
						TreeCollection aux2 = (TreeCollection) aux1;

						Step step = Current.getInstance().getStep();

						// NOTE - fc - 2.2.2010 - starter.getModel () is
						// available for interveners,
						// would be better here
						GModel model = step.getProject().getModel();
						// NOTE - fc - 2.2.2010 - starter.getModel () is
						// available for interveners,
						// would be better here

						// To be able to call processPostIntervention () below
						// (fc + tf - 17.6.2009)
						Relay relay = model.getRelay();

						// System.out.println
						// ("*** HistoThinner.additionalUpdate");

						HistoThinnerCutJob[] cutJobs = intervener.buildCutJobs(equalizer.getInitialValues(),
								equalizer.getValues(), histo, perHectare.isSelected(), hectareCoefficient);

						// cutJobs contain trees in stand, replace them by trees
						// in aux with same
						// ids
						for (int k = 0; k < cutJobs.length; k++) {
							HistoThinnerCutJob cutJob = cutJobs[k];
							if (cutJob == null) {
								continue;
							} // next cutJob
							Collection trees = cutJob.getTrees();
							if (trees == null) {
								continue;
							} // next cutJob

							Collection auxTrees = new ArrayList();
							for (Iterator l = trees.iterator(); l.hasNext();) {
								Tree t = (Tree) l.next();
								Tree auxTree = aux2.getTree(t.getId());
								auxTrees.add(auxTree);
							}
							cutJob.setTrees(auxTrees);
						}

						// relay.processPreIntervention (aux, starter);

						// THIS IS NOT ENOUGH. HistoThinner is GroupableElement,
						// consider the trees in aux
						// with same ids than thinner.getConcernedTrees () !!!
						// This is done lower (A) , whould be done here.
						Collection c = aux2.getTrees();

						HistoThinner previewIntervener = new HistoThinner(cutJobs);
						previewIntervener.setGrouper(intervener.getGrouper()); // fc-8.6.2020 before init ()
						previewIntervener.init(model, step, aux1, c);

						if (previewIntervener.isReadyToApply()) {

							// ~ System.out.println
							// ("HistoThinnerDialog additionalUpdate calling apply ()... n = "+c.size
							// ());

							GScene z = (GScene) previewIntervener.apply();

							// Trick fc-1.10.2015 Robert Schneider / PlantaBSL
							// -> Robert wants a dialog to open at the end of
							// the intervention to tell something to user
							// We reset interventionResult to false during
							// preview to prevent several dialog openings
							// -> when interventionResult is true, that's the
							// real intervention end -> one single dialog
							z.setInterventionResult(false); // fc-1.10.2015

							// ~ System.out.println
							// ("HistoThinnerDialog additionalUpdate apply () done n = "+((TreeList)
							// z).getTrees ().size ());

							// fc + tf - 17.6.2009 - needed to
							// "update the stand level variables"
							relay.processPostIntervention(z, step.getScene());

							// ~ System.out.println
							// ("HistoThinnerDialog additionalUpdate processPostIntervention () done n = "+((TreeList)
							// z).getTrees ().size ());

							// (A) should be done upper
							// find the trees in aux which are in concernedTrees
							// (if one group was
							// activated)
							// i.e. trees with same ids
							Collection auxConcernedTrees = null;
							if (groupInUse) { // group activated ?
								auxConcernedTrees = new GroupCollection(); // fc
																			// -
																			// 8.9.2005
								for (Iterator i = concernedTrees.iterator(); i.hasNext();) {
									int id = ((Tree) i.next()).getId();
									if (aux2.getTree(id) == null) {
										continue;
									} // this tree was cut, see next one
									auxConcernedTrees.add(aux2.getTree(id));
								}
							} else { // no group activated ?
								auxConcernedTrees = aux2.getTrees();
							}

							Collection cutTrees = new ArrayList();

							// Find the cut trees
							if (aux1 instanceof TreeList) {
								cutTrees = ((TreeList) aux1).getTrees("cut");
							} else {
								for (Tree t : stand2.getTrees()) {
									if (!aux2.getTrees().contains(t)) {
										cutTrees.add(t);
									}
								}

							}

							// ~ System.out.println
							// ("HistoThinnerDialog additionalUpdate processPostIntervention () done cutTrees = "+cutTrees.size
							// ());

							if (groupInUse) { // fc - 8.9.2005
								// ~ if (cutTrees == null || cutTrees.isEmpty
								// ()) {
								// ~ cutTrees = new GroupCollection ();
								// ~ } else {
								cutTrees = new GroupCollection(cutTrees);
								// ~ }
							}

							// ~ System.out.println ();
							// ~ System.out.println
							// ("additionalUpdate (): aux="+aux+" tree number: "+aux.getTrees
							// ().size ());

							MethodProvider mp = model.getMethodProvider();
							double haCoeff = histo.getHectareCoefficient();

							if (mp instanceof GProvider) {
								GProvider p = (GProvider) mp;
								synchronized (G) {
									G[0] = p.getG(stand1, concernedTrees) * haCoeff;
									G[1] = p.getG(aux1, auxConcernedTrees) * haCoeff;
									G[2] = G[0] - G[1]; // fc + tf - 17.6.2009 -
														// good idea
									// ~ G[2] = p.getG (aux, cutTrees) *
									// haCoeff;
								}
							}

							// ~ System.out.println
							// ("HistoThinnerDialog additionalUpdate calculated G "+G[0]+" "+G[1]+" "+G[2]+" ");

							if (mp instanceof DgProvider) {
								DgProvider p = (DgProvider) mp;
								synchronized (Dg) {
									Dg[0] = p.getDg(stand1, concernedTrees);

									if (N[1] == 0) {
										Dg[1] = 0;
									} else {
										Dg[1] = p.getDg(aux1, auxConcernedTrees);
									}

									// fc + tf - 17.6.2009 - good idea also
									if (N[2] == 0) {
										Dg[2] = 0;
									} else if (mp instanceof GProvider && N[2] != 0) {
										Dg[2] = Math.sqrt(G[2] / N[2] * 40000d / Math.PI);
									} else {
										Dg[2] = p.getDg(aux1, cutTrees); // if
																			// idea
																			// not
																			// possible
									}
								}
							}
						}

						// ~ System.out.println
						// ("HistoThinnerDialog additionalUpdate calculated Dg "+Dg[0]+" "+Dg[1]+" "+Dg[2]+" ");

					} catch (Exception e) {
						Log.println(Log.ERROR, "HistoThinnerDialog.additionalUpdate ()",
								"Exception in thread construct () method ", e);
					}
					return null; // results in arrays (N, G, Dg... see upper)
				}

				// Runs in dispatch event thread when construct is over
				public void finished() {
					try {

						resultTable.revalidate();
						resultTable.repaint();

						// ~ System.out.println
						// ("HistoThinnerDialog additionalUpdate finished ()");

					} catch (Exception e) {
						Log.println(Log.ERROR, "HistoThinnerDialog.additionalUpdate ()",
								"Exception in thread finished () method ", e);

					} finally {

						decrementNumberOfThreads();
						if (getNumberOfThreads() <= 0) {
							enableResultTable(true);
						}

					}
				}
			};
			worker.start();

		} catch (Exception e) {
			e.printStackTrace(System.out);
		}

	}

	/**
	 * Counting the worker threads
	 */
	private synchronized void incrementNumberOfThreads() {
		numberOfThreads++;
	}

	/**
	 * Counting the worker threads
	 */
	private synchronized void decrementNumberOfThreads() {
		numberOfThreads--;
	}

	/**
	 * Counting the worker threads
	 */
	private synchronized int getNumberOfThreads() {
		return numberOfThreads;
	}

	/**
	 * Synchronize the user interface depending on the current user choices
	 */
	private void synchro() {
		alderValue.setEnabled(useAlderValue.isSelected());
		alderWilsonFactor.setEnabled(useWilsonFactor.isSelected());
		alderStandDensityIndex.setEnabled(useStandDensityIndex.isSelected());
	}

	/**
	 * Create the dialog box user interface.
	 */
	private void createUI() {

		// 1. Main scroll pane with interactive histo inseide
		//
		scrollpane = new JScrollPane();
		scrollpane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);

		// 2. Parameters panel
		//
		ColumnPanel actionPanel = new ColumnPanel();
		Border etched = BorderFactory.createEtchedBorder();
		Border b1 = BorderFactory.createTitledBorder(etched, Translator.swap("HistoThinnerDialog.parameters"));
		actionPanel.setBorder(b1);

		LinePanel aux1 = new LinePanel();

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("HistoThinnerDialog.classWidth") + " :"));
		classWidth = new JTextField(5);
		classWidth.setText("" + histo.getClassWidth());
		classWidth.addActionListener(this);
		l1.add(classWidth);

		LinePanel l12 = new LinePanel();
		l12.add(new JLabel(Translator.swap("HistoThinnerDialog.minSize") + " :"));
		minSize = new JTextField(5);
		minSize.setText("" + histo.getForcedXMin());
		minSize.addActionListener(this);
		l12.add(minSize);

		aux1.add(l1);
		aux1.add(l12);
		aux1.addStrut0();
		actionPanel.add(aux1);

		LinePanel l2 = new LinePanel();
		l2.addGlue();
		perHectare = new JCheckBox(Translator.swap("HistoThinnerDialog.perHectare"), histo.isPerHectare());
		perHectare.addActionListener(this);
		l2.add(perHectare);

		girthMode = new JCheckBox(Translator.swap("HistoThinnerDialog.girthMode"), histo.isGirthMode());
		girthMode.addActionListener(this);
		l2.add(girthMode);

		centerClasses = new JCheckBox(Translator.swap("HistoThinnerDialog.centerClasses"), false);
		centerClasses.addActionListener(this);
		l2.add(centerClasses);
		l2.addGlue();
		actionPanel.add(l2);

		// Alder algorithm panel
		ColumnPanel c1 = new ColumnPanel(Translator.swap("HistoThinnerDialog.optionUseAldersAlgorithmToThin"));
		actionPanel.add(c1);

		JPanel panel = new JPanel(new GridLayout(2, 2));
		c1.add(panel);
		c1.addStrut0();

		LinePanel l5 = new LinePanel();
		useWilsonFactor = new JRadioButton(Translator.swap("HistoThinnerDialog.useWilsonFactor") + " : ");
		useWilsonFactor.addActionListener(this);
		l5.add(useWilsonFactor);
		alderWilsonFactor = new JTextField(4);
		alderWilsonFactor.addActionListener(this);
		l5.add(alderWilsonFactor);
		l5.addStrut0();
		panel.add(ColumnPanel.addWithStrut0(l5));
		if (hdProvider == null)
			useWilsonFactor.setEnabled(false);

		LinePanel l6 = new LinePanel();
		useStandDensityIndex = new JRadioButton(Translator.swap("HistoThinnerDialog.useStandDensityIndex") + " : ");
		useStandDensityIndex.addActionListener(this);
		l6.add(useStandDensityIndex);
		alderStandDensityIndex = new JTextField(4);
		alderStandDensityIndex.addActionListener(this);
		l6.add(alderStandDensityIndex);
		l6.addStrut0();
		panel.add(ColumnPanel.addWithStrut0(l6));
		if (sdiInterface == null || basalAreaProvider == null)
			useStandDensityIndex.setEnabled(false);

		LinePanel l4 = new LinePanel();
		useAlderValue = new JRadioButton(Translator.swap("HistoThinnerDialog.useAlderValue") + " : ");
		useAlderValue.addActionListener(this);
		l4.add(useAlderValue);
		alderValue = new JTextField(4);
		alderValue.addActionListener(this);
		l4.add(alderValue);
		l4.addStrut0();
		panel.add(ColumnPanel.addWithStrut0(l4));

		group = new ButtonGroup();
		group.add(useAlderValue);
		group.add(useWilsonFactor);
		group.add(useStandDensityIndex);
		useAlderValue.setSelected(true);

		LinePanel l7 = new LinePanel();
		alderApply = new JButton(Translator.swap("HistoThinnerDialog.alderApply"));
		alderApply.addActionListener(this);
		l7.addGlue();
		l7.add(alderApply);
		l7.addStrut0();
		panel.add(l7);

		synchro();

		// add cm tl 02 05 2013 add check box for FomPine
		LinePanel l8 = new LinePanel();

		l8.add(new JLabel(Translator.swap("HistoThinnerDialog.probSporeInfection")));
		probSporeInfection = new JTextField(6);
		// probSporeInfection.setText (String.valueOf
		// (pinuspinasterSettings.probSporeInfection));
		probSporeInfection.setText(String.valueOf(0.4));
		l8.add(probSporeInfection);

		stumpTreatment = new JCheckBox(Translator.swap("HistoThinnerDialog.stumpTreatment"));
		stumpTreatment.addActionListener(this);
		l8.add(stumpTreatment);

		l8.add(new JLabel(Translator.swap("HistoThinnerDialog.treatedFailure")));
		treatedFailure = new JTextField(6);
		// treatedFailure.setText (String.valueOf
		// (pinuspinasterSettings.treatedFailure));
		treatedFailure.setText(String.valueOf(0));
		l8.add(treatedFailure);

		// if (pinuspinasterSettings.fungusEnabled) {
		stumpTreatment.setEnabled(true);
		probSporeInfection.setEnabled(true);
		treatedFailure.setEnabled(true);
		probSporeInfection.setEditable(true);
		treatedFailure.setEditable(false);
		/*
		 * }else{ stumpTreatment.setEnabled (false);
		 * probSporeInfection.setEnabled (false); treatedFailure.setEnabled
		 * (false); }
		 */

		// stumpTreatment = new JCheckBox (Translator.swap
		// ("HistoThinnerDialog.stumpTreatment"));
		// stumpTreatment.addActionListener (this);
		// l8.add (stumpTreatment);
		// l8.addGlue ();
		// actionPanel.add (l8);
		// if (stumpTreatmentAvailabilityProvider == null)
		// stumpTreatment.setEnabled (false);
		if (stumpTreatmentAvailabilityProvider != null)
			actionPanel.add(l8);

		// 3. Result panel
		LinePanel resultPanel = new LinePanel();
		Border b2 = BorderFactory.createTitledBorder(etched, Translator.swap("HistoThinnerDialog.results"));
		resultPanel.setBorder(b2);

		String[] colNames = new String[4];
		colNames[0] = "";
		colNames[1] = Translator.swap("HistoThinnerDialog.before");
		colNames[2] = Translator.swap("HistoThinnerDialog.after");
		colNames[3] = Translator.swap("HistoThinnerDialog.cut");
		int numRows = 3;
		resultModel = new DefaultTableModel(colNames, numRows) {

			public Object getValueAt(int row, int col) {
				if (row == 0) {
					if (col == 0)
						return Translator.swap("HistoThinnerDialog.N");
					if (col == 1)
						return "" + N[0];
					if (col == 2)
						return "" + N[1];
					if (col == 3)
						return "" + N[2];
				} else if (row == 1) {
					if (col == 0)
						return Translator.swap("HistoThinnerDialog.G");
					synchronized (G) {
						if (col == 1)
							return (G[0] == -1) ? "" : formater.format(G[0]);
						if (col == 2)
							return (G[1] == -1) ? "" : formater.format(G[1]);
						if (col == 3)
							return (G[2] == -1) ? "" : formater.format(G[2]);
					}
				} else if (row == 2) {
					if (col == 0)
						return Translator.swap("HistoThinnerDialog.Dg");
					synchronized (Dg) {
						if (col == 1)
							return (Dg[0] == -1) ? "" : formater.format(Dg[0]);
						if (col == 2)
							return (Dg[1] == -1) ? "" : formater.format(Dg[1]);
						if (col == 3)
							return (Dg[2] == -1) ? "" : formater.format(Dg[2]);
					}
				}
				return "-";
			}
		};
		resultTable = new JTable(resultModel);

		StringRenderer stringRenderer = new StringRenderer();
		resultTable.setDefaultRenderer(Object.class, stringRenderer);
		resultTable.setDefaultRenderer(String.class, stringRenderer);
		resultTable.setDefaultRenderer(Number.class, stringRenderer);
		resultTable.setDefaultRenderer(Double.class, stringRenderer);
		resultTable.setDefaultRenderer(Integer.class, stringRenderer);

		JComponent aux = new JScrollPane(resultTable);
		aux.setPreferredSize(new Dimension(150, resultTable.getRowHeight() * (numRows + 2)));

		resultPanel.add(aux);
		resultPanel.addStrut0();

		// General disposition
		JPanel aux2 = new JPanel(new BorderLayout());
		aux2.add(actionPanel, BorderLayout.NORTH);
		aux2.add(scrollpane, BorderLayout.CENTER);
		aux2.add(resultPanel, BorderLayout.SOUTH);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(aux2, BorderLayout.CENTER);

		// 4. Control panel (ok cancel help);
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		help = new JButton(Translator.swap("Shared.help"));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(help);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);
		getContentPane().add(pControl, BorderLayout.SOUTH);

		// Disabled - fc - 3.4.2003
		//
		// sets ok as default (see AmapDialog)
		// ~ ok.setDefaultCapable (true);
		// ~ getRootPane ().setDefaultButton (ok);

	}

	/** Main accessor */
	public HistoThinnerCutJob[] getCutJobs() {
		return cutJobs;
	}

	// ComponentListener interface
	public void componentHidden(ComponentEvent e) {
	}

	public void componentMoved(ComponentEvent e) {
	}

	public void componentResized(ComponentEvent e) {
		if (h0 == 0) {
			return;
		} // h0 not yet initialized -> do nothing
		int h = getSize().height;

		int ew = equalizerPanel.getPreferredSize().width;
		equalizerPanel.setPreferredSize(new Dimension(ew, eh0 + (h - h0)));

		// ~ System.out.println
		// ("equalizerPanel.getPreferredSize="+equalizerPanel.getPreferredSize
		// ());

		equalizerPanel.revalidate();
		equalizerPanel.repaint();

	}

	public void componentShown(ComponentEvent e) {
		// ~ System.out.println ("componentShown size="+getSize ());
		h0 = getSize().height;
		eh0 = equalizerPanel.getPreferredSize().height;
	}
	// ComponentListener interface

}
