package capsis.extension.intervener;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Dictionary;
import java.util.Hashtable;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * Graphical user interface for {@link TargetTypeRandomnessThinner}.
 * 
 * @author N. Beudez, G. Ligot - September 2020
 */
public class TargetTypeRandomnessThinnerDialog extends AmapDialog implements ActionListener, ChangeListener {

	static {
		Translator.addBundle("capsis.extension.intervener.TargetTypeRandomnessThinner");
	}

	private JRadioButton targetCutPercentageButton;
	private JRadioButton targetBasalAreaButton;
	private JRadioButton targetRelativeDensityIndexButton;
	private JRadioButton targetDensityButton;
	private JSlider thinningTypeSlider;
	private JButton okButton;
	private JButton cancelButton;
	private JButton helpButton;

	/**
	 * Constructor.
	 */
	public TargetTypeRandomnessThinnerDialog(TargetTypeRandomnessThinner thinner) {

		super();

		createUI();

		setTitle(thinner.getName());
		setModal(true);
		pack();
		show();
	}

	/**
	 * Construct the GUI
	 */
	private void createUI () {

		ColumnPanel mainPanel = new ColumnPanel();

		// Choice of thinning: target cut percentage, target basal area, target relative density index, target density.
		LinePanel thinningChoiceLine = new LinePanel(Translator.swap("TargetTypeRandomnessThinnerDialog.thinningChoice"));
		
		ButtonGroup group = new ButtonGroup();
		
		targetCutPercentageButton = new JRadioButton(Translator.swap("TargetTypeRandomnessThinnerDialog.targetCutPercentage"));
		targetBasalAreaButton = new JRadioButton(Translator.swap("TargetTypeRandomnessThinnerDialog.targetBasalArea"));
		targetRelativeDensityIndexButton = new JRadioButton(Translator.swap("TargetTypeRandomnessThinnerDialog.targetRelativeDensityIndex"));
		targetDensityButton = new JRadioButton(Translator.swap("TargetTypeRandomnessThinnerDialog.targetDensity"));

		group.add(targetCutPercentageButton);
		group.add(targetBasalAreaButton);
		group.add(targetRelativeDensityIndexButton);
		group.add(targetDensityButton);

		targetCutPercentageButton.setSelected(true);
		
		targetCutPercentageButton.addActionListener(this);
		targetBasalAreaButton.addActionListener(this);
		targetRelativeDensityIndexButton.addActionListener(this);
		targetDensityButton.addActionListener(this);

		thinningChoiceLine.add(targetCutPercentageButton);
		thinningChoiceLine.addStrut1();
		thinningChoiceLine.add(targetBasalAreaButton);
		thinningChoiceLine.addStrut1();
		thinningChoiceLine.add(targetRelativeDensityIndexButton);
		thinningChoiceLine.addStrut1();
		thinningChoiceLine.add(targetDensityButton);
		thinningChoiceLine.addGlue();

		// Type of thinning: from below or from above.
		ColumnPanel thinningTypeColumn = new ColumnPanel(Translator.swap("TargetTypeRandomnessThinnerDialog.thinningType"));

		thinningTypeSlider = new JSlider(SwingConstants.HORIZONTAL);
		thinningTypeSlider.setMajorTickSpacing(50);
		thinningTypeSlider.setMinorTickSpacing(10);
		thinningTypeSlider.setPaintTicks(true);
		thinningTypeSlider.setPaintLabels(true);
		Dictionary<Integer, JComponent> labels = new Hashtable<Integer, JComponent>();
		labels.put(0, new JLabel(Translator.swap("TargetTypeRandomnessThinnerDialog.fromBelow")));
		labels.put(50, new JLabel(Translator.swap("TargetTypeRandomnessThinnerDialog.random")));
		labels.put(100, new JLabel(Translator.swap("TargetTypeRandomnessThinnerDialog.fromAbove")));
		thinningTypeSlider.setLabelTable(labels);
		thinningTypeSlider.setSnapToTicks(true);
		thinningTypeSlider.addChangeListener((ChangeListener) this);
		
		//ColumnPanel thinningTypeColumn = new ColumnPanel();
	
		LinePanel typeLine = new LinePanel();
		typeLine.add(new JWidthLabel(Translator.swap("TargetTypeRandomnessThinnerDialog.type") + " :", 10));
		JTextField typeTextField = new JTextField(10);
		typeTextField.setText("" + Settings.getProperty("capsis.extension.intervener.TargetTypeRandomnessThinnerDialog.type", 0));
		typeLine.add(typeTextField);
		typeLine.addStrut0();
		
		LinePanel randomnessLine = new LinePanel();
		randomnessLine.add(new JWidthLabel(Translator.swap("TargetTypeRandomnessThinnerDialog.randomness") + " :", 10));
		JTextField randomnessTextField = new JTextField(10);
		randomnessTextField.setText("" + Settings.getProperty("capsis.extension.intervener.TargetTypeRandomnessThinnerDialog.randomness", 0.0));
		randomnessLine.add(randomnessTextField);
		randomnessLine.addStrut0();
		
		thinningTypeColumn.add(thinningTypeSlider);
		thinningTypeColumn.addStrut(8);
		thinningTypeColumn.add(typeLine);
		thinningTypeColumn.addStrut1();
		thinningTypeColumn.add(randomnessLine);
		thinningTypeColumn.addStrut0();
		
		mainPanel.add(thinningChoiceLine);
		mainPanel.add(thinningTypeColumn);
		mainPanel.addStrut0();

		// Control line: Ok Cancel Help buttons
		LinePanel controlPanel = new LinePanel();

		okButton = new JButton(Translator.swap("Shared.ok"));
		cancelButton = new JButton(Translator.swap("Shared.cancel"));
		helpButton = new JButton(Translator.swap("Shared.help"));

		controlPanel.addGlue();  // adding glue first -> the buttons will be right justified
		controlPanel.add(okButton);
		controlPanel.add(cancelButton);
		controlPanel.add(helpButton);
		controlPanel.addStrut0();

		okButton.addActionListener(this);
		cancelButton.addActionListener(this);
		helpButton.addActionListener(this);

		getContentPane().setLayout(new BorderLayout ());
		getContentPane().add(mainPanel, BorderLayout.NORTH);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		setDefaultButton(okButton);
	}

	/**
	 * From ActionListener interface.
	 */
	@Override
	public void actionPerformed(ActionEvent e) {

		
	}

	/**
	 * From ChangeListener interface.
	 */
	@Override
	public void stateChanged(ChangeEvent e) {

		double randomnessValue = getThinningRandomnessValue();
	}

//	/**
//	 * Returns a value representing the thinning type. 0 means thinning from below (totally), 1 means thinning 
//	 * from above (totally) and a value between 0 and 1 means a combination of thinning from below and from above. 
//	 */
//	private double getThinningTypeValue() {
//		return ((double) thinningTypeSlider.getValue()) / 100.0;
//	}

	/**
	 * Returns the randomness value given by the slider: value between 0 et 1. 0 means a thinning totally deterministic 
	 * (totally from below or totally from above). 1 means a thinning equally distributed between thinning from below 
	 * and thinning from above. A value between 0 and 1 means a combination of thinning from below and from above.
	 */
	private double getThinningRandomnessValue() {

		double value = ((double) thinningTypeSlider.getValue()) / 100.0; // Value from slider is in [0,100].

		if (value >= 0.0 && value <=0.5) {
			return 2.0 * value;
		} else {
			return 2.0 * (1.0 - value);
		}

	}
}
