package capsis.extension.intervener;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.Hashtable;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import capsis.commongui.util.Helper;
import capsis.defaulttype.ContainsTargetTrees;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * UI for a thinner that groups the NHA, GHA (RDI could be implemented in the future) thinners of Gymnos module in one generic class
 * 
 * @author Gauthier Ligot
 */
public class DensityTypeRandomnessThinnerDialog extends AmapDialog implements ActionListener {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	static {
		Translator.addBundle ("capsis.extension.intervener.DensityTypeRandomnessThinner");
	}
	
	private NumberFormat nf;
	private DensityTypeRandomnessThinner intervener;
	
	private JRadioButton sliderRadio;
	private JRadioButton txtRadio;
	private JTabbedPane standTypeTab;

	private LinePanel l1;
	private LinePanel l2;
	private JCheckBox excludeTargetTrees;
	private JRadioButton nhaThinner;
	private JRadioButton ghaThinner;
//	private JRadioButton rdiThinner;
	private JSlider intensitySlider;
	private JSlider typeSlider;
//	private JWidthLabel target;
	
	private JTextField intensityTxt;
	private JTextField typeTxt;
	private JTextField randomnessTxt;
	
	protected JButton ok;
	protected JButton cancel;
	protected JButton help;
	
	/**
	 * Default constructor.
	 */
	public DensityTypeRandomnessThinnerDialog (DensityTypeRandomnessThinner intervener) {
		super ();

		this.intervener = intervener;

		// To show numbers in a nice way
		nf = NumberFormat.getInstance (Locale.ENGLISH);
		nf.setGroupingUsed (false);
		nf.setMaximumFractionDigits (3);

		createUI ();
		
		// fc-31.8.2018
		setTitle (intervener.getName());

		setModal (true);

		// location is set by the AmapDialog superclass
		pack (); // uses component's preferredSize
		show ();

	}
	
	/**
	 * Construct the GUI
	 */
	private void createUI () {
		// vertical panel for sliders
		Box sliderPanel = Box.createVerticalBox ();

		if (intervener.stand instanceof ContainsTargetTrees) {
			LinePanel l0 = new LinePanel ();
			excludeTargetTrees = new JCheckBox (Translator.swap("DensityTypeRandomnessThinnerDialog.excludeTargetTrees"), true);
			l0.add(excludeTargetTrees);
			l0.addGlue ();
			sliderPanel.add (l0);
		}
		
		// type of thinning
		
		ColumnPanel topPanel = new ColumnPanel(Translator.swap ("DensityTypeRandomnessThinnerDialog.options"));
		
		LinePanel l00 = new LinePanel();
		ButtonGroup bgroup = new ButtonGroup();
		nhaThinner = new JRadioButton("NHA");
		nhaThinner.addActionListener (this);
		nhaThinner.setSelected (true);
		ghaThinner = new JRadioButton("GHA");
		ghaThinner.addActionListener (this);
//		rdiThinner = new JRadioButton("RDI");
//		rdiThinner.addActionListener (this);
		bgroup.add (nhaThinner);
		bgroup.add (ghaThinner);
//		bgroup.add (rdiThinner);
		l00.add (nhaThinner);
		l00.add (ghaThinner);
//		l00.add (rdiThinner);
		l00.addGlue ();
		
		topPanel.add (l00);
		topPanel.addStrut1();

		
		// real or virtual stand
		ButtonGroup bgroup2 = new ButtonGroup();
		sliderRadio = new JRadioButton(Translator.swap ("DensityTypeRandomnessThinnerDialog.slider"));
		txtRadio = new JRadioButton(Translator.swap ("DensityTypeRandomnessThinnerDialog.txtfield"));

		sliderRadio.addActionListener(new StateListener());
		txtRadio.addActionListener(new StateListener());
		sliderRadio.setSelected(true); //default

		LinePanel rvpanel = new LinePanel();
		bgroup2.add(sliderRadio);
		bgroup2.add(txtRadio);
		
		rvpanel.add(sliderRadio);
		rvpanel.add(txtRadio);
		rvpanel.addGlue ();

		topPanel.add(rvpanel);
		topPanel.addStrut1();
		
		standTypeTab = new JTabbedPane();
		standTypeTab.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent evt) {
				JTabbedPane pane = (JTabbedPane) evt.getSource();
				int sel = pane.getSelectedIndex();
				if(sel==0){
					sliderRadio.setSelected(true);
				}else if(sel==1){
					txtRadio.setSelected(true);
				}
			}
		});
		
		
		//sliders
		int min = 0;
		int max = 100;
		int initialValue = 50;

		//density slider
		l1 = new LinePanel (Translator.swap ("DensityTypeRandomnessThinnerDialog.targetNHA")); //this is the default option
		intensitySlider = new JSlider (JSlider.HORIZONTAL, min, max, initialValue);
		//				intensitySlider.setBorder(BorderFactory.createTitledBorder(Translator.swap ("GymnoRDIThinnerDialog.intensity") + " : "));
		intensitySlider.setMajorTickSpacing(10);
		intensitySlider.setMinorTickSpacing(5);
		intensitySlider.setPaintTicks (true);
		intensitySlider.setPaintLabels (true);
		intensitySlider.setSnapToTicks(false);
		l1.add(intensitySlider);
		l1.addStrut0 ();

		//type slider
		l2 = new LinePanel (Translator.swap ("DensityTypeRandomnessThinnerDialog.type"));
		typeSlider = new JSlider (JSlider.HORIZONTAL, min, max, initialValue);
		//				typeSlider.setBorder(BorderFactory.createTitledBorder(Translator.swap ("GymnoRDIThinnerDialog.type") + " : "));
		typeSlider.setMajorTickSpacing(10);
		typeSlider.setMinorTickSpacing(5);
		typeSlider.setPaintTicks (true);
		typeSlider.setPaintLabels (true);
		Hashtable <Integer, JComponent> typePaintlabels = new Hashtable <Integer, JComponent> ();
		typePaintlabels.put(0, new JLabel (Translator.swap ("DensityTypeRandomnessThinnerDialog.fromBelow"))); //to add in Translator.swap
		typePaintlabels.put(50, new JLabel (Translator.swap ("DensityTypeRandomnessThinnerDialog.random")));
		typePaintlabels.put(100, new JLabel (Translator.swap ("DensityTypeRandomnessThinnerDialog.fromAbove")));
		typeSlider.setLabelTable (typePaintlabels);
		typeSlider.setSnapToTicks(true);
		l2.add(typeSlider);
		l2.addStrut0 ();

		intensitySlider.addChangeListener(new SliderListener());
		typeSlider.addChangeListener(new SliderListener());
		
		// Density, type and randomness textfield
		ColumnPanel txtfields = new ColumnPanel("");
		
		LinePanel lt1 = new LinePanel();
		lt1.add (new JWidthLabel (Translator.swap ("DensityTypeRandomnessThinnerDialog.targetDensity") + " [0;1] :", 50));
		intensityTxt = new JTextField("", 10);
		lt1.add (intensityTxt);
		txtfields.add(lt1);
		txtfields.addStrut1();
		
		LinePanel lt2 = new LinePanel();
		lt2.add (new JWidthLabel (Translator.swap ("DensityTypeRandomnessThinnerDialog.type") + " [0;1] :", 50));
		typeTxt = new JTextField("", 10);
		lt2.add (typeTxt);
		txtfields.add(lt2);
		txtfields.addStrut1();
		
		LinePanel lt3 = new LinePanel();
		lt3.add (new JWidthLabel(Translator.swap ("DensityTypeRandomnessThinnerDialog.randomness") + " [0;1] :", 50));
		randomnessTxt = new JTextField("", 10);
		lt3.add (randomnessTxt);
		txtfields.add(lt3);
		txtfields.addStrut1();
		
		// fa-09.01.2023: initialize at first dialog display (before any slider change)
		intensitySlider.getChangeListeners()[0].stateChanged(new ChangeEvent(intensitySlider));
		
		//put it in a nice panel
		sliderPanel.add(l1);
		sliderPanel.add(l2);
		sliderPanel.add (Box.createHorizontalStrut (400));	// minimal size of the sliders

		standTypeTab.addTab(Translator.swap ("DensityTypeRandomnessThinnerDialog.slider"), sliderPanel);
		standTypeTab.addTab(Translator.swap ("DensityTypeRandomnessThinnerDialog.txtfield"), txtfields);

		//Control Line : Ok Cancel Help
		LinePanel controlPanel = new LinePanel ();
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));

		controlPanel.addGlue ();  // adding glue first -> the buttons will be right justified
		controlPanel.add (ok);
		controlPanel.add (cancel);
		controlPanel.add (help);
		controlPanel.addStrut0 ();

		ok.addActionListener (this); //listeners
		cancel.addActionListener (this);
		help.addActionListener (this);

		// Put the label panels to the left, the text boxes to the right
		// and control to the bottom
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (topPanel, BorderLayout.NORTH);
		getContentPane ().add (standTypeTab, BorderLayout.CENTER);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);

		// Set Ok as default (see AmapDialog)
		setDefaultButton (ok);
	}

	
	/**
	 * Listener actionPerformed
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} 
		else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} 
		else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
		else if (evt.getSource () == nhaThinner) {
			l1.setTitle (Translator.swap ("DensityTypeRandomnessThinnerDialog.targetNHA"));
			setToNhaOrRdi();
		}
		else if (evt.getSource () == ghaThinner) {
			l1.setTitle (Translator.swap ("DensityTypeRandomnessThinnerDialog.targetGHA"));
			setToGha();
		}
//		else if (evt.getSource () == rdiThinner) {
//			l1.setTitle (Translator.swap ("DensityTypeRandomnessThinner.targetRDI"));
//			setToNhaOrRdi();
//		}
	}
	
	/**
	 * Define slider of basal area
	 */
	private void setToGha() {
//		target.setText ("Target : ");
		intensitySlider.setMaximum ((int) intervener.getInitGHA ());
		intensitySlider.setValue ((int) (intervener.getInitGHA () / 2));
		intensitySlider.setMajorTickSpacing(5);
		intensitySlider.setMinorTickSpacing(1);
	}
	
	/**
	 * Define slider of tree density
	 */
	private void setToNhaOrRdi() {
//		target.setText (Translator.swap ("DensityTypeRandomnessThinner.targetGHA") + ": ");
		intensitySlider.setMaximum (100);
		intensitySlider.setValue (50);
		intensitySlider.setMajorTickSpacing(10);
		intensitySlider.setMinorTickSpacing(5);
	}
	
	/**
	 * ok action
	 */
	private void okAction () {
		setValidDialog (true);
	}

	/**
	 * cancel action
	 */
	private void cancelAction () {
		setValidDialog (false);
	}
	
	/**
	 * extractor used by DensityTypeRandomnessThinner to get the intensity coefficient
	 */
	public double getIntensity () {
		if (intensityTxt.getText ().trim ().length () > 0) //txt field are updated on slider modification
			return Check.doubleValue(intensityTxt.getText ().trim ());	
		else
			Log.println (Log.ERROR, "DensityTypeRandomnessThinnerDialog.getIntensity ()",
					"DensityTypeRandomnessThinnerDialog() - The thinning intensity must be a double between 0 and 1");
			return -1d;
	}
	
	/**
	 * extractor used by DensityTypeRandomnessThinner to get the type coefficient
	 */
	public double getTypeValue () {

		if (typeTxt.getText ().trim ().length () > 0) { //txt field are updated on slider modification
			return Check.doubleValue(typeTxt.getText ().trim ());
		}else{
			Log.println (Log.ERROR, "DensityTypeRandomnessThinnerDialog.getTypeValue ()",
					"DensityTypeRandomnessThinnerDialog() - The thinning type must be a double between 0 and 1");
			return -1;
		}			
	}
	
	/**
	 * extractor used by DensityTypeRandomnessThinner to get the randomness coefficient
	 */
	public double getRandomness () {

		if (randomnessTxt.getText ().trim ().length () > 0) {
			return Check.doubleValue(randomnessTxt.getText ().trim ());
		}else {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinnerDialog.getRandomness ()",
					"DensityTypeRandomnessThinnerDialog() - error while computing the randnomness");
			return -1;

		}
	}

	/**
	 * extractor used by DensityTypeRandomnessThinner to know whether target trees should be excluded
	 */
	public boolean isExcludeTargetTrees () {
		return excludeTargetTrees != null && excludeTargetTrees.isSelected ();
	}
	
	/**
	 * extractor used by DensityTypeRandomnessThinner to know the type of target density that has been used (a string: nha, gha, rdi)
	 */
	public String getThinningType () {
		if (nhaThinner.isSelected ())
			return "nha";
		else if (ghaThinner.isSelected ())
			return "gha";
		else
			return "rdi";
	}
	
	/**
	 * private class of stateListener
	 */
	class StateListener implements ActionListener{
		public void actionPerformed(ActionEvent e) {
			if(sliderRadio.isSelected()){
				standTypeTab.setSelectedIndex(0);
			}else if (txtRadio.isSelected()){
				standTypeTab.setSelectedIndex(1);
			}
		}
	}
	
	/**
	 * private class of SliderListener
	 */
	class SliderListener implements ChangeListener {
	    public void stateChanged(ChangeEvent e) {
	    	
	        JSlider source = (JSlider)e.getSource();
	        
	        if (!source.getValueIsAdjusting()) {
	        	
        		double t = -1;
        	 	double i = -1;
        	 	double r = -1;
	        	
	        	double typeUserValue = (double) typeSlider.getValue()/100;
				
	        	//compute corresponding randomness coefficient
				if (0 <= typeUserValue && typeUserValue <= 0.5) {
					r = 2 * typeUserValue;
					t = 0d;
				}else if (typeUserValue <= 1){
					r = 2 - 2 * typeUserValue;
					t = 1d;
				}else{
					Log.println (Log.ERROR, "DensityTypeRandomnessThinnerDialog.SliderListener ()",
							"DensityTypeRandomnessThinnerDialog() - could not compute the random coefficient and the thinning type");
				}

	        	double intensityUserValue = (double) intensitySlider.getValue();
        		
	        	if (ghaThinner.isSelected()) {
	        		double maxG = (double) intensitySlider.getMaximum();
	        		i = (maxG - intensityUserValue)/maxG;
	        		
	        	}else if (nhaThinner.isSelected()) {
	        		i = (double) intensitySlider.getValue()/100;
	        	}
	        	
	        	//update advanced text fields
	        	intensityTxt.setText(nf.format(i));
	        	typeTxt.setText(nf.format(t));
	        	randomnessTxt.setText(nf.format(r));
	        	
	        }    
	    }
	}
}
