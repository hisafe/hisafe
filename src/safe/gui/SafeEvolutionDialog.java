package safe.gui;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import jeeb.lib.util.Check;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import safe.model.SafeEvolutionParameters;
import safe.model.SafeInitialParameters;
import safe.model.SafeInitialValues;
import safe.model.SafeModel;
import safe.model.SafePlotSettings;
import safe.model.SafeStand;
import safe.pgms.SafeDbhCalibrationLoader;
import safe.pgms.SafeRootCalibrationLoader;
import safe.pgms.SafeSimulationLoader;
import capsis.commongui.EvolutionDialog;
import capsis.commongui.util.Helper;
import capsis.kernel.Step;

/**
 * SafeEvolutionDialog - Dialog box to input the limit parameters for growth. Number of years,
 * Climat options.
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeEvolutionDialog extends EvolutionDialog implements ActionListener, MouseListener {

	private SafeInitialParameters safeSettings;
	private SafeEvolutionParameters evolutionParameters; // fc-5.5.2014 was SafeSimulationSettings
	private SafePlotSettings plotSettings;
	private SafeInitialValues  initialValues;

	private SafeModel model;
	private int simulationIndex; 
	
	// Date for starting simulation and nbr days
	private JTextField fldJulianStart;
	private JTextField fldYearStart;
	private JTextField fldNbrDays;

	// plot design
	private JTextField fldTreeCropDistance;
	private JTextField fldWeadedArea;
	
	// CROP Species selection
	private JTextField fldCropFileName1;
	private JTextField fldCropFileName2;
	private JButton browseCropButton1;
	private JButton browseCropButton2;
	
	// ITK selection
	private JTextField fldItkFileName1;
	private JTextField fldItkFileName2;
	private JButton browseItkButton1;
	private JButton browseItkButton2;
	

	// weather selection
	private JTextField fldWeatherFileName;
	private JButton browseWeatherButton;


	// control buttons
	private JButton cropInit;
	private JButton ok;
	private JButton cancel;
	private JButton help;


	// temporary variables to store entry result
	private int dayStart;
	private int yearStart;
	private int  nbrDays;
	private double treeCropDistance, treeCropRadius;

	// DECLARATION - RE-IMPLEMENTATION of FileFilter Class
	// For selection only files.tec
	private javax.swing.filechooser.FileFilter tecFilter = new javax.swing.filechooser.FileFilter () {

		public boolean accept (File f) {
			if (f.isDirectory ()) return true;

			String nom = f.getName ();
			int i = nom.lastIndexOf ('.');
			String ext = "";
			if (i > 0 && i < nom.length () - 1) {
				ext = nom.substring (i + 1).toLowerCase ();
			}
			if (ext.equals ("tec"))
				return true;
			else
				return false;
		}

		public String getDescription () {
			return "(*.tec) " + Translator.swap ("Shared.tec");
		}

	};
	// For selection only files.plt
	private javax.swing.filechooser.FileFilter pltFilter = new javax.swing.filechooser.FileFilter () {

		public boolean accept (File f) {
			if (f.isDirectory ()) return true;

			String nom = f.getName ();
			int i = nom.lastIndexOf ('.');
			String ext = "";
			if (i > 0 && i < nom.length () - 1) {
				ext = nom.substring (i + 1).toLowerCase ();
			}
			if (ext.equals ("plt"))
				return true;
			else
				return false;
		}

		public String getDescription () {
			return "(*.plt) " + Translator.swap ("Shared.plt");
		}

	};
	/**
	 * The aim is to initialize the stand given to the constructor.
	 */
	public SafeEvolutionDialog (Step s) throws Exception {

		super ();

		try {
			model = (SafeModel) s.getProject ().getModel ();

			safeSettings = (SafeInitialParameters) model.getSettings ();

			evolutionParameters = new SafeEvolutionParameters(safeSettings,null, null,null,null,null, null, null);

			plotSettings = model.getPlotSettings ();

			initialValues = safeSettings.initialValues; 
			
			simulationIndex = model.getSimulationIndex();
			
			model.setBatchMode(false);

			// The evolution is not from the root step
			// No need to enter day and year for starting simulation
			if (!s.isRoot ()) {
				SafeStand stand = (SafeStand) s.getScene ();
				dayStart = stand.getJulianDay ();
				yearStart = stand.getWeatherYear ();

				if (dayStart >= 365) {
					dayStart = 1;
					yearStart++;
				} else {
					dayStart++;
				}

				evolutionParameters.simulationDayStart = dayStart;
				evolutionParameters.simulationYearStart = yearStart;
				
			}

			// Activate AmapDialog features
			activateSizeMemorization (getClass ().getName ());
			activateLocationMemorization (getClass ().getName ());

			createUI ();

			// location is set by AmapDialog
			pack ();
			show ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "SafeEvolutionDialog.c ()", "Error in constructor", e);
			throw e;
		}
	}

	/**
	 * Checking all entry fields
	 */
	private boolean check () {

		// year of simulation start
		if (Check.isEmpty (fldYearStart.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.yearStartError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isInt (fldYearStart.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.yearStartError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		yearStart = Integer.parseInt (fldYearStart.getText ());

		// day of simulation start
		if (Check.isEmpty (fldJulianStart.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.julianStartError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isInt (fldJulianStart.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.julianStartError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		dayStart = Integer.parseInt (fldJulianStart.getText ());
		if ((dayStart < 0) || (dayStart > 365)) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.julianStartError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		// nbr days of simulation
		if (Check.isEmpty (fldNbrDays.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.nbrDaysError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isInt (fldNbrDays.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.nbrDaysError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		nbrDays = Integer.parseInt (fldNbrDays.getText ());
		if ((nbrDays <= 0) || (nbrDays > 365)) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.nbrDaysError"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		// tree crop distance
		if (Check.isEmpty (fldTreeCropDistance.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.treeCropDistanceError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isDouble (fldTreeCropDistance.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.treeCropDistanceError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		treeCropDistance = Check.doubleValue (fldTreeCropDistance.getText ());
		if ((treeCropDistance < 0) || (treeCropDistance > plotSettings.plotWidth)
				|| (treeCropDistance > plotSettings.plotHeight)) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.treeCropDistanceError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		if (Check.isEmpty (fldWeadedArea.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weededAreaRadiusError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isDouble (fldWeadedArea.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weededAreaRadiusError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		treeCropRadius = Check.doubleValue (fldWeadedArea.getText ());
		if ((treeCropRadius < 0) || (treeCropRadius > plotSettings.plotWidth)
				|| (treeCropRadius > plotSettings.plotHeight)) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weededAreaRadiusError"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		// checks crop file name...
		if (Check.isEmpty (fldCropFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isFile (fldCropFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		// checks crop file name...
		if (Check.isEmpty (fldCropFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		if (!Check.isFile (fldCropFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}		
		if (Check.isEmpty (fldItkFileName1.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weatherFileNameIsEmpty"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		if (Check.isEmpty (fldItkFileName2.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weatherFileNameIsEmpty"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		if (Check.isEmpty (fldWeatherFileName.getText ())) {
			JOptionPane
					.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.weatherFileNameIsEmpty"), Translator
							.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}

		return true;
	}

	/**
	 * Click on OK button
	 **/
	private void okAction () {

		// Launch the simulation
		if (check ()) {
			
			evolutionParameters.simulationYearStart = yearStart;
			evolutionParameters.simulationDayStart = dayStart;
			evolutionParameters.simulationNbrDays[simulationIndex] = nbrDays;
			evolutionParameters.treeCropDistance[simulationIndex] = treeCropDistance;
			evolutionParameters.treeCropRadius[simulationIndex] = treeCropRadius;
			evolutionParameters.mainCropSpeciesFile[simulationIndex] = fldCropFileName1.getText ();
			evolutionParameters.interCropSpeciesFile[simulationIndex] = fldCropFileName2.getText ();
			evolutionParameters.mainCropInterventionFile[simulationIndex] = fldItkFileName1.getText ();
			evolutionParameters.interCropInterventionFile[simulationIndex] = fldItkFileName2.getText ();
			evolutionParameters.weatherFile = fldWeatherFileName.getText ();
			setEvolutionParameters (evolutionParameters);
			setValidDialog (true);

		}
	}

	/**
	 * Browsing crop files 1 and 2
	 **/
	private void browseCrop1Action () {
		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.safe.crop.path1", (String) null));
		new FileAccessory (chooser);

		chooser.setFileFilter (pltFilter);

		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			System.out.println(chooser.getSelectedFile ().toString ()); 
			Settings.setProperty ("capsis.safe.crop.path1", chooser.getSelectedFile ().toString ());
			fldCropFileName1.setText (chooser.getSelectedFile ().toString ());
		}
		// checks crop species file name...
		if (Check.isEmpty (fldCropFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}
		if (!Check.isFile (fldCropFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}

	}

	private void browseCrop2Action () {
		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.safe.crop.path2", (String) null));
		new FileAccessory (chooser);

		chooser.setFileFilter (pltFilter);

		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.safe.crop.path2", chooser.getSelectedFile ().toString ());
			fldCropFileName2.setText (chooser.getSelectedFile ().toString ());

		}

		// checks cro intervention file name...
		if (Check.isEmpty (fldCropFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}
		if (!Check.isFile (fldCropFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}

	}
	
	/**
	 * Browsing itk files 1 and 2
	 **/
	private void browseItk1Action () {
		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.safe.itk.path1", (String) null));
		new FileAccessory (chooser);

		chooser.setFileFilter (tecFilter);

		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.safe.itk.path1", chooser.getSelectedFile ().toString ());
			fldItkFileName1.setText (chooser.getSelectedFile ().toString ());

		}
		// checks crop intervention file name...
		if (Check.isEmpty (fldItkFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}
		if (!Check.isFile (fldItkFileName1.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}

	}

	private void browseItk2Action () {
		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.safe.itk.path2", (String) null));
		new FileAccessory (chooser);

		chooser.setFileFilter (tecFilter);

		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.safe.itk.path2", chooser.getSelectedFile ().toString ());
			fldItkFileName2.setText (chooser.getSelectedFile ().toString ());

		}

		// checks cro intervention file name...
		if (Check.isEmpty (fldItkFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsEmpty"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}
		if (!Check.isFile (fldItkFileName2.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("SafeEvolutionDialog.fileNameIsNotFile"), Translator
					.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return;
		}

	}



	
	/**
	 * Browsing weather file
	 **/
	private void browseWeatherAction () {
		JFileChooser chooser = new JFileChooser (Settings.getProperty ("capsis.safe.wheather.path", (String) null));
		new FileAccessory (chooser);


		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.safe.wheather.path", chooser.getSelectedFile ().toString ());
			evolutionParameters.weatherFile = chooser.getSelectedFile ().toString ();
			fldWeatherFileName.setText (evolutionParameters.weatherFile);

		}
	}




	
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();

		} else if (evt.getSource ().equals (browseCropButton1)) {
			browseCrop1Action ();
		} else if (evt.getSource ().equals (browseCropButton2)) {
			browseCrop2Action ();
		} else if (evt.getSource ().equals (browseItkButton1)) {
			browseItk1Action ();
		} else if (evt.getSource ().equals (browseItkButton2)) {
			browseItk2Action ();
		} else if (evt.getSource ().equals (browseWeatherButton)) {
			browseWeatherAction ();

		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this); // fc-9.8.2018 was missing
		}
	}

	/**
	 * Initializes the GUI. restincliere
	 */
	private void createUI () {

		Box box = Box.createVerticalBox ();

		// simulation duration
		JPanel l1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		fldNbrDays = new JTextField (5);
		fldNbrDays.setText (""+evolutionParameters.simulationNbrDays[0]);

		// day and year start
		fldJulianStart = new JTextField (5);
		fldJulianStart.addMouseListener (this);
		fldJulianStart.setText (""+evolutionParameters.simulationDayStart);
		fldYearStart = new JTextField (5);
		fldYearStart.setText(""+evolutionParameters.simulationYearStart);

		l1.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.julianStart") + " :", 150));
		l1.add (fldJulianStart);
		l1.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.yearStart") + " :", 160));
		l1.add (fldYearStart);
		l1.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.nbrDays") + " :", 20));
		l1.add (fldNbrDays);

		box.add (l1);

		// plot design
		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		fldTreeCropDistance = new JTextField (5);
		fldTreeCropDistance.setText ("0.5");
		fldWeadedArea = new JTextField (5);
		fldWeadedArea.setText ("0");

		l2.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.treeCropDistance") + " :", 150));
		l2.add (fldTreeCropDistance);
		l2.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.weadedAreaRadius") + " :", 160));
		l2.add (fldWeadedArea);

		box.add (l2);

		// main and understore crop with intervention file names


		JPanel l3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l3.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.cropName1") + " :", 150));

		fldCropFileName1 = new JTextField (30);
		fldCropFileName1.setText (evolutionParameters.mainCropSpeciesFile[0]);
		l3.add (fldCropFileName1);
		browseCropButton1 = new JButton (Translator.swap ("Shared.browse"));
		browseCropButton1.addActionListener (this);
		l3.add (browseCropButton1);


		l3.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.itkFileName") + " :", 155));
		fldItkFileName1 = new JTextField (30);
		fldItkFileName1.setText (evolutionParameters.mainCropInterventionFile[0]);
		l3.add (fldItkFileName1);
		browseItkButton1 = new JButton (Translator.swap ("Shared.browse"));
		browseItkButton1.addActionListener (this);
		l3.add (browseItkButton1);

		box.add (l3);





		JPanel l4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l4.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.cropName2") + " :", 150));

		fldCropFileName2 = new JTextField (30);
		fldCropFileName2.setText (evolutionParameters.interCropSpeciesFile[0]);
		l4.add (fldCropFileName2);
		browseCropButton2 = new JButton (Translator.swap ("Shared.browse"));
		browseCropButton2.addActionListener (this);
		l4.add (browseCropButton2);

		l4.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.itkFileName3") + " :", 155));
		fldItkFileName2 = new JTextField (30);
		fldItkFileName2.setText (evolutionParameters.interCropInterventionFile[0]);
		l4.add (fldItkFileName2);
		browseItkButton2 = new JButton (Translator.swap ("Shared.browse"));
		browseItkButton2.addActionListener (this);
		l4.add (browseItkButton2);

		box.add (l4);

		// weather file name
		JPanel l6 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		l6.add (new JWidthLabel (Translator.swap ("SafeEvolutionDialog.weatherFileName") + " :", 150));
		fldWeatherFileName = new JTextField (30);
		fldWeatherFileName.setText (evolutionParameters.weatherFile);
		l6.add (fldWeatherFileName);
		box.add (l6);
		browseWeatherButton = new JButton (Translator.swap ("Shared.browse"));
		browseWeatherButton.addActionListener (this);
		l6.add (browseWeatherButton);
		box.add (l6);


		

		
		/* control panel */
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		
		ok = new JButton (Translator.swap ("SafeEvolutionDialog.run"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));


		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);

		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);


		setDefaultButton (ok); // from AmapDialog
		JPanel part1 = new JPanel (new BorderLayout ());
		part1.add (box, BorderLayout.NORTH);
		part1.add (box);

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, "Center");
		getContentPane ().add (pControl, "South");

		setTitle (Translator.swap ("SafeEvolutionDialog.title"));

		setModal (true);
	}

	/**
	 * Mouse click event
	 */
	public void mousePressed (MouseEvent e) {

		if (SwingUtilities.isLeftMouseButton (e)) {} else if (SwingUtilities.isMiddleMouseButton (e)) {} else if (SwingUtilities
				.isRightMouseButton (e)) {
			new JulianDialog (this, fldJulianStart); // right click = julian day enter
		}
	}

	public void mouseReleased (MouseEvent e) {}

	public void mouseExited (MouseEvent e) {}

	public void mouseEntered (MouseEvent e) {}

	public void mouseClicked (MouseEvent e) {}

}
