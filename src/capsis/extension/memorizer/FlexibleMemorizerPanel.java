package capsis.extension.memorizer;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import javax.swing.DefaultListModel;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.fileloader.CheckingFileChooser;
import jeeb.lib.util.gui.NorthPanel;

/**
 * Configuration for Flexible Memorizer.
 *
 * @author F. de Coligny - May 2020
 */
public class FlexibleMemorizerPanel extends AmapDialog implements ActionListener {

	private String modelName;
	private List<Integer> candidateDates;

	// User choice
	private DefaultListModel<Integer> listModel;
	private JList<Integer> dateList;

	private JTabbedPane tabs;

	private JTextField datesFileName;
	private JButton browse;
	private JButton readTheFile;

	private JButton writeFile;

	private JTextField status;

	private JTextField minBound;
	private JTextField maxBound;
	private JButton allDates;
	private JButton someDates;
	private JTextField frequency;

	private List<Integer> candidateAddition;
	private JTextField preview;
	private JButton clearPreview;
	private JButton addPreviewToList;
	private JButton removePreviewFromList;

	private JButton clearList;
	private JButton sortList;

	private JButton ok;
	private JButton cancel;

	/**
	 * Constructor. Model name is model package name, from model idCard.
	 */
	public FlexibleMemorizerPanel(Window parentWindow, String modelName, FlexibleMemorizer baseMemorizer) {

		this(parentWindow, modelName, baseMemorizer.getSortedDates());

	}

	/**
	 * Constructor. Model name is model package name, from model idCard.
	 */
	public FlexibleMemorizerPanel(Window parentWindow, String modelName, List<Integer> candidateDates) {

		super(parentWindow);

		// Try to reuse previous dates
		this.modelName = modelName;
		this.candidateDates = candidateDates;

		// // If no previous dates found, try to remember dates for this
		// modelName
		// if (this.candidateDates == null || this.candidateDates.isEmpty())
		// this.candidateDates =
		// Settings.getProperty("FlexibleMemorizerPanel.candidateDates" + "-" +
		// modelName, "");

		createUI();

		setModal(true);

		pack();
		setVisible(true);

	}

	public void browseAction() {

		// fc-4.12.2017 A file chooser showing which files can be loaded
		JFileChooser chooser = new CheckingFileChooser(Settings
				.getProperty("FlexibleMemorizerPanel.dates.file.path" + "-" + modelName, PathManager.getDir("data"))) {

			/**
			 * Throws an exception if the file is not correct.
			 */
			@Override
			public void check(File f) throws Exception {
				String fileName = f.getAbsolutePath();

				readFile(fileName);
				// No exception, file seems ok

			}
		};

		// chooser.setFileSelectionMode ();
		int returnVal = chooser.showOpenDialog(this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty("FlexibleMemorizerPanel.datesFileName" + "-" + modelName,
					chooser.getSelectedFile().toString());

			datesFileName.setText(chooser.getSelectedFile().toString());

		}

	}

	/**
	 * Read the file and update dateList.
	 */
	private void readTheFileAction() {
		List<Integer> list = null;

		String fileName = datesFileName.getText().trim();

		try {
			list = readFile(fileName);

		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("FlexibleMemorizerPanel.wrongFile"), e);
			return;

		}

		addToList(list);

		status.setText(Translator.swap("FlexibleMemorizerPanel.read") + " " + list.size() + " "
				+ Translator.swap("FlexibleMemorizerPanel.datesFrom") + fileName);

	}

	/**
	 * Read the file, contains dates, one date per line.
	 */
	private List<Integer> readFile(String fileName) throws Exception {

		List<Integer> list = new ArrayList<>();

		BufferedReader in = new BufferedReader(new FileReader(fileName));

		String line;
		while ((line = in.readLine()) != null) {

			// Skip headers and empty lines
			if (line.startsWith("#") || line.length() == 0)
				continue;

			int i = Integer.parseInt(line);
			list.add(i);
		}
		in.close();

		return list;

	}

	/**
	 * Writes the listModel to file.
	 */
	private void writeFile(String fileName) throws Exception {

		Set<Integer> list = getDates();

		BufferedWriter out = new BufferedWriter(new FileWriter(fileName));

		// Header
		out.write("# Date list for the Capsis-FlexibleMemorizer");
		out.newLine();

		for (int i : list) {

			out.write("" + i);
			out.newLine();

		}
		out.close();

	}

	/**
	 * Writes the lists in a file.
	 */
	private void writeFileAction() {

		boolean trouble = false;
		JFileChooser chooser = null;
		int returnVal = 0;

		do {
			trouble = false;

			chooser = new JFileChooser(Settings.getProperty("FlexibleMemorizerPanel.datesFileName" + "-" + modelName,
					PathManager.getDir("data")));
			chooser.setDialogType(JFileChooser.SAVE_DIALOG);
			// chooser.setApproveButtonText (Translator.swap
			// ("FlexibleMemorizerPanel.saveDatesFile"));
			chooser.setDialogTitle(Translator.swap("FlexibleMemorizerPanel.saveDatesFile"));

			chooser.setDialogType(JFileChooser.SAVE_DIALOG);

			returnVal = chooser.showDialog(this, null); // null : approveButton
														// text was
														// already set

			if (returnVal == JFileChooser.APPROVE_OPTION && chooser.getSelectedFile().exists()) {

				if (!Question.ask(this, Translator.swap("SaveAsProject.confirm"),
						"" + chooser.getSelectedFile().getPath() + "\n"
								+ Translator.swap("FlexibleMemorizerPanel.fileExistsPleaseConfirmOverwrite"),
						Translator.swap("FlexibleMemorizerPanel.overwrite"))) {
					trouble = true;
				}
			}

		} while (trouble);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String fileName = chooser.getSelectedFile().getAbsolutePath();
			Settings.setProperty("FlexibleMemorizerPanel.datesFileName" + "-" + modelName, fileName);

			try {
				writeFile(fileName);

				status.setText(Translator.swap("FlexibleMemorizerPanel.wroteDatesIn") + ": " + fileName);

			} catch (Exception e) {
				Log.println(Log.ERROR, "FlexibleMemorizerPanel.writeTheFileAction ()", "Could not write the dates file",
						e);
				MessageDialog.print(this,
						Translator.swap("FlexibleMemorizerPanel.errorWhileWritingTheDatesFileSeeLog"));
				return;
			}

		}

	}

	private void addToList(List<Integer> dates) {

		int index = 0;
		for (int i : dates) {
			if (!listModel.contains(i))
				listModel.add(index++, i);
		}

	}

	private void removeFromList(List<Integer> dates) {

		for (int i : dates) {
			if (listModel.contains(i))
				listModel.removeElement(i);
		}

	}

	public void clearListAction() {

		listModel.clear();

	}

	public void sortListAction() {

		List<Integer> aux = new ArrayList<>();
		for (int i = 0; i < listModel.size(); i++) {
			aux.add(listModel.getElementAt(i));
		}

		Collections.sort(aux);

		listModel.clear();

		addToList(aux);

	}

	public void allDatesWithFrequencyAction(int frequency) {

		int min = 0;
		try {
			min = Check.intValue(minBound.getText().trim());
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("FlexibleMemorizerPanel.minBoundShouldBeAnInteger"));
			return;
		}

		int max = 0;
		try {
			max = Check.intValue(maxBound.getText().trim());
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("FlexibleMemorizerPanel.maxBoundShouldBeAnInteger"));
			return;
		}

		// We always want the first date
		int k = frequency;

		candidateAddition = new ArrayList<>();
		for (int i = min; i <= max; i++) {

			if (k == frequency) {
				candidateAddition.add(i);
				k = 0;
			}
			k++;

		}

		addToPreview(candidateAddition);

	}

	private void someDatesAction() {

		int freq = 0;
		try {
			freq = Integer.parseInt(frequency.getText().trim());
			if (freq <= 0)
				throw new Exception();
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("FlexibleMemorizerPanel.frequencyShouldBeAnInteger"));
			return;

		}

		allDatesWithFrequencyAction(freq);

	}

	private void addToPreview(List<Integer> list) {

		String prevText = preview.getText().trim();

		preview.setText(prevText + " " + listToString(list));
	}

	public void addPreviewToListAction() {

		String dates = preview.getText().trim();

		try {
			addToList(stringToList(dates));

		} catch (Exception e) {
			MessageDialog.print(this, Translator
					.swap("FlexibleMemorizerPanel.errorInPreviewPleaseCheckItContainsValidDatesSeparatedByBlanks"));
			return;
		}

	}

	public void removePreviewFromListAction() {

		String dates = preview.getText().trim();

		try {
			removeFromList(stringToList(dates));

		} catch (Exception e) {
			MessageDialog.print(this, Translator
					.swap("FlexibleMemorizerPanel.errorInPreviewPleaseCheckItContainsValidDatesSeparatedByBlanks"));
			return;
		}

	}

	public void okAction() {

		if (dateList.getModel().getSize() == 0) {
			MessageDialog.print(this, Translator.swap("FlexibleMemorizerPanel.theListIsEmpty"));
			return;
		}

		// try {
		// new FlexibleMemorizer(dates.getText().trim());
		// } catch (Exception e) {
		// MessageDialog.print(this,
		// Translator.swap("FlexibleMemorizerPanel.errorExpectedAListOfIntDatesSeparatedByBlanks"));
		// return;
		// }

		setValidDialog(true);
	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(clearList)) {
			clearListAction();
		} else if (evt.getSource().equals(sortList)) {
			sortListAction();
		} else if (evt.getSource().equals(browse)) {
			browseAction();
		} else if (evt.getSource().equals(readTheFile)) {
			readTheFileAction();
		} else if (evt.getSource().equals(writeFile)) {
			writeFileAction();
		} else if (evt.getSource().equals(allDates)) {
			allDatesWithFrequencyAction(1);
		} else if (evt.getSource().equals(someDates)) {
			someDatesAction();
		} else if (evt.getSource().equals(clearPreview)) {
			preview.setText("");
		} else if (evt.getSource().equals(addPreviewToList)) {
			addPreviewToListAction();
		} else if (evt.getSource().equals(removePreviewFromList)) {
			removePreviewFromListAction();
		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else {
			setValidDialog(false);
		}
	}

	private void createUI() {

		// The dates list is on the left
		ColumnPanel c1 = new ColumnPanel(Translator.swap("FlexibleMemorizerPanel.dates"));
		listModel = new DefaultListModel<>();

		dateList = new JList<>(listModel);

		addToList(candidateDates);

		c1.add(new JScrollPane(dateList));
		c1.addStrut0();

		// Tools to populate the list are on the right

		// Editor
		ColumnPanel editorPanel = new ColumnPanel();

		// Interactive tools to add / remove dates
		ColumnPanel inter = new ColumnPanel(Translator.swap("FlexibleMemorizerPanel.addOrRemoveDatesInteractively"));

		LinePanel l3 = new LinePanel();

		l3.add(new JLabel(Translator.swap("FlexibleMemorizerPanel.pickDatesBetween")));
		minBound = new JTextField();
		l3.add(minBound);
		l3.add(new JLabel(Translator.swap("FlexibleMemorizerPanel.and")));
		maxBound = new JTextField();
		l3.add(maxBound);
		l3.addStrut0();
		inter.add(l3);

		LinePanel l4 = new LinePanel();
		allDates = new JButton(Translator.swap("FlexibleMemorizerPanel.allDates"));
		allDates.addActionListener(this);
		l4.add(allDates);

		someDates = new JButton(Translator.swap("FlexibleMemorizerPanel.someDates") + " : ");
		someDates.addActionListener(this);
		l4.add(someDates);
		frequency = new JTextField();
		l4.add(frequency);

		l4.addGlue();
		inter.add(l4);

		editorPanel.add(inter);

		// Preview
		ColumnPanel previewPanel = new ColumnPanel(Translator.swap("FlexibleMemorizerPanel.preview"));

		LinePanel l5 = new LinePanel();
		// l5.add(new JLabel(Translator.swap("FlexibleMemorizerPanel.preview") +
		// " : "));
		preview = new JTextField();
		l5.add(preview);
		Icon closeIcon = IconLoader.getIcon("black-cross_16.png");
		clearPreview = new JButton(closeIcon);
		clearPreview.setToolTipText(Translator.swap("FlexibleMemorizerPanel.clearPreview"));
		clearPreview.addActionListener(this);
		l5.add(clearPreview);
		l5.addStrut0();
		previewPanel.add(l5);

		LinePanel l6 = new LinePanel();
		addPreviewToList = new JButton(Translator.swap("FlexibleMemorizerPanel.addPreviewToList"));
		addPreviewToList.addActionListener(this);
		l6.add(addPreviewToList);

		removePreviewFromList = new JButton(Translator.swap("FlexibleMemorizerPanel.removePreviewFromList"));
		removePreviewFromList.addActionListener(this);
		l6.add(removePreviewFromList);

		l6.addStrut0();
		previewPanel.add(l6);

		editorPanel.add(previewPanel);

		LinePanel tools = new LinePanel();

		// Clear
		clearList = new JButton(Translator.swap("FlexibleMemorizerPanel.clearList"));
		clearList.addActionListener(this);
		tools.add(clearList);

		// Sort
		sortList = new JButton(Translator.swap("FlexibleMemorizerPanel.sortList"));
		sortList.addActionListener(this);
		tools.add(sortList);

		tools.addGlue();

		editorPanel.add(tools);

		// Files
		JPanel filePanel = new JPanel(new BorderLayout ());
		
		ColumnPanel filePanelTop = new ColumnPanel ();
		
		// Dates file
		LinePanel l2 = new LinePanel(Translator.swap("FlexibleMemorizerPanel.addDatesFromAFile"));

		l2.add(new JWidthLabel(Translator.swap("FlexibleMemorizerPanel.datesFileName") + " : ", 150));

		datesFileName = new JTextField();
		datesFileName.setText(Settings.getProperty("FlexibleMemorizerPanel.datesFileName" + "-" + modelName, ""));
		l2.add(datesFileName);

		browse = new JButton(Translator.swap("Shared.browse"));
		browse.addActionListener(this);
		l2.add(browse);

		readTheFile = new JButton(Translator.swap("FlexibleMemorizerPanel.readTheFile"));
		readTheFile.addActionListener(this);
		l2.add(readTheFile);

		l2.addStrut0();

		filePanelTop.add(l2);

		// Write to file
		LinePanel l0 = new LinePanel(Translator.swap("FlexibleMemorizerPanel.writeTheListToFile"));
		writeFile = new JButton(Translator.swap("FlexibleMemorizerPanel.writeFile"));
		writeFile.addActionListener(this);
		l0.add(writeFile);

		l0.addGlue();

		filePanelTop.add(l0);

		filePanelTop.addStrut0 ();

		// Status bar in filePanel
		LinePanel l8 = new LinePanel();
		status = new JTextField();
		status.setEditable(false);
		l8.add(status);
		l8.addStrut0();

		filePanel.add(filePanelTop, BorderLayout.NORTH);
		filePanel.add(l8, BorderLayout.SOUTH);

		// Tabbed pane
		tabs = new JTabbedPane();
		tabs.addTab(Translator.swap("FlexibleMemorizerPanel.editorPanel"), new NorthPanel(editorPanel));
		tabs.addTab(Translator.swap("FlexibleMemorizerPanel.filePanel"), filePanel);

		// Control panel

		LinePanel controlPanel = new LinePanel();
		ok = new JButton(Translator.swap("Shared.ok"));
		ok.addActionListener(this);
		// setDefaultButton(ok);
		cancel = new JButton(Translator.swap("Shared.cancel"));
		cancel.addActionListener(this);

		controlPanel.addGlue();
		controlPanel.add(ok);
		controlPanel.add(cancel);
		controlPanel.addStrut0();

		// List size in the split pane
		c1.setPreferredSize(new Dimension(300, 300));
		c1.setMinimumSize(new Dimension(200, 200));

		JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, c1, tabs);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(split, BorderLayout.CENTER);

		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		setTitle(Translator.swap("FlexibleMemorizerPanel"));

	}

	private Set<Integer> getDates() {
		Set<Integer> dates = new HashSet<>();

		for (int i = 0; i < listModel.getSize(); i++) {
			dates.add(listModel.getElementAt(i));
		}
		return dates;
	}

	public FlexibleMemorizer getFlexibleMemorizer() throws Exception {
		// Caller can get the chosen memorizer once user has closed the dialog
		// by Ok
		// Checks have been done in okAction ()
		return new FlexibleMemorizer(getDates());

	}

	static public List<Integer> stringToList(String s) {

		List<Integer> list = new ArrayList<>();

		for (StringTokenizer st = new StringTokenizer(s, " "); st.hasMoreTokens();) {

			String token = st.nextToken();

			try {
				int i = Integer.parseInt(token);
				list.add(i);
			} catch (Exception e) {
				throw e;
			}
		}

		return list;

	}

	static public String listToString(Collection<Integer> list) {
		StringBuffer b = new StringBuffer();

		for (int i : list) {

			if (b.length() != 0)
				b.append(" "); // separator

			b.append("" + i);
		}

		return "" + b;

	}

}
