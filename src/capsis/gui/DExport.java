/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.kernel.GModel;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Question;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.DirectoryExport;
import capsis.util.JSmartFileChooser;

/**
 * DExport is a dialog box to choose an file format for export. File formats are
 * capsis extensions.
 * 
 * @author F. de Coligny - march 2001
 */
public class DExport extends AmapDialog implements ActionListener {
	private GModel model;

	private String formatClassName;
	private OFormat oFormat;

	private String troubleDuringInitialization;

	// we need 1 table
	private Map extensionKey_classname; // when extension chosen in list, get
										// className (and wait for ok)
	private JComboBox types;

	// private JTextField className;
	// private JTextField version;
	// private JTextField mode;
	// private JTextField author;
	// private JTextArea description;

	private JTextPane propertyPanel;

	private JButton extensionHelp; // fc - 9.2.2004
	private Icon helpIcon = IconLoader.getIcon("help_16.png");

	private JButton browse;
	private JTextField fileName;

	// fc-3.9.2015
	private JCheckBox openFileInCsvViewer;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor.
	 */
	public DExport(GModel model) {
		super();
		this.model = model;
		formatClassName = "";

		searchFormats();
		createUI();

		// Properties panel initialisation
		typesAction();

		// location is set by AmapDialog
		pack(); // computes size
		setVisible(true); // i.e. setVisible (true)

	}

	private void searchFormats() {

		// Ask the ExtensionManager for file formats compatible with the model
		// behind the scenario
		ExtensionManager extMan = CapsisExtensionManager.getInstance();
		Collection classNames = extMan.getExtensionClassNames(CapsisExtensionManager.IO_FORMAT, model);

		// Remove the ones not in export (APiboule - fc - 23.10.2003)
		//
		for (Iterator i = classNames.iterator(); i.hasNext();) {
			String className = (String) i.next();
			try {

				if (!(CapsisExtensionManager.isExport(className))) {
//					System.out.println("remove " + className);
					i.remove();
				}
			} catch (Exception e) {
				i.remove();
				Log.println(Log.WARNING, "DExport.searchFormats ()", "Exception related to ioformat :" + className, e);
			}
		}

		// Nothing left -> error
		//
		if (classNames.size() == 0) {
			troubleDuringInitialization = Translator.swap("DExport.noCompatibleFormatFound");
			return;
		}

		extensionKey_classname = new HashMap();

		for (Iterator i = classNames.iterator(); i.hasNext();) {
			String className = (String) i.next();

			// 1. getExtensionName () loads class -> labels added to Translator
			String extensionKey = ExtensionManager.getName(className);
			extensionKey_classname.put(extensionKey, className);
		}

	}

	/**
	 * Deletes all the content of the given directory
	 */
	// fc-24.5.2023 REMOVED (T. Bronner lost data by answering quickly)
//	public void emptyDirectory(File folder) {
//		for (File file : folder.listFiles()) {
//			if (file.isDirectory()) {
//				emptyDirectory(file);
//			}
//			file.delete();
//		}
//	}

	private void okAction() {

		// Load ioformat with standard constructor (ExtensionStarter)
		//
		String formatClassName = (String) types.getSelectedItem();

		Step step = Current.getInstance().getStep();
		GModel model = step.getProject().getModel();
		ExtensionManager em = CapsisExtensionManager.getInstance();

		String fullClassName = (String) extensionKey_classname.get(formatClassName);
		try {
			oFormat = (OFormat) em.instantiate(fullClassName);

			// fc-7.9.2015 Check if a directory is required, tell user
			if (oFormat instanceof DirectoryExport) {
				File f = new File (getFileName ());
				if (!f.isDirectory()) {
					MessageDialog.print(this, Translator.swap("DExport.thisExportRequiresADirectoryPleaseChooseATargetDirectory"));
					return;
				}
				
				// fc-24.5.2023 REMOVED (T. Bronner lost data by answering quickly)
//				// fc-20.8.2020 Ask confirmation to user and delete existing files in the dir
//				File dir = new File (getFileName ());
//				if (dir.exists () && dir.list().length != 0) {
//					boolean deleteAllFilesFirst = Question.ask(this,  Translator.swap("DExport.confirm"), 
//							getFileName ()+"\n"+Translator.swap("DExport.allExistingFilesInTheDirectoryWillBeDeletedPleaseConfirm"));
//					
//					if (deleteAllFilesFirst)
//						emptyDirectory(dir);
//					
//				}
				
				
			} else {
				File f = new File (getFileName ());
				if (f.isDirectory()) {
					MessageDialog.print(this, Translator.swap("DExport.thisExportRequiresARegularFileNamePleaseChooseACorrectTargetFileName"));
					return;
				}
				
			}
			
			if (oFormat == null) {
				throw new Exception("ioFormat == null");
			}
			oFormat.initExport(model, step);

		} catch (CancellationException e) { // fc - 11.9.2008
			return; // fc - 11.9.2008
			
		} catch (Exception exc) {
			Log.println(Log.ERROR, "DExport.okAction ()", "Error during ioformat instanciation. "
					+ ((troubleDuringInitialization != null) ? troubleDuringInitialization : ""), exc); // maybe
																										// details
			MessageDialog.print(this, Translator.swap("DExport.thisFormatCanNotBeUsedSeeLog"), exc);
			return;
		}

		// Classic checks...
		//
		if (formatClassName == null || formatClassName.length() == 0) {
			MessageDialog.print(this, Translator.swap("DExport.chooseAFormat"));
		} else if (!CapsisExtensionManager.isExport(fullClassName)) {
			MessageDialog.print(this, Translator.swap("DExport.chooseAnFormatWithExportMode"));

		} else if (fileName.getText() == null || fileName.getText().trim().length() == 0) {
			MessageDialog.print(this, Translator.swap("DExport.chooseAFileName"));
		} else {

			// fc - 18.11.2004 - remember export file name
			try {
				Settings.setProperty("capsis.last.selected.export.file.name", getFileName());
			} catch (Exception e) {
			} // never know
			
			// fc-3.9.2015
			Settings.setProperty ("DExport.openFileInCsvViewer", openFileInCsvViewer.isSelected ());
			
			setValidDialog(true);
		}

	}

	private void browseAction() {
		// fc - 15.1.2008
		boolean directoriesOnly = false;
		try {
			String formatClassName = (String) types.getSelectedItem();
			String fullClassName = (String) extensionKey_classname.get(formatClassName);

			Class<?> cl = Class.forName(fullClassName);

			if (DirectoryExport.class.isAssignableFrom(cl)) {
				directoriesOnly = true;
			}

		} catch (Exception e) {
		} // does not matter

		// FileName ? -> get it with a JFileChooser
		String name = "";
		String defaultExportPath = Settings.getProperty("capsis.export.path", "");
		if ((defaultExportPath == null) || defaultExportPath.equals("")) {
			defaultExportPath = PathManager.getInstallDir();
		}

		JFileChooser chooser = new JSmartFileChooser(
				Translator.swap("DExport.exportStep") + " - " + Current.getInstance().getStep().getCaption(),
				Translator.swap("Shared.select"), defaultExportPath);

		// ~ System.out.println ("DExport... ");
		if (directoriesOnly) {
			// ~ System.out.println
			// ("DExport... JFileChooser.DIRECTORIES_ONLY");
			chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		}

		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
																			// :
																			// approveButton
																			// text
																			// was
																			// already
																			// set

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty("capsis.export.path", chooser.getSelectedFile().toString());
			name = chooser.getSelectedFile().toString();

			fileName.setText(name);

		} else {
			return; // cancel on file chooser -> do nothing
		}

	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(types)) {
			typesAction();
		} else if (evt.getSource().equals(browse)) {
			browseAction();
		} else if (evt.getSource().equals(extensionHelp)) { // fc - 9.2.2004
			String formatClassName = (String) types.getSelectedItem();
			String fullClassName = (String) extensionKey_classname.get(formatClassName);
			Helper.helpFor(fullClassName);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * An export format is chosen.
	 */
	public void typesAction() {

		formatClassName = (String) types.getSelectedItem();

		// fc - 18.11.2004 - remember this
		try {
			Settings.setProperty("capsis.last.selected.export.format", formatClassName);
		} catch (Exception e) {
		} // formatClassName may be null if early execution

		try {

			String className = (String) extensionKey_classname.get(formatClassName);

			CapsisExtensionManager.getInstance().getPropertyPanel(className, propertyPanel);

		} catch (Exception e) {
			// There can be an exception if change in combo and there was a
			// selection
		}

		// try {
		//
		// // We just need the phantom extension (unusable) to ask some identity
		// questions
		// //
		// String fullClassName = (String) extensionKey_classname.get
		// (formatClassName);
		// System.out.println(fullClassName);
		// // fc - 9.2.2004
		// extensionHelp.setEnabled (false);
		// if (Helper.hasHelpFor (fullClassName)) {extensionHelp.setEnabled
		// (true);}
		//
		// setField (className, fullClassName);
		// setField (author, ExtensionManager.getAuthor (fullClassName));
		// setField (version, ExtensionManager.getVersion (fullClassName));
		//
		// String s = "";
		// if (CapsisExtensionManager.isImport (fullClassName)) {s+="Import";}
		// if (CapsisExtensionManager.isExport (fullClassName)) {
		// if (s.length () != 0) {s+=" / ";}
		// s+="Export";
		// }
		// setField (mode, s);
		//
		// description.setText (ExtensionManager.getDescription
		// (fullClassName)); // text zone
		//
		// } catch (Exception exc) {
		// setField (className, "");
		// setField (author, "");
		// setField (version, "");
		// setField (mode, "");
		// if (troubleDuringInitialization != null) {
		// description.setText (troubleDuringInitialization);
		// } else {
		// description.setText (Translator.swap
		// ("DExport.thisFormatCanNotBeUsedSeeLog"));
		// }
		// }

	}

	// Put text in field, tool tip and set caret position to begin of line.
	//
	private void setField(JTextField field, String text) {
		field.setText(text);
		field.setToolTipText(text);
		field.setCaretPosition(0);
	}

	// Initialize the dialog's GUI.
	//
	private void createUI() {
		int gap = 2;

		// 1. Format panel
		ColumnPanel pFormat = new ColumnPanel();

		// 2. Combo for formats
		LinePanel p2 = new LinePanel();

		// fc - 9.2.2004 - adding extension help
		extensionHelp = new JButton(helpIcon);
		Tools.setSizeExactly(extensionHelp);
		extensionHelp.setToolTipText(Translator.swap("Shared.help"));
		extensionHelp.addActionListener(this);

		types = new JComboBox();
		try {
			types = new JComboBox(new Vector(new TreeSet(extensionKey_classname.keySet())));
		} catch (Exception exc) {
		} // there may have been a trouble during initialisation
		types.addActionListener(this);
		p2.add(new JWidthLabel(Translator.swap("DExport.availableFormats") + " :", 130));
		p2.add(types);

		p2.add(extensionHelp);

		p2.addStrut0();
		pFormat.add(p2);

		// 3. Current format properties
		// LinePanel l1 = new LinePanel ();
		// LinePanel l2 = new LinePanel ();
		// LinePanel l3 = new LinePanel ();
		// LinePanel l4 = new LinePanel ();
		// LinePanel l6 = new LinePanel ();
		// className = new JTextField (20);
		// className.setEditable (false);
		// version = new JTextField (5);
		// version.setEditable (false);
		// mode = new JTextField (10);
		// mode.setEditable (false);
		// author = new JTextField (20);
		// author.setEditable (false);
		// description = new JTextArea (3, 15);
		// description.setEditable (false);
		// description.setLineWrap (true);
		// description.setWrapStyleWord (true);
		// JScrollPane scrollpane = new JScrollPane (description);
		// l1.add (new JWidthLabel (Translator.swap ("DExport.className")+" :",
		// 75));
		// l1.add (className);
		// l1.addStrut0 ();
		// l6.add (new JWidthLabel (Translator.swap ("DExport.mode")+" :", 75));
		// l6.add (mode);
		// l6.addStrut0 ();
		// l2.add (new JWidthLabel (Translator.swap ("DExport.version")+" :",
		// 75));
		// l2.add (version);

		propertyPanel = new JTextPane() {
			@Override
			public Dimension getPreferredScrollableViewportSize() {
				return new Dimension(100, 100);
			}
		};
		propertyPanel.setEditable(false);
		JScrollPane scrollpane = new JScrollPane(propertyPanel);

		// l2.addStrut0 ();
		// l3.add (new JWidthLabel (Translator.swap ("DExport.author")+" :",
		// 75));
		// l3.add (author);
		// l3.addStrut0 ();
		// l4.add (new JWidthLabel (Translator.swap
		// ("DExport.description")+" :", 75));
		// l4.addGlue ();
		// pFormat.add (l1);
		// pFormat.add (l3); // l3 is before l2
		// pFormat.add (l6);
		// pFormat.add (l2);
		// pFormat.add (l4);
		// pFormat.addStrut1 ();

		JPanel subPart = new JPanel(new BorderLayout());
		subPart.add(pFormat, BorderLayout.NORTH);
		subPart.add(scrollpane, BorderLayout.CENTER);

		subPart.setBorder(BorderFactory.createTitledBorder(Translator.swap("DExport.exportFormat")));

		// 4. File name panel
		ColumnPanel pFileName = new ColumnPanel();
		pFileName.setBorder(BorderFactory.createTitledBorder(Translator.swap("DExport.targetFile")));

		LinePanel l5 = new LinePanel();
		l5.add(new JWidthLabel(Translator.swap("Shared.name") + " :", 50));
		fileName = new JTextField(20);
		l5.add(fileName);
		browse = new JButton(Translator.swap("Shared.browse"));
		browse.addActionListener(this);
		l5.add(browse);
		l5.addStrut0();
		pFileName.add(l5);

		// fc-3.9.2015 Propose to open the file in CsvViewer
		LinePanel l6 = new LinePanel();
		openFileInCsvViewer = new JCheckBox(Translator.swap("DExport.openFileInCsvViewer"));
		openFileInCsvViewer.setSelected(Settings.getProperty("DExport.openFileInCsvViewer", true));
		l6.add(openFileInCsvViewer);
		l6.addGlue();
		pFileName.add(l6);

		pFileName.addStrut0();

		// fc - 18.11.2004 - try to reselect last used export format
		try {
			String v = Settings.getProperty("capsis.last.selected.export.format", "");
			if (v != null && !v.equals("")) {
				// Check if the memo name is in the current list (compatible)
				if (extensionKey_classname.keySet().contains(v)) {
					types.getModel().setSelectedItem(v);
				}
			}
		} catch (Exception e) {
		} // maybe no value memorized

		// fc - 18.11.2004 - try to reselect last used export file name
		try {
			String v = Settings.getProperty("capsis.last.selected.export.file.name", "");
			if (v != null && !v.equals("")) {
				fileName.setText(v);
			}
		} catch (Exception e) {
		} // maybe no value memorized

		// 5. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon("ok_16.png");
		ok.setIcon(icon);

		cancel = new JButton(Translator.swap("Shared.cancel"));
		icon = IconLoader.getIcon("cancel_16.png");
		cancel.setIcon(icon);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);

		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(help);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// set ok as default (see AmapDialog)
		ok.setDefaultCapable(true);
		getRootPane().setDefaultButton(ok);

		JPanel part1 = new JPanel(new BorderLayout());
		part1.add(subPart, BorderLayout.CENTER);
		part1.add(pFileName, BorderLayout.SOUTH);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(part1, BorderLayout.CENTER);
		getContentPane().add(pControl, BorderLayout.SOUTH);

		setTitle(Translator.swap("DExport") + " - " + Current.getInstance().getStep().getCaption());

		setModal(true);
	}

	public OFormat getOFormat() {
		return oFormat;
	}

	public String getFileName() {
		return fileName.getText().trim();
	}

	// fc-3.9.2015
	public boolean isRequiredOpenFileInCsvViewer() {
		return openFileInCsvViewer.isSelected();
	}
}
