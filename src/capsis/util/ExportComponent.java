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
package capsis.util;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.gui.MainFrame;

/**
 * A class to export a component in an image file.
 * 
 * @author F. de Coligny - 22.7.2004
 */
public class ExportComponent extends AmapDialog implements ActionListener {
	private JComponent component;

	private Object[] candidateFormats = { "JPEG", "PNG" };
	private Object[] formatExtensions = { "jpeg", "png" }; // order must match
															// candidateFormats

	private JTextField dirName;
	private JTextField filePrefix;
	private JButton browse;
	private JComboBox format;
	private JButton ok;
	private JButton cancel;
	private JButton help;

	/** Creates a new instance of ExportComponent */
	public ExportComponent(JComponent component, JDialog mother) {
		super(mother);
		init(component);
	}

	/** Creates a new instance of ExportComponent */
	public ExportComponent(JComponent component, JFrame mother) {
		super(mother);
		init(component);
	}

	/** Creates a new instance of ExportComponent */
	public ExportComponent(JComponent component) {
		super(MainFrame.getInstance());
		init(component);
	}

	private void init(JComponent component) {
		this.component = component;
		setTitle(Translator.swap("ExportComponent"));

		// check default path
		String path = Settings.getProperty("capsis.export.dir", "");
		if (path == null || path.length() == 0) {
			Settings.setProperty("capsis.export.dir", PathManager.getDir("tmp"));
		}

		// check default path
		String prefix = Settings.getProperty("capsis.export.file.prefix", "");
		if (prefix == null || prefix.length() == 0) {
			Settings.setProperty("capsis.export.file.prefix", "export");
		}

		createUI();
		pack();
		setModal(true);
		show();
	}

	/** Exports the given component into the file with the required format */
	static public void export(JComponent component, String fileName, String format) throws Exception {
		Rectangle rect = component.getBounds();
		BufferedImage bi = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB);
		Graphics g = bi.getGraphics();

		// write to the image and get a BufferedImage
		component.paint(g);

		// write it out in the format you want
		ImageIO.write(bi, format, new File(fileName)); // exception may be
														// thrown

		// dispose of the graphics content
		g.dispose();
	}

	private void okAction() {

		// Checks
		if (!Check.isDirectory(dirName.getText().trim())) {
			MessageDialog.print(this, Translator.swap("ExportComponent.wrongDirectory"));
			return;
		}

		if (Check.isEmpty(filePrefix.getText().trim())) {
			MessageDialog.print(this, Translator.swap("ExportComponent.wrongFilePrefix"));
			return;
		}

		// Checks ok
		String fileName = dirName.getText().trim() + File.separator + filePrefix.getText().trim() + "."
				+ formatExtensions[format.getSelectedIndex()];
		try {
			ExportComponent.export(component, fileName, (String) format.getSelectedItem());
		} catch (Exception e) {
			MessageDialog.print(this, "ExportComponent: could not write image into file " + fileName + " due to " + e);
			return; // do not close dialog till user cancels
		}

		Settings.setProperty("capsis.export.dir", dirName.getText().trim());
		Settings.setProperty("capsis.export.file.prefix", filePrefix.getText().trim());
		Settings.setProperty("capsis.export.format", (String) format.getSelectedItem());

		StatusDispatcher.print(Translator.swap("ExportComponent.exportSucceddedIntoFile") + " " + fileName);

		setValidDialog(true);
	}

	private void browseAction() {
		JFileChooser chooser = new JFileChooser(Settings.getProperty("capsis.export.dir", (String) null));
		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		new FileAccessory(chooser);
		
		// chooser.setFileSelectionMode ();
		int returnVal = chooser.showOpenDialog(this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String fn = chooser.getSelectedFile().toString().trim();
			// Settings.setProperty ("capsis.export.dir", fn);
			dirName.setText(fn);
		}
	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(browse)) {
			browseAction();
		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	private void createUI() {
		getContentPane().setLayout(new BorderLayout());

		ColumnPanel part1 = new ColumnPanel();

		// dirName
		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("ExportComponent.dirName") + " :", 140));
		dirName = new JTextField(25);
		dirName.setText(Settings.getProperty("capsis.export.dir", "")); // default
		l1.add(dirName);
		browse = new JButton(Translator.swap("Shared.browse"));
		browse.addActionListener(this);
		l1.add(browse);
		part1.add(l1);

		// filePrefix
		LinePanel l2 = new LinePanel();
		l2.add(new JWidthLabel(Translator.swap("ExportComponent.filePrefix") + " :", 140));
		filePrefix = new JTextField(25);
		filePrefix.setText(Settings.getProperty("capsis.export.file.prefix", "")); // default
		l2.add(filePrefix);
		part1.add(l2);

		// format
		LinePanel l3 = new LinePanel();
		l3.add(new JWidthLabel(Translator.swap("ExportComponent.format") + " :", 140));
		format = new JComboBox(candidateFormats);
		String lastSelection = Settings.getProperty("capsis.export.format", "");
		format.setSelectedItem(lastSelection == null ? candidateFormats[0] : lastSelection); // default
		l3.add(format);
		part1.add(l3);

		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		ok.addActionListener(this);
		controlPanel.add(ok);
		cancel = new JButton(Translator.swap("Shared.cancel"));
		cancel.addActionListener(this);
		controlPanel.add(cancel);
		help = new JButton(Translator.swap("Shared.help"));
		help.addActionListener(this);
		controlPanel.add(help);

		getContentPane().add(part1, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

	}
}
