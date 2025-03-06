/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableRowSorter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Style;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.text.StyledDocument;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.kernel.Engine;
import capsis.kernel.IdCard;
import capsis.kernel.ModelManager;

/**
 * DAbout tells about the version of Capsis and what models are available on disk.
 * 
 * @author F. de Coligny - september 1999 - may 2002
 */
public class DAbout extends AmapDialog implements ActionListener, ListSelectionListener {

	public static final int ICON_SIZE = 23;

	private Engine engine; // used several times
	private JButton ok;
	private JTable table;
	private int lastSelectedRow;

	private List<String> packageNames;
	private String currentPackageName;

	private JButton info;
	private JButton license;
	private Icon infoIcon = IconLoader.getIcon ("information_16.png");
	private Icon licenseIcon = IconLoader.getIcon ("license_16.png");

	// Details of the selected model
	private JTextPane details;

	/**
	 * Constructor.
	 */
	public DAbout (JFrame frame) {
		super (frame);
		engine = Engine.getInstance ();
		currentPackageName = "";
		lastSelectedRow = -1;
		createUI ();

		activateSizeMemorization (getClass ().getName ());

		pack ();
		setVisible (true);

	}

	private void okAction () {
		dispose ();
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (info)) {
			Helper.helpFor (currentPackageName);
		} else if (evt.getSource ().equals (license)) {
			Helper.licenseFor (currentPackageName);
		}
	}

	/**
	 * Processes actions on an item in the models list.
	 */
	public void valueChanged (ListSelectionEvent evt) {

		// Ignore extra messages
		if (evt.getValueIsAdjusting ()) return;

		license.setEnabled (true); // from now on, it stays true

		// Selection throws 2 events : mouse press and release -> we want only one
		int row = table.getSelectedRow ();

		// Ensure selection is visible (while sorting...)
		Rectangle cellRect = table.getCellRect (row, 0, false);
		if (cellRect != null) table.scrollRectToVisible (cellRect);

		// System.out.println ("DAbout raw selection: "+row);

		int selectedRow = 0;
		try {
			selectedRow = table.convertRowIndexToModel (row); // consider the sorter, get the right
																// index in the tableModel
		} catch (Exception e) {} // May occur when sorting -> selectedRow stays to 0

		if (selectedRow != lastSelectedRow) {
			lastSelectedRow = selectedRow;

			// Retrieve modelPackageName (ex: "mountain")
			currentPackageName = packageNames.get (selectedRow);
			info.setEnabled (Helper.hasHelpFor (currentPackageName));

			// fc - 5.5.2009 - write more information on the selected model below the table
			updateLabel (currentPackageName);
		}
	}

	/** Update model label */
	private void updateLabel (String pkgName) {

		IdCard card;
		String name = "", author = "", institute = "", description = "";
		try {
			card = ModelManager.getInstance ().getIdCard (pkgName);
			name = card.getModelName ();
			author = card.getModelAuthor ();
			institute = card.getModelInstitute ();
			description = card.getModelDescription ();
		} catch (Exception e) {
			Log.println (Log.WARNING, "DAbout.updateLabel ()", "Could not get the idCard for package "
					+ pkgName, e);
			return;
		}

		// Write in text pane
		// Initialize some styles...
		details.setText ("");
		StyledDocument doc = details.getStyledDocument ();
		Style def = StyleContext.getDefaultStyleContext ().getStyle (StyleContext.DEFAULT_STYLE);

		Style regular = doc.addStyle ("regular", def);
		StyleConstants.setFontFamily (def, "SansSerif");

		Style s = doc.addStyle ("italic", regular);
		StyleConstants.setItalic (s, true);

		s = doc.addStyle ("bold", regular);
		StyleConstants.setBold (s, true);

		try {
			doc.insertString (doc.getLength (), name, doc.getStyle ("bold"));
			doc.insertString (doc.getLength (), " : " + author, doc.getStyle ("regular"));
			doc.insertString (doc.getLength (), ", " + institute + "\n", doc.getStyle ("italic"));
			doc.insertString (doc.getLength (), description, doc.getStyle ("regular"));
		} catch (BadLocationException e) {
			Log.println (Log.WARNING, "DNewScenario.updateLabel ()", "Could not insert text into text pane", e);
			return;
		}

		// Ensure the top of the text pane is visible
		details.setCaretPosition (0);

	}

	/**
	 * Initialize the dialog's GUI.
	 */
	protected void createUI () {
		JPanel p1 = new JPanel () {

			public void paintComponent (Graphics g) {
				super.paintComponent (g);

				try {
					ImageIcon logo = IconLoader.getIcon ("capsis-logo.png");
					g.drawImage (logo.getImage (), 0, 0, this);
				} catch (Exception e) {} // if image is missing do nothing
			}
		};

		// Module Table

		final Vector rows = new Vector ();
		final Vector columnNames = new Vector ();
		columnNames.add (Translator.swap ("DAbout.Model"));
		columnNames.add (Translator.swap ("DAbout.ModelType"));
		columnNames.add (Translator.swap ("DAbout.ModelAuthor"));
		columnNames.add (Translator.swap ("DAbout.AuthorInstitute"));

		// fc-3.12.2019 exclude technical model packages
		boolean includingTechnicalPackageNames = false;
		packageNames = new ArrayList (new TreeSet (ModelManager.getInstance ().getPackageNames (includingTechnicalPackageNames)));
//		packageNames = new ArrayList (new TreeSet (ModelManager.getInstance ().getPackageNames ()));

		// ~ for (String pkgName : packageNames) {

		for (Iterator i = packageNames.iterator (); i.hasNext ();) {
			String pkgName = (String) i.next ();

			IdCard card;
			try {
				card = ModelManager.getInstance ().getIdCard (pkgName);
			} catch (Exception e) {
				Log.println (e.toString ());
				i.remove ();
				continue;
			}

			Vector row = new Vector ();
			row.add (card.getModelName () + " (v" + card.getModelVersion () + ")");
			row.add (card.getModelType ());
			row.add (card.getModelAuthor ());
			row.add (card.getModelInstitute ());
			rows.add (row);

		}

		TableModel tm = new DefaultTableModel (rows, columnNames);
		table = new JTable (tm);

		// Sorting on column 0 at opening time fc-21.9.2012
		TableRowSorter<TableModel> sorter = new TableRowSorter<TableModel> (tm);
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey> ();
		sortKeys.add (new RowSorter.SortKey (0, SortOrder.ASCENDING));
		sorter.setSortKeys (sortKeys);
		table.setRowSorter (sorter);

		table.setDefaultEditor (Object.class, null); // not editable
		table.setCellEditor (null);
		table.setSelectionMode (ListSelectionModel.SINGLE_SELECTION); // but selectable
		table.getSelectionModel ().addListSelectionListener (this);

		JScrollPane scrollpane = new JScrollPane (table);
		scrollpane.setPreferredSize (new Dimension (400, 100));

		// Related buttons
		// Model info button
		info = new JButton (Translator.swap ("DAbout.documentation"), infoIcon);
		// info.setToolTipText (Translator.swap ("Shared.info"));
		info.addActionListener (this);
		info.setEnabled (false);

		// Model license button
		license = new JButton (Translator.swap ("DAbout.license"), licenseIcon);
		// license.setToolTipText (Translator.swap ("Shared.licence"));
		license.addActionListener (this);
		license.setEnabled (false);

		// Documentation and License for the current model
		LinePanel buttons = new LinePanel ();
		
		buttons.add( new JLabel(Translator.swap("DAbout.selectedModel") + " :") );
		buttons.add (info);
		buttons.add (license);
		buttons.addGlue();

		LinePanel tableLine = new LinePanel ();

		JPanel centered = new JPanel (new BorderLayout ());
		centered.add (scrollpane, BorderLayout.CENTER);
		tableLine.add (centered);
		// tableLine.add (buttons);
		tableLine.addStrut0 ();

		LinePanel t1 = new LinePanel ();
		t1.add (new JLabel ("<html><b>" + Translator.swap ("DAbout.capsis") + " "
				+ engine.getVersionAndRevision () + "</b></html>"));
		t1.addGlue ();

		LinePanel t2 = new LinePanel ();
		t2.add (new JLabel ("<html><i>" + Translator.swap ("DAbout.whatDoesItMean") + "</i></html>"));
		t2.addGlue ();

		LinePanel t3 = new LinePanel ();
		t3.add (new JLabel (Translator.swap ("DAbout.developers")));
		t3.addGlue ();

		LinePanel t4 = new LinePanel ();
		t4.add (new JLabel (Translator.swap ("DAbout.modellers")));
		t4.addGlue ();

		LinePanel t5 = new LinePanel ();
		t5.add (new JLabel ("<html><i>" + Translator.swap ("DAbout.author") + "</i></html>"));
		t5.addGlue ();

		LinePanel t6 = new LinePanel ();
		t6.add (new JLabel ("<html><i>" + Translator.swap ("DAbout.author2") + "</i></html>"));
		t6.addGlue ();

		// LinePanel t7 = new LinePanel ();
		// t7.add (new JLabel ("<html><i>"+Translator.swap ("DAbout.author3")+"</i></html>"));
		// t7.addGlue ();

		LinePanel t8 = new LinePanel ();
		t8.add (new JLabel (Translator.swap ("DAbout.availableModules") + " ("
				+ table.getRowCount () + ") :"));
		t8.addGlue ();

		// LinePanel s1 = new LinePanel ();
		// s1.add (new JWidthLabel (Translator.swap ("DAbout.javaVmName") + " :", 190));
		// s1.add (new JLabel (System.getProperty ("java.vm.name", "")));
		// s1.add (new JLabel (" " + System.getProperty ("java.vm.version", "")));
		// s1.addGlue ();

		LinePanel s2 = new LinePanel ();
		String jvm = System.getProperty ("java.vm.name", "") + " "
				+ System.getProperty ("java.vm.version", "");
		s2.add (new JWidthLabel (Translator.swap ("DAbout.javaVersion") + " :", 190));
		s2.add (new JLabel (System.getProperty ("java.version", "") + " (" + jvm + ")"));
		s2.addGlue ();

		LinePanel s3 = new LinePanel ();
		s3.add (new JWidthLabel (Translator.swap ("DAbout.osName") + " :", 190));
		s3.add (new JLabel (System.getProperty ("os.name", "")));
		s3.add (new JLabel (" " + System.getProperty ("os.version", "")));
		s3.addGlue ();

		LinePanel s4 = new LinePanel ();
		s4.add (new JWidthLabel (Translator.swap ("DAbout.architecture") + " :", 190));
		s4.add (new JLabel (""+AmapTools.getJavaArchitecture ()+" bits"));
//		s4.add (new JLabel (System.getProperty ("os.arch", "")));
		s4.addGlue ();

		LinePanel l10 = new LinePanel ();
		ok = new JButton (Translator.swap ("Shared.ok"), IconLoader.getIcon ("ok_16.png"));
		ok.addActionListener (this);
		l10.addGlue();
		l10.add (ok);
		l10.addStrut0();

		ColumnPanel box1 = new ColumnPanel ();
		box1.add (t1);
		box1.add (t2);
		box1.addStrut(5);
		box1.add (t3);
		box1.add (t4);
		box1.add (t5);
		box1.add (t6);
		// box1.add (t7);
		box1.addStrut(15);
		box1.add (t8);
		box1.addStrut0 ();

		ColumnPanel box2 = new ColumnPanel ();

		// Details of the model selected in the table
		details = new JTextPane ();
		details.setPreferredSize (new Dimension (100, 60));
		details.setEditable (false);
		JScrollPane scroll = new JScrollPane (details);
		// fc-30.4.2020 addWithStrut0 () for fine layout
		box2.add (LinePanel.addWithStrut0(scroll));

		box2.add (buttons); // new bigger doc / license buttons
		box2.addStrut(20);
		box2.add (s2);
		box2.add (s4);
		box2.add (s3);
		box2.addStrut(15);
		box2.add (l10);
		box2.addStrut0 ();

		JPanel rightPart = new JPanel (new BorderLayout ());
		rightPart.add (box1, BorderLayout.NORTH);
		rightPart.add (tableLine, BorderLayout.CENTER);
		rightPart.add (box2, BorderLayout.SOUTH);

		LinePanel l1 = new LinePanel (4, 4);
		l1.add (p1);
		l1.addStrut0 ();
		ColumnPanel c1 = new ColumnPanel (4, 4);
		c1.add (l1);
		c1.addStrut0 ();

		p1.setOpaque (false);
		l1.setOpaque (false);
		c1.setBackground (Color.WHITE);

		c1.setPreferredSize (new Dimension (100, 100));

		// Set ok as default (see AmapDialog)
		setDefaultButton (ok);
		// ok.setDefaultCapable (true);
		// getRootPane ().setDefaultButton (ok);

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (c1, BorderLayout.WEST);
		getContentPane ().add (rightPart, BorderLayout.CENTER);

		try {
			table.setRowSelectionInterval (0, 0);
		} catch (Exception e) {} // does not matter

		setTitle (Translator.swap ("DAbout.about"));

		// setModal (true); // not needed
	}

}
