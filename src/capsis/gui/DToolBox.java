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
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.kernel.GModel;

/**
 * DToolBox is a dialog box to choose a model tool to apply on a step.
 * Model Tools are capsis extensions.
 * 
 * @author F. de Coligny - may 2002
 */
public class DToolBox extends AmapDialog implements ActionListener, ListSelectionListener {

	private GModel model;
	private String modelToolClassName;

	private Map extensionKey_classname;	// when extension chosen in list, get className (and wait for ok)

	private JList list;
//	private JTextArea description;
	
	private JTextPane propertyPanel;

	private JButton ok;
	private JButton cancel;
	private JButton help;


	/**
	 * Constructor.
	 */
	public DToolBox (GModel model) {
		super ();
		this.model = model;
		modelToolClassName = "";
		
		searchModelTools ();
		createUI ();
		
		// location is set by AmapDialog
		activateLocationMemorization(getClass ().getName ());
		activateSizeMemorization(getClass ().getName ());
		
		pack ();
		setVisible (true);
		
	}

	private void searchModelTools () {
		
		// Ask the ExtensionManager for model tools compatible with the model 
		// behind the scenario
		ExtensionManager extMan = CapsisExtensionManager.getInstance ();
		Collection classNames = extMan.getExtensionClassNames (CapsisExtensionManager.MODEL_TOOL, model);
		
		extensionKey_classname = new HashMap ();	
		
		for (Iterator i = classNames.iterator (); i.hasNext ();) {
			String className = (String) i.next ();
			
			// getExtensionName () loads class -> labels added to Translator
			String extensionKey = ExtensionManager.getName (className);
			extensionKey_classname.put (extensionKey, className);
		}
	}

	private void okAction () {
		// classic checks...
		if (modelToolClassName != null && modelToolClassName.length () != 0) {
			setValidDialog (true);
		}
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
			//dispose (); done by the caller
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/** 
	 * Processes actions on an item in the models list. 
	 */
	public void valueChanged (ListSelectionEvent evt) {
		
		updatePropertyPanel ();
		
		// fc-7.10.2020
		synchro ();
		
//		JList src = (JList) evt.getSource ();
//		String humanKey = (String) src.getSelectedValue ();
//		modelToolClassName = (String) extensionKey_classname.get (humanKey);
//		
//		Settings.setProperty ("toolbox.last.selection", humanKey);
//		
//		// user information
//		// descriptionKey = className (without package i.e;"little name)+".description"
//		String descriptionKey = AmapTools.getClassSimpleName (modelToolClassName)+".description";
//		updateDescription (Translator.swap (descriptionKey));
	}

	private void synchro () {
		// fc-7.10.2020
		boolean aToolIsSelected = (list.getSelectedIndex() != -1);
		
		ok.setEnabled(aToolIsSelected);
	}
	
	private void updatePropertyPanel () {
	
		try {
			String humanKey = (String) list.getSelectedValue ();
			modelToolClassName = (String) extensionKey_classname.get (humanKey);
			
			Settings.setProperty ("toolbox.last.selection", humanKey);
			
			CapsisExtensionManager.getInstance ().getPropertyPanel (modelToolClassName, propertyPanel);

		} catch (Exception e) {
			// There can be an exception if change in combo and there was a selection
		}
		
	}
	
	
	/**
	 * From ListSelectionListener interface.
	 * Called when selection changes on list 2 (selected filters).
	 */
//	private void updateDescription (String text) {
//		description.setText (text);
//		description.setToolTipText (text);
//		description.setCaretPosition (0);
//	}
	
	/** 
	 * Initializes the dialog's GUI. 
	 */
	private void createUI () {
		
		// 1. Help text
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("DToolBox.selectAModelTool")));
		l1.addGlue ();
		
		// 2. List of candidate model tools
		LinePanel l2 = new LinePanel ();
		JWidthLabel lab = new JWidthLabel (Translator.swap ("DToolBox.modelTools")+" :", 130);
 		ColumnPanel component1 = new ColumnPanel ();
		component1.add (lab);
		component1.addGlue ();
		
		TreeSet sortedExtensionKeys = new TreeSet<String>(extensionKey_classname.keySet());
		Vector modelTools = new Vector(sortedExtensionKeys);
		list = new JList(modelTools);
		list.addListSelectionListener(this);
		
		// added to enable the double-click option in the JList object MFortin2010-01-20
		MouseListener mouseListener = new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					okAction();
				}
			}
		};
		list.addMouseListener(mouseListener); 		
	
		
		list.setVisibleRowCount (5);
		list.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		JScrollPane scroll = new JScrollPane (list);
		
		JPanel centered = new JPanel (new BorderLayout ());
		centered.add (scroll, BorderLayout.CENTER);
				
		l2.add (component1);
		l2.add (centered);
		l2.addStrut0 ();
		
		// 3. Selected modelTool properties
		LinePanel l4 = new LinePanel ();
		l4.add (new JLabel (Translator.swap ("DToolBox.description")+" :"));
		l4.addGlue ();
		
		propertyPanel = new JTextPane () {
			@Override
			public Dimension getPreferredScrollableViewportSize () {
				return new Dimension (100, 100);
			}
		};
		propertyPanel.setEditable (false);

		JScrollPane scrollpane = new JScrollPane (propertyPanel);

		
//		description = new JTextArea (4, 15);
//		description.setEditable (false);
//		description.setLineWrap (true);
//		description.setWrapStyleWord (true);
//		JScrollPane scrollpane = new JScrollPane (description);

		ColumnPanel c = new ColumnPanel ();
		c.add (scrollpane);
		c.addStrut0 ();
		LinePanel l = new LinePanel ();
		l.add (c);
		l.addStrut0 ();
		
		// 4. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon ("ok_16.png");
		ok.setIcon(icon);
		
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		icon = IconLoader.getIcon ("cancel_16.png");
		cancel.setIcon(icon);
		
		help = new JButton (Translator.swap ("Shared.help"));
		icon = IconLoader.getIcon ("help_16.png");
		help.setIcon(icon);
		
		
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);
		
		// Set ok as default (see AmapDialog)
		setDefaultButton(ok); // fc-29.8.2018
//		ok.setDefaultCapable (true);
//		getRootPane ().setDefaultButton (ok);
		
		ColumnPanel part1 = new ColumnPanel ();
		part1.add (l1);
		part1.add (l2);
		part1.add (l4);
		part1.addGlue ();
		
		// fc-7.10.2020
		ColumnPanel aux = new ColumnPanel ();
		aux.add(l);
		aux.add(pControl);
		aux.addStrut0 ();
		
		getContentPane ().setLayout (new BorderLayout ());
//		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (aux, BorderLayout.SOUTH);
		
		part1.setPreferredSize (new Dimension (500, 200));
		
		setTitle (Translator.swap ("DToolBox")+" - "+Current.getInstance ().getStep ().getCaption ());
		
		// Try to restore selection (nov 2011)
		String lastSelection = Settings.getProperty ("toolbox.last.selection", ""); // default value: ""
		try {
			boolean shouldScroll = true;
			list.setSelectedValue (lastSelection, shouldScroll);
		} catch (Exception e) {} // if trouble, no selection
		
		synchro ();
		
		setModal (true);
	}

	public String getModelToolClassName () {
		return modelToolClassName;
	}

}



