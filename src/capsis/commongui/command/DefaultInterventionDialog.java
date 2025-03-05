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
package capsis.commongui.command;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;


/**	
 * A default dialog for the Intervention command.	
 * 
 * @author F. de Coligny - October 2010
 */
public class DefaultInterventionDialog extends AmapDialog implements InterventionDialog,  
		ActionListener, ListSelectionListener {

	private GModel model;
	private Step refStep;
	private Intervener intervener;


	private String intervenerClassName;

	// We need 3 tables
	private Map<String, String> subTypeKey_subType;		// subTypeKey -> to comboBox -> retrieve subType
	private Map<String, Vector<String>> subType_extensionKeys;	// from selected subType, matching existing extensions keys
	private Map<String, String> extensionKey_classname;	// when extension chosen in list, get className (and wait for ok)

	private JComboBox interventionTypes;
	private JList intervenerList;
	private JTextArea description;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	
	

	/**	Default constructor.
	 */
	public DefaultInterventionDialog (JFrame frame, Step step) {
		super (frame);
		
		this.refStep = step;
		this.model = step.getProject().getModel();
		
		intervenerClassName = "";

		searchInterveners ();
		createUI ();

		// Location is set by AmapDialog
		pack ();  // computes size
		setVisible (true);

	}

	/**	Returns a list with the interveners matching the user selection, 
	 *	never returns null.
	 *	The returned vector is sorted (fc-19.5.2011)
	 */
	private Vector<String> getCurrentInterveners () {
		Vector<String> v = null;
		try {
			String subTypeKey = (String) interventionTypes.getSelectedItem ();
			String subType = (String) subTypeKey_subType.get (subTypeKey);
			v = (Vector<String>) subType_extensionKeys.get (subType);
			v = new Vector (new TreeSet (v));  // sort
		} catch (Exception e) {
			// this may happen if no intervener is selected
			return new Vector<String> ();
		}
		if (v == null) {return new Vector<String> ();}
		return v;
	}

	/**	Manages add in subType_extensionKeys map: for each subType,
	 *	vector containing the matching extension keys.
	 *	An extension key is something translated by Translator.swap ().
	 *	It can appear on a gui.
	 */
	private void addSubType_extensionKeys (String st, String ek) {

		Vector<String> extensionKeys = (Vector<String>) subType_extensionKeys.get (st);

		// No entry for subType, create one
		if (extensionKeys == null) {
			extensionKeys = new Vector<String> ();
			extensionKeys.add (ek);
			subType_extensionKeys.put (st, extensionKeys);
			return;
		}

		// Entry exists, add extensionKey in related collection
		extensionKeys.add (ek);
	}

	/**	Builds the interveners list
	 */
	private void searchInterveners () {

		// Ask the ExtensionManager for interveners compatible with the model
		// behind the project
		ExtensionManager extMan = Engine.getInstance ().getExtensionManager ();
		Collection<String> classNames = extMan.getExtensionClassNames ("Intervener", model);  // was CapsisExtensionManager.INTERVENER

		// Interveners may be categorized in subTypes
		subTypeKey_subType = new TreeMap<String, String> ();  // sort
		subType_extensionKeys = new HashMap<String, Vector<String>> ();
		extensionKey_classname = new HashMap<String, String> ();

		for (Iterator i = classNames.iterator (); i.hasNext ();) {
			String className = (String) i.next ();

			// 1. getExtensionName () loads class -> labels added to Translator
			String extensionKey = ExtensionManager.getName (className);
			
			// Get intervener subType property
			String subType = ExtensionManager.getStaticField(className, "SUBTYPE");
			if (subType == null) {subType = "Unknown";}

			// 2. subType label was updated during class load in previous getExtensionName ()
			String subTypeKey = Translator.swap (subType);

			if (subTypeKey == null) {
				subTypeKey = "NoSubType";
				subType = "NosubType";
			}
			
			subTypeKey_subType.put (subTypeKey, subType);
			addSubType_extensionKeys (subType, extensionKey);
			extensionKey_classname.put (extensionKey, className);
		}

//		for (Iterator i = subTypeKey_subType.keySet ().iterator (); i.hasNext ();) {
//			System.out.println ("type "+(String) i.next ());
//		}
//		for (Iterator i = extensionKey_classname.keySet ().iterator (); i.hasNext ();) {
//			System.out.println ("name "+(String) i.next ());
//		}

	}

	/**	Action on ok
	 */
	private void okAction () {
		// Classic checks...
		
		if (intervenerClassName != null && intervenerClassName.length () != 0) {

			// Intervener class name is retrieved from DefaultInterventionDialog dialog
			String className = intervenerClassName;
			
			GScene refStand = refStep.getScene ();

			// This object is a copy of refStand which step instance variable is null
			GScene newStand = (GScene) refStand.getInterventionBase ();
			
			if (newStand == null) {	// fc - 17.12.2003 - getInterventionBase may return null if trouble
				MessageDialog.print (this, Translator.swap ("DefaultInterventionDialog.interventionBaseError"));
				return;
			}

			// Load an extension of type chosen by the user
			Collection indivs = null;  // no groups

			try {
				intervener = (Intervener) Engine.getInstance ().getExtensionManager ().instantiate (className);
				
				intervener.init (refStep.getProject ().getModel (), refStep, newStand, indivs);
				intervener.initGUI ();
				
			} catch (Exception e) {
				Log.println (Log.ERROR, "DefaultInterventionDialog.okAction ()", 
						"Could not instanciate intervener: "+className, e);
				MessageDialog.print (this, Translator.swap ("DefaultInterventionDialog.intervenerCanNotBeLoaded")
						+" "+className, e);
				return;
			}

			// Intervener has opened a dialog and configuration is correct
			// => close this dialog valid, the intervention will be processed in command.Intervention
			if (intervener.isReadyToApply ()) {
				setValidDialog (true);
			} else {
				// user canceled the intervener dialog, he can choose another
				// intervener or cancel the whole intervention by canceling this dialog
			}

		}
	}


	/**	Changing intervention type triggers update on the intervener list
	 */
	private void interventionTypesAction () {
		intervenerList.setListData (getCurrentInterveners ());
		Settings.setProperty ("capsis.last.intervention.group",
				""+interventionTypes.getSelectedItem ());	// memo for next time
	}

	/**	Some button was hit
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (interventionTypes)) {
			interventionTypesAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**	Process actions on an item in the models list.
	*/
	public void valueChanged (ListSelectionEvent evt) {
		try {
			JList src = (JList) evt.getSource ();

			String humanKey = (String) src.getSelectedValue ();
			Settings.setProperty ("capsis.last.selected.intervention", humanKey);	// memo for next time
			
			intervenerClassName = (String) extensionKey_classname.get (humanKey);

			// user information
			// descriptionKey = className (without package i.e;"little name)+".description"
			String descriptionKey = AmapTools.getClassSimpleName (intervenerClassName)+".description";
			updateDescription (Translator.swap (descriptionKey));
		} catch (Exception e) {}  // can be an exception if change in combo and there was a selection in the list
	}


	/**	From ListSelectionListener interface.
	 *	Called when selection changes on list 2 (selected filters).
	 */
	private void updateDescription (String text) {
		description.setText (text);
		description.setToolTipText (text);
		description.setCaretPosition (0);
	}

	/**	Initialize the dialog's GUI.
	 */
	private void createUI () {

		ColumnPanel interventionPanel = new ColumnPanel (
				Translator.swap ("DefaultInterventionDialog.selectAnIntervener"));

		// Combo for interventionTypes
		LinePanel l2 = new LinePanel ();

		Set keys = subTypeKey_subType.keySet ();
		interventionTypes = new JComboBox (new Vector (keys));
		// Try to reset combo like last time it was used
		try {
			if (keys.contains (Settings.getProperty ("capsis.last.intervention.group", ""))) {
				interventionTypes.setSelectedItem (Settings.getProperty ("capsis.last.intervention.group", ""));
			}
		} catch (Exception e) {}
		interventionTypes.addActionListener (this);
		l2.add (new JWidthLabel (Translator.swap ("DefaultInterventionDialog.interventionType")+" :", 200));
		l2.add (interventionTypes);
		l2.addStrut0 ();

		// List of candidate interveners
		LinePanel l3 = new LinePanel ();
		JWidthLabel lab = new JWidthLabel (Translator.swap ("DefaultInterventionDialog.interventionMethod")+" :", 200);
 		ColumnPanel component1 = new ColumnPanel ();
		component1.add (lab);
		component1.addGlue ();

		Vector interveners = getCurrentInterveners ();	// according to type comboBox

		intervenerList = new JList (interveners);
		intervenerList.addListSelectionListener (this);
		intervenerList.setVisibleRowCount (4);
		intervenerList.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		intervenerList.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
		        if (e.getClickCount() == 2) {
		        	okAction();
		        }
			}
		});
		JScrollPane scroll = new JScrollPane (intervenerList);

		JPanel aux = new JPanel (new BorderLayout ());
		aux.add (scroll, BorderLayout.CENTER);

		l3.add (component1);
		l3.add (aux);
		l3.addStrut0 ();

		// Label "description"
		LinePanel l4 = new LinePanel ();
		l4.add (new JLabel (Translator.swap ("DefaultInterventionDialog.description")+" :"));
		l4.addGlue ();

		// Selected intervener description
		description = new JTextArea (4, 15);
		description.setEditable (false);
		description.setLineWrap (true);
		description.setWrapStyleWord (true);
		JScrollPane scrollpane = new JScrollPane (description);
		ColumnPanel c = new ColumnPanel ();
		c.add (scrollpane);
		c.addStrut0 ();
		LinePanel l = new LinePanel ();
		l.add (c);
		l.addStrut0 ();

		
		// Try to restore list selection
		try {
			if (interveners.contains (Settings.getProperty ("capsis.last.selected.intervention", ""))) {
				boolean shouldScroll = true;
				intervenerList.setSelectedValue(Settings.getProperty ("capsis.last.selected.intervention", ""), shouldScroll);
			} else {
				throw new Exception ();
			}
		} catch (Exception e) {
			try {intervenerList.setSelectedIndex (0);} catch (Exception e2) {} // list may be empty
		}
//		try {
//			intervenerList.setSelectedIndex (0);	// list may be empty
//		} catch (Exception e) {
//			updateDescription ("");
//		}  // if empty, nothing more

		interventionPanel.add (l2);
		interventionPanel.add (l3);
		interventionPanel.add (l4);
		interventionPanel.add (l);
		interventionPanel.addStrut1 ();

		// Control panel
		LinePanel controlPanel = new LinePanel ();
		
		ok = new JButton (Translator.swap ("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon ("ok_16.png");
		ok.setIcon(icon);
				
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		icon = IconLoader.getIcon ("cancel_16.png");
		cancel.setIcon(icon);
		
		help = new JButton (Translator.swap ("Shared.help"));
		icon = IconLoader.getIcon ("help_16.png");
		help.setIcon(icon);
		
		controlPanel.addGlue ();  // right justified
		controlPanel.add (ok);
		controlPanel.add (cancel);
		controlPanel.add (help);
		controlPanel.addStrut0 ();
		
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		// Set ok as default (see AmapDialog)
		setDefaultButton(ok);


		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (interventionPanel, BorderLayout.CENTER);		// description
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);

		setTitle (Translator.swap ("DefaultInterventionDialog")+" - "+Current.getInstance ().getStep ().getCaption ());
		
		setModal (true);
	}

	/**	Tool method: put a collection into a JComboBox
	 *	The related intervener list is also updated, see interventionTypesAction ()
	 */
	private void fillComboBox (JComboBox combo, Collection items) {
		combo.removeAllItems ();
		Iterator i = items.iterator ();
		while (i.hasNext ()) {combo.addItem (i.next ());}
	}


	public String getIntervenerClassName () {
		return intervenerClassName;
	}

	public Intervener getIntervener () {
		return intervener;
	}


}



