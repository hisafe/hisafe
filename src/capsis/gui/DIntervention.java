/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2018 INRA
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski,
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
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.command.InterventionDialog;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.util.Helper;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import capsis.util.FishGroupHelper;
import capsis.util.group.Group;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import jeeb.lib.util.task.StatusBar;

/**
 * DIntervention is a dialog box to choose an intervener to be applied on a
 * step. Interveners are Capsis extensions.
 * 
 * @author F. de Coligny - November 2000
 */
public class DIntervention extends AmapDialog
		implements InterventionDialog, ActionListener, ItemListener, GrouperChooserListener, ListSelectionListener {

	// fc-28.8.2018 Complete review
	// fc-14.4.2006 Grouper chooser may throw an exception -> added
	// groupEnabled

	// This dialog was opened on a Step, there is a GModel connected to the
	// related Project (Capsis architecture)
	private GModel model;
	private Step step;

	// The intervener selected by user at dialog validation time
	private Intervener intervener;

	// SubType -> list of matching extensions translated names
	private ListMap<String, String> subType_extensionNames;

	// Extension translated name -> extension className
	private Map<String, String> extensionName_classname;

	// User can optionally select a grouper to restrict the intervention to a
	// sublist of individuals (not all interveners are compatible with groupers)
	private GrouperChooser grouperChooser;

	// Will be true if groupers are found compatible with the model
	private boolean groupEnabled; // fc-15.4.2006 null error on IfnCA

	// Available intervention subTypes (names)
	// I.e. Intervener.getSubType ()
	private JComboBox subTypeCombo;

	// fc-25.10.2019 combo fine management (when grouping / degrouping)
	private boolean comboListeningEnabled;

	// List of interveners matching the selected subType
	private JList intervenerList;

	// Properties of the selected intervener
	private JTextPane propertyPanel;

	private StatusBar statusBar;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Constructor. The chosen Intervener is supposed to be run on the given
	 * simulation Step.
	 */
	public DIntervention(JFrame frame, Step step) {
		super(frame);

		this.step = step;
		this.model = step.getProject().getModel();

		comboListeningEnabled = true; // default

		// At first time, no grouper
		GroupableType grouperType = null;
		updateIntervenerMaps(grouperType);

		createUI();

		activateSizeMemorization(getClass().getName());
		activateLocationMemorization(getClass().getName());

		pack(); // computes size

		setVisible(true);

	}

	/**
	 * Build interveners maps (grouper is considered if activated) grouperType
	 * may be null -> no grouper active
	 */
	private void updateIntervenerMaps(GroupableType grouperType) {

		// Ask the ExtensionManager for interveners compatible with the model
		// behind the scenario
		ExtensionManager extMan = CapsisExtensionManager.getInstance();
		Collection<String> classNames = extMan.getExtensionClassNames(CapsisExtensionManager.INTERVENER, model);

		subType_extensionNames = new ListMap<String, String>();

		extensionName_classname = new HashMap<String, String>();

		// Loop on the interveners classNames, keep the ones with the expected
		// subType
		for (Iterator i = classNames.iterator(); i.hasNext();) {

			String className = (String) i.next();

			// fc-22.9.2004 If grouper active, keep only compatible interveners
			if (groupEnabled && grouperType != null) {
				try {

					// Intervener is not groupable -> ignore it
					if (!(ExtensionManager.isInstanceOf(className, GroupableIntervener.class)))
						continue;

					GroupableIntervener interv = (GroupableIntervener) ExtensionManager
							.getExtensionPrototype(className);

					GroupableType intervType = interv.getGrouperType();

					// Intervener with wrong grouper type -> ignore it
					if (!intervType.equals(grouperType))
						continue;

				} catch (Exception e) {
					Log.println(Log.ERROR, "DIntervention.searchInterveners ()",
							"Error while trying to know if intervener " + className + " deals with grouper type "
									+ grouperType,
							e);
				}
			}

			// CHANGED (consistent extension framework 2018)
			// CHANGED (consistent extension framework 2018)

			// This line was doing the job temporarily by searching both the
			// constant and the method, REMOVED when getSubType () was
			// added in Interveners, getInfo should not be used any more
			// String subType = getInfo(className, "SUBTYPE", "getSubType");

			// Framework 2018 getName () and getSubType () asked to an extension
			// prototype
			Intervener intervener = (Intervener) CapsisExtensionManager.getExtensionPrototype(className);
			String extensionName = intervener.getName();

			String subType = intervener.getSubType();

			// fc-30.8.2018 getSubType() now returns a translated value
			if (subType == null)
				// This should never happen: the Translator returns the key to
				// be translated if it can not find a translation
				subType = Translator.swap("DIntervention.miscellaneous");

			// CHANGED (consistent extension framework 2018)
			// CHANGED (consistent extension framework 2018)

			subType_extensionNames.addObject(subType, extensionName); // ListMap
			extensionName_classname.put(extensionName, className);
		}

	}

	// fc-31.8.2018 REMOVED, use extensionPrototype.getSubType () instead
	// fc-31.8.2018 REMOVED, use extensionPrototype.getSubType () instead
	// static protected String getInfo(String className, String fieldName,
	// String methodName) {
	//
	// try {
	//
	// // New extension framework
	// // Search the standard static fields: NAME, AUTHOR, VERSION,
	// // DESCRIPTION...
	// Class<?> cl = ExtensionManager.getClass(className);
	// String ret = ExtensionManager.getStaticField(cl, fieldName);
	// if (ret != null) {
	// return ret;
	// }
	//
	// // TO BE REMOVED when all the extensions are adapted to the new
	// // framework...
	//
	// // If the static field was not found, try the former extension
	// // framework: standard methods: getName (), getAuthor (), getVersion
	// // (), getDescription ()
	//
	// // Do not call Component.getName () (technical method, not what we
	// // expect here)
	// if (methodName.equals("getName") && Component.class.isAssignableFrom(cl))
	// throw new Exception(); // fc-14.12.2016 Go to Log lower
	// // return null;
	//
	// Object ext = ClassUtils.instantiateClass(cl);
	// Method m = cl.getMethod(methodName, (Class<?>[]) null);
	// return (String) m.invoke(ext, (Object[]) null);
	//
	// // ...TO BE REMOVED
	//
	// } catch (Exception e) {
	//
	// // REMOVED fc-27.1.2017 the Log is full of these messages, prefer
	// // GraphicalExtensionManager > Select lines > Warnings for selected
	// // extensions
	// // Log.println(Log.ERROR, "ExtensionManager.getInfo ()",
	// // "Extension framework: missing static field " + fieldName
	// // + " (and also former framework method "
	// // + methodName + "()), " + className, e);
	//
	// return null;
	//
	// }
	//
	// }
	// fc-31.8.2018 REMOVED, use extensionPrototype.getSubType () instead
	// fc-31.8.2018 REMOVED, use extensionPrototype.getSubType () instead

	/**
	 * Grouper changed in grouper chooser.
	 */
	public void grouperChanged(String grouperName) {

		// System.out.println("[DIntervention] grouperChanged, groupEnabled: " +
		// groupEnabled + "...");

		if (!groupEnabled)
			return;

		if (grouperName == null || grouperName.equals("")) {
			synchronizeOnGrouperType(null);

		} else {
			GrouperManager gm = GrouperManager.getInstance();
			Grouper newGrouper = gm.getGrouper(grouperName);

			synchronizeOnGrouperType(newGrouper.getType());
		}
	}

	/**
	 * If grouper is changed, update intervenerList and subTypeCombo
	 * accordingly.
	 */
	private void synchronizeOnGrouperType(GroupableType grouperType) {

		// System.out.println("[DIntervention] synchronizeOnGrouperType,
		// grouperType: " + grouperType + "...");

		if (!groupEnabled)
			return; // nothing to do

		updateIntervenerMaps(grouperType);

		// subType names, they are translated
		Set<String> subTypes = new TreeSet<String>(subType_extensionNames.getKeys());

		fillComboBox(subTypeCombo, subTypes);

		subTypeCombo.revalidate();
		subTypeCombo.repaint();
		intervenerList.revalidate();
		intervenerList.repaint();

		// fc-25.10.2019
		// Try to reselected same entry in combo, else first entry
		try {

			if (subTypes.contains(Settings.getProperty("capsis.last.intervention.subtype", "")))
				subTypeCombo.setSelectedItem(Settings.getProperty("capsis.last.intervention.subtype", ""));

		} catch (Exception e) {

			try {
				subTypeCombo.setSelectedIndex(0);

			} catch (Exception e2) {
				// combo may be empty, ignore
			}

		}

		// fc-25.10.2019 needed in special cases, e.g. groups enabled and the
		// current subtype contains no intervention compatible with groups ->
		// the subtype is removed from the combo, another subtype is selected
		// and the extension list needs a refresh
		typeComboAction();

		// System.out.println("[DIntervention] synchronizeOnGrouperType: in the
		// end, capsis.last.intervention.subtype: "
		// + Settings.getProperty("capsis.last.intervention.subtype", ""));

		// fc-25.10.2019

		// This will restore selection in the list below the combo
		restoreExtensionListSelection();

	}

	/**
	 * Action on ok
	 */
	private void okAction() {

		String humanKey = (String) intervenerList.getSelectedValue();

		if (humanKey == null) { // If no selection
			MessageDialog.print(this, Translator.swap("DIntervention.chooseAnIntervener"));
			return;
		}

		// Intervener class name
		String intervenerClassName = (String) extensionName_classname.get(humanKey);

		if (intervenerClassName != null && intervenerClassName.length() != 0) {

			statusBar.print(Translator.swap("DIntervention.sceneCopy") + "...");

			// Get the intervention base (a copy of the scene to be processed)
			GScene refScene = step.getScene();
			GScene newStand = (GScene) refScene.getInterventionBase();

			if (newStand == null) {
				// fc-17.12.2003 - getInterventionBase can return null if
				// trouble
				MessageDialog.print(this, Translator.swap("DIntervention.interventionBaseError"));
				return; // Abort
			}

			statusBar.print(Translator.swap("DIntervention.sceneCopyDone"));

			// Is there a Grouper selected ?
			// fc-22.9.2004 Interveners may be ran on groups
			Collection indivs = null; // default: no grouper

			// fc-22.9.2004 Maybe null if no grouper selected
			String grouperName = getGrouperName();
			Grouper grouper = null; // fc-8.6.2020

			if (groupEnabled && grouperName != null) {

				// There is a grouper selected
				GrouperManager gm = GrouperManager.getInstance();
				grouper = gm.getGrouper(grouperName);
				indivs = Group.whichCollection(newStand, grouper.getType());

				// Apply the grouper on the scene
				indivs = grouper.apply(indivs, grouperName.toLowerCase().startsWith("not "));
			}

			try {
				// Instanciate the selected intervener
				intervener = (Intervener) CapsisExtensionManager.getInstance().instantiate(intervenerClassName);

				statusBar.print(Translator.swap("DIntervention.intervenerInit") + "...");

				// fc-8.6.2020 The intervener is now aware of being grouped and
				// can ask getSceneArea to the grouper, before init ()
				if (grouper != null && intervener instanceof GroupableIntervener)
					((GroupableIntervener) intervener).setGrouper(grouper);
				
				intervener.init(step.getProject().getModel(), step, newStand, indivs);

				statusBar.print(Translator.swap("DIntervention.intervenerConfig") + "...");
				intervener.initGUI();

			} catch (Exception e) {
				MessageDialog.print(this, Translator.swap("Intervention.intervenerCanNotBeLoaded"), e);
				Log.println(Log.ERROR, "Intervention.execute ()", "Exception caught:", e);
				return; // Abort
			}

			// Intervener has opened a dialog and configuration is correct
			// => close this dialog valid, the intervention will be processed in
			// command.Intervention
			if (intervener.isReadyToApply()) {
				setValidDialog(true);
			} else {
				// User canceled the intervener dialog, he can choose another
				// intervener or cancel the whole intervention by canceling this
				// dialog
				statusBar.print(Translator.swap("DIntervention.chooseAnIntervener"));
			}

		}
	}

	/**
	 * Some button was hit
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/**
	 * Selection in intervention subType combo box
	 */
	@Override
	public void itemStateChanged(ItemEvent evt) {

		// fc-25.10.2019 don't listen during combo clearance and refill
		if (!comboListeningEnabled) {
			// System.out.println("[DIntervention] ### itemStateChanged,
			// comboListeningEnabled: " + comboListeningEnabled
			// + ", aborted ###");
			return;
		}

		if (evt.getSource().equals(subTypeCombo)) {

			if (evt.getStateChange() == ItemEvent.SELECTED)
				typeComboAction();

		}

		// System.out.println("[DIntervention] itemStateChanged: in the end,
		// subTypeCombo selection: "
		// + subTypeCombo.getSelectedItem() + "...");
	}

	/**
	 * Changing intervention subType triggers an update of the intervener list.
	 */
	private void typeComboAction() {

		// System.out.println("[DIntervention] typeComboAction...");

		List<String> intervenerNames = getIntervenerNamesMatchingTypeCombo();

		intervenerList.setListData(new Vector<String>(intervenerNames));

		// Memo for next time
		Settings.setProperty("capsis.last.intervention.subtype", "" + subTypeCombo.getSelectedItem());

		restoreExtensionListSelection();

	}

	/**
	 * Returns the list of names of interveners matching the type in
	 * subTypeCombo. Returns a non null vector. The returned vector is sorted
	 * (fc-19.5.2011)
	 */
	private List<String> getIntervenerNamesMatchingTypeCombo() {

		List<String> v = null;

		try {
			// fc-30.8.2018 getSubType() now returns a translated label
			String subType = (String) subTypeCombo.getSelectedItem();

			v = subType_extensionNames.getObjects(subType); // a ListMap
			v = new Vector(new TreeSet(v)); // sorted

		} catch (Exception e) {
			// This may happen if no intervener in the list (grouperMode...)
			// fc-22.9.2004 -> do not write to Log
			return new Vector<String>();
		}

		if (v == null)
			return new Vector<String>();

		return v;
	}

	/**
	 * Selection change in intervenerList
	 */
	public void valueChanged(ListSelectionEvent evt) {

		try {
			// Memo selection for next time
			String extensionName = (String) intervenerList.getSelectedValue();
			String subType = (String) subTypeCombo.getSelectedItem();
			Settings.setProperty("capsis.last.selected.intervention.for.subType." + subType, extensionName);

			updatePropertyPanel();

		} catch (Exception e) {
			// Maybe no selection
		}

	}

	/**
	 * Updates the property panel for the selected intervener.
	 */
	private void updatePropertyPanel() {
		try {
			String extensionName = (String) intervenerList.getSelectedValue();
			String intervenerClassName = (String) extensionName_classname.get(extensionName);

			CapsisExtensionManager.getInstance().getPropertyPanel(intervenerClassName, propertyPanel);

		} catch (Exception e) {
			// There can be an exception if change in combo and there was a
			// selection, ignored
		}
	}

	private void restoreExtensionListSelection() {

		// Try to restore list selection

		// If the property is not found, memory = ""
		String subType = (String) subTypeCombo.getSelectedItem();
		String memory = Settings.getProperty("capsis.last.selected.intervention.for.subType." + subType, "");

		boolean shouldScroll = true;
		intervenerList.setSelectedValue(memory, shouldScroll);

		if (intervenerList.getSelectedIndex() == -1)
			intervenerList.setSelectedIndex(0); // first item

	}

	/**
	 * Initialize the dialog's GUI.
	 */
	private void createUI() {

		// ----- Panel 1: groups -----
		ColumnPanel groupPanel = new ColumnPanel(Translator.swap("DIntervention.groupRestriction"));

		// fc-29.8.2018 Check if groupers can be used
		groupEnabled = true;
		try {

			// Make a GrouperChooser
			GScene refScene = Current.getInstance().getStep().getScene();
			GroupableType type = null;

			// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
			// Added groups: fishes, reaches and weirs. nb-14.08.2020
			List<GroupableType> expectedTypes = new ArrayList<GroupableType>();
			expectedTypes.add(TreeList.GROUP_ALIVE_TREE);
			expectedTypes.add(FishGroupHelper.GROUP_FISH);
			expectedTypes.add(FishGroupHelper.GROUP_REACH);
			expectedTypes.add(FishGroupHelper.GROUP_WEIR);
			grouperChooser = new GrouperChooser(refScene, type, expectedTypes, "", false,
					true, false);
//			grouperChooser = new GrouperChooser(refScene, type, Arrays.asList(TreeList.GROUP_ALIVE_TREE), "", false,
//					true, false);
			grouperChooser.addGrouperChooserListener(this);

			LinePanel l10 = new LinePanel();
			l10.add(grouperChooser);
			l10.addStrut0();

			groupPanel.add(l10);

			// fc-30.8.2018 Added an explanation about groupers
			MemoPanel userMemo = new MemoPanel(Translator.swap("DIntervention.explanationAboutInterventionOnAGroup"));
			groupPanel.add(userMemo);

			groupPanel.addStrut1();

		} catch (Exception e) {
			groupEnabled = false; // in case of trouble
		}

		// ----- Panel 2: subType and intervener list -----
		ColumnPanel interventionPanel = new ColumnPanel(Translator.swap("DIntervention.selectAnIntervener"));

		// Combo subTypeCombo

		// subType names, they are translated
		Set<String> subTypes = new TreeSet<String>(subType_extensionNames.getKeys());

		subTypeCombo = new JComboBox(new Vector(subTypes));
		subTypeCombo.addItemListener(this);

		// Try to restore combo last selection
		try {
			if (subTypes.contains(Settings.getProperty("capsis.last.intervention.subtype", "")))
				subTypeCombo.setSelectedItem(Settings.getProperty("capsis.last.intervention.subtype", ""));
		} catch (Exception e) {
			// No matter if failure
		}

		LinePanel l2 = new LinePanel();
		l2.add(new JWidthLabel(Translator.swap("DIntervention.interventionType") + " :", 200));
		l2.add(subTypeCombo);
		l2.addStrut0();

		// List of candidate interveners
		LinePanel l3 = new LinePanel();
		JWidthLabel lab = new JWidthLabel(Translator.swap("DIntervention.interventionMethod") + " :", 200);
		ColumnPanel component1 = new ColumnPanel();
		component1.add(lab);
		component1.addGlue();

		List<String> interveners = getIntervenerNamesMatchingTypeCombo();

		intervenerList = new JList(new Vector<String>(interveners));
		intervenerList.addListSelectionListener(this); // see valueChanged ()
		intervenerList.setVisibleRowCount(7);
		intervenerList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		intervenerList.addMouseListener(new MouseAdapter() {

			public void mouseClicked(MouseEvent e) {
				if (e.getClickCount() == 2) {
					okAction();
				}
			}
		});

		JScrollPane scroll = new JScrollPane(intervenerList);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(scroll, BorderLayout.CENTER);

		l3.add(component1);
		l3.add(aux);
		l3.addStrut0();

		// Selected intervener properties
		propertyPanel = new JTextPane() {
			@Override
			public Dimension getPreferredScrollableViewportSize() {
				return new Dimension(100, 100);
			}
		};
		propertyPanel.setEditable(false);

		JScrollPane scrollpane = new JScrollPane(propertyPanel);

		ColumnPanel c = new ColumnPanel();
		c.add(scrollpane);
		c.addStrut0();
		LinePanel l = new LinePanel();
		l.add(c);
		l.addStrut0();

		// Try to restore list selection
		restoreExtensionListSelection();

		interventionPanel.add(l2);
		interventionPanel.add(l3);

		interventionPanel.add(l);
		interventionPanel.addStrut1();

		// ----- Panel 3: Control panel -----
		LinePanel pControl = new LinePanel();

		// StatusDispatcher will print in this status bar
		boolean withTaskManagerView = true; // fc-29.9.2021
		boolean withTaskManagerButton = false; // would not work properly in a
												// modal dialog
		statusBar = new StatusBar(withTaskManagerView, withTaskManagerButton);
		statusBar.print(Translator.swap("DIntervention.chooseAnIntervener"));
		pControl.add(statusBar);

		pControl.addGlue();

		ok = new JButton(Translator.swap("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon("ok_16.png");
		ok.setIcon(icon);
		pControl.add(ok);

		cancel = new JButton(Translator.swap("Shared.cancel"));
		icon = IconLoader.getIcon("cancel_16.png");
		cancel.setIcon(icon);
		pControl.add(cancel);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);
		pControl.add(help);

		pControl.addStrut0();

		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// Set ok as default (see AmapDialog)
		setDefaultButton(ok);

		getContentPane().setLayout(new BorderLayout());

		if (groupEnabled)
			getContentPane().add(groupPanel, BorderLayout.NORTH);

		getContentPane().add(interventionPanel, BorderLayout.CENTER); // description
		getContentPane().add(pControl, BorderLayout.SOUTH);

		setTitle(Translator.swap("DIntervention") + " - " + Current.getInstance().getStep().getCaption());

		setModal(true);
	}

	/**
	 * Tool method: put a collection into a JComboBox The related intervener
	 * list is also updated, see interventionTypesAction ()
	 */
	private void fillComboBox(JComboBox combo, Collection items) {

		// System.out.println("[DIntervention] fillComboBox...");
		// System.out.println("[DIntervention] combo.removeAllItems()...");

		// fc-25.10.2019 We are going to clear and refill the combo, disable
		// events meanwhile (checked)
		comboListeningEnabled = false;

		combo.removeAllItems();

		// System.out.println("[DIntervention] combo.removeAllItems() done");
		// System.out.println("[DIntervention] several combo.addItem()...");

		Iterator i = items.iterator();
		while (i.hasNext()) {
			combo.addItem(i.next());
		}

		comboListeningEnabled = true;

		// System.out.println("[DIntervention] several combo.addItem() done");
	}

	public Intervener getIntervener() { // fc-1.10.2004
		return intervener;
	}

	public String getGrouperName() {
		if (groupEnabled) {
			return grouperChooser.isGrouperAvailable() ? grouperChooser.getGrouperName() : null;
		} else {
			return null;
		}
	}

}
