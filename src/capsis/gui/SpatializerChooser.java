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

import java.awt.ItemSelectable;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JComboBox;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.defaulttype.Tree;
import capsis.util.Action4Event;

/**	Component to choose a Spatializer extension, it is a combo box.
*
*	@author F. de Coligny - july 2006
*/
public class SpatializerChooser extends LinePanel implements ItemListener {
	public final static int BUTTON_SIZE = 23;
	//~ public final static String HELP_LINE = "["+Translator.swap ("SpatializerChooser.spatializer")+"]";
	
	private Collection<Tree> candidateTrees;	// trees to be spatialized
	private Object caller;	// for memorization of last mode selected by user
	private String lastSelectedOption;
	private ExtensionManager extMan;
	
	private JComboBox combo;		// main combo box
	
	private Map spatializerToClassName;
	private Object lastSelection;	// updated each time select is called
	
	private Collection<ActionListener> listeners;
	
	
	/** 	Constructor. 
	*/
	public SpatializerChooser (Collection<Tree> candidateTrees, Object caller) {
		super (0, 0);
		
		this.candidateTrees = candidateTrees;
		this.caller = caller;
		lastSelectedOption = null;
		
		extMan = CapsisExtensionManager.getInstance ();
		
		prepareCombo ();
		createUI ();
	}
	
	
	// Get compatible spatializers for candidate
	//
	private void prepareCombo () {
        spatializerToClassName = new HashMap ();
        //~ spatializerToClassName.put (HELP_LINE, null);

		Set spatializers = new HashSet ();

		// Get the spatializers for this collection of trees
		spatializers.addAll (extMan.getExtensionClassNames (
				CapsisExtensionManager.SPATIALIZER, candidateTrees));

		// Build Map SpatializerName -> className
		for (Iterator i = spatializers.iterator (); i.hasNext ();) {
			String className = (String) i.next ();
			spatializerToClassName.put (ExtensionManager.getName (className), className);
		}
    }
	
	
	//	Select a compatible spatializer for the caller
	//	Caller is normally notified by itemStateChanged ()
	//	This method can be called by caller once its gui correctly set to 
	//	choose a first spatializer in the combo.
	//
	public void initSelection () {
		//~ if () {	// memorized selection for this caller ? - todo
		
		//~ } else {
			combo.setSelectedIndex (0);
			forceItemStateChanged ();
		//~ }
    }
	
	// Display a message in standard output (do nothing in normal mode)
	//
	//~ public void display (String message) {
		//System.out.println ("SpatializerChooser: "+message);
    //~ }
	
	
	
    // Create a selected viewer (an object viewer extension) for given subject on user request
    //
    //~ protected ObjectViewer getViewer (String className, Object subject) {
        //~ ObjectViewer ov = null;
        
        //~ ExtensionStarter starter = new ExtensionStarter ();
        //~ starter.setObject (subject);
        //~ try {
           //~ ov = (ObjectViewer) extMan.loadExtension (className, starter);
		   
		   //~ // Memorize lastSelection
		   //~ // Caller may ask for it in order to enlight last selection
		   //~ //
		   //~ lastSelection = subject;
		   
        //~ } catch (Throwable e) {
            //~ Log.println (Log.WARNING, "SVSimple.getViewer ()", 
                         //~ "could not open viewer "+className+" due to exception ", e);
            //MessageDialog.promptError (Translator.swap ("SVSimple.errorWhileOpeningObjectViewerSeeLog"));
			//~ return null;	// fc - 4.11.2003
        //~ }
		//~ return ov;
	//~ }


	/**	Last selection is a reference to the last subject shown by select ()
	*	and getViewer ().
	*	It may be an Object (ex: a GMaddTree) or a Collection of 
	*	objects (ex: a Collection of cells), or an array of things
	*	(ex: an int[] or an Object[]), or a Map...
	*	It can be used by selection caller to enlight last selection.
	*/
	//~ public Object getLastSelection () {return lastSelection;}


	/**	Actions on components.
	*/
	//~ public void actionPerformed (ActionEvent evt) {
		//~ if (evt.getSource ().equals (enabled)) {
			//~ combo.setEnabled (enabled.isSelected ());
			//~ if (select != null) {select.setEnabled (enabled.isSelected ());}
		//~ } else if (evt.getSource ().equals (select)) {
			//~ fireSelectWasHit ();	// tell listeners
		//~ }
	//~ }

	private void forceItemStateChanged () {
		ItemSelectable source = combo;
		int id = -1;
		Object item = combo.getSelectedItem ();
		int stateChange = -1;
		
		ItemEvent e = new ItemEvent (source, id, item, stateChange);
		itemStateChanged (e);
	}

	/**	Called when an item is selected in combo.
	*/
	public void itemStateChanged (ItemEvent evt) {
System.out.println ("SpatializerChooser.itemStateChanged ()...");
		if (evt.getSource().equals (combo)) {
			String option = (String) evt.getItem ();
System.out.println ("SpatializerChooser option="+option);
			
			// fc - 21.9.2006 - initSelection () must notifyListeners () even is 
			// only one spatializer in combo (and reselect it)
			//~ if (!(option.equals (lastSelectedOption))) {
				lastSelectedOption = option;
				// Memorize selected option to reselect it next time SpatializerChooser is opened
				Settings.setProperty ("SpatializerChooser.last.selection.for."+caller.getClass ().getName (), 
						option);
				String className = (String) spatializerToClassName.get (option);
				
				// Notify listeners of the spatializer change
				Object source = this;
				String command = "spatializerChanged";
				Action4Event event = new Action4Event (source, command);
				event.set1 (option);		// the new spatializer name
				event.set2 (className);	// the new spatializer class name
				notifyActionListeners (event);
				
			//~ }
		}
	}
	
	// Create the gui.
	//
	private void createUI () {
		
		// 3. Object spatializers names combo box.
		// 
		TreeSet spaNames = new TreeSet (spatializerToClassName.keySet ());	// sorted
		combo = new JComboBox (spaNames.toArray ());
		combo.addItemListener (this);
		
		// 3.1 restore last user selection in combo
		//
		String lastSelectedSpa = Settings.getProperty (
				"SpatializerChooser.last.selection.for."+caller.getClass ().getName (), (String)null);
		
		if (lastSelectedSpa != null && spaNames.contains (lastSelectedSpa)) {
			combo.setSelectedItem (lastSelectedSpa);
		//~ } else {
			//~ combo.setSelectedItem (HELP_LINE);
		}
		
		add (combo);
		
		addStrut0 ();
	}


	// Add a listener
	//
	public void addActionListener (ActionListener l) {
		if (listeners == null) {listeners = new ArrayList<ActionListener> ();}
		listeners.add (l);
	}


	// Remove a listener
	//
	public void removeActionListener (ActionListener l) {
		if (listeners == null) {return;}
		listeners.remove (l);
	}


	// Notify the listeners that some event occurred
	//
	private void notifyActionListeners (ActionEvent evt) {
		if (listeners == null) {return;}
		for (ActionListener l : listeners) {l.actionPerformed (evt);}
	}


}

	//~ /**	Instanciate an object viewer.
	//~ */
	//~ public JPanel select (Object object) {
		//~ lastSelection = null;	// changed in getViewer () if some selection is found
		
		//~ if (object == null) {
			//~ display ("case 1. Null object");
			//~ return null;	// case 1. Null object
		//~ }
		
		//~ // Retrieve current ObjectViewer (in combo) className
		//~ String ovName = (String) combo.getSelectedItem ();
		//~ String className  = (String) spatializerToClassName.get (ovName);
		
		//~ // fc - 2.2.2004 - If no OV is selected in combo, prompt user.
		//~ //
		//~ if (className == null) {
			//~ MessageDialog.promptInfo (Translator.swap ("SpatializerChooser.pleaseChooseSelectionMode"));
			//~ return null;
		//~ }
		
		//~ if (object instanceof Collection) {
			//~ Collection collection = (Collection) object;
			//~ if (collection.isEmpty ()) {
				//~ display ("case 2. Empty collection");
				//~ return null;	// case 2. Empty collection
			//~ }
			
			//~ // Find representative objects (ie with different classes)
			//~ Collection reps = Tools.getRepresentatives (collection);
			
			//~ if (reps.size () == 1) {	// case 3. Homogeneous collection (all elts of same types)
				//~ // case 3.1 Current combo accepts this collection directly
				//~ if (extMan.isCompatible (className, collection)) {
					//~ display ("case 3.1 Current combo accepts this collection directly");
					//~ return getViewer (className, collection);
				
				//~ // case 3.2 Try with collection's first element
				//~ } else {
					//~ Object firstObject = collection.iterator ().next ();
					//~ if (extMan.isCompatible (className, firstObject)) {
						//~ display ("case 3.2 Try with collection's first element");
						//~ return getViewer (className, firstObject);
					//~ } else {
						//~ display ("case 3.3 First element of homogeneous collection don't match");
						//~ return null;	// case 3.3 First element of homogeneous collection don't match
					//~ }
				//~ }
			//~ } else {	// case 4. Messy collection (elts of different types, ex: trees and cells)
				
				//~ // case 4.0 This messy collection is accepted directly - fc - 22.9.2005
				//~ if (extMan.isCompatible (className, reps)) {
					//~ display ("case 4.0 Current combo accepts this messy collection directly");
					//~ return getViewer (className, collection);
				//~ }
				
				//~ // Classify items per class -> several homogeneous collections
				//~ Map klass2Items = new HashMap ();
				//~ for (Iterator i = collection.iterator (); i.hasNext ();) {
					//~ Object item = i.next ();
					//~ Collection items = null;
					//~ try {
						//~ items = (Collection) klass2Items.get (item.getClass ());
						//~ if (items == null) {throw new Exception ();}	// fc - 20.11.2003
					//~ } catch (Exception e) {
						//~ items = new ArrayList ();
						//~ klass2Items.put (item.getClass (), items);
					//~ }
					//~ items.add (item);
				//~ }
				//~ // Evaluate the homogeneous sub collections, consider the first matching one
				//~ for (Iterator k = klass2Items.values ().iterator (); k.hasNext ();) {
					//~ Collection co = (Collection) k.next ();
					//~ JPanel p = select (co);	// recursive call, may match case 3.1 or case 3.2
					//~ if (p != null) {
						//~ display ("4.1 One homogeneous sub collection match");
						//~ return p;	// 4.1 One homogeneous sub collection match
					//~ }
				//~ }
				
				//~ display ("4.2 No answer");
				//~ return null;	// 4.2 No answer
			//~ }
			
		//~ } else {
			//~ // case 5.1 Single object match
			//~ if (extMan.isCompatible (className, object)) {
				//~ return getViewer (className, object);
			
			//~ // case 5.2 No answer
			//~ } else {
				//~ return null;
			//~ }
		//~ }
    //~ }
	
