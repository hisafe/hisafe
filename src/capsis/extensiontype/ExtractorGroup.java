/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2013 INRA
 * 
 * Author: F. de Coligny
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
 */
package capsis.extensiontype;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jeeb.lib.defaulttype.Extension;
import jeeb.lib.util.Alert;
import jeeb.lib.util.Translator;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.ButtonColorerListener;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * A group of data extractors in Capsis.
 * 
 * @author F. de Coligny - September 2013
 */
public abstract class ExtractorGroup implements ButtonColorerListener, Extension {

	static {
		Translator.addBundle ("capsis.extensiontype.ExtractorGroup");
	}

//	SHOULD BE REMOVED, but makes bugs: this superclass should not be part of the extractorgroup
//	list when asked to the CapsisExtensionManager fc-14.10.2013
//	public static final String NAME = "ExtractorGroup";
//	public static final String VERSION = "1.0";
//	public static final String AUTHOR = "F. de Coligny";
//	public static final String DESCRIPTION = "ExtractorGroup.description";
//	SHOULD BE REMOVED
	
	protected Step step;
	protected List<DataBlock> blocks;

	/**
	 * Constructor. 
	 */
	public ExtractorGroup () {}

	/**
	 * Returns the list of extractors this group contains.
	 * To be overriden in subclasses.
	 */
	abstract public List<String> getExtractorClassNames ();
	
	/**
	 * Inits the extractorGroup on the given step (creates the extractors, they render themselves
	 * through their dataBlock).
	 */
	public void init (Step step) {
		blocks = new ArrayList<DataBlock> ();
		this.step = step;
		GModel model = step.getProject ().getModel ();
		boolean foundOne = false;

		for (String className : getExtractorClassNames ()) {
		
			if (!CapsisExtensionManager.matchWith (className, model)) continue;

			foundOne = true;

			DataBlock db = new DataBlock (className, step);
			blocks.add (db);

		}
		if (!foundOne) {
			Alert.print (Translator.swap ("ExtractorGroup.noExtractorFoundInTheGroupIsCompatibleWithTheCurrentModel"));
			return;
		}

		// Register to the ButtonColorer
		ButtonColorer.getInstance ().addListener (this);

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		// ExtractorGroup is a superclass, never compatible
		return false; // needed
	}
	
	// fc-19.8.2018 this is an abstract class for extensions, no name...
//	@Override
//	public String getName() {
//		return Translator.swap("ExtractorGroup.name");
//	}
//	
//	@Override
//	public String getAuthor() {
//		return "F. de Coligny";
//	}
//	
//	@Override
//	public String getDescription() {
//		return Translator.swap("ExtractorGroup.description");
//	}
//	
//	@Override
//	public String getVersion() {
//		return "1.0";
//	}
	
	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks if the extension can
	 * deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (List<String> extractorClassNames, Object referent) {
		List<String> matching = getMatchingExtractorClassNames (extractorClassNames, referent);
		return !matching.isEmpty ();
	}

	static private List<String> getMatchingExtractorClassNames (List<String> extractorClassNames, Object referent) {
		List<String> copy = new ArrayList<String> (extractorClassNames); // a copy
		for (Iterator<String> i = copy.iterator (); i.hasNext ();) {
			String className = i.next ();

			boolean match = CapsisExtensionManager.matchWith (className, referent);

			if (!match) i.remove ();

		}
		return copy;
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public void colorMoved (StepButton previousButton, StepButton newButton) {
		for (DataBlock db : blocks) {
			db.colorMoved (previousButton, newButton);
		}
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public void colorRemoved (StepButton stepButton) {
		for (DataBlock db : blocks) {
			db.colorRemoved (stepButton);
		}
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public boolean isListening(StepButton sb) { // fc-2.1.2017
		
		if (sb == null)
			return false; // nonsense
		
		for (DataBlock db : blocks) {
			if (db.isListening (sb))
				return true; // someone listens
		}
		
		// noone listens
		return false;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
