/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.memorizer;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Memorizer;

/**
 * A Project Memorizer for complete memorization: all steps are memorized.
 * @author F. de Coligny - October 2002, October 2010
 */
public class DefaultMemorizer implements Memorizer {

	static {
		Translator.addBundle("capsis.extension.memorizer.DefaultMemorizer");
	}

	/**
	 * Default constructor.
	 */
	public DefaultMemorizer() {
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the extension can deal (i.e.
	 * is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) { return false; }
		}
		catch (Exception e) {
			Log.println(Log.ERROR, "DefaultMemorizer.matchWith ()", "Exception in matchWith () method, returned false", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("DefaultMemorizer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DefaultMemorizer.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}
	
	/**
	 * Memorizer interface.
	 */
	@Override
	public void reinit() {
	};

	/**
	 * Memorizer interface. Default memorization strategy: memorize every step.
	 */
	@Override
	public void memorize(Project s, Step fatherStep, Step newStep) {
		s.tieStep(fatherStep, newStep);
	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public String getCaption() {
		return getName();
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public String toString() {
		return "DefaultMemorizer";
	}
}
