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

package capsis.kernel.extensiontype;

import java.util.Collection;

import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import jeeb.lib.defaulttype.Extension;

/**
 * Intervener Interface.
 * 
 * @author F. de Coligny - November 2000
 */
public interface Intervener extends Extension {

	// fc-14.10.2020 Review. There was a grouper optional param in init(), was
	// moved, see AbstractGroupableIntervener

	public static final int CUT = 0;
	public static final int MARK = 1;

	/**
	 * Initializes the intervener. It is given the model object of the current
	 * project, the step on which it was called, a copy of the scene under the step,
	 * and an optional collection of elements (may be null). The intervener is
	 * supposed to act in the given scene, e.g. remove trees in the scene. If the
	 * scene contains elements (not sure, stand models do not manage elements), the
	 * given collection may contain elements. If the collection is not null, it is a
	 * sub collection of the elements in the scene (e.g. a group, e.g. Spruce
	 * trees), the action must be restricted to the elements in this collection
	 * (e.g. cut only some Spruce trees).
	 */
	public void init(GModel model, Step step, GScene scene, Collection elements);

	/**
	 * Opens a dialog box to configure interactively the intervener. If the user
	 * validates the dialog, all user inputs must be checked for validity and all
	 * the intervener's parameters must be set in order to then call apply (). Can
	 * be ignored in script mode if all the parameters are set programmatically, for
	 * instance with a dedicated constructor.
	 */
	public boolean initGUI() throws Exception;

	/**
	 * Makes tests on parameters, returns true if apply () can be called. It could
	 * be wrong because of wrong parameters in console mode or cancel in gui mode.
	 */
	public boolean isReadyToApply();

	/**
	 * Makes the actual intervention. Works on the scene copy previously passed to
	 * init (), returns it after alteration 'e.g. with less trees inside).
	 */
	public Object apply() throws Exception;

	/**
	 * Gets the sub-type of the intervener (used to classify them in GUI mode). The
	 * returned String will appear in the GUI and therefore should be translated
	 * with the Translator.swap () method.
	 */
	public String getSubType();

}
