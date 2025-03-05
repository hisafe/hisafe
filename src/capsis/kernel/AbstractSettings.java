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

package capsis.kernel;

/**
 * AbstractSettings is a Serializable superclass for all the model settings.
 * 
 * @author F. de Coligny, S. Dufour - December 2008
 */
public abstract class AbstractSettings implements InitialParameters {

	// fc-15.10.2020 in Capsis modules architecture 2.0, initialParameters implement
	// InitialParameters
	// fc-15.10.2020 in Capsis modules architecture 1.0, settings extend
	// AbstractSettings
	// fc-15.10.2020 For other tools, settings now extend AbstractCloenable

	// fc-14.10.2020 Review, trying to get rid of AbstractSettings in Capsis modules
	// architecture 2.0 -> now implements InitialParameters to replace
	// AbstractSettings by InitialParameters in GModel. InitialParameters methods
	// can not be called in AbstractSettings subclasses which do not override them.

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	// fc-14.10.2020 Removed, was needed for tools settings, moved to
	// AbstractCloneable
//	public Object clone() {
//		Object o = null;
//		try {
//			o = super.clone();
//		} catch (Exception exc) {
//		}
//		return o;
//	}

	/**
	 * Implement InitialParameters.buildInitScene() only to use the
	 * InitialParameters type in GModel and get rid of AbstractSettings in Capsis
	 * modules architecture 2.0 (where buildInitScene () is redefined). This method
	 * must not be called in Capsis modules architecture 1.0.
	 */
	@Override
	public void buildInitScene(GModel model) throws Exception {
		// fc-14.10.2020 buildInitScene () can not be called in Capsis modules
		// architecture 1.0 relying on AbstractSettings
		throw new IllegalAccessError(
				"AbstractSettings error: Can not call buildInitScene () (Capsis modules architecture 2.0) on AbstractSettings (Capsis modules architecture 1.0)"
						+ ", see documentation in AbstractSettings");
	}

	/**
	 * Implement InitialParameters.getInitScene() only to use the InitialParameters
	 * type in GModel and get rid of AbstractSettings in Capsis modules architecture
	 * 2.0 (where getInitScene () is redefined). This method must not be called in
	 * Capsis modules architecture 1.0.
	 */
	@Override
	public GScene getInitScene() {
		// fc-14.10.2020 getInitScene () can not be called in Capsis modules
		// architecture 1.0 relying on AbstractSettings
		throw new IllegalAccessError(
				"AbstractSettings error: Can not call getInitScene () (Capsis modules architecture 2.0) on AbstractSettings (Capsis modules architecture 1.0)"
						+ ", see documentation in AbstractSettings");
	}

}

// fc-14.10.2020 Former version
//public abstract class AbstractSettings implements Serializable, Cloneable {
//
//	// nb-27.08.2018
//	// private static final long serialVersionUID = 1L;
//
//	public Object clone() {
//		Object o = null;
//		try {
//			o = super.clone();
//		} catch (Exception exc) {
//		}
//		return o;
//	}
//
//}
