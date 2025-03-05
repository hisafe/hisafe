/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
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
package capsis.commongui;

import java.lang.reflect.Constructor;

import jeeb.lib.sketch.kernel.SketchLinkable;
import jeeb.lib.sketch.kernel.SketchLinker;
import jeeb.lib.util.ClassUtils;
import jeeb.lib.util.Log;
import jeeb.lib.util.autoui.validators.DialogCloserValidator;
import jeeb.lib.util.autoui.validators.ObjectValidator;
import capsis.kernel.AbstractSettings;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Relay;
import capsis.kernel.Step;

/**
 * Default Relay for GUI
 * 
 * @author sdufour - december 2009
 */
public class DefaultRelay extends Relay implements SketchLinkable {

	// fc - june 2010 - it would be nice that the linker and other stuff
	// related to Sketch would not be saved with the projects -> added transient
	// below
	transient private SketchLinker sketchLinker;

	/**
	 * Constructor
	 */
	public DefaultRelay (GModel m) {
		super (m);

	}

	/**
	 * Return dialog to set initial parameters Used by default package.gui.PrefixInitialDialog
	 */
	@Override
	public InitialParameters getInitialParameters () throws Exception {

		InitialParameters ret = null;

		String className = model.getInitialDialogClassName ();

		// Load the initial dialog
		InitialDialog dlg = null;
		try {
			Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
			Constructor<?> ctr = c.getConstructor (new Class[] {GModel.class});
			dlg = (InitialDialog) ctr.newInstance (new Object[] {model});

		} catch (ClassNotFoundException exc) {

			// The initial dialog could not be found, try an auto dialog
			try {
				return autoInitialParameters ();

			} catch (Exception e2) {
				throw new Exception ("DefaultRelay could neither open an initial dialog: "
						+ className + "(" + exc + ") nor an autodialog", e2);
			}

		} catch (Exception exc) {
			Log.println (Log.ERROR, " ()", "Error while creating the initial dialog: " + className, exc);
			throw exc;
		}

		if (dlg.isValidDialog ()) {
			ret = dlg.getInitialParameters ();
			if (ret == null) { throw new Exception ("cannot get initial parameters"); }
		}

		dlg.dispose ();

		return ret;
	}

	/**
	 * Return dialog to set evolution parameters Used by default package.gui.PrefixEvolutionDialog
	 */
	@Override
	public EvolutionParameters getEvolutionParameters (Step stp) throws Exception {

		EvolutionParameters ret = null;

		String className = model.getEvolutionDialogClassName ();

		EvolutionDialog dlg = null;
		try {
			Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
			Constructor<?> ctr = c.getConstructor (new Class[] {Step.class});
			dlg = (EvolutionDialog) ctr.newInstance (new Object[] {stp});

		} catch (ClassNotFoundException exc) {

			// Use Auto Dialog
			className = model.getEvolutionParametersClassName ();
			return autoEvolutionParameters (className, stp);

		} catch (Exception exc) {
			Log.println (Log.ERROR, " ()", "Error while creating evolution dialog: " + className, exc);
			throw exc;
		}

		if (dlg.isValidDialog ()) {
			ret = dlg.getEvolutionParameters ();
			if (ret == null) { throw new Exception ("cannot get evolution parameters"); }

		}

		dlg.dispose ();

		return ret;
	}

	/**
	 * Open Initital Parameters Autodialog
	 */
	protected InitialParameters autoInitialParameters () throws Exception {

		final InitialParameters param;
		InitialParameters ip;

		// try to use settings class
		try {
			ip = (InitialParameters) model.getSettings ();
		} catch (ClassCastException e) {
			ip = null;
		}

		// create initial parameter class if necessary
		if (ip == null) {
			String className = model.getInitialParametersClassName ();
			try {
				// Use Auto Dialog
				Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
				param = (InitialParameters) ClassUtils.instantiateClass (c);

			} catch (Exception exc) {
				Log.println (Log.ERROR, " ()", "Error while creating dialog: " + className, exc);
				throw exc;
			}
		} else {
			param = ip;
		}

		// Build auto dialog with validator
		final AutoDialog<InitialParameters> dlg = new AutoDialog<InitialParameters> (param, true);

		dlg.setValidator (new CloserValidator (model, dlg, param));

		// // Build auto dialog with validator
		// AutoDialog<InitialParameters> dlg = new
		// AutoDialog<InitialParameters>(param,
		// new ObjectValidator() {
		//
		// @Override
		// public void validate() throws Exception {
		// param.buildInitScene(model);
		// }
		//
		// }, true);

		dlg.setVisible (true);
		if (!dlg.isValidDialog ()) {
			dlg.dispose ();
			return null;

		}

		dlg.dispose ();

		// set class settings if possible
		if (param instanceof AbstractSettings) {
			model.setSettings ((AbstractSettings) param);
		}

		return param;
	}


	/**
	 * This validator will run buildInitScene in a task and close the dialog with setValidDialog
	 * (true) if successful
	 * 
	 * @author F. de Coligny - December 2011
	 */
	static private class CloserValidator implements ObjectValidator, DialogCloserValidator {

		private GModel model;
		private InitialDialogInterface initialDialog;
		private InitialParameters initialParameters;

		public CloserValidator (GModel model, InitialDialogInterface initialDialog,
				InitialParameters initialParameters) {
			this.model = model;
			this.initialDialog = initialDialog;
			this.initialParameters = initialParameters;
		}

		@Override
		public void validate () throws Exception {
			// param.buildInitScene(model);

			// Run buildInitScene in a task, closes the dialog at the end with
			// setValidDialog (true) if ok
			InitialDialog.buildInitScene (model, initialDialog, initialParameters);

		}

	}

	/**
	 * Open Evolution Parameters AutoDialog
	 * 
	 * @param className
	 */
	protected EvolutionParameters autoEvolutionParameters (String className, Step stp)
			throws Exception {

		EvolutionParameters param;
		// Create class

		// First try to get constructor with a step
		try {
			Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
			Constructor<?> ctr = c.getConstructor (new Class[] {Step.class});
			param = (EvolutionParameters) ctr.newInstance (new Object[] {stp});

		} catch (NoSuchMethodException exc) {

			// Then try default constructor
			try {
				Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
				param = (EvolutionParameters) ClassUtils.instantiateClass (c);;

			} catch (Exception e) {
				Log.println (Log.ERROR, " ()", "Error while creating evolution parameters: "
						+ className, e);
				throw exc;
			}
		}

		// Auto Dialog
		AutoDialog<EvolutionParameters> dlg = new AutoDialog<EvolutionParameters> (param, true);
		dlg.setVisible (true);
		if (!dlg.isValidDialog ()) {
			param = null;
		}

		dlg.dispose ();

		return param;
	}

	/**
	 * Return sketch linker instance Used by default package.sketch.PrefixSketchLinker May return
	 * null if the module is not sketch compatible
	 */
	@Override
	public SketchLinker getSketchLinker () {

		if (sketchLinker != null) { return sketchLinker; }

		String modelPackage = model.getIdCard ().getModelPackageName ();
		String modelPrefix = model.getIdCard ().getModelPrefix ();

		String className = modelPackage + ".sketch." + modelPrefix + "SketchLinker";

		sketchLinker = null;
		try {
			Class<?> c = model.getClass ().getClassLoader ().loadClass (className);
			Constructor<?> ctr = c.getConstructor (new Class[] {GModel.class});
			sketchLinker = (SketchLinker) ctr.newInstance (new Object[] {model});

		} catch (ClassNotFoundException exc) {
			// nothing: sketch linkers are optional
		} catch (Exception exc) {
			Log.println (Log.ERROR, " ()", "Error while creating sketch linker: " + className, exc);

		}
		return sketchLinker;

	}

}
