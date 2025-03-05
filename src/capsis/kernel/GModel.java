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

package capsis.kernel;

import java.io.Serializable;

/**
 * GModel is an abstract superclass for all the 'model classes' of the modules.
 * This is a key class in the capsis.kernel package.
 * 
 * @author F. de Coligny - June 1999, September 2010
 */
public abstract class GModel implements Serializable {

	// The transient fields are not saved when saving the Project

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/** The idCard contains management information about the module (name...) */
	private IdCard idCard;
	/** The model class is linked to a given project */
	private Project project;

	/**
	 * The set of parameters of the model class (generally an InitialParameters
	 * instance)
	 */
	protected InitialParameters settings; // fc-14.10.2020 Replaced AbstractSettings by InitialParameters
//	protected AbstractSettings settings;

	/**
	 * This object contains methods that can be detected by other tools
	 * (graphics...)
	 */
	transient private MethodProvider methodProvider;
	/** The specific relay for the module and the current pilot */
	transient private Relay relay;
	/** Convenient for the project manager */
	transient private int defaultStepVisibilityFreq = 5;

	/**
	 * Constructor.
	 */
	public GModel() {
	}

	/**
	 * This method must be redefined to return true for models enabling automations.
	 */
	public boolean enablesAutomation() { // fc-28.11.2012 Automations are buggy,
											// disturb projects
											// saving
		return false; // default: no automations
	}

	// Main accessors

	public void init(IdCard idCard) {
		this.idCard = idCard;
	}

	public IdCard getIdCard() {
		return idCard;
	}

	public void setProject(Project s) {
		project = s;
	}

	public Project getProject() {
		return project;
	}

	// fc-14.10.2020 Replaced AbstractSettings by InitialParameters
	public void setSettings(InitialParameters s) {
		settings = s;
	}
//	public void setSettings(AbstractSettings s) {
//		settings = s;
//	}

	// fc-14.10.2020 Replaced AbstractSettings by InitialParameters
	public InitialParameters getSettings() {
		return settings;
	}
//	public AbstractSettings getSettings() {
//		return settings;
//	}

	/**
	 * Returns the MethodProvider for this model
	 */
	public MethodProvider getMethodProvider() {
		if (methodProvider == null) {
			methodProvider = createMethodProvider();
		}
		return methodProvider;
	}

	/**
	 * Create the MethodProvider for this model, must be overriden
	 */
	abstract protected MethodProvider createMethodProvider();

	/**
	 * Set the relay. The current Pilot talks to the model through its relay.
	 */
	public void setRelay(Relay r) {
		relay = r;
	}

	public Relay getRelay() {
		return relay;
	}

	// Main methods

	/**
	 * Initializes the model (Optional). This is called just after the
	 * InitialParameters buildInitScene () method. If some extra inits are needed,
	 * override this method. Must return the Step carrying the initialScene. In case
	 * the initial process would create other steps, may return the last step. E.g.
	 * Run a radiative balance process on the initial scene...
	 */
	// Should become abstract when all the modules have migrated to the new
	// architecture
	public Step initializeModel(InitialParameters p) throws Exception {
		return p.getInitScene().getStep();
	}

	/**
	 * Runs an evolution stage from the given step to the limit in the given
	 * EvolutionParameters object. As the capsis.kernel was built to be the base of
	 * apps that run growth models, this is generally the main method of the model.
	 * This method creates new steps in the project, at least 1 (the last one). Must
	 * return the last step created. E.g. run an evolution of 10 years OR 20
	 * timeSteps OR until basalArea >= 20...
	 */
	// Should become abstract when all the modules have migrated to the new
	// architecture
	public Step processEvolution(Step stp, EvolutionParameters par) throws Exception {
		return null;
	}

	// Optional methods, may be overriden or not

	/**
	 * This is called after an intervention. The newScene is the result of the
	 * intervention (e.g. trees OR branches were cut...). An optional process may be
	 * performed on the newScene. If nothing to be done, do not override it. E.g.
	 * Run a radiative balance process on the newScene...
	 */
	public void processPostIntervention(GScene newStand, GScene oldStand) throws Exception { // fc-7.12.2016...
	}

	// ...CAUTION fc-7.12.2016 added throws Exception to processPostIntervention
	// Case: Heterofor Model needs to be able to stop the simulation if a tree
	// got not energy at all after a process Lighting.
	// And there is a processLighting () in processPostIntervention ()
	// Redefinitions in subclasses may have or not throws Exception

	/**
	 * If false, this means that processEvolution () is not implemented for this
	 * model and that it should not be called. E.g. GenLoader loads an inventory
	 * file for 2D and 3D viewing only, but does not provide a processEvolution ()
	 * method. May be redefined in some modules to return false.
	 */
	public boolean isEvolutionModel() { // fc-7.2.2018 for clarification
		return true;
	}

	/**
	 * In case of an individual based models, if true, tell to just 'mark' the
	 * individuals (e.g. trees) instead of removing them from the scene when they
	 * are dead, cut... To use the mark mechanism, redefine this method and return
	 * true. This feature is used by thinning interveners.
	 */
	public boolean isMarkModel() {
		return false;
	}

	/**
	 * When a project is opened (deserialized) from disk (open project), this method
	 * is called. It can be redefined to make some technical re-initializations in
	 * some connected dynamic link library or shared object (ex : STICS in the Safe
	 * module). Generally unused: default body does nothing.
	 */
	protected void projectJustOpened() {
	}

	/**
	 * Stand information is a String (english) describing the given stand. Was
	 * introduced for Stretch to give the current growth option (Slim / MMR,
	 * Lieberman...). Was not placed in the Stand class because settings (...) would
	 * have been more difficult to find. Usable by the script pilot in the Step
	 * Properties. Can be redefined in subclasses to return custom descriptions.
	 */
	public String getInformation(GScene stand) {
		return "";
	} // nothing by default

	/**
	 * Return the optional Summary class if any (or null if not found).
	 */
	public Class<?> getSummaryClass() {

		String modelPackage = getIdCard().getModelPackageName();
		String modelPrefix = getIdCard().getModelPrefix();
		String className = modelPackage + ".model." + modelPrefix + "Summary";

		try {
			return Class.forName(className);
		} catch (ClassNotFoundException e) {
			return null;
		}
	}

	// Shortcuts to the idCard (not too many...)

	/**
	 * Returns the model package name.
	 */
	public String getPackageName() {
		return idCard.getModelPackageName();
	}

	/**
	 * Returns the model version
	 */
	public String getVersion() {
		return idCard.getModelVersion();
	}

	// New architecture management methods

	/**
	 * Returns the InitialParameters' className for this model.
	 */
	public String getInitialParametersClassName() {

		String modelPackage = getIdCard().getModelPackageName();
		String modelPrefix = getIdCard().getModelPrefix();
		String className = modelPackage + ".model." + modelPrefix + "InitialParameters";
		return className;
	}

	/**
	 * Returns the EvolutionParameters' className for this model.
	 */
	public String getEvolutionParametersClassName() {

		String modelPackage = getIdCard().getModelPackageName();
		String modelPrefix = getIdCard().getModelPrefix();
		String className = modelPackage + ".model." + modelPrefix + "EvolutionParameters";
		return className;
	}

	/**
	 * Returns the gui pilot InitialDialog's className for this model.
	 */
	public String getInitialDialogClassName() {

		String modelPackage = getIdCard().getModelPackageName();
		String modelPrefix = getIdCard().getModelPrefix();
		String className = modelPackage + ".gui." + modelPrefix + "InitialDialog";
		return className;
	}

	/**
	 * Returns the gui pilot EvolutionDialog's className for this model.
	 */
	public String getEvolutionDialogClassName() {

		String modelPackage = getIdCard().getModelPackageName();
		String modelPrefix = getIdCard().getModelPrefix();
		String className = modelPackage + ".gui." + modelPrefix + "EvolutionDialog";
		return className;
	}

	// Other methods

	/**
	 * Default step visibility frequency
	 */
	public int getDefaultStepVisibilityFreq() {
		return defaultStepVisibilityFreq;
	}

	public void setDefaultStepVisibilityFreq(int defaultStepVisibilityFreq) {
		this.defaultStepVisibilityFreq = defaultStepVisibilityFreq;
	}

	/**
	 * This method is useful to clear some members of the class if desired. In the
	 * original GModel is does not clear anything but overrides are allowed in
	 * derived class.
	 */
	public void clear() {
	}

	/**
	 * Return true if the model linked to the project forbids deletion of this step.
	 * Can be overriden in subclasses to forbid step deletion in some cases, user
	 * should be explained by a translated message with Alert.print().
	 */
	public boolean checkIfDeleteStepVeto(Step step) {
		return false; // default: no veto
	}

	/**
	 * Return true if the model linked to the project forbids intervention.
	 * Can be overriden in subclasses to forbid intervention in some cases, user
	 * should be explained by a translated message with Alert.print().
	 */
	public boolean checkIfInterventionVeto(Step step) {
		return false; // default: no veto
	}

}
