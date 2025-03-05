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

/**	Relay is an abstract superclass for all the models relays.
 *	(e.g. mountain.gui.MountRelay). The apps based on the capsis.kernel
 *	communicate with the models (embedded in modules) through their relay. 
 *
 * @author F. de Coligny - September 2001, September 2010
 */
public abstract class Relay {
	
	/**	The model this relay belongs to */
	protected GModel model;

	
	
	/**	Constructor.
	 */
	protected Relay (GModel m) {
		model = m;
	}

	/**	Accessor to the model.
	 */
	public GModel getModel () {return model;}
	
	/**	Accessor for the InitialParameters object of the model. 
	 * 	This object contains methods to build and get the initial scene 
	 * 	of the Project.
	 */
	public abstract InitialParameters getInitialParameters () throws Exception;
	
	/** Runs optional Initializations in the model just after the initial 
	 * 	scene was built by getInitialParameters ()
	 *  Redirects to the initializeModel () method in the model.
	 */
	public Step	initializeModel (InitialParameters p) throws Exception {
		return model.initializeModel(p);
	}

	/**	Accessor for the EvolutionParameters object of the model. 
	 * 	This object contains the target limit for the next evolution stage
	 * 	to be run. 
	 */
	public abstract EvolutionParameters getEvolutionParameters (Step stp) throws Exception;
	
	/**	Runs the processEvolution () method of the model from the given Step. 
	 * 	The other parameter is the EvolutionParameters object that was just 
	 * 	returned by getEvolutionParameters (). It contains the target limit to
	 * 	be reached (e.g. target year).
	 * 	The processEvolution () method implements the main grow algorithm
	 * 	of the linked model.
	 */
	public Step	processEvolution (Step stp, EvolutionParameters par) throws Exception {
		return model.processEvolution (stp, par);
	}
	
	/**	Runs an optional process on newScene after an intervener was 
	 * 	run on oldScene.
	 * 	E.g. run a radiative balance after a thinning action on a plantation.
	 */
	public void	processPostIntervention	(GScene newScene, GScene oldScene) throws Exception {
		model.processPostIntervention (newScene, oldScene);
	};

}







