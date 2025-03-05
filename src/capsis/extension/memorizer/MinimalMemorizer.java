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

import jeeb.lib.util.Translator;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Memorizer;

/**
 * A Project Memorizer to save space: memorizes the minimum possible, less than
 * the compact memorizer, does not keep the root step. Tries to keep only the
 * last simulated step in memory. During model evolution, two steps may be kept
 * in memory until the new one replaces the previous one. Built for Phenofit4
 * big simulations.
 * 
 * @author F. de Coligny, Daphne Asse - March 2018
 */
public class MinimalMemorizer implements Memorizer {

	static {
		Translator.addBundle("capsis.extension.memorizer.MinimalMemorizer");
	}

	/**
	 * Default constructor.
	 */
	public MinimalMemorizer() {
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {

		// fc-4.4.2018
		// Minimal memorizer is a good idea: keep only last calculated step in
		// memory (at most 2 steps in memory at the time of memorizing the
		// second).
		// But this would mean a better management of the links to the root step
		// in capsis kernel and in the modules.
		// In particular, InitialParameters have a ref to the initScene built by
		// buildInitScene (), it should be changed each time a new Step replaces
		// the root.
		// -> disabled by making matchWith () returning false
		return false;

		// return referent instanceof GModel; // normal return value

	}

	@Override
	public String getName() {
		return getName (); // fc-15.3.2019
//		return Translator.swap("MinimalMemorizer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny, Daphne Asse";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MinimalMemorizer.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public void reinit() {
	};

	/**
	 * Memorizer interface. Memorization strategy: if father has no son replace
	 * father by new step, else tie new step behind father.
	 */
	@Override
	public void memorize(Project s, Step fatherStep, Step newStep) {

		// fc+da-30.3.2018 newStep replaces root step
		s.tieStep(null, newStep);

		// if (fatherStep == null) { // newStep is a root step -> tie it
		// s.tieStep(fatherStep, newStep);
		//
		// } else { // normal case only for visible steps -> if father is leaf
		// // replace it else tie new step behind
		//
		// // keep last tied step
		// if (fatherStep.isLeaf() && !fatherStep.isRoot()) {
		// s.replaceStep(fatherStep, newStep);
		//
		// } else {
		// s.tieStep(fatherStep, newStep);
		// }
		// }

	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public String getCaption() {
		return Translator.swap("MinimalMemorizer");
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public String toString() {
		return "MinimalMemorizer";
	}

}
