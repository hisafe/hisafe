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

import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Memorizer;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * A Project Memorizer to save space: memorizes according to a frequency.
 * 
 * @author F. de Coligny - October 2002, October 2010
 */
public class FrequencyMemorizer implements Memorizer {

	static {
		Translator.addBundle("capsis.extension.memorizer.FrequencyMemorizer");
	}

	private int frequency;

	// fc+xm-15.3.2019 if true, 2 steps are memorized each time: the step
	// matching the frequency and the step before
	private boolean alsoMemoEachStepBefore;
	
	private int n;

	/**
	 * Default constructor.
	 */
	public FrequencyMemorizer() {
		this (5); // default value
	}

	/**
	 * Constructor with a frequency.
	 */
	public FrequencyMemorizer(int frequency) {
		this.frequency = frequency;
		this.alsoMemoEachStepBefore = false; // default is false
	}

	/**
	 * Constructor with a frequency and the alsoMemoEachStepBefore boolean.
	 */
	public FrequencyMemorizer(int frequency, boolean alsoMemoEachStepBefore) {
		this.frequency = frequency;
		this.alsoMemoEachStepBefore = alsoMemoEachStepBefore;
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "FrequencyMemorizer.matchWith ()",
					"Exception in matchWith () method, returned false", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("FrequencyMemorizer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FrequencyMemorizer.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	public int getFrequency() {
		return frequency;
	}

	public void setFrequency(int frequency) {
		this.frequency = frequency;

		// fc-15.3.2019
		reinit();
		// n = 0;
	}

	public void setAlsoMemoEachStepBefore(boolean alsoMemoEachStepBefore) {
		this.alsoMemoEachStepBefore = alsoMemoEachStepBefore;
		reinit();
	}

	public boolean isAlsoMemoEachStepBefore() {
		return alsoMemoEachStepBefore;
	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public void reinit() {
		n = 0;
	}

	/**
	 * Memorizer interface. Memorization strategy: memorizes one step every n.
	 */
	@Override
	public void memorize(Project project, Step fatherStep, Step newStep) {

		boolean weHaveAnotherReasonToMemorize = false;

		// fc-15.3.2019
		if (alsoMemoEachStepBefore)
			// We are on the step before if next time we will be on the step
			// matching the frequency
			weHaveAnotherReasonToMemorize = ((n + 1) % frequency) == 0;

		if (fatherStep == null || (n % frequency) == 0 || weHaveAnotherReasonToMemorize) {
			project.tieStep(fatherStep, newStep);

		} else {
			project.replaceStep(fatherStep, newStep);

		}
		n++;

	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public String getCaption() {
		String caption = getName() + " (f : " + frequency;

		if (alsoMemoEachStepBefore)
			caption += " " + Translator.swap("FrequencyMemorizer.alsoMemoEachStepBefore");
		
		caption += ")";
				
		return caption; // fc-15.3.2019

		// return Translator.swap("FrequencyMemorizer") + " f=" + frequency;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public String toString() {
		return getCaption(); // fc-15.3.2019
		// return "FrequencyMemorizer f=" + frequency;
	}

}
