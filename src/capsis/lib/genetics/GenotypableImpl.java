/*
 * The Genetics library for Capsis4
 *
 * Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
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
package capsis.lib.genetics;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.Map;

import capsis.kernel.Step;
import capsis.lib.genetics.memory.GeneticMemory;
import jeeb.lib.util.Log;

/**
 * GenotypableImpl is an implementation help for Genotypable objects. It can be
 * used by GeneticTree or others individuals / cohorts which manage a genotype
 * as planned in the Genetics library.
 *
 * @author F. de Coligny - November 2004
 */
public class GenotypableImpl implements Serializable {

	public Genotype genotype;
	public int mId;
	public int pId;
	public int creationDate;
	public double consanguinity;
	public double globalConsanguinity;
	public Map genoValue;
	public Map fixedEnvironmentalValue;

	/**
	 * Constructor. Use init () just after.
	 */
	public GenotypableImpl() {
	}

	/**
	 * Initialize the genotypable implementation. To be called after constructor
	 * new GenotypableImpl (). Must be used for each genotypable in the initial
	 * scene of the simulation. When simulation is run for other steps, used
	 * update ().
	 */
	public void init(Genotypable gee, Genotype genotype, int mId, int pId, int creationDate, double consanguinity,
			double globalConsanguinity, Map genoValue, Map fixedEnvironmentalValue) throws Exception {

		if (gee == null) {
			String m = "Genotypable must not be null at this time";
			Log.println(Log.ERROR, "GenotypableImpl.init ()", m);
			throw new Exception(m);
		}

		if (gee.getGeneticScene() == null) { // fc - 16.11.2004
			String m = "geeId=" + gee.getId() + " Genotypable.getScene () must not be null at this time";
			Log.println(Log.ERROR, "GenotypableImpl.init ()", m);
			throw new Exception(m);
		}

		if (!gee.getGeneticScene().isInitialScene() && genotype != null
				&& genotype.equals(Genotype.EMPTY_MULTI_GENOTYPE)) {
			String m = "geeId=" + gee.getId() + " Can't create empty MultiGenotype during simulation";
			Log.println(Log.ERROR, "GenotypableImpl.init ()", m);
			throw new Exception(m);
		}

		this.mId = mId;
		this.pId = pId;
		this.creationDate = creationDate;
		this.consanguinity = consanguinity;
		this.globalConsanguinity = globalConsanguinity;
		this.genoValue = genoValue;
		this.fixedEnvironmentalValue = fixedEnvironmentalValue;

		if (genotype == null) {
			this.genotype = null;
			gee.setMultiGenotype(null);
		} else if (genotype.equals(Genotype.EMPTY_INDIVIDUAL_GENOTYPE)) {
			this.genotype = Genotype.EMPTY_INDIVIDUAL_GENOTYPE;
			gee.setMultiGenotype(null);
		} else if (genotype.equals(Genotype.EMPTY_MULTI_GENOTYPE)) {
			this.genotype = null;
			gee.setMultiGenotype(Genotype.EMPTY_MULTI_GENOTYPE);
		} else if (genotype instanceof IndividualGenotype) {
			this.genotype = genotype;
			gee.setMultiGenotype(null);
		} else if (genotype instanceof MultiGenotype) {
			this.genotype = null;
			gee.setMultiGenotype((MultiGenotype) genotype);
		} else {
			String m = "geeId=" + gee.getId() + " Wrong genotype";
			Log.println(Log.ERROR, "GenotypableImpl.init ()", m);
			throw new Exception(m);
		}

		gee.setPhenoValues(null);
	}

	/**
	 * Used when a new occurrence of a genotypable with same id is built.
	 * Example: build tree 12 at simulation begin time year 2000 with init ()
	 * build tree 12 at years 2001, 2002 and 2003 with update ().
	 */
	public void update(Genotypable newGee) {

		int id = newGee.getId();
		Step previousStep = GeneticTools.lastKnownStep;
		GeneticScene previousScene = (GeneticScene) previousStep.getScene();
		Genotypable previousGee = previousScene.getGenotypable(id);
		if (previousGee.getMultiGenotype() == null) {
			newGee.setMultiGenotype(null);
		} else {
			newGee.setMultiGenotype(Genotype.EMPTY_MULTI_GENOTYPE);
		}

		newGee.setPhenoValues(null);
	}

	/**
	 * Tell if a genotypable is genotyped.
	 */
	public boolean isGenotyped(Genotypable gee) {
		if (gee.getGenoSpecies().getAlleleDiversity() != null) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Tell if a genotypable is multiGenotyped.
	 */
	public boolean isMultiGenotyped(Genotypable gee) {
		return (gee.getMultiGenotype() != null);
	} // fc & phd - 14.3.2003

	// --------------------------------------------------------------------------------------------------------->
	// A C C E S S O R S
	// --------------------------------------------------------------------------------------------------------->

	public int getMId() {
		return mId;
	}

	public int getPId() {
		return pId;
	}

	public int getCreationDate() {
		return creationDate;
	}

	public AlleleParameters getAlleleParameters(Genotypable gee) {
		if (genotype instanceof IndividualGenotype) {
			return gee.getGenoSpecies().getGeneticMap();
		} else {
			return gee.getGenoSpecies().getAlleleDiversity();
		}
	}

	// --------------------------------------------------------------------------------------------------------->
	// G E N O T Y P E C O M P U T A T I O N
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * If genotype != null, return it, else calculate it from the parents,
	 * getGametes (getGeneticMap ()) and fusesGametes () if genotype is an
	 * IndividualGenotype, or from actualizeMultiGenotype () if genotype is a
	 * MutliGenotype, then return it.
	 */
	public Genotype getGenotype(Genotypable gee) {

		// Return null if the genotypable is not genotyped
		if (genotype == null && gee.getMultiGenotype() == null) {
			return null;

			// Compute genotype if genotype is equal to
			// EMPTY_INDIVIDUAL_GENOTYPE
		} else if (gee.getMultiGenotype() == null && genotype.equals(Genotype.EMPTY_INDIVIDUAL_GENOTYPE)) {
			GeneticScene s = gee.getGeneticScene();
			try {
				if (s.isInitialScene())
					return genotype;

				Genotypable mTree = s.getGeneticMemory().searchGee(s, mId);
				Genotypable pTree = s.getGeneticMemory().searchGee(s, pId);

				Genotype mGenotype = mTree.getGenotype();
				Genotype pGenotype = pTree.getGenotype();

				IndividualGenotype mGamete = mGenotype.getGamete(mTree.getAlleleParameters());
				IndividualGenotype pGamete = pGenotype.getGamete(pTree.getAlleleParameters());

				IndividualGenotype g = Genotype.fuseGametes(mGamete, pGamete);
				genotype = (Genotype) g;
				return genotype;

			} catch (Exception e) { // fc-12.10.2007 added try catch
				String m = "geeId=" + gee.getId() + " Cannot get genotype for genotypable in scene " + s.getDate()
						+ " with parents: mId=" + mId + " pId=" + pId;
				Log.println(Log.ERROR, "GenotypableImpl.getGenotype ()", m, e);
				return null;
			}

			// Compute multiGenotype if multiGenotype is equal to
			// EMPTY_MULTI_GENOTYPE
		} else if (genotype == null && gee.getMultiGenotype().equals(Genotype.EMPTY_MULTI_GENOTYPE)) {
			GeneticScene s = gee.getGeneticScene();
			if (s.isInitialScene()) {
				return gee.getMultiGenotype();
			}

			try {
				MultiGenotype mg = GeneticTools.actualizeMultiGenotype(gee);
				return mg;
			} catch (Exception e) {
				String m = "geeId=" + gee.getId() + " Exception during multiGenotype actualization";
				Log.println(Log.ERROR, "GenotypableImpl.getGenotype ()", m, e);
				return null;
			}

			// In other cases, genotype was already calculated
		} else if (genotype != null && !genotype.equals(Genotype.EMPTY_INDIVIDUAL_GENOTYPE)
				&& gee.getMultiGenotype() == null) {
			return genotype;

		} else if (gee.getMultiGenotype() != null && !gee.getMultiGenotype().equals(Genotype.EMPTY_MULTI_GENOTYPE)
				&& genotype == null) {
			return gee.getMultiGenotype();

		} else {
			// In case of trouble, return null
			return null;
		}
	}

	// --------------------------------------------------------------------------------------------------------->
	// C O N S A N G U I N I T Y
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * Return consanguinity of a tree. If tree is a mean tree, consanguinity is
	 * equal to mean individual consanguinity.
	 */
	public double getConsanguinity(Genotypable gee) {
		// System.out.println ("genotypable "+this.getId ());
		GeneticScene scene = gee.getGeneticScene();

		Step step = scene.getStep();
		if (step == null)
			step = GeneticTools.lastKnownStep;

//		Step initialStep = (Step) step.getProject().getRoot();
//		GeneticScene initialScene = (GeneticScene) initialStep.getScene();

		// fc+som-27.6.2017
		GeneticMemory geneticMemory = scene.getGeneticMemory();

		if (!gee.isGenotyped() || consanguinity != -1 ||

				geneticMemory.initialSceneContains(gee.getId())
//				initialScene.getGenotypable(gee.getId()) != null

				|| gee.getMultiGenotype() != null || gee.getGenoSpecies().getKinship() == null) {

			// pichot
			// System.out.println (consanguinity);
			// pichot

			return consanguinity;

		} else {

			GeneticScene s = gee.getGeneticScene();

			// System.out.println
			// ("genotypable "+gee.getId()+" mid="+this.getMId ()+
			// " pid="+gee.getPId ());

			consanguinity = GeneticTools.phi(s, mId, pId, creationDate);

			// pichot
			// System.out.println ("cas2");
			// pichot

			return consanguinity;
		}
	}

	public void setConsanguinity(double f) {
		consanguinity = f;
	}

	/**
	 * Return global consanguinity of a genotypable. If gee is an individual
	 * genotypable, global consanguinity is equal to consanguinity. If gee is a
	 * mean genotypable, global consanguinity is the probability that two genes
	 * randomly drawn in MultiGenotype are equal.
	 */
	public double getGlobalConsanguinity(Genotypable gee) {
		if (!gee.isGenotyped() || gee.getGenoSpecies().getKinship() == null) {
			return -1;
		} else if (gee.getMultiGenotype() != null) {
			return globalConsanguinity;
		} else {
			return gee.getConsanguinity();
		}
	}

	public void setGlobalConsanguinity(double f) {
		globalConsanguinity = f;
	}

	// --------------------------------------------------------------------------------------------------------->
	// T O O L S T O C O M P U T E Q U A N T I T A T I V E G E N E T I C P A R A
	// M E T E R
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * If genoValue != null, return it, else calculate it from parameterName,
	 * AlleleEffect.getGeneticValue (), then return it.
	 */
	public double getGeneticValue(Genotypable gee, String caractereName) {
		Map gv = genoValue;
		if (gv != null) {
			Double g = (Double) gv.get(caractereName);
			if (g != null) {
				return g.doubleValue();
			}
		}

		double g = gee.getGenoSpecies().getAlleleEffect().getGeneticValue(caractereName, gee);
		if (gv == null) {
			gv = new Hashtable();
			genoValue = gv;
		}
		gv.put(caractereName, new Double(g));
		return g;
	}

	/**
	 * If initialEnvironmentalValue != null, return it, else calculate it from
	 * parameterName, AlleleEffect.getEnvironmentalVariance (), then return it.
	 */
	public double getFixedEnvironmentalValue(Genotypable gee, String parameterName) {
		Map iev = fixedEnvironmentalValue;
		if (iev != null) {
			Double p = (Double) iev.get(parameterName);
			if (p != null) {
				return p.doubleValue();
			}
		}

		double p = gee.getGenoSpecies().getAlleleEffect().getFixedEnvironmentalValue(parameterName);
		if (iev == null) {
			iev = new Hashtable();
			fixedEnvironmentalValue = iev;
		}
		iev.put(parameterName, new Double(p));
		return p;
	}

	/**
	 * If phenoValue != null, return it, else calculate it from parameterName,
	 * AlleleEffect.getPhenoValue (), then return it.
	 */
	public double getPhenoValue(Genotypable gee, String caractereName) {
		Map pv = gee.getPhenoValues();
		if (pv != null) {
			Double p = (Double) pv.get(caractereName);
			if (p != null) {
				return p.doubleValue();
			}
		}

		double p = gee.getGenoSpecies().getAlleleEffect().getPhenoValue(caractereName, gee);
		if (pv == null) {
			pv = new Hashtable();
			gee.setPhenoValues(pv);
		}
		pv.put(caractereName, new Double(p));
		return p;
	}

	/**
	 * Version of the genetics library.
	 */
	// fc-5.11.2004 genetics version can be checked in an inspector
	// fc-30.5.2017 changed version to 2.1
	public String getGeneticsVersion() {
		return "2.1";
	}

}
