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

import capsis.defaulttype.Species;

/**
 * GenoSpecies is an interface for a Genotypable species object.
 * 
 * @author F. de Coligny - November 2004
 */
public interface GenoSpecies extends Species {

	public int getValue(); // each species must have a different value

	public String getName(); // Species interface

	public GeneticMap getGeneticMap();

	public void setGeneticMap(GeneticMap gm);

	public AlleleDiversity getAlleleDiversity();

	public void setAlleleDiversity(AlleleDiversity ad);

	public AlleleEffect getAlleleEffect();

	public void setAlleleEffect(AlleleEffect ae);

	public Kinship getKinship();

	public void setKinship(Kinship a);

	// The code below may be copied and adapted in the species object of the
	// modules
	// when the species properties can change in time. E.g. this implementation
	// allows
	// The alleleEffects to change during the simulation. fc-30.8.2013 + J
	// Labonne
	// /**
	// * Clones an GenoSpecies, added with J. Labonne for mutation.
	// * fc-30.8.2013
	// */
	// public Object clone () {
	// try {
	// GenoSpecies clone = (GenoSpecies) super.clone ();
	//
	// clone.setGeneticMap (getGeneticMap ()); // not cloned
	// clone.setAlleleDiversity (getAlleleDiversity ()); // not cloned
	// clone.setAlleleEffect ((AlleleEffect) getAlleleEffect ().clone ()); //
	// cloned
	//
	// return clone;
	//
	// } catch (Exception e) {
	// Log.println (Log.ERROR, "GenoSpecies.clone ()",
	// "Trouble while cloning an GenoSpecies instance", e);
	// return null;
	// }
	// }

}
