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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.Spatialized;

/**
 * LoiselleKinshipCoefficientSubLoci
 * 
 * @author Sylvie Muratorio, F. de Coligny - December 2006
 */
public class LoiselleKinshipCoefficientSubLoci {

	// computation of SGS parameters for two kind of loci (SELCTED and NEUTRAL)
	// of the Allele nuclear Diversity

	static public class DistanceClass {
		public DistanceClass(int nbLoci, int maxPairsNumber) {

			locusFijMean = new double[nbLoci];
			pairwiseMatrix = new double[maxPairsNumber][3];
		}

		public int index; // O to n-1
		public int numberOfPairs;
		public double upperBound;
		public int censusNumber;
		public double[] locusFijMean; // per locus
		public double FijMeanSelected;
		public double FijMeanNeutral;
		// public double FijVariance;
		public double FijSlopeSelected;
		public double FijSlopeNeutral;
		public double SpStatSelected;
		public double SpStatNeutral;
		public double[][] pairwiseMatrix;

		public String toString() {
			StringBuffer b = new StringBuffer();
			b.append("DistanceClass #");
			b.append(index);
			b.append("\n");
			b.append("upperBound=");
			b.append(upperBound);
			b.append("\n");
			b.append("censusNumber=");
			b.append(censusNumber);
			b.append("\n");
			b.append("locusFijMean:");
			for (int i = 0; i < locusFijMean.length; i++) {
				b.append(locusFijMean[i]);
				if (i < locusFijMean.length - 1) {
					b.append(" ");
				}
			}
			b.append("\n");
			b.append("FijMeanSelected=");
			b.append(FijMeanSelected);
			b.append("\n");
			b.append("FijMeanNeutral=");
			b.append(FijMeanNeutral);
			b.append("\n");
			b.append("FijSlopeSelected=");
			b.append(FijSlopeSelected);
			b.append("\n");
			b.append("FijSlopeNeutral=");
			b.append(FijSlopeNeutral);
			// b.append ("\n");
			// b.append ("FijVariance=");
			// b.append (FijVariance);
			return b.toString();
		}

		public String toStringLine() {
			StringBuffer b = new StringBuffer();
			b.append(censusNumber);
			b.append("\t");
			for (int i = 0; i < locusFijMean.length; i++) {
				b.append(locusFijMean[i]);
				if (i < locusFijMean.length - 1) {
					b.append(" ");
				}
			}
			b.append("\t");
			b.append(FijMeanSelected);
			b.append("\t");
			b.append(FijMeanNeutral);
			b.append("\t");
			b.append("FijSlopeSelected=");
			b.append("\t");
			b.append(FijSlopeSelected);
			b.append("\t");
			b.append("FijSlopeNeutral=");
			b.append("\t");
			b.append(FijSlopeNeutral);
			// b.append ("\n");
			// b.append ("FijVariance=");
			// b.append (FijVariance);
			return b.toString();
		}
	}

	/**
	 * Calculation of the Loiselle Kinship Coefficient. The gees must have
	 * Individual genotypes. Does not handle missing genotype data. Upper bounds
	 * are distance classes, ex: 10, 20, 50, 100.
	 */
	static public DistanceClass[] getFijDistribution(Collection gees, int[] targetLoci, // 1
																						// to
																						// n
			double[] upperBounds, int maxPairsNumber, int nlSelected, int nlNeutral) throws Exception {
		Set processedKeys = new HashSet();

		List list1 = new ArrayList(gees);
		List list2 = new ArrayList(gees);
		Collections.shuffle(list1);
		Collections.shuffle(list2);

		int nbPairs = gees.size() * (gees.size() - 1) / 2;
		double[][] pairwiseDistance = new double[nbPairs][3];

		// 0. tests
		if (gees == null || gees.isEmpty() || gees.size() < 2) {
			throw new Exception("gees must contain at least 2 individuals");
		}
		for (Iterator i = gees.iterator(); i.hasNext();) {
			Genotypable gee = (Genotypable) i.next();
			if (!(gee instanceof Spatialized)) {
				throw new Exception("gees must all be Spatialized");
			}
		}

		if (targetLoci == null) {
			throw new Exception("targetLoci == null");
		}
		// check targetLoci for wrong loci numbers -> Exception
		if (upperBounds == null) {
			throw new Exception("upperBounds == null");
		}
		// check upperBounds for wrong classes order -> Exception

		// 1. create result Distance classes
		Genotypable gee = (Genotypable) gees.iterator().next();
		DistanceClass[] FijDistribution = new DistanceClass[upperBounds.length];
		for (int i = 0; i < upperBounds.length; i++) {
			IndividualGenotype g = (IndividualGenotype) gee.getGenotype();
			DistanceClass c = new DistanceClass(g.getNuclearDNA().length, nbPairs);
			c.index = i;
			c.upperBound = upperBounds[i];
			System.out.println("Distance class " + c.index + "upper bound=" + c.upperBound);
			FijDistribution[i] = c;
			c.numberOfPairs = 0;
		}

		// 2. calculate Pla with GeneticTools.computeNuclearAlleleFrequencies
		boolean withUnknownAllele = false;
		double[][] Pla = GeneticTools.computeNuclearAlleleFrequencies(gees, targetLoci, withUnknownAllele);

		// 3. Initialisation of the iterator on the number of valid pairs of
		// individuals per loci
		int[] nbValidPairs = new int[targetLoci.length];
		for (int k = 0; k < targetLoci.length; k++) {
			int l = targetLoci[k] - 1;
			nbValidPairs[l] = 0;
		}

		// 3. calculate cumulated Fij AND census number PER distance classes
		short[][] nad = null; // NuclearAlleleDiversity
		int cptPair = 0;

		System.out.println("LoiselleKinshipCoefficientSubLoci.getFijDistribution().getTrees ()...");
		for (Iterator first = list1.iterator(); first.hasNext();) {
			Genotypable i = (Genotypable) first.next();
			// System.out.println ("gee "+i.getId ());
			IndividualGenotype gi = (IndividualGenotype) i.getGenotype();

			if (nad == null) {
				nad = getNuclearAlleleDiversity(i);
			}

			for (Iterator second = list2.iterator(); second.hasNext();) {
				Genotypable j = (Genotypable) second.next();
				// System.out.println ("gee "+j.getId ());

				if (i.getId() == j.getId()) {
					continue;
				} // same tree

				String key = (i.getId() < j.getId()) ? "" + i.getId() + "." + j.getId() : "" + j.getId() + "."
						+ i.getId();
				if (processedKeys.contains(key)) {
					continue;
				} // pair was already processed

				processedKeys.add(key);
				IndividualGenotype gj = (IndividualGenotype) j.getGenotype();

				double distance = getDistance((Spatialized) i, (Spatialized) j);
				DistanceClass c = getDistanceClass(FijDistribution, distance);
				// if ( (c.index ==1) || (c.index ==0) )System.out.println
				// ("distance "+distance+ "  ->class " + c.index);

				// limit the number of considered pairs per class
				if (c.censusNumber >= maxPairsNumber) {
					continue;
				}

				double cumulatedNumerator = 0;
				double cumulatedDenominator = 0;
				double pairwiseFijSelected = 0;
				double pairwiseFijNeutral = 0;
				int nbValidLociSelected = 0;
				int nbValidLociNeutral = 0;

				for (int k = 0; k < targetLoci.length; k++) {
					// System.out.println ("gee "+i.getId ()+"   gee "+j.getId
					// ()+"   Locus "+ (k+1));
					int l = targetLoci[k] - 1; // l is locus position, 0 to n-1

					double numerator = 0;
					double denominator = 0;
					// iterate on all alleles at this locus
					short[] allAlleles = nad[l];
					for (int a = 0; a < allAlleles.length; a++) {
						// System.out.println ("Locus "+l+", Allele "+ (a+1));
						// if this allele is not present in the gees collection
						// -> next allele
						if (Pla[l][a] == 0) {
							break;
						}

						double Pila = Pkla(gi.getNuclearDNA()[l], allAlleles[a]);
						double Pjla = Pkla(gj.getNuclearDNA()[l], allAlleles[a]);
						if ((!Double.isNaN(Pila)) && (!Double.isNaN(Pjla))) {
							numerator += (Pila - Pla[l][a]) * (Pjla - Pla[l][a]);
							denominator += Pla[l][a] * (1 - Pla[l][a]);
							cumulatedNumerator += numerator;
							cumulatedDenominator += denominator;
						} else {
							System.out.println("gee " + i.getId() + "  & gee " + j.getId() + "   Locus " + (k + 1)
									+ "->Pila= " + Pila + "   Pjla = " + Pjla + "   Pla=" + Pla[l][a]);
						}

						// System.out.println ("Pila= "+ Pila+"   Pjla = "+
						// Pjla+ "   Pla="+ Pla[l][a]
						// +"   cumulatedNumerator="+cumulatedNumerator+"  cumulatedDenominator"+cumulatedDenominator
						// );
					}

					// gees.size ()*2 : the last term ajusts for low frequency
					// alleles
					// double Fijl = numerator / denominator + 1 / (gees.size
					// ()*2 - 1);
					double Fijl;
					if (denominator != 0) {
						Fijl = numerator / denominator;
						c.locusFijMean[l] += Fijl;
						if (k < nlSelected) {
							pairwiseFijSelected += Fijl;
							nbValidLociSelected++;
						} else {
							pairwiseFijNeutral += Fijl;
							nbValidLociNeutral++;
						}
						nbValidPairs[l]++; // SOM do not understand what it is
											// for !!!
					}

				} // (existing in the gees collection)

				c.censusNumber++;

				// Storing pairwise Fij
				pairwiseDistance[cptPair][0] = distance;
				pairwiseFijSelected /= nbValidLociSelected;
				pairwiseFijNeutral /= nbValidLociNeutral;
				pairwiseDistance[cptPair][1] = pairwiseFijSelected;
				pairwiseDistance[cptPair][2] = pairwiseFijNeutral;

				cptPair++;

				if (Double.isNaN(pairwiseFijSelected)) {
					// System.out.println("Indiv " +i.getId ()+
					// " &indiv "+j.getId ()+ " (dist= "+distance+
					// ") -> Fij= "+pairwiseFij);
				}

				c.pairwiseMatrix[c.numberOfPairs][0] = distance;
				c.pairwiseMatrix[c.numberOfPairs][1] = pairwiseFijSelected;
				c.pairwiseMatrix[c.numberOfPairs][2] = pairwiseFijNeutral;
				c.numberOfPairs++; // add the pair to the list
				// System.out.println("Indiv " +i.getId ()+ " &indiv "+j.getId
				// ()+ " (dist= "+distance+ ") -> Fij= "+pairwiseFij);

				// double Fij = cumulatedNumerator / cumulatedDenominator + 1 /
				// (gees.size ()*2 - 1);
				// c.FijMean += Fij;
			}
		}

		// 4. Calculate means and variance per class
		for (int i = 0; i < FijDistribution.length; i++) {
			DistanceClass c = FijDistribution[i];
			// c.FijMean /= c.censusNumber;

			if ((c.censusNumber >= 2) || (i == 0)) {

				// SELECTED loci
				double FijSelected = 0;
				int ns = nlSelected; // number of loci
				System.out.println(" class:  " + (i + 1) + " #  pairs:  " + c.numberOfPairs);
				for (int l = 0; l < nlSelected; l++) {
					// c.locusFijMean[l] /= c.censusNumber;
					// c.locusFijMean[l] /= nbValidPairs[l]; // SOM : not
					// logical !!! nbValidPairs varies among classes !!!
					c.locusFijMean[l] /= c.numberOfPairs;
					if ((c.locusFijMean[l] > -1) && (c.locusFijMean[l] < 1)) {
						FijSelected += c.locusFijMean[l];
					} else {
						ns--;
					}
				}

				double FijNeutral = 0;
				int nn = nlNeutral; // number of loci
				for (int l = nlSelected; l < nlSelected + nlNeutral; l++) {
					// c.locusFijMean[l] /= c.censusNumber;
					// c.locusFijMean[l] /= nbValidPairs[l];// SOM : not logical
					// !!! nbValidPairs varies among classes !!!
					c.locusFijMean[l] /= c.numberOfPairs;
					if ((c.locusFijMean[l] > -1) && (c.locusFijMean[l] < 1)) {
						FijNeutral += c.locusFijMean[l];
					} else {
						nn--;
					}
				}

				c.FijMeanSelected = FijSelected / ns;
				c.FijMeanNeutral = FijNeutral / nn;
				// System.out.println (" class:  "+(i+1) +"FijMeanSelected= "+
				// c.FijMeanSelected +" FijMeanNeutral =" + c.FijMeanNeutral );
				c.FijMeanSelected = getMean(c.pairwiseMatrix, c.numberOfPairs, 1);
				c.FijMeanNeutral = getMean(c.pairwiseMatrix, c.numberOfPairs, 2);
				// System.out.println (" class:  "+(i+1) +" with "+
				// c.numberOfPairs+ "ind. ; FijMeanSelected= "+
				// c.FijMeanSelected +" FijMeanNeutral =" + c.FijMeanNeutral );

				if (i == 0) {

					try {
						c.FijSlopeSelected = calculateFijSlopeLeastSquare(pairwiseDistance, cptPair, true, 0, 1);
						c.SpStatSelected = -(c.FijSlopeSelected / (1 - c.FijMeanSelected));
						c.FijSlopeNeutral = calculateFijSlopeLeastSquare(pairwiseDistance, cptPair, true, 0, 2);
						c.SpStatNeutral = -(c.FijSlopeNeutral / (1 - c.FijMeanNeutral));
					} catch (Exception e) {
						System.out.println("exception: " + e); // USEFULL TO
																// WRITE DETAILS
																// OF BUG IN THE
																// RUN WINDOW
						e.printStackTrace(); // USEFULL TO WRITE DETAILS OF BUG
												// IN THE RUN WINDOW
					}

				} else {
					c.FijSlopeSelected = FijDistribution[0].FijSlopeSelected;
					c.SpStatSelected = FijDistribution[0].SpStatSelected;
					c.FijSlopeNeutral = FijDistribution[0].FijSlopeNeutral;
					c.SpStatNeutral = FijDistribution[0].SpStatNeutral;
				}
			} else {
				c.FijMeanSelected = 999.0;
				c.FijMeanNeutral = 999.0;
			}

		}

		// 5. Return distanceClass array
		return FijDistribution;
	}

	static private short[][] getNuclearAlleleDiversity(Genotypable gee) {
		AlleleDiversity ad = gee.getGenoSpecies().getAlleleDiversity();
		short[][] nad = ad.getNuclearAlleleDiversity();
		return nad;
	}

	static private double getDistance(Spatialized i, Spatialized j) {
		return Math.sqrt((j.getX() - i.getX()) * (j.getX() - i.getX()) + (j.getY() - i.getY()) * (j.getY() - i.getY()));
	}

	static private DistanceClass getDistanceClass(DistanceClass[] FijDistribution, double distance) throws Exception {
		for (int i = 0; i < FijDistribution.length; i++) {
			if (distance < FijDistribution[i].upperBound) {
				return FijDistribution[i];
			}
		}
		throw new Exception("distance out of range: d=" + distance);
	}

	static private double Pkla(short[] locusGenotype, short allele) {
		double value = 0;
		if (locusGenotype[0] == allele) {
			value += 0.5;
		}
		if (locusGenotype[1] == allele) {
			value += 0.5;
		}
		return value;
	}

	static public String traceFij(DistanceClass[] FijDistribution) {
		StringBuffer b = new StringBuffer();
		for (int i = 0; i < FijDistribution.length; i++) {
			b.append(FijDistribution[i]);
			b.append("\n");
		}
		return b.toString();
	}

	static double getMean(double[][] pairwiseMatrix, int trueNbPairs, int posVar) {
		double meanVar = 0;
		for (int i = 0; i < trueNbPairs; i++) {
			meanVar += pairwiseMatrix[i][posVar];
		}

		meanVar /= trueNbPairs;
		return meanVar;
	}

	static double calculateFijSlopeLeastSquare(double[][] pairwiseDistance, int trueNbPairs, boolean logDist, int posX,
			int posY) {

		double covXY = 0;
		double varX = 0;
		double meanX = 0;
		double meanY = 0;

		for (int i = 0; i < trueNbPairs; i++) {
			if (logDist) {
				meanX += Math.log(pairwiseDistance[i][posX]);
			} else {
				meanX += pairwiseDistance[i][posX];
			}
			meanY += pairwiseDistance[i][posY];
		}

		meanX = meanX / trueNbPairs;
		meanY = meanY / trueNbPairs;

		for (int i = 0; i < trueNbPairs; i++) {
			if (logDist) {
				covXY += (Math.log(pairwiseDistance[i][posX]) - meanX) * (pairwiseDistance[i][posY] - meanY);
				varX += (Math.log(pairwiseDistance[i][posX] - meanX)) * (pairwiseDistance[i][posX] - meanX);
			} else {
				covXY += (pairwiseDistance[i][posX] - meanX) * (pairwiseDistance[i][posY] - meanY);
				varX += (pairwiseDistance[i][posX] - meanX) * (pairwiseDistance[i][posX] - meanX);
			}
		}
		covXY = covXY / (trueNbPairs - 1);
		varX = varX / (trueNbPairs - 1);

		double slope;
		if (varX != 0) {
			slope = covXY / varX;
			// System.out.println("slope is " + slope);
			return slope;
		} else {
			System.out.println("Problem in computing VarX (VarX null)");
			return (-99);
		}

	}

}
