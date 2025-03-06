/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.lib.biomechanics;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import capsis.defaulttype.Tree;
import jeeb.lib.util.Spatialized;

/**
 * MecaTree -  Additive data structure for biomechanics.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaTree {
//checked for c4.1.1_08 - fc - 4.2.2003

	private MecaProcess mecaProcess;
	private int id;
	private int age;
	private double x;
	private double y;
	private double z;
	private double xTop;
	private double yTop;
	private double zTop;
	private double deltaXTop;
	private double deltaYTop;
	private double deltaZTop;
	private double xCrownCenter;
	private double yCrownCenter;
	private double zCrownCenter;
	private double crownBaseHeight;
	private double crownRadius;
	private double crownDragCoefficient;
	private double height;
	private double dbh;
	private double encastreMoment;
	private double encastreMomentCrit;
	private double stressMax;
	private double stressCrit;
	private double heightBreakage;
	private boolean noDamage;
	private boolean windThrow;
	private boolean stemBreakage;
	private double stemMass;
	private double crownMass;

	private double density;		// g/cm3
	private double modulus;		// MPa
	private double crownDensity;// kg/m3
	private double crownStemRatio;

	private double crownArea;	//m2
	private double crownVolume;	//m3
	private double windSpeedTop; // m.s-1
	private double gustFactor;
	private double edgeFactor;

	// A tree (precisely corresponding MecaTree) is discribed as a Growth Units collection.
	private Collection mecaGUs;
	private double [] crownRadii;

	private double [][] contributionToMencastr;


	/**
	 * Constructor.
	 */
	public MecaTree (	MecaProcess mecaProcess,
						Tree tree,
						double cdc,
						double E,
						double roS,
						double roC,
						double csr) {
		this.mecaProcess = mecaProcess;
		id = tree.getId ();
		age = tree.getAge ();
		mecaGUs = new Vector ();	// if order of GU addition is not conserved -> class Stack !...
		// The tree can be a Maid or Madd Tree...
		// So default position of trunk base is (0,0,0) i.e. xTop and yTop of GU are equal to zero.
		// if Madd Tree : spatialization using the x, y, z properties of tree.
		// if Maid Tree : simulated spatialization later if necessary.
		if (mecaProcess.isSpatialized ()) {
			Spatialized spt = (Spatialized) tree;
			x = spt.getX ();
			y = spt.getY ();
			z = spt.getZ ();
		} else {
			x = 0d;
			y = 0d;
			z = 0d;
		}
		xTop = x;
		yTop = y;
		zTop = z + tree.getHeight ();
		height = tree.getHeight ();
		dbh = tree.getDbh ();
		MecaTreeInfo mti = (MecaTreeInfo) tree;
		crownBaseHeight = mti.getCrownBaseHeight ();
		crownRadius = mti.getCrownRadiusAt (crownBaseHeight);
		crownRadii = new double [age];
		crownDragCoefficient = cdc;

		modulus = E;
		density = roS;
		crownDensity = roC;
		crownStemRatio = csr;

		xCrownCenter = x;
		yCrownCenter = y;
		zCrownCenter = z + crownBaseHeight + (height - crownBaseHeight) / 3;
		encastreMoment = 0d;
		heightBreakage = 0d;
		noDamage = true;
		windThrow = false;
		stemBreakage = false;

		contributionToMencastr = new double [age][5];
		/*
		col [0] : mid height of current GU
		col [1] : stem weight contribution
		col [2] : stem wind contribution
		col [3] : crown weight contribution
		col [4] : crown wind contribution
		*/
	}

	public MecaProcess getMecaProcess () {return mecaProcess;}

	public int getId () {return id;}

	public int getAge () {return age;}

	public double getX () {return x;}

	public double getY () {return y;}

	public double getZ () {return z;}

	public Collection getMecaGUs () {return mecaGUs;}

	public double getXTop () {return xTop;}

	public double getYTop () {return yTop;}

	public double getZTop () {return zTop;}

	public double getDeltaXTop () {return deltaXTop;}

	public double getDeltaYTop () {return deltaYTop;}

	public double getDeltaZTop () {return deltaZTop;}

	public double getXCrownCenter () {return xCrownCenter;}

	public double getYCrownCenter () {return yCrownCenter;}

	public double getZCrownCenter () {return zCrownCenter;}

	public double getCrownBaseHeight () {return crownBaseHeight;}

	public double getCrownRadius () {return crownRadius;}

	public double getCrownDragCoefficient () {return crownDragCoefficient;}

	public double getHeight () {return height;}

	public double getDbh () {return dbh;}

	public double getEncastreMoment () {return encastreMoment;}

	public double getEncastreMomentCrit () {return encastreMomentCrit;}

	public double getStressMax () {return stressMax;}

	public double getStressCrit () {return stressCrit;}

	public double getHeightBreakage () {return heightBreakage;}

	public boolean isNoDamage () {return noDamage;}

	public boolean isWindThrow () {return windThrow;}

	public boolean isStemBreakage () {return stemBreakage;}

	public double getStemMass () {return stemMass;}

	public double getCrownMass () {return crownMass;}

	public double getDensity () {return density;}

	public double getModulus () {return modulus;}

	public double getCrownDensity () {return crownDensity;}

	public double getCrownStemRatio () {return crownStemRatio;}

	public double getCrownArea () {return crownArea;}

	public double getCrownVolume () {return crownVolume;}

	public double getWindSpeedTop () {return windSpeedTop;}

	public double getGustFactor () {return gustFactor;}

	public double getEdgeFactor () {return edgeFactor;}

	public void setCrownArea (double ACz) {crownArea += ACz;}

	public void setCrownVolume (double VCz) {crownVolume += VCz;}

	public void setWindSpeedTop (double u) {windSpeedTop = u;}

	public double [][] getContributionToMencastr () {return contributionToMencastr;}

	public void incrementAge () {age++;}

	public MecaGU firstGU () {
		return (MecaGU) (((Vector) mecaGUs).firstElement ());
	}

	public MecaGU lastGU () {
		return (MecaGU) (((Vector) mecaGUs).lastElement ());
	}

	public MecaGU getGU (int guId) {
		int index = guId - id * 10000;
		index = index / 100 - 1;
		Vector vmecaGUs = (Vector) getMecaGUs ();
		int size = vmecaGUs.size ();
		if (index < 0 || index > size-1) {
			return null;
		} else {
			return (MecaGU) vmecaGUs.get (index);
		}
	}

	public double CrownRadiusAt (int ngu) {
		return crownRadii [ngu];
	}

	/**
	 * Create default GU when growth data of a tree are unknown.
	 * Example : for a tree created at root step with initial age > 1.
	 */
	public void createDefaultGUs (Tree t/*, MecaSettings ms*/) {
		if (age == 0) {return;}	// 0 : error
		double height = t.getHeight ();
		double defaultDh = height / age;
		double currentH = defaultDh / 2;
		double diameterAtCurrentH;
		MecaTreeInfo mti = (MecaTreeInfo) t;
		double xTopGU, yTopGU, zTopGU;
		for (int i = 1; i <= age; i++) {
			diameterAtCurrentH = MecaTools.getDiameter (t, currentH);
			crownRadii [i-1] = mti.getCrownRadiusAt (currentH);

			// Default creation of GUs considers that the tree is perfectly vertical...
			xTopGU = x;
			yTopGU = y;
			zTopGU = z + (currentH + defaultDh / 2);
			MecaGU gu = new MecaGU (this, id, i, diameterAtCurrentH, defaultDh, xTopGU, yTopGU, zTopGU/*, ms*/);
			mecaGUs.add (gu);
			currentH += defaultDh;
		}


	}

	/**
	 * Add a GU to the collection according to the increments of tree growth data
	 * of the step read during the biomechanical structure construction process.
	 * Also set diameter of existing GU.
	 */
	public void createGUs (Tree t/*, MecaSettings ms*/) {
		// R�actualisation des propri�t�s pour le step trait� dans MecaProcess.
		age = t.getAge ();
		height = t.getHeight ();
		dbh = t.getDbh ();
		zTop = z + height;
		MecaTreeInfo mti = (MecaTreeInfo) t;
		crownBaseHeight = mti.getCrownBaseHeight ();
		crownRadius = mti.getCrownRadiusAt (crownBaseHeight);
		crownRadii = new double [age];
		xCrownCenter = x;
		yCrownCenter = y;
		zCrownCenter = z + crownBaseHeight + (height - crownBaseHeight) / 3;
		double currentH = 0d;
		double diameterAtCurrentH;
		double deltaD;
		int ngu = 0;
		// Iteration on existing GUs.
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			currentH += (currentGU.getHeight () / 2);
			diameterAtCurrentH = MecaTools.getDiameter (t, currentH);
			crownRadii [ngu] = mti.getCrownRadiusAt (currentH);
			deltaD = diameterAtCurrentH - currentGU.getDiameter ();
			// Set the current GU diameter.
			currentGU.growthD (deltaD);
			currentH += (currentGU.getHeight () / 2);
			ngu++;
		}

		// Creation of the new GU corresponding to the step read.
		double xTopGU = x;
		double yTopGU = y;
		double zTopGU = z + height;
		currentH = (t.getHeight () + currentH) / 2;
		diameterAtCurrentH = MecaTools.getDiameter (t, currentH);
		crownRadii [ngu] = mti.getCrownRadiusAt (currentH);
		MecaGU gu = new MecaGU (this, id, age, diameterAtCurrentH, mti.getHeightIncrement (), xTopGU, yTopGU, zTopGU/*, ms*/);
		mecaGUs.add (gu);
	}

	/**
	 * Compute the biomechanical behaviour of tree under specified constraints.
	 * The news shape and stresses of the trunk are obtained.
	 */
	// Version pour forces ponct � la base des UC avec effet de la derni�re
	// Vent sur tronc -> force r�partie
	// Vent sur houppier -> force ponct
	// Poids tronc -> force r�partie
	// Poids houppier -> force ponct...
	//public void computeBiomechanicalBehaviour (MecaConstraints mc) {

	public void computeBiomechanicalBehaviour () {

		double meanHeight = mecaProcess.getMeanHeight ();
		double meanSpacing = mecaProcess.getMeanSpacing ();
		double distanceToEdge = Math.abs (x - mecaProcess.getXEdge ());
		double sdh = meanSpacing / meanHeight;
		double xdh = 0d;
		if (mecaProcess.getConstraints ().location.equals ("edge")) {
			xdh = distanceToEdge / meanHeight;
			//xdh = 0;
		} else if (mecaProcess.getConstraints ().location.equals ("stand")) {
			xdh = 50d;
		}
		double BMean = (0.68 * sdh - 0.0385) + (-0.68 * sdh + 0.4785) * Math.pow ((1.7239 * sdh + 0.0316), xdh);
		double BMean0 = (0.68 * sdh - 0.0385) + (-0.68 * sdh + 0.4785);
		double BMax = (2.7193 * sdh - 0.061) + (-1.273 * sdh + 0.9701) * Math.pow ((1.1127 * sdh + 0.0311), xdh);
		gustFactor = BMax / BMean;
		edgeFactor = BMean / BMean0;
		if (mecaProcess.getConstraints ().location.equals ("edge")) {
			gustFactor *= edgeFactor;
		}
		//gustFactor = 4.8017;
		//gustFactor = 5.2582;

		MecaMatrix PT = new MecaMatrix (6);
		MecaVector sF = new MecaVector (6);
		MecaVector dpl = new MecaVector (3);
		MecaVector S = new MecaVector (12);
		MecaVector dPGs = new MecaVector (12);
		MecaVector sR = new MecaVector (6);
		MecaVector cg = new MecaVector (3);
		MecaVector pg = new MecaVector (3);
		MecaVector mg = new MecaVector (3);
		MecaVector sNa = new MecaVector (6);
		MecaVector dmg = new MecaVector (3);
		MecaVector dPDFinal = new MecaVector (12);

		MecaMatrix [] G = new MecaMatrix [age];
		MecaMatrix [] R = new MecaMatrix [age];
		MecaMatrix [] P = new MecaMatrix [age];
		MecaMatrix [] H = new MecaMatrix [age];
		MecaMatrix [] U = new MecaMatrix [age];
		MecaVector [] s = new MecaVector [age];
		MecaVector [] dl = new MecaVector [age];
		MecaVector [] dPG = new MecaVector [age];
		MecaVector [] dPD = new MecaVector [age];
		MecaGU [] GUs = new MecaGU [age];

		int ngu;
		for (ngu=0; ngu<age; ngu++) {
			G [ngu] = new MecaMatrix (12);
			R [ngu] = new MecaMatrix (3);
			P [ngu] = new MecaMatrix (6);
			H [ngu] = new MecaMatrix (6);
			U [ngu] = new MecaMatrix (12);
			s [ngu] = new MecaVector (6);
			dl [ngu] = new MecaVector (12);
			dPG [ngu] = new MecaVector (12);
			dPD [ngu] = new MecaVector (12);
			GUs [ngu] = firstGU ();
		}

		double pl,l;
		double xc, yc, zc;
		ngu = 0;
		MecaGU lastGU = lastGU ();

		double [][] momentDueToWindStem;
		momentDueToWindStem = new double [age][2];
		double [][] momentDueToWindCrown;
		momentDueToWindCrown = new double [age][2];


		// Application du vent.

		stemMass = 0d;
		crownArea = 0d;
		crownVolume = 0d;
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			//currentGU.setMatrices (mc.windVelocity);
			//currentGU.setMatrices (mecaProcess.getConstraints ().windVelocity);
			currentGU.setMatrices ("wind");
			G [ngu] = currentGU.getG();
			R [ngu] = currentGU.getR();
			P [ngu] = currentGU.getP();
			H [ngu] = currentGU.getH();
			U [ngu] = currentGU.getU();

			MecaVector sGForce = new MecaVector (3);
			MecaVector sGTotal = new MecaVector (6);
			// Remise � z�ro de la partie force de sR.
			sR.setSubVector (0, sGForce);

			// R�sultantes du vent sur tronc comme appliqu� au centre de l'UC courante.
			dl [ngu] = currentGU.getDl ();
			pl = currentGU.getLineicWeight ();
			l = currentGU.getLength ();
			MecaVector base = new MecaVector (3);
			base.setElement (0, x);
			base.setElement (1, y);
			base.setElement (2, z);
			cg = currentGU.getCg ().copy ();
			cg = cg.dif (base);
			pg.setElement (0, pl * l);
			mg = cg.cross (pg);
			sR.setElement (0, pg.getElement (0));
			sR.setSubVector (3, mg);
			sF = sF.sum (sR);
			momentDueToWindStem [ngu][0] = cg.getElement (2); // hauteur d'application
			momentDueToWindStem [ngu][1] = mg.getElement (1);

			contributionToMencastr [ngu][0] = cg.getElement (2);

			// First calculation of stem mass.
			stemMass += 1000d * density * currentGU.getSection () * currentGU.getLength ();

			//------------------------------------------------>
			// S est aplliqu� � la base de l'UC courante
			if (ngu != 0) {
				s [ngu-1] = currentGU.getS ().copy ();

				// remise � z�ro de la partie force de sR
				sR.setSubVector (0, sGForce);
				MecaMatrix PGU = currentGU.getP ();
				sGTotal = PGU.pro (s [ngu-1]);

				// indice c pour p en fait : base de l'UC...
				xc = GUs [ngu-1].getXTop ();
				yc = GUs [ngu-1].getYTop ();
				zc = GUs [ngu-1].getZTop ();

				sF = sF.sum (sGTotal);
				sGForce = sGTotal.getSubVector (0, 2);
				cg.setElement (0, xc - x);
				cg.setElement (1, yc - y);
				cg.setElement (2, zc - z);
				mg = cg.cross (sGForce);
				sR.setSubVector (3, mg);
				sF = sF.sum (sR);

				momentDueToWindCrown [ngu][0] = cg.getElement (2); // hauteur d'application
				momentDueToWindCrown [ngu][1] = mg.getElement (1);
			}
			//----------------------------------------->

			GUs [ngu] = currentGU;
			ngu++;
		}

		crownMass = stemMass * crownStemRatio;
		crownDensity = crownMass / crownVolume;
		mecaProcess.setMeanCrownDensity (crownDensity);
		mecaProcess.setFrontalAreaIndex (crownArea);

		encastreMoment += Math.sqrt(sF.getElement (3) * sF.getElement (3) +
				sF.getElement (4) * sF.getElement (4) +
				sF.getElement (5) * sF.getElement (5));

		ngu = 0;
		PT = P [ngu].transpose ();
		sF = PT.pro (sF.pro (-1d));
		dPD [ngu] = new MecaVector (12);
		dPD [ngu].setSubVector (6, sF);

		// On parcours les UC et on travaille en local
		// sur chacune d'elle
		// il faut repasser le vecteur d'�tat du
		// premier nd en local

		String dplstr = "\n\t\tDEPLACEMENTS du sommet de l'UC n� :\n";
		String coostr = "\n\t\tCOORDONNEES du sommet de l'UC n� :\n";
		String effstr = "\n\t\tEFFORTS au milieu de l'UC n� :\n";

		for (ngu=0; ngu<age; ngu++) {
			dPG [ngu] = (G [ngu].pro (dPD [ngu])).sum (dl [ngu]);	// en local
			dpl = R [ngu].pro (dPG [ngu].getSubVector (0, 2));		// en global
			dplstr += "\t\t" + (ngu+1) + "\t" + dpl.bigSimpleString () + "\n";
			//GUs [ngu].setInternalForcesB ((dPD [ngu].getSubVector (6, 11)).pro (-1d));	// en local
			//GUs [ngu].setInternalForcesT (dPG [ngu].getSubVector (6, 11));	// en local

			GUs [ngu].setCoordinates (dpl);		// en global
			MecaVector coordGU = new MecaVector (3);
			coordGU. setElement (0, (GUs [ngu].getXTop () - x));
			coordGU. setElement (1, (GUs [ngu].getYTop () - y));
			coordGU. setElement (2, (GUs [ngu].getZTop () - z));
			coostr += "\t\t" + (ngu+1) + "\t" + coordGU.bigSimpleString () + "\n";

			// To save internal forces at current GU middle

			{	// <- fc - 4.2.2003 - what is this block ?

				//GUs [ngu].setMatricesForStress (mc.windVelocity);
				//GUs [ngu].setMatricesForStress (mecaProcess.getConstraints ().windVelocity);
				GUs [ngu].setMatricesForStress ("wind");
				G [ngu] = GUs [ngu].getG ();
				dl [ngu] = GUs [ngu].getDl ();
				dPGs = (G [ngu].pro (dPD [ngu])).sum (dl [ngu]);			// en local
				GUs [ngu].setInternalForcesM (dPGs.getSubVector (6, 11));	// en local
				effstr += "\t\t" + (ngu+1) + "\t";

			}	// <- fc - 4.2.2003 - what is this block ?

			GUs [ngu].setRPCg ();

			if (ngu < age-1) {
				S.setSubVector(6, s [ngu]);
				dPD [ngu+1] = (U [ngu].pro (dPG [ngu])).sum (S);
			}

		}

		deltaXTop = dpl.getElement (0);
		deltaYTop = dpl.getElement (1);
		deltaZTop = dpl.getElement (2);
		xTop += deltaXTop;
		yTop += deltaYTop;
		zTop += deltaZTop;


		//----------------------------------------------------->>
		double [][] momentDueToStem;
		momentDueToStem = new double [age][2];
		double [][] momentDueToCrown;
		momentDueToCrown = new double [age][2];
		stemMass = 0d;
		crownMass = 0d;
		ngu = 0;
		MecaVector sNull = new MecaVector (6);
		sR = new MecaVector (6);
		sF = new MecaVector (6);

		// application du poids propre + houppier

		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			currentGU.setCrownDensity (crownDensity);
			//currentGU.setMatrices (-1d); // code for weight application
			currentGU.setMatrices ("weight");
			G [ngu] = currentGU.getG();
			R [ngu] = currentGU.getR();
			P [ngu] = currentGU.getP();
			H [ngu] = currentGU.getH();
			U [ngu] = currentGU.getU();

			xc = currentGU.getXTop ();
			yc = currentGU.getYTop ();
			zc = currentGU.getZTop ();

			MecaVector sGForce = new MecaVector (3);
			MecaVector sGTotal = new MecaVector (6);
			// remise � z�ro de la partie force de sR
			sR.setSubVector (0, sGForce);
			pg = sGForce.copy ();

			// R�sultantes du poids propre comme appliqu� au centre de l'UC courante
			dl [ngu] = currentGU.getDl ();
			pl = currentGU.getLineicWeight ();
			l = currentGU.getLength ();
			MecaVector base = new MecaVector (3);
			base.setElement (0, x);
			base.setElement (1, y);
			base.setElement (2, z);
			cg = currentGU.getCg ().copy ();
			cg = cg.dif (base);
			pg.setElement (2, -pl * l); // "incr�ment" total de poids du tronc
			mg = cg.cross (pg);
			sR.setElement (2, pg.getElement (2));
			sR.setSubVector (3, mg);
			sF = sF.sum (sR);
			momentDueToStem [ngu][0] = cg.getElement (0);// deflection due au vent � la hauteur d'appl�
			momentDueToStem [ngu][1] = mg.getElement (1);
			stemMass += pl * l / 9.81;

			//------------------------------------------------------------>
			if (ngu != 0) {
				sR.setSubVector (0, sGForce);
				s [ngu-1] = currentGU.getS ().copy ();
				MecaMatrix PGU = currentGU.getP ();
				sGTotal = PGU.pro (s [ngu-1]);

				// indice c pour p en fait : base de l'UC...
				xc = GUs [ngu-1].getXTop ();
				yc = GUs [ngu-1].getYTop ();
				zc = GUs [ngu-1].getZTop ();

				sF = sF.sum (sGTotal);
				sGForce = sGTotal.getSubVector (0, 2);
				cg.setElement (0, xc - x);
				cg.setElement (1, yc - y);
				cg.setElement (2, zc - z);
				mg = cg.cross (sGForce);
				sR.setSubVector (3, mg);
				sF = sF.sum (sR);
				momentDueToCrown [ngu][0] = cg.getElement (2);// hauteur d'application
				momentDueToCrown [ngu][1] = mg.getElement (1);
				crownMass += -sGTotal.getElement (2) / 9.81;
			}
			//------------------------------------------------------------>

			GUs [ngu] = currentGU;
			ngu++;
		}

		encastreMoment += Math.sqrt(sF.getElement (3) * sF.getElement (3) +
				sF.getElement (4) * sF.getElement (4) +
				sF.getElement (5) * sF.getElement (5));

		double windContribution = 0d;
		double weightContribution = 0d;

		for (ngu=0; ngu<age; ngu++) {
			contributionToMencastr [ngu][1] = 100 * momentDueToStem [ngu][1] / encastreMoment;
			contributionToMencastr [ngu][2] = 100 * momentDueToWindStem [ngu][1] / encastreMoment;
			contributionToMencastr [ngu][3] = 100 * momentDueToCrown [ngu][1] / encastreMoment;
			contributionToMencastr [ngu][4] = 100 * momentDueToWindCrown [ngu][1] / encastreMoment;
			windContribution += contributionToMencastr [ngu][2] + contributionToMencastr [ngu][4];
			weightContribution += contributionToMencastr [ngu][1] + contributionToMencastr [ngu][3];
		}
		mecaProcess.setMeanContributionToMB (windContribution, weightContribution);

		ngu = 0;
		PT = P [ngu].transpose ();
		sF = PT.pro (sF.pro (-1d));
		dPD [ngu] = new MecaVector (12);
		dPD [ngu].setSubVector (6, sF);

		// On parcoure les UC et on travaille en local
		// sur chacune d'elle
		// il faut repasser le vecteur d'�tat du
		// premier nd en local

		dplstr = "\n\t\tDEPLACEMENTS du sommet de l'UC n� :\n";
		coostr = "\n\t\tCOORDONNEES du sommet de l'UC n� :\n";
		effstr = "\n\t\tEFFORTS au milieu de l'UC n� :\n";
		for (ngu=0; ngu<age; ngu++) {
			dPG [ngu] = (G [ngu].pro (dPD [ngu])).sum (dl [ngu]);	// en local
			dpl = R [ngu].pro (dPG [ngu].getSubVector (0, 2));		// en global
			dplstr += "\t\t" + (ngu+1) + "\t" + dpl.bigSimpleString () + "\n";
			//GUs [ngu].setInternalForcesB ((dPD [ngu].getSubVector (6, 11)).pro (-1d));	// en local
			//GUs [ngu].setInternalForcesT (dPG [ngu].getSubVector (6, 11));	// en local

			GUs [ngu].setCoordinates (dpl);		// en global
			MecaVector coordGU = new MecaVector (3);
			coordGU. setElement (0, (GUs [ngu].getXTop () - x));
			coordGU. setElement (1, (GUs [ngu].getYTop () - y));
			coordGU. setElement (2, (GUs [ngu].getZTop () - z));
			coostr += "\t\t" + (ngu+1) + "\t" + coordGU.bigSimpleString () + "\n";

			// To save internal forces at current GU middle.

			{	// <- fc - 4.2.2003 - what is this block ?

				//GUs [ngu].setMatricesForStress (-1d); // code for weight application
				GUs [ngu].setMatricesForStress ("weight"); // code for weight application
				G [ngu] = GUs [ngu].getG ();
				dl [ngu] = GUs [ngu].getDl ();
				dPGs = (G [ngu].pro (dPD [ngu])).sum (dl [ngu]);			// en local
				GUs [ngu].setInternalForcesM (dPGs.getSubVector (6, 11));	// en local
				effstr += "\t\t" + (ngu+1) + "\t";

			}	// <- fc - 4.2.2003 - what is this block ?

			GUs [ngu].setRPCg ();

			if (ngu < age-1) {
				S.setSubVector(6, s [ngu]);
				dPD [ngu+1] = (U [ngu].pro (dPG [ngu])).sum (S);
			}
		}

		deltaXTop += dpl.getElement (0);
		deltaYTop += dpl.getElement (1);
		deltaZTop += dpl.getElement (2);
		xTop += deltaXTop;
		yTop += deltaYTop;
		zTop += deltaZTop;

	}

	/**
	 * Analyse the mechanical state of the Mecatrees after loading i.e. predict damages.
	 */
	public String damageAnalysis () {

		encastreMomentCrit = 111d * stemMass;	// resistive moment of anchorage: Gardiner et al 2000
		stressCrit = 30.6d;		// MPa : breakage stress for green wood in the stem: Gardiner et al 2000
		double sxxMax = 0d;
		double hSxxMax = 0d;

		double sxxC;
		int ngu = 1;
		double currentH = 0d;
		// on stoppe l'analyse des contraintes apres 50% du houppier...
		//double heightMaxForStress = height;
		double heightMaxForStress = crownBaseHeight + 0.5 * (height - crownBaseHeight);
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			currentH += (currentGU.getHeight () / 2);
			sxxC = currentGU.getSxxCM ();
			if (currentH <= heightMaxForStress && sxxC > sxxMax) {
				sxxMax = sxxC;
				hSxxMax = currentH;
			}
			currentH += (currentGU.getHeight () / 2);
			ngu++;
		}

		double v = stemMass / (density * 1000); // m3
		mecaProcess.setStandVolume (v);

		stressMax = sxxMax;
		//stressMax = sxxMax / sxxMax;
		heightBreakage = hSxxMax;

		//encastreMoment *= 2.6364;	// h=20 s=3.8
		//encastreMoment *= 2.6907;	// h=20 s=3.8
		//encastreMoment *= 2.7235;	// h=16 s=3.2
		//encastreMoment *= 2.7509;	// h=12 s=2.5
		//encastreMoment *= 4.8017;	// seez in stand

		encastreMoment *= gustFactor;

		String result = "\nTree" + id + "\t";

		if (encastreMoment >= encastreMomentCrit && stressMax >= stressCrit) {
			result += "WT&SB";
			noDamage = false;
			double critEMC = 100 * encastreMoment / encastreMomentCrit;
			double critSC = 100 * stressMax / stressCrit;
			if (critEMC >= critSC) {
				windThrow = true;
				mecaProcess.setNbTreesWindThrow ();
				mecaProcess.setWindThrowVolume (v);
			} else {
				stemBreakage = true;
				mecaProcess.setNbTreesStemBreakage ();
				mecaProcess.setStemBreakageVolume (v);
			}
		} else if (encastreMoment >= encastreMomentCrit) {
			result += "WT";
			windThrow = true;
			mecaProcess.setNbTreesWindThrow ();
			mecaProcess.setWindThrowVolume (v);
			noDamage = false;
		} else if(stressMax >= stressCrit) {
			result += "SB";
			stemBreakage = true;
			mecaProcess.setNbTreesStemBreakage ();
			mecaProcess.setStemBreakageVolume (v);
			noDamage = false;
		} else {
			result += "ND";
		}

		if (!noDamage) {
			Integer idt = new Integer (id);
			((ArrayList) mecaProcess.getTreeIdsToCut ()).add (idt);
		}

		return result;
	}

	/**
	 * Compute biomechanical light behaviour.
	 */
	public void computeBiomechanicalBehaviourLight () {

		stemMass = 0d;
		crownArea = 0d;
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			currentGU.setMatrices ("wind");
			stemMass += 1000d * density * currentGU.getSection () * currentGU.getLength ();
		}

		crownMass = stemMass * crownStemRatio;
		crownVolume = crownMass;
		crownDensity = 1;
		mecaProcess.setMeanCrownDensity (crownDensity);
		mecaProcess.setFrontalAreaIndex (crownArea);

		double u10 = mecaProcess.getWindSpeedEdgeAt10m ();
		double distanceToEdge = Math.abs (x - mecaProcess.getXEdge ());
		double hsd = 100 * height / dbh;
		double CrRatio = 100 * (height - crownBaseHeight) / height;
		double SArea = Math.PI * Math.pow (dbh, 2) / 4;
		double CrAreasG = crownArea / SArea;

		if (mecaProcess.getConstraints ().location.equals ("edge")) {
			//double distanceToEdge = Math.abs (x - mecaProcess.getXEdge ());
			double p1 = 37.129;
			double p2 = 1.676;
			double p3 = -0.014;
			double p4 = 0.193;
			double p5 = -0.019;
			double p6 = 9.578;
			double p7 = -0.012;
			encastreMoment = p1 * Math.pow (u10, p2) * Math.exp (p3 * distanceToEdge + p4 * height + p5 * hsd + p6 * CrAreasG + p7 * CrRatio);
			//double hsd = 100 * height / dbh;
			p1 = 0.011;
			p2 = 1.815;
			p3 = -0.017;
			p4 = 0.021;
			p5 = 0.072;
			p6 = 8.637;
			p7 = -0.015;
			stressMax = p1 * Math.pow (u10, p2) * Math.exp (p3 * distanceToEdge + p4 * hsd + p5 * height + p6 * CrAreasG + p7 * CrRatio);
		} else if (mecaProcess.getConstraints ().location.equals ("stand")) {
			double p1 = 7.703;
			double p2 = 1.907;
			double p3 = 0.174;
			encastreMoment = p1 * Math.pow (u10, p2) * Math.exp (p3 * height);
			//double hsd = 100 * height / dbh;
			p1 = 0.014;
			p2 = 1.887;
			p3 = 0.030;
			stressMax = p1 * Math.pow (u10, p2) * Math.exp (p3 * hsd);
		}

	}

	/**
	 * Analyse the mechanical state of the Mecatrees after loading i.e. predict damages.
	 */
	public String damageAnalysisLight () {

		encastreMomentCrit = 111d * stemMass;	// resistive moment of anchorage: Gardiner et al 2000
		stressCrit = 30.6d;		// MPa : breakage stress for green wood in the stem: Gardiner et al 2000

		double v = stemMass / (density * 1000); // m3
		mecaProcess.setStandVolume (v);

		String result = "\nTree" + id + "\t";

		if (encastreMoment >= encastreMomentCrit && stressMax >= stressCrit) {
			result += "WT&SB";
			noDamage = false;
			double critEMC = 100 * encastreMoment / encastreMomentCrit;
			double critSC = 100 * stressMax / stressCrit;
			if (critEMC >= critSC) {
				windThrow = true;
				mecaProcess.setNbTreesWindThrow ();
				mecaProcess.setWindThrowVolume (v);
			} else {
				stemBreakage = true;
				mecaProcess.setNbTreesStemBreakage ();
				mecaProcess.setStemBreakageVolume (v);
			}
		} else if (encastreMoment >= encastreMomentCrit) {
			result += "WT";
			windThrow = true;
			mecaProcess.setNbTreesWindThrow ();
			mecaProcess.setWindThrowVolume (v);
			noDamage = false;
		} else if(stressMax >= stressCrit) {
			result += "SB";
			stemBreakage = true;
			mecaProcess.setNbTreesStemBreakage ();
			mecaProcess.setStemBreakageVolume (v);
			noDamage = false;
		} else {
			result += "ND";
		}

		return result;
	}

	/**
	 * Retrieve the MecaTree structure as character string.
	 */
	public String toString () {
		String str = "MecaTree_" + id;
		return str;
	}

	/**
	 * Retrieve the MecaTree structure as character string.
	 */
	public String bigString () {
		StringBuffer b = new StringBuffer ("MecaTree_");
		b.append (id);

		b.append ("\t\tcm\t\t\tm\t\t\tm\t\t\tm\t\t\tm\t\t\tm\t\t\tm2\t\t\t\t\tm4\t\t kg/m3\t\t\t\tPa\n");
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU gu = (MecaGU) i.next ();
			b.append ("    ");
			b.append (gu.bigString ());
			b.append ("\n");
		}
		return b.toString ();
	}

}




