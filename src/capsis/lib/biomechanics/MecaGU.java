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

import java.text.NumberFormat;
import java.util.Vector;

/**
 * MecaGU - Growth Unit to describe a tree.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaGU {
//checked for c4.1.1_08 - fc - 3.2.2003

	private MecaTree mecaTree;
	private int id;
	private double diameter;	// cm
	private double height;		// m
	private double xTop;		// m
	private double yTop;		// m
	private double zTop;		// m
	private double radius;		// m
	private double section;		// m2
	private double inertia;		// m4
	private double density;		// kg/m3
	private double modulus;		// Pa
	private double crownDensity;// kg/m3
	private double length;		// m
	private double lineicWeight;// N/m

	// stresses in MPa...
	private double sxxCB;		// axial compression stress at GU base in outer wood
	private double sxxCM;		// axial compression stress at GU middle in outer wood
	private double sxxCT;		// axial compression stress at GU top in outer wood

	private MecaMatrix G;
	private MecaMatrix R;
	private MecaMatrix P;
	private MecaMatrix H;
	private MecaMatrix U;
	private MecaVector s;
	private MecaVector dl;
	private MecaVector cg;
	private MecaVector internalForcesB;
	private MecaVector internalForcesM;
	private MecaVector internalForcesT;


	/**
	 * Constructor with tree parent, identity, diameter, height, coordinates and settings.
	 */
	public MecaGU (	MecaTree mecaTree,
					int treeId,
					int treeAge,
					double diameter,
					double height,
					double xTop,
					double yTop,
					double zTop/*,
					MecaSettings ms*/) {
		this.mecaTree = mecaTree;
		id = treeId*10000+100*(treeAge);
		this.diameter = diameter;
		this.height = height;
		this.xTop = xTop;
		this.yTop = yTop;
		this.zTop = zTop;
		radius = diameter / 200;
		setSection ();
		setInertia ();

		/*MecaSettings ms = mecaTree.getMecaProcess ().getSettings ();
		density = ms.woodDensity * 1000;
		modulus = ms.youngModulus * 1000000;
		crownDensity = ms.crownDensity;*/

		density = mecaTree.getDensity () * 1000;
		modulus = mecaTree.getModulus () * 1000000;
		crownDensity = mecaTree.getCrownDensity ();

		length = height;
		lineicWeight = density * section * 10d;

		G = new MecaMatrix (12);
		R = new MecaMatrix (3);
		P = new MecaMatrix (6);
		H = new MecaMatrix (6);
		U = new MecaMatrix (12);
		s = new MecaVector (6);
		dl = new MecaVector (12);
		cg = new MecaVector (3);
		internalForcesB = new MecaVector (6);
		internalForcesM = new MecaVector (6);
		internalForcesT = new MecaVector (6);

		setRPCg ();
	}

	public MecaTree getMecaTree () {return mecaTree;}

	public int getId () {return id;}

	public double getDiameter () {return diameter;}

	public double getHeight () {return height;}

	public double getXTop () {return xTop;}

	public double getYTop () {return yTop;}

	public double getZTop () {return zTop;}

	public double getRadius () {return radius;}

	public double getSection () {return section;}

	public double getInertia () {return inertia;}

	public double getDensity () {return density;}

	public double getModulus () {return modulus;}

	public double getCrownDensity () {return crownDensity;}

	public void setCrownDensity (double roC) {crownDensity = roC;}

	public double getLength () {return length;}

	public double getLineicWeight () {return lineicWeight;}

	public double getSxxCB () {return sxxCB;}

	public double getSxxCM () {return sxxCM;}

	public double getSxxCT () {return sxxCT;}

	public MecaMatrix getG () {return G;}

	public MecaMatrix getR () {return R;}

	public MecaMatrix getP () {return P;}

	public MecaMatrix getH () {return H;}

	public MecaMatrix getU () {return U;}

	public MecaVector getS () {return s;}

	public MecaVector getDl () {return dl;}

	public MecaVector getCg () {return cg;}

	public MecaVector getInternalForcesB () {return internalForcesB;}

	public MecaVector getInternalForcesM () {return internalForcesM;}

	public MecaVector getInternalForcesT () {return internalForcesT;}

	/**
	 * Return the previous Growth Unit.
	 */
	public MecaGU previousGU () {
		int index = id - mecaTree.getId () * 10000;
		index = index / 100 - 2;
		Vector mecaGUs = (Vector) mecaTree.getMecaGUs ();
		int size = mecaGUs.size ();
		if (index < 0 || index > size-1) {
			return null;
		} else {
			return (MecaGU) mecaGUs.get (index);
		}
	}

	/**
	 * Return the next Growth Unit.
	 */
	public MecaGU nextGU () {
		int index = id - mecaTree.getId () * 10000;
		index = index / 100;
		Vector mecaGUs = (Vector) mecaTree.getMecaGUs ();
		int size = mecaGUs.size ();
		if (index < 0 || index > size-1) {
			return null;
		} else {
			return (MecaGU) mecaGUs.get (index);
		}
	}

	/**
	 * Set GU diameter with provided diameter increment.
	 */
	public void growthD (double deltaD) {
		if (deltaD < 0d) {
			deltaD = 0.1;
		}
		diameter += deltaD;
		radius = diameter / 200;
		setSection ();
		setInertia ();
		lineicWeight = density * section * 10d;
	}

	/**
	 * Calculate GU section versus diameter.
	 */
	private void setSection () {
		section = Math.PI * Math.pow (radius, 2);
	}

	/**
	 * Calculate GU inertia versus diameter.
	 */
	private void setInertia () {
		inertia = Math.PI * Math.pow (radius, 4) / 4;
	}

	/**
	 * Matrices management.
	 */
	public void setMatrices (String loading) {
		boolean computeStress = false;
		//setRPCg (); //fait dans le constructeur...
		setG (computeStress);
		setHU ();

		if (loading.equals ("wind")) {
			setSWind ();
			setDlWind (computeStress);
		}
		if (loading.equals ("weight")) {
			setSWeight ();
			setDlWeight (computeStress);
		}
	}

	/**
	 * Matrices management.
	 */
	public void setMatricesForStress (String loading) {
		boolean computeStress = true;
		setG (computeStress);

		if (loading.equals ("wind")) {
			setDlWind (computeStress);
		}
		if (loading.equals ("weight")) {
			setDlWeight (computeStress);
		}
	}

	/**
	 * Manage RPCg.
	 */
	public void setRPCg () {
		MecaVector vxc = new MecaVector (3);
		vxc.setElement (0, xTop);
		vxc.setElement (1, yTop);
		vxc.setElement (2, zTop);
		cg = vxc.copy();
		MecaVector pguTop = new MecaVector (3);
		MecaGU previousGU = previousGU ();
		if (previousGU != null) {
			pguTop.setElement (0, previousGU.xTop);
			pguTop.setElement (1, previousGU.yTop);
			pguTop.setElement (2, previousGU.zTop);
		} else {
			pguTop.setElement (0, mecaTree.getX ());
			pguTop.setElement (1, mecaTree.getY ());
			pguTop.setElement (2, mecaTree.getZ ());
		}

		vxc = vxc.dif (pguTop);
		length = vxc.norm ();
		if (length != 0) {
			vxc = vxc.div (length);
		}
		cg = cg.sum (pguTop);
		cg = cg.div (2);

		MecaVector vyc = new MecaVector (3);
		MecaVector vzc = new MecaVector (3);
		double norm;
		if (vxc.getElement (2) == 1d) {
			// when Z and x are parallels : not leaning plane
			// then ZX plane contains y
			vyc.setElement (0, 1d);
			vzc.setElement (1, 1d);
		} else {
			// when Z and x are not parallels : leaning plane Zx (vertical)
			// then Zx plane contains y
			MecaVector Z = new MecaVector (3);
			Z.setElement (2, 1d);
			vzc = Z.cross (vxc);
			norm = vzc.norm ();
			if (norm != 0) {
				vzc = vzc.div (norm);
			}
			vyc = vxc.rot3D (vzc, Math.PI / 2);
		}

		R.setColumn (0, vxc);
		R.setColumn (1, vyc);
		R.setColumn (2, vzc);
		P.setSubMatrix (0, 0, R);
		P.setSubMatrix (3, 3, R);

		//Log.print ("\nvxc GU " + id + "\n" + vxc.bigSimpleString () + "\n");
		//Log.print ("pguTop GU " + id + "\n" + pguTop.bigSimpleString () + "\n");
		//Log.print ("vxc GU " + id + "\n" + vxc.bigSimpleString () + "\n");
		//Log.print ("length GU " + id + "\n" + length + "\n");
	}

	// G diagonal management.
	//
	private void setGDiagonal () {
		int i;
		for (i=0; i<6; i++) {
			G.setElement (i, i, 1d);
		}
		for (i=6; i<12; i++) {
			G.setElement (i, i, -1d);
		}
	}

	// G management.
	//
	private void setG (boolean computeStress) {
		double l, l2, l3, ea, ei, gj;
		if(computeStress) {
			double computeStressLevel = 1d / 2d;
			l = length * computeStressLevel;
		} else {
			l = length;
		}
		l2 = l*l;
		l3 = l*l2;
		ea = modulus * section;
		ei = modulus * inertia;
		gj = ei / (1 + 0.3);

		setGDiagonal ();
		G.setElement (0, 6, (-l / ea));
		G.setElement (1, 5, l);
		G.setElement (1, 7, (l3 / (6 * ei)));
		G.setElement (1, 11, (-l2 / (2 * ei)));
		G.setElement (2, 4, -l);
		G.setElement (2, 8, (l3 / (6 * ei)));
		G.setElement (2, 10, (l2 / (2 * ei)));
		G.setElement (3, 9, (-l / gj));
		G.setElement (4, 8, (-l2 / (2 * ei)));
		G.setElement (4, 10, (-l / ei));
		G.setElement (5, 7, (l2 / (2 * ei)));
		G.setElement (5, 11, (-l / ei));
		G.setElement (10, 8, -l);
		G.setElement (11, 7, l);
	}

	// HU management.
	//
	private void setHU () {
		MecaGU nextGU = nextGU ();
		double norm;
		if(nextGU == null) {
			H.identity ();
			U.identity ();
		} else {
			MecaVector vxs = new MecaVector (3);
			vxs.setElement (0, nextGU.xTop - xTop);
			vxs.setElement (1, nextGU.yTop - yTop);
			vxs.setElement (2, nextGU.zTop - zTop);
			norm = vxs.norm ();
			if (norm != 0) {
				vxs = vxs.div (norm);
			}

			MecaVector vys = new MecaVector (3);
			MecaVector vzs = new MecaVector (3);
			if(vxs.getElement (2) == 1d) {
				// when Z and x are parallels : not leaning plane
				// then ZX plane contains y
				vys.setElement (0, 1d);
				vzs.setElement (1, 1d);
			} else {
				// when Z and x are not parallels : leaning plane Zx (vertical)
				// then Zx plane contains y
				MecaVector Z = new MecaVector (3);
				Z.setElement (2, 1d);
				vzs = Z.cross (vxs);
				norm = vzs.norm ();
				if (norm != 0) {
					vzs = vzs.div (norm);
				}
				vys = vxs.rot3D (vzs, Math.PI / 2);
			}

			MecaMatrix Rs = new MecaMatrix (3);
			Rs.setRow (0, vxs);
			Rs.setRow (1, vys);
			Rs.setRow (2, vzs);
			MecaMatrix Ps = Rs.pro (R);
			H.setSubMatrix (0, 0, Ps);
			H.setSubMatrix (3, 3, Ps);
			U.setSubMatrix (0, 0, H);
			MecaMatrix MH = H.pro (-1d);
			U.setSubMatrix (6, 6, MH);
		}
	}

	// S management.
	//
	private void setS (MecaVector v) {
		s = v.copy ();
	}

	// SWind management.
	//
	private void setSWind () {
		double Uz = 0d;
		double z = 0d;
		if (this == mecaTree.lastGU ()) {
			z = zTop - mecaTree.getZ ();
			Uz = mecaTree.getMecaProcess ().windSpeedAt (z);	// m.s-1
			//mecaTree.setWindSpeedTop (3.6 * Uz); // km.h-1
			mecaTree.setWindSpeedTop (Uz); // m.s-1
		}

		double ro = 1.226;
		double CdC = mecaTree.getCrownDragCoefficient ();

		double Fz;	// vent sur houppier
		if (zTop <= (mecaTree.getZ () + mecaTree.getCrownBaseHeight ())) {
			Fz = 0d;
		} else {
			if (this != mecaTree.firstGU ()) {
				int ngu = id - mecaTree.getId () * 10000;
				ngu = ngu / 100 - 1;
				double crownRadius = mecaTree.CrownRadiusAt (ngu);

				double ACz = height * (2 * crownRadius);	// vent sur surface non déformée !
				double ATz = height * (2 * radius);		// force sur UC non déformée
				ACz -= ATz;
				mecaTree.setCrownArea (ACz);

				double VCz = height * (Math.PI * Math.pow (crownRadius,2));
				double VTz = height * (Math.PI * Math.pow (radius,2));
				VCz -= VTz;
				mecaTree.setCrownVolume (VCz);

				double zTopPrev = previousGU ().getZTop ();
				z = ((zTop + zTopPrev) / 2.0) - mecaTree.getZ ();
				Uz = mecaTree.getMecaProcess ().windSpeedAt (z);	// m.s-1

				double St;
				if (Uz <= 11) {
					St = 0.8;
				} else if (Uz > 11 && Uz <= 20) {
					St = (10d / Uz) - 0.1;
				} else {
					St = 0.4;
				}
				//St = 0.84;
				/*if (mecaTree.getId() == 2) {
					Log.print("\nSt=\t" + St);
				}*/

				/*if (mecaTree.getId() == 2) {
					Log.print("\nACz=\t"+ACz+"\tACzR=\t"+(ACz*St));
				}*/

				ACz *= St;
				Fz = 0.5 * CdC * ro * ACz * Math.pow (Uz, 2);
			} else {
				Fz = 0d;
			}
		}

		MecaVector sGlobal = new MecaVector (6);
		sGlobal.setElement (0, Fz);

		MecaMatrix PTGU = getP().transpose ();
		// on passe s en local courant !	// interesting !
		s = PTGU.pro (sGlobal);
	}

	// SWeight management.
	//
	private void setSWeight () {
		if (zTop <= (mecaTree.getZ () + mecaTree.getCrownBaseHeight ())) {
			s = new MecaVector (6);
		} else {
			if (this != mecaTree.firstGU ()) {
				int ngu = id - mecaTree.getId () * 10000;
				ngu = ngu / 100 - 1;
				double crownRadius = mecaTree.CrownRadiusAt (ngu);
				double VCz = height * (Math.PI * Math.pow (crownRadius,2)); // poids sur volume non déformé !
				double VTz = height * (Math.PI * Math.pow (radius,2)); // force sur volume non déformé
				VCz -= VTz;

				double Fz = crownDensity * VCz * 9.81;
				MecaVector sGlobal = new MecaVector (6);
				sGlobal.setElement (2, -Fz);
				MecaMatrix PTGU = getP().transpose ();
				// on passe s en local courant !
				s = PTGU.pro (sGlobal);
			} else {
				s = new MecaVector (6);
			}
		}
	}

	// DlWind management.
	//
	private void setDlWind (boolean computeStress) {	// vent sur tronc
		double Uz = 0d;
		double zTopPrev;
		if (this == mecaTree.firstGU ()) {
			zTopPrev = mecaTree.getZ ();
		} else {
			zTopPrev = previousGU ().getZTop ();
		}
		double z = ((zTop + zTopPrev) / 2.0) - mecaTree.getZ ();
		Uz = mecaTree.getMecaProcess ().windSpeedAt (z); // m.s-1

		double ro = 1.226;
		double CdS = mecaTree.getCrownDragCoefficient (); // avant 0.8

		double ATz = height * (2 * radius); // force sur UC non déformée
		double Fz = 0.5 * CdS * ro * ATz * Math.pow (Uz, 2);

		lineicWeight = Fz / length; // ici le vent est le premier chargement :
		// length = height car la structure n'est pas encore déformée...

		double l, l2, l3, l4, ea, ei;
		if(computeStress) {
			double computeStressLevel = 1d / 2d;
			l = length * computeStressLevel;
		} else {
			l = length;
		}
		l2 = l*l;
		l3 = l*l2;
		l4 = l*l3;
		ea = modulus * section;
		ei = modulus * inertia;

		MecaVector dlg = new MecaVector (3);
		dlg.setElement (0, lineicWeight);
		MecaVector dll= new MecaVector (3);
		MecaMatrix RT = R.transpose ();
		dll = RT.pro (dlg);

		dl.setElement (0, (-(l2 / (2 * ea)) * dll.getElement (0)));
		dl.setElement (1, ((l4 / (24 * ei)) * dll.getElement (1)));
		dl.setElement (2, ((l4 / (24 * ei)) * dll.getElement (2)));
		dl.setElement (4, (-(l3 / (6 * ei)) * dll.getElement (2)));
		dl.setElement (5, ((l3 / (6 * ei)) * dll.getElement (1))) ;
		dl.setElement (6, (-l * dll.getElement (0)));
		dl.setElement (7, (-l * dll.getElement (1)));
		dl.setElement (8, (-l * dll.getElement (2)));
		dl.setElement (10, (-(l2 / 2) * dll.getElement (2)));
		dl.setElement (11, ((l2 / 2) * dll.getElement (1)));
	}

	// DlWeight management.
	//
	private void setDlWeight (boolean computeStress) {	// poids du tronc
		lineicWeight = density * section * 9.81; // for total weight of GU !
		lineicWeight *= height; // poids réel de l'UC non déformée
		lineicWeight /= length;	// poids linéique rapporté sur l'UC déformée allongée !

		double l, l2, l3, l4, ea, ei;
		if(computeStress) {
			double computeStressLevel = 1d / 2d;
			l = length * computeStressLevel;
		} else {
			l = length;
		}
		l2 = l*l;
		l3 = l*l2;
		l4 = l*l3;
		ea = modulus * section;
		ei = modulus * inertia;

		MecaVector dlg = new MecaVector (3);
		dlg.setElement (2, -lineicWeight);
		MecaVector dll= new MecaVector (3);
		MecaMatrix RT = R.transpose ();
		dll = RT.pro (dlg);

		dl.setElement (0, (-(l2 / (2 * ea)) * dll.getElement (0)));
		dl.setElement (1, ((l4 / (24 * ei)) * dll.getElement (1)));
		dl.setElement (2, ((l4 / (24 * ei)) * dll.getElement (2)));
		dl.setElement (4, (-(l3 / (6 * ei)) * dll.getElement (2)));
		dl.setElement (5, ((l3 / (6 * ei)) * dll.getElement (1))) ;
		dl.setElement (6, (-l * dll.getElement (0)));
		dl.setElement (7, (-l * dll.getElement (1)));
		dl.setElement (8, (-l * dll.getElement (2)));
		dl.setElement (10, (-(l2 / 2) * dll.getElement (2)));
		dl.setElement (11, ((l2 / 2) * dll.getElement (1)));
	}

	/**
	 * Coordinates.
	 */
	public void setCoordinates (MecaVector dpl) {
		xTop += dpl.getElement (0);
		yTop += dpl.getElement (1);
		zTop += dpl.getElement (2);
	}

	/**
	 * B internal forces calculation.
	 */
	public void setInternalForcesB (MecaVector iefg) {
		// ces calculs ne sont valables que pour une flexion
		// dans le plan global OXZ (autour de Y)
		// soit le plan local Gxy (autour de z)
		internalForcesB = internalForcesB.sum (iefg);
		double Nx = internalForcesB.getElement (0);
		double Mz = internalForcesB.getElement (5);
		sxxCB = (Nx / section) - (Mz * radius / inertia);
		// conversion to positive MPa...
		sxxCB /= -1000000d;
	}

	/**
	 * M internal forces calculation.
	 */
	public void setInternalForcesM (MecaVector iefg) {
		internalForcesM = internalForcesM.sum (iefg);
		double Nx = internalForcesM.getElement (0);
		double Mz = internalForcesM.getElement (5);

		//Mz *= 2.6364;	// h=20 s=3.8
		//Mz *= 2.6907;	// h=20 s=3.8
		//Mz *= 2.7235;	// h=16 s=3.2
		//Mz *= 2.7509;	// h=12 s=2.5
		//Mz *= 2.8;	//2.94 h=6 s=1.6
		//Mz *= 4.8017;	// seez in stand

		Mz *= mecaTree.getGustFactor ();

		sxxCM = (Nx / section) - (Mz * radius / inertia);
		// conversion to positive MPa...
		sxxCM /= -1000000d;
	}

	/**
	 * T internal forces calculation.
	 */
	public void setInternalForcesT (MecaVector iefg) {
		internalForcesT = internalForcesT.sum (iefg);
		double Nx = internalForcesT.getElement (0);
		double Mz = internalForcesT.getElement (5);
		sxxCT = (Nx / section) - (Mz * radius / inertia);
		// conversion to positive MPa...
		sxxCT /= -1000000d;
	}

	public void setStrainIncrements (MecaVector iefg) {}

	/**
	 * Rotation.
	 */
	public void rotate (MecaVector axisR, double angle) {
		double xt = mecaTree.getX ();
		double yt = mecaTree.getY ();
		double zt = mecaTree.getZ ();
		MecaVector vxc = new MecaVector (3);
		vxc.setElement (0, xTop - xt);
		vxc.setElement (1, yTop - yt);
		vxc.setElement (2, zTop - zt);
		double angleRad = angle * Math.PI / 180;
		MecaVector vxr = new MecaVector (3);
		vxr = vxc.rot3D (axisR, angleRad);
		xTop = xt + vxr.getElement (0);
		yTop = yt + vxr.getElement (1);
		zTop = zt + vxr.getElement (2);

		setRPCg ();
	}

	/**
	 * Retrieve the growth unit as character string.
	 */
	public String toString () {
		StringBuffer b = new StringBuffer ("MecaGU_");
		b.append (id);
		return b.toString ();
	}
	/**
	 * Retrieve the growth unit as detailed character string.
	 */
	public String bigString () {
		//~ String str = "MecaGU_"+id;
		NumberFormat nf2 = NumberFormat.getInstance ();
		nf2.setMinimumFractionDigits (2);
		nf2.setMaximumFractionDigits (2);
		nf2.setGroupingUsed (false);
		NumberFormat nf6 = NumberFormat.getInstance ();
		nf6.setMinimumFractionDigits (6);
		nf6.setMaximumFractionDigits (6);
		nf6.setGroupingUsed (false);
		NumberFormat nf0 = NumberFormat.getInstance ();
		nf0.setMinimumFractionDigits (0);
		nf0.setMaximumFractionDigits (0);
		nf0.setGroupingUsed (false);
		NumberFormat nf9 = NumberFormat.getInstance ();
		nf9.setMinimumFractionDigits (9);
		nf9.setMaximumFractionDigits (9);
		nf9.setGroupingUsed (false);

		StringBuffer b = new StringBuffer ("MecaGU_");
		b.append (id);
		b.append ("\tD = ");
		b.append (nf2.format (diameter));
		b.append ("\tH = ");
		b.append (nf2.format (height));
		b.append ("\tx = ");
		b.append (nf2.format (xTop));
		b.append ("\ty = ");
		b.append (nf2.format (yTop));
		b.append ("\tz = ");
		b.append (nf2.format (zTop));
		b.append ("\tr = ");
		b.append (nf2.format (radius));
		b.append ("\tS = ");
		b.append (nf6.format (section));
		b.append ("\tI = ");
		b.append (nf9.format (inertia));
		b.append ("\tdv = ");
		b.append (nf0.format (density));
		b.append ("\tE = ");
		b.append (nf0.format (modulus));
		return b.toString ();
	}

//------------------------------------------>
// brouillons !	(drafts...)

	public void setRPCgPrint () {
		MecaVector vxc = new MecaVector (3);
		vxc.setElement (0, xTop);
		vxc.setElement (1, yTop);
		vxc.setElement (2, zTop);
		cg = vxc.copy();
		MecaVector pguTop = new MecaVector (3);
		MecaGU previousGU = previousGU ();

		if (previousGU != null) {
		//if (mecaTree.getId () == 6 && id == 60100) {
			System.out.println ("MecaTree_" + mecaTree.getId () + "  MecaGU_" + id + "  previousGU :\n" +
					"\t id = " + previousGU.getId () );
		//}
			pguTop.setElement (0, previousGU.xTop);
			pguTop.setElement (1, previousGU.yTop);
			pguTop.setElement (2, previousGU.zTop);
		} else {
			pguTop.setElement (0, mecaTree.getX ());
			pguTop.setElement (1, mecaTree.getY ());
			pguTop.setElement (2, mecaTree.getZ ());
		}

		//if (mecaTree.getId () == 6 && id == 60100) {
			System.out.println ("MecaTree_" + mecaTree.getId () + "  MecaGU_" + id + "  vecteur vxc :\n" +
					"\t dim = " + vxc.getDimension () + "\n" +
					"\t vxc(0) = " + vxc.getElement (0) + "\n" +
					"\t vxc(1) = " + vxc.getElement (1) + "\n" +
					"\t vxc(2) = " + vxc.getElement (2) );
		//}

		//if (mecaTree.getId () == 6 && id == 60100) {
			System.out.println ("MecaTree_" + mecaTree.getId () + "  MecaGU_" + id + "  vecteur pguTop :\n" +
					"\t dim = " + pguTop.getDimension () + "\n" +
					"\t pguTop(0) = " + pguTop.getElement (0) + "\n" +
					"\t pguTop(1) = " + pguTop.getElement (1) + "\n" +
					"\t pguTop(2) = " + pguTop.getElement (2) );
		//}

		vxc = vxc.dif (pguTop);

		//if (mecaTree.getId () == 6 && id == 60100) {
			System.out.println ("MecaTree_" + mecaTree.getId () + "  MecaGU_" + id + "  vecteur vxc :\n" +
					"\t dim = " + vxc.getDimension () + "\n" +
					"\t vxc(0) = " + vxc.getElement (0) + "\n" +
					"\t vxc(1) = " + vxc.getElement (1) + "\n" +
					"\t vxc(2) = " + vxc.getElement (2) );
		//}

		length = vxc.norm ();
		if (length != 0) {
			vxc = vxc.div (length);
		}

		else /*if (mecaTree.getId () == 6 && id == 60100)*/ {
				System.out.println ("MecaTree_" + mecaTree.getId () + "  MecaGU_" + id + "  longueur nulle");
		}

		cg = cg.sum (pguTop);
		cg = cg.div (2);

		MecaVector vyc = new MecaVector (3);
		MecaVector vzc = new MecaVector (3);
		double norm;
		if (vxc.getElement (2) == 1d) {
			// when Z and x are parallels : not leaning plane
			// then ZX plane contains y
			vyc.setElement (0, 1d);
			vzc.setElement (1, 1d);
		} else {
			// when Z and x are not parallels : leaning plane Zx (vertical)
			// then Zx plane contains y
			MecaVector Z = new MecaVector (3);
			Z.setElement (2, 1d);
			vzc = Z.cross (vxc);
			norm = vzc.norm ();
			if (norm != 0) {
				vzc = vzc.div (norm);
			}
			vyc = vxc.rot3D (vzc, Math.PI / 2);
		}

		R.setColumn (0, vxc);
		R.setColumn (1, vyc);
		R.setColumn (2, vzc);
		P.setSubMatrix (0, 0, R);
		P.setSubMatrix (3, 3, R);
	}

	/*public void setMatrices (double windVelocity)
	//public void setMatrices (double windIntensity)
	{
		boolean computeStress = false;
		//setRPCg (); //fait dans le constructeur...
		setG (computeStress);
		setHU ();

		if (windVelocity >= 0d) {
			setSWind (windVelocity);
		} else {
			setSWeight ();
		}
		if (windVelocity >= 0d) {
			setDlWind (computeStress, windVelocity);
		} else {
			setDlWeight (computeStress);
		}
	}

	public void setMatricesForStress (double windVelocity)
	{
		boolean computeStress = true;
		setG (computeStress);
		if (windVelocity >= 0d) {
			setDlWind (computeStress, windVelocity);
		} else {
			setDlWeight (computeStress);
		}
	}*/

	/*private void setSWind (double windVelocity) {
		if (windVelocity == 0d) {
			s = new MecaVector (6);
		} else {
			double vonKarman = 0.41;
			//double Zo = 0.20;
			double hDom = mecaTree.getMecaProcess ().getDominantHeight ();
			double Zo = mecaTree.getMecaProcess ().getConstraints ().dLog * hDom;
			double alpha = mecaTree.getMecaProcess ().getConstraints ().aExp;

			double Uz = 0d;
			if (this == mecaTree.lastGU ()) {
				if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "log") {
					Uz = (windVelocity / vonKarman) * Math.log ((zTop - mecaTree.getZ ()) / Zo);
				}
				if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "exp") {
					Uz = windVelocity * Math.exp (alpha * (((zTop - mecaTree.getZ ()) / hDom) - 1));
				}
				mecaTree.setWindSpeedTop (3.6 * Uz);
			}

			double ro = 1.2226;
			double CdC = mecaTree.getCrownDragCoefficient ();

			double Fz;// vent sur houppier
			if (zTop <= (mecaTree.getZ () + mecaTree.getCrownBaseHeight ())) {
				Fz = 0d;
		 	} else {
				if (this != mecaTree.firstGU ()) {
					int ngu = id - mecaTree.getId () * 10000;
					ngu = ngu / 100 - 1;
					double crownRadius = mecaTree.CrownRadiusAt (ngu);

					double ACz = height * (2 * crownRadius);	// vent sur surface non déformée !
					double ATz = height * (2 * radius); // force sur UC non déformée
					ACz -= ATz;
					mecaTree.setCrownArea (ACz);

					double VCz = height * (Math.PI * Math.pow (crownRadius,2));
					double VTz = height * (Math.PI * Math.pow (radius,2));
					VCz -= VTz;
					mecaTree.setCrownVolume (VCz);

					double zTopPrev = previousGU ().getZTop ();
					if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "log") {
						Uz = (windVelocity / vonKarman) * Math.log ((((zTop + zTopPrev) / 2.0) - mecaTree.getZ ()) / Zo);
					}
					if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "exp") {
						Uz = windVelocity * Math.exp (alpha * (((((zTop + zTopPrev) / 2.0) - mecaTree.getZ ()) / hDom) - 1));
					}
					if (Uz < 0d) {
						Uz = 0d;
					}

					double St;
					if (Uz <= 11) {
						St = 0.8;
					} else if (Uz > 11 && Uz <= 20) {
						St = (10d / Uz) - 0.1;
					} else {
						St = 0.4;
					}
					ACz *= St;
					Fz = 0.5 * CdC * ro * ACz * Math.pow (Uz, 2);
				} else {
					Fz = 0d;
				}
			}

			MecaVector sGlobal = new MecaVector (6);
			sGlobal.setElement (0, Fz);

			MecaMatrix PTGU = getP().transpose ();
			// on passe s en local courant !
			s = PTGU.pro (sGlobal);
		}

		//Log.print ("GU: " + id + "\tUz =\t" + Uz + "\tat z =\t" + zTop + "\n");
		//Log.print ("windVelocity =\t" + windVelocity + "\tvonKarman =\t" + vonKarman + "\n");
		//Log.print ("zTop =\t" + zTop + "\tZo =\t" + Zo + "\n");
		//Log.print ("(windVelocity / vonKarman) =\t" + (windVelocity / vonKarman) + "\tMath.log (zTop / Zo) =\t" + (Math.log (zTop / Zo)) + "\n");
		//Log.print ("Uz GU " + id + "\t" + Uz + "\n");
		//Log.print ("ATz GU " + id + "\t" + ATz + "\n");
		//Log.print ("(zTop - mecaTree.getZ ()) GU " + id + "\t" + (zTop - mecaTree.getZ ()) + "\n");
		//Log.print ("((zTop - mecaTree.getZ ()) - (height / 2d)) GU " + id + "\t" + ((zTop - mecaTree.getZ ()) - (height / 2d)) + "\n");
		//Log.print ("crownRadius GU " + id + "\t" + crownRadius + "\n");
		//Log.print ("ACz GU " + id + "\t" + ACz + "\n");
		//Log.print ("Fz GU " + id + "\t" + Fz + "\n");
		//Log.print ("Uz GU " + id + "\n" + sGlobal.bigSimpleString () + "\n");
			//Log.print ("PTNextGU GU " + id + "\n" + PTNextGU.bigComplexString () + "\n");
		//Log.print ("s GU " + id + "\n" + s.bigSimpleString () + "\n");
	}

	private void setSWeight () {
		if (zTop <= (mecaTree.getZ () + mecaTree.getCrownBaseHeight ())) {
			s = new MecaVector (6);
		} else {
			if (this != mecaTree.firstGU ()) {
				int ngu = id - mecaTree.getId () * 10000;
				ngu = ngu / 100 - 1;
				double crownRadius = mecaTree.CrownRadiusAt (ngu);
				double VCz = height * (Math.PI * Math.pow (crownRadius,2)); // poids sur volume non déformé !
				double VTz = height * (Math.PI * Math.pow (radius,2)); // force sur volume non déformé
				VCz -= VTz;

				double Fz = crownDensity * VCz * 9.81;
				MecaVector sGlobal = new MecaVector (6);
				sGlobal.setElement (2, -Fz);
				MecaMatrix PTGU = getP().transpose ();
				// on passe s en local courant !
				s = PTGU.pro (sGlobal);
			} else {
				s = new MecaVector (6);
			}
		}
	}

	private void setDlWind (boolean computeStress, double windVelocity)// vent sur tronc
	{
		double vonKarman = 0.41;
		//double Zo = 0.20;
		double hDom = mecaTree.getMecaProcess ().getDominantHeight ();
		double Zo = mecaTree.getMecaProcess ().getConstraints ().dLog * hDom;
		double alpha = mecaTree.getMecaProcess ().getConstraints ().aExp;
		double zTopPrev;
		if (this == mecaTree.firstGU ()) {
			zTopPrev = mecaTree.getZ ();
		} else {
			zTopPrev = previousGU ().getZTop ();
		}

		double Uz = 0d;
		if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "log") {
			Uz = (windVelocity / vonKarman) * Math.log ((((zTop + zTopPrev) / 2.0) - mecaTree.getZ ()) / Zo);
		}
		if (mecaTree.getMecaProcess ().getConstraints ().windProfile == "exp") {
			Uz = windVelocity * Math.exp (alpha * (((((zTop + zTopPrev) / 2.0) - mecaTree.getZ ()) / hDom) - 1));
		}

		if (Uz < 0d) {
			Uz = 0d;
		}
		double ro = 1.2226;
		double CdS = mecaTree.getCrownDragCoefficient ();

		double ATz = height * (2 * radius); // force sur UC non déformée
		double Fz = 0.5 * CdS * ro * ATz * Math.pow (Uz, 2);

		lineicWeight = Fz / length; // ici le vent est le premier chargement :
		// length = height car la structure n'est pas encore déformée...

		double l, l2, l3, l4, ea, ei;
		if(computeStress) {
			double computeStressLevel = 1d / 2d;
			l = length * computeStressLevel;
		} else {
			l = length;
		}
		l2 = l*l;
		l3 = l*l2;
		l4 = l*l3;
		ea = modulus * section;
		ei = modulus * inertia;

		MecaVector dlg = new MecaVector (3);
		dlg.setElement (0, lineicWeight);
		MecaVector dll= new MecaVector (3);
		MecaMatrix RT = R.transpose ();
		dll = RT.pro (dlg);

		dl.setElement (0, (-(l2 / (2 * ea)) * dll.getElement (0)));
		dl.setElement (1, ((l4 / (24 * ei)) * dll.getElement (1)));
		dl.setElement (2, ((l4 / (24 * ei)) * dll.getElement (2)));
		dl.setElement (4, (-(l3 / (6 * ei)) * dll.getElement (2)));
		dl.setElement (5, ((l3 / (6 * ei)) * dll.getElement (1))) ;
		dl.setElement (6, (-l * dll.getElement (0)));
		dl.setElement (7, (-l * dll.getElement (1)));
		dl.setElement (8, (-l * dll.getElement (2)));
		dl.setElement (10, (-(l2 / 2) * dll.getElement (2)));
		dl.setElement (11, ((l2 / 2) * dll.getElement (1)));
	}

	private void setDlWeight (boolean computeStress)// poids du tronc
	{
		lineicWeight = density * section * 9.81; // for total weight of GU !
		lineicWeight *= height; // poids réel de l'UC non déformée
		lineicWeight /= length;	// poids linéique rapporté sur l'UC déformée allongée !

		double l, l2, l3, l4, ea, ei;
		if(computeStress) {
			double computeStressLevel = 1d / 2d;
			l = length * computeStressLevel;
		} else {
			l = length;
		}
		l2 = l*l;
		l3 = l*l2;
		l4 = l*l3;
		ea = modulus * section;
		ei = modulus * inertia;

		MecaVector dlg = new MecaVector (3);
		dlg.setElement (2, -lineicWeight);
		MecaVector dll= new MecaVector (3);
		MecaMatrix RT = R.transpose ();
		dll = RT.pro (dlg);

		dl.setElement (0, (-(l2 / (2 * ea)) * dll.getElement (0)));
		dl.setElement (1, ((l4 / (24 * ei)) * dll.getElement (1)));
		dl.setElement (2, ((l4 / (24 * ei)) * dll.getElement (2)));
		dl.setElement (4, (-(l3 / (6 * ei)) * dll.getElement (2)));
		dl.setElement (5, ((l3 / (6 * ei)) * dll.getElement (1))) ;
		dl.setElement (6, (-l * dll.getElement (0)));
		dl.setElement (7, (-l * dll.getElement (1)));
		dl.setElement (8, (-l * dll.getElement (2)));
		dl.setElement (10, (-(l2 / 2) * dll.getElement (2)));
		dl.setElement (11, ((l2 / 2) * dll.getElement (1)));
	}*/

//------------------------------------------>
// fin class !
}





