package safe.model;

import java.io.Serializable;


/**
 * Temporary object to store and sort data attached to a voxel and a plant
 * This is used by waterRepartion ICRAF module
 *
 * @author Isabelle Lecomte - November 2004
 */
public class SafeRootVoxel implements Serializable, Comparable   {

	private SafeFineRoot fineRoot;				//plant (tree or crop) fine root reference address
	private SafeVoxel voxel;					//rooted voxel reference address
	private float rootDensity;					//plant (tree or crop) root density (m m-3)
	private float waterRhizospherePotential;	//Voxel rhizosphere water potential (cm)
	private float phiPf;						//cm2 day-1
	private float waterUptakePotential;			//liters
	private float nitrogenShareUptake;			//ND
	private float nitrogenZeroSinkPotential;	//g
	private float nitrogenUptakePotential;		//g

	public SafeRootVoxel (SafeFineRoot root, SafeVoxel voxel, double rootDensity, double rhizospherePotential, double phiPf) {
		this.fineRoot = root;
		this.voxel = voxel;
		this.rootDensity = (float) rootDensity;
		this.waterRhizospherePotential= (float) rhizospherePotential;
		this.phiPf= (float) phiPf;
		this.waterUptakePotential = 0;
		this.nitrogenShareUptake = 0;
		this.nitrogenZeroSinkPotential  = 0;
		this.nitrogenUptakePotential = 0;
	}

	public SafeFineRoot getFineRoot () {return fineRoot;}
	public SafeVoxel getVoxel () {return voxel;}
	public double getRootDensity () {return (double) rootDensity;}
	public double getWaterRhizospherePotential () {return (double) waterRhizospherePotential;}
	public double getPhiPf () {return (double) phiPf;}
	public double getWaterUptakePotential () {return (double) waterUptakePotential;}
	
	public void addWaterUptakePotential (double v) {waterUptakePotential += (float) v;}


	public double getNitrogenShareUptake() {return (double) nitrogenShareUptake;}
	public double getNitrogenZeroSinkPotential () {return (double) nitrogenZeroSinkPotential;}
	public double getNitrogenUptakePotential () {return (double) nitrogenUptakePotential;}
	public void setNitrogenShareUptake (double v) {nitrogenShareUptake = (float)  v;}
	public void setNitrogenZeroSinkPotential (double v) {nitrogenZeroSinkPotential = (float)  v;}
	public void addNitrogenUptakePotential (double v) {nitrogenUptakePotential += (float)  v;}


	public int compareTo (Object other) {
	  double nombre1 = ((SafeRootVoxel) other).getPhiPf();
	  double nombre2 = this.getPhiPf();
	  if (nombre1 > nombre2)  return -1;		// gt - 12.11.2009 - ">" instead of "<" : sort from low phipf to large phipf instead of the contrary
	  else if(nombre1 == nombre2) return 0;
	  else return 1;
	}

	public String toString(){
		String str = "";
		str = "RootVoxel  voxel="+voxel.getId()+" z="+voxel.getZ()+" fR="+rootDensity+" phiPf="+phiPf+" Rhizo="+waterRhizospherePotential;
		return str;
	}
	
}
