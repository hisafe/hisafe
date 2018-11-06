package safe.model;

import java.io.Serializable;
import safe.stics.SafeSticsItk;

/**
 * Crop species parameters HISAFE specific
 * STICS parameters are in safe.stics.SafeSticsCrop
 *
 * @author Isabelle Lecomte - July 2002
 */
public class SafeCropSpecies implements Serializable, Cloneable {	// fc - 29.7.2004 - EnumProperty

	public SafeSticsItk sticsItk;			//Crop intervention in STICS
	
	private boolean isInitialized;			//STICS initialisation is done
	private SafeCell firstCell;				//First cell planted with this crop species
	private String name; 					//name of crop species
	private String fileName; 				//name of crop species file name

	/*------- WATER REPARTITION PARAMETERS --------------------------------*/
	private double cropRootDiameter;			//cm

	//For calculating the transpiration reduction factor following Campbell
	private double cropAlpha;
	private double cropMinTranspirationPotential;		//cm
	private double cropMaxTranspirationPotential;		//cm

	//Root axial conductance (1/resistance involved in water transport inside the root per unit gradient in water potential and per unit path-length)
	//Unit should be here kg s-1 cm-1, or if the flux is expressed in cm, cm cm-1
	//According to Tyree, root axial conductance is higher for large roots
	private double cropRootConductivity;				//cm cm-1

	//Potential drop needed to enter the root expressed as a % of soil water potential
	private double cropBufferPotential;					//cm

	//Longitudinal resistance factor for root sap
	private double cropLongitudinalResistantFactor;		//mm.cm-1.m-1


	/**	Constructor
	*/
	public SafeCropSpecies ()   {
		this.isInitialized = false;
	}

	public SafeSticsItk getSticsItk() {
		return sticsItk;
	}
	public String getName () {return name;}
	public String getFileName () {return fileName;}
	public boolean getIsInitialized () {return isInitialized;}
	public SafeCell getFirstCell () {return firstCell;}
	public void setIsInitialized (boolean b) {isInitialized = b;}
	public void setFirstCell(SafeCell c) {firstCell = c;}

	/** WATER REPARTITION PARAMETERS **/
	public double getCropRootDiameter() {return cropRootDiameter;}
	public double getCropAlpha() {return cropAlpha;}
	public double getCropRootConductivity() {return cropRootConductivity;}
	public double getCropMaxTranspirationPotential() {return cropMaxTranspirationPotential;}
	public double getCropMinTranspirationPotential() {return cropMinTranspirationPotential;}
	public double getCropBufferPotential() {return cropBufferPotential;}
	public double getCropLongitudinalResistantFactor() {return cropLongitudinalResistantFactor;}


	//TO FORCED PARAMETERS VALUES
	public void setName (String v) {name = v;}
	public void setFileName (String v) {fileName = v;}
	public void setCropRootDiameter (double v) {cropRootDiameter = v;}
	public void setCropAlpha (double v) {cropAlpha = v;}
	public void setCropRootConductivity(double v) {cropRootConductivity = v;}
	public void setCropMaxTranspirationPotential (double v) {cropMaxTranspirationPotential = v;}
	public void setCropMinTranspirationPotential (double v) {cropMinTranspirationPotential = v ;}
	public void setCropBufferPotential(double v) {cropBufferPotential = v;}
	public void setCropLongitudinalResistantFactor(double v) {cropLongitudinalResistantFactor = v;}

	/**
	 * return Campbell factor  (dimensionless)
	 * ICRAF method
	 */
	public double getCampbellFactorIcraf() {
		return (2 * Math.log (cropAlpha / (1 - cropAlpha))
				/ Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}
	
	/**
	 * return Campbell factor (dimensionless) 
	 * NOT USED
	 */
	public double getCampbellFactor (double plantWaterPotential) {
		double halfCurrWaterPotential= getHalfCurrWaterPotential();
		double a = getA();
		return 1.0/(1.0+Math.pow(plantWaterPotential/halfCurrWaterPotential,a));
	}
	
	/**
	*  return water potential where tranpiration demand is half of its potential
	*  ICRAF method
	*/
	public double getHalfCurrWaterPotentialIcraf() {
			return (cropMaxTranspirationPotential * Math.pow ((1 - cropAlpha) / cropAlpha, 1 / getCampbellFactorIcraf()));
	}

	/**
	 * return water potential where tranpiration demand is half of its potential 
	 * NOT USED
	 */
	public double getHalfCurrWaterPotential() {
			return -Math.sqrt (cropMaxTranspirationPotential * cropMinTranspirationPotential);
	}
	

	public double getA() {
			return (2.0 * Math.log (cropAlpha / (1 - cropAlpha))
					   / Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}

}


