/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.model;

import java.io.Serializable;

/**
 * Pedologic SOIL LAYERS description
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeLayer implements Serializable {

	/** Layer id  */
	private int id;
	/** Depth from surface (m)  */
	private double surfaceDepth;  		
	/** Thickness (m)  */
	private double thickness;  			
	/** Percentage of sand (%)   */
	private double sand;				
	/**  Percentage of silt (%)  */
	private double silt;				
	/** Percentage of clay (%)  */
	private double clay;				
	/**  Percentage of limestone (%)  */
	private double limestone;		
	/**  Percentage of organic matter (%)  */
	private double organicMatter;		
	/**  Particle size of sand  (micrometers) */
	private double particleSizeSand;	
	/**  Percentage of stones  (%) */
	private double stone; 				
	/**  Stones type 1=limestone B1, 2=limestone B2, 3=limestone L, 4=scree L, 5=gravel m, 6=flint, 7=granite a, 8=limestone J, 9=other1, 10=other2 */
	private int    stoneType; 
	/**  Infiltrability rate at the base of the layer (mm day-1) */
	private double infiltrability; 	
	/**  Total saturated porosity (m3 water m-3 soil) */
	private double thetaSat;		
	/**  Bulk density (fine soil+stones) (kg m-3) */
	private double bulkDensity;		
	/**  Bulk density (fine soil) (kg m-3) */
	private double bulkDensityFineSoil;		
	/**  Field capacity (fine soil+stones) (m3 m-3) */
	private double fieldCapacity;	
	/**  Field capacity (fine soil) (m3 m-3) */
	private double fieldCapacityFineSoil;	
	/**  Field capacity (stones) (m3 m-3) */
	private double fieldCapacityStone;	
	/**  Wilting point (fine soil+stones) (m3 m-3) */
	private double wiltingPoint;		
	/**  Wilting point (fine soil) (m3 m-3) */
	private double wiltingPointFineSoil;		
	/**  Wilting point (stones) (m3 m-3) */
	private double wiltingPointStone;		
	
	/**  Saturated conductivity	(cmm day-1) */
	private double kSat;
	private double alpha;
	private double lambda;
	private double n;
	
	/**
	 * Constructor
	 */
	public SafeLayer (int id) {
		this.id = id;
	}
	
	/**
	 * Constructor
	 *
	 * @param id Layer id
	 * @param surfaceDepth Depth from surface (m)
	 * @param thickness Thickness (m)
	 * @param sand Percentage of sand (%) 
	 * @param clay Percentage of clay (%) 
	 * @param limestone Percentage of limestone (%) 
	 * @param organicMatter Percentage of organicMatter (%) 
	 * @param particleSizeSand Particle size of sand (micrometers) 
	 * @param stonePercent Percentage of stones (%)  
	 * @param stoneType Type of stones 1=limestone B1, 2=limestone B2, 3=limestone L, 4=scree L, 5=gravel m, 6=flint, 7=granite a, 8=limestone J, 9=other1, 10=other2
	 * @param infiltrability Infiltrability rate at the base of the layer (mm day-1)
	 * @param generalParameters Reference to SafeGeneralParameters object
	 */
	public SafeLayer (int id, double surfaceDepth, double thickness,  
						double sand, double clay,  double limestone,
						double organicMatter, double particleSizeSand,
						double stonePercent, int stoneType, double infiltrability,
						SafeSoil soil,
						SafeGeneralParameters generalParameters) {

		this.id = id;
		this.surfaceDepth = surfaceDepth;
		this.thickness 	 =  thickness;
		this.silt = 100 - sand - clay;
		this.sand = sand;
		this.clay = clay;
		this.particleSizeSand = particleSizeSand;
		this.organicMatter = organicMatter;
		this.limestone = limestone;
		this.infiltrability = infiltrability;

		//TOPSOIL TYPES
		int topSoil = 0;
		if (id == 0) topSoil = 1;

		//Initialisation of soil pedoTransfert PROPERTIES (without stones)
		this.bulkDensityFineSoil = 	SafePedotransferUtil.getBulkDensity (
							particleSizeSand,
							clay,
							silt,
							organicMatter,
							topSoil, 
							soil);
		this.thetaSat =	SafePedotransferUtil.getThetaSat (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil,
							soil);

		this.kSat =	SafePedotransferUtil.getKSat (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil,
							soil);
		this.alpha = SafePedotransferUtil.getAlpha (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil,
							soil);
		this.lambda = SafePedotransferUtil.getLambda (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							soil);
		this.n = SafePedotransferUtil.getN (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil,
							soil);

		//field capacity
		double p = SafePedotransferUtil.getP (generalParameters.PF_FIELD_CAPACITY);
		this.fieldCapacityFineSoil = SafePedotransferUtil.getTheta (p, thetaSat, alpha, n);

		
		//wilting point
		p = SafePedotransferUtil.getP (generalParameters.PF_WILTING_POINT);
		this.wiltingPointFineSoil = SafePedotransferUtil.getTheta (p, thetaSat, alpha, n);

		//If stone, calculation of fine soil properties
		// it was decided to let other variables : ksat, alpha, lambda and n unchanged 
		// This code have been copied and removed from STICS InitialGeneral (Initial.c line 165 to 175) 
		this.stoneType  =  stoneType;
		
		if (stoneType > 0) {

			this.stone   	= stonePercent;
			
			this.fieldCapacityStone = generalParameters.STONE_VOLUMIC_DENSITY [this.stoneType-1] * generalParameters.STONE_WATER_CONTENT [this.stoneType-1];    // % 
			
			this.wiltingPointStone =  this.fieldCapacityStone * this.wiltingPointFineSoil / this.fieldCapacityFineSoil;

			
			this.bulkDensity  = (generalParameters.STONE_VOLUMIC_DENSITY [this.stoneType-1] * this.stone 
								+ (100 - this.stone) * this.bulkDensityFineSoil) 
								/ 100;
 	
			this.fieldCapacity = (this.fieldCapacityStone * this.stone
								+ (100 - this.stone) * this.fieldCapacityFineSoil)
								/ 100;
			
			this.wiltingPoint = (this.wiltingPointStone * this.stone
								+ (100 - this.stone) * this.wiltingPointFineSoil)
								 / 100;
											
			//to avoid STICS stop error
			if ((this.wiltingPoint/this.bulkDensity) < 0.01)
						this.wiltingPoint = 0.01 * this.bulkDensity;

			
		}
		else
		{
			this.stone   			= 0;
			this.bulkDensity 		= this.bulkDensityFineSoil;
			this.wiltingPoint 		= this.wiltingPointFineSoil;
			this.fieldCapacity 		= this.fieldCapacityFineSoil;
			this.wiltingPointStone 	= 0;
			this.fieldCapacityStone = 0;			
		}
	}
	/**
	 * Return the layer id 
	 **/	
	public int getId () {return id;}
	/**
	 * Return the layer depth from surface (m)  
	 **/	
	public double getSurfaceDepth () {return surfaceDepth;}
	/**
	 * Return the layer Thickness (m)
	 **/	
	public double getThickness () {return thickness;}
	/**
	 * Return the layer sand (%)
	 **/	
	public double getSand () {return sand;}
	/**
	 * Return the layer silt (%)
	 **/	
	public double getSilt () {return silt;}
	/**
	 * Return the layer clay (%)
	 **/	
	public double getClay () {return clay;}
	/**
	 * Return the layer limestone (%)
	 **/	
	public double getLimestone () {return limestone;}
	/**
	 * Return the layer organic matter (%)
	 **/	
	public double getOrganicMatter () {return organicMatter;}
	/**
	 * Return the layer particle size of sand  (micrometers)
	 **/	
	public double getParticleSizeSand() {return particleSizeSand;}
	/**
	 * Return the percentage of stones  (%)
	 **/	
	public double getStone () {return stone;}
	/**
	 * Return the layer stones type 1=limestone B1, 2=limestone B2, 3=limestone L, 4=scree L, 5=gravel m, 6=flint, 7=granite a, 8=limestone J, 9=other1, 10=other2 
	 **/	
	public int 	  getStoneType () {return stoneType;}
	/**
	 * Return the layer infiltrability rate at the base of the layer (mm day-1)
	 **/	
	public double getInfiltrability () {return infiltrability;}
	/**
	 * Return the layer residual humidity 
	 **/	
	public double getResidualHumidity() {return silt/100/15;}
	/**
	 * Return the layer saturated conductivity	(cm day-1)
	 **/
	public double getKSat () {return kSat;}
	/**
	 * Return the layer alpha
	 **/	
	public double getAlpha () {return alpha;}
	/**
	 * Return the layer lambda
	 **/	
	public double getLambda () {return lambda;}
	/**
	 * Return the layer N
	 **/	
	public double getN () {return n;}
	/**
	 * Return the layer bulk density (fine soil+stones) (kg m-3)
	 **/	
	public double getBulkDensity () {return bulkDensity;}
	/**
	 * Return the layer bulk density (fine soil) (kg m-3)
	 **/	
	public double getBulkDensityFineSoil () {return bulkDensityFineSoil;}
	/**
	 * Return the layer field capacity (fine soil+stones) (m3 m-3)
	 **/	
	public double getFieldCapacity() {return fieldCapacity;}
	/**
	 * Return the layer field capacity (fine soil) (m3 m-3)
	 **/	
	public double getFieldCapacityFineSoil() {return fieldCapacityFineSoil;}
	/**
	 * Return the layer field capacity (stones) (m3 m-3)
	 **/	
	public double getFieldCapacityStone() {return fieldCapacityStone;}
	/**
	 * Return the layer wilting point (fine soil+stones) (m3 m-3)
	 **/	
	public double getWiltingPoint() {return wiltingPoint;}
	/**
	 * Return the layer wilting point (fine soil) (m3 m-3)
	 **/	
	public double getWiltingPointFineSoil() {return wiltingPointFineSoil;}
	/**
	 * Return the layer wilting point (stones) (m3 m-3)
	 **/	
	public double getWiltingPointStone() {return wiltingPointStone;}
	/**
	 * Return the layer thetaSat (Total saturated porosity) in m3 water m-3 soil
	 **/	
	public double getThetaSat () {return thetaSat;}
	/**
	 * Return the layer theta in m3 water m-3 soil
	 **/	
	public double getTheta(double p){			
		return(SafePedotransferUtil.getTheta(p,this.getThetaSat(),this.getAlpha(),this.getN()));		
	}
	
}
