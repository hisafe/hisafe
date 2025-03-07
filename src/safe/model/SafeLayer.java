/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
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
import java.util.ArrayList;
import java.util.Iterator;

/**
 * Pedologic layers description
 *
 * @author Isabelle Lecomte - INRAE Montpellier - July 2002
 */
public class SafeLayer implements Serializable {


	private static final long serialVersionUID = 1L;
	private int id;
	private int topSoil;				// 1=topsoil 0=subsoil

	private double surfaceDepth;  		// m
	private double thickness;  			// m
	private double sand;				// %
	private double silt;				// %
	private double clay;				// %
	private double limestone;			// %
	private double organicMatter;		// %
	private double medianPartSizeSand;	// %

	private int    stoneType; 			// 0-10
	private double stone; 				// %
	private double infiltrability; 		// mm j-1
	private double thetaSat;			// m3 m-3
	
	//Pedotransfert properties (total voxel fine soil and stone)
	private double bulkDensity;			//kg m-3
	private double fieldCapacity;		//m3 m-3
	private double wiltingPoint;		//m3 m-3
	

	//Fine soil pedotransfert properties (fine soil only)
	private double bulkDensityFineSoil;			//kg m-3
	private double fieldCapacityFineSoil;		//m3 m-3
	private double wiltingPointFineSoil;		//m3 m-3

	//Fine soil pedotransfert properties (stones)
	private double fieldCapacityStone;		//m3 m-3
	private double wiltingPointStone;		//m3 m-3
	
	
	//other pedotransfert properties
	private double kSat;
	private double alpha;
	private double lambda;
	private double n;
	
	//List of voxels for this layer
	private ArrayList<SafeVoxel> voxelList;
	
	public SafeLayer (int id) {
		this.id = id;
	}
	
	public SafeLayer (int id, double surfaceDepth, double thickness,  double sand, double clay,  double limestone,
						double organicMatter, double medianPartSizeSand,
						double stonePercent, int stoneType, double infiltrability,
						SafeGeneralParameters safeSettings) {

		this.id = id;
		this.surfaceDepth = surfaceDepth;
		this.thickness 	 =  thickness;
		this.silt = 100 - sand - clay;
		this.sand = sand;
		this.clay = clay;
		this.medianPartSizeSand = medianPartSizeSand;
		this.organicMatter = organicMatter;
		this.limestone = limestone;
		this.infiltrability = infiltrability;

		//TOPSOIL TYPES
		this.topSoil = 0;
		if (id == 0) this.topSoil = 1;

		//Initialisation of soil pedoTransfert PROPERTIES (without stones)
		this.bulkDensityFineSoil = 	SafePedotransferUtil.getBulkDensity (
							medianPartSizeSand,
							clay,
							silt,
							organicMatter,
							topSoil);
		this.thetaSat =	SafePedotransferUtil.getThetaSat (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil);
		this.kSat =	SafePedotransferUtil.getKSat (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil);
		this.alpha = SafePedotransferUtil.getAlpha (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil);
		this.lambda = SafePedotransferUtil.getLambda (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter);
		this.n = SafePedotransferUtil.getN (
							clay,
							bulkDensityFineSoil,
							silt,
							organicMatter,
							topSoil);

		//field capacity
		double p = SafePedotransferUtil.getP (safeSettings.PF_FIELD_CAPACITY);
		this.fieldCapacityFineSoil = SafePedotransferUtil.getTheta (p, thetaSat, alpha, n);

		//wilting point
		p = SafePedotransferUtil.getP (safeSettings.PF_WILTING_POINT);
		this.wiltingPointFineSoil = SafePedotransferUtil.getTheta (p, thetaSat, alpha, n);


		//If stone, calculation of fine soil properties
		// it was decided to let other variables : ksat, alpha, lambda and n unchanged 
		// This code have been copied and removed from STICS InitialGeneral (Initial.c line 165 to 175) 
		this.stoneType  =  stoneType;
		
		if (stoneType > 0) {

			this.stone   	= stonePercent;
			
			this.fieldCapacityStone = safeSettings.STONE_VOLUMIC_DENSITY [this.stoneType-1] * safeSettings.STONE_WATER_CONTENT [this.stoneType-1];    // % 
			
			this.wiltingPointStone =  this.fieldCapacityStone * this.wiltingPointFineSoil / this.fieldCapacityFineSoil;

			
			this.bulkDensity  = (safeSettings.STONE_VOLUMIC_DENSITY [this.stoneType-1] * this.stone 
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

	public int 	  getId () {return id;}
	public int 	  getStoneType () {return stoneType;}
	public double getSurfaceDepth () {return surfaceDepth;}
	public double getThickness () {return thickness;}
	public double getSand () {return sand;}
	public double getSilt () {return silt;}
	public double getClay () {return clay;}
	public double getLimestone () {return limestone;}
	public double getOrganicMatter () {return organicMatter;}
	public double getMedianPartSizeSand () {return medianPartSizeSand;}
	
	
	
	public double getStone () {return stone;}
	public double getInfiltrability () {return infiltrability;}
	public double getResidualHumidity() {return silt/100/15;}
	public double getKSat () {return kSat;}
	public double getAlpha () {return alpha;}
	public double getLambda () {return lambda;}
	public double getN () {return n;}
	
	//voxel values (fine soil + stone) 
	public double getBulkDensity () {return bulkDensity;}
	public double getFieldCapacity() {return fieldCapacity;}
	public double getWiltingPoint() {return wiltingPoint;}
	public double getThetaSat () {return thetaSat;}
	
	
	//voxel values (fine soil only) 
	public double getBulkDensityFineSoil () {return bulkDensityFineSoil;}
	public double getFieldCapacityFineSoil() {return fieldCapacityFineSoil;}
	public double getWiltingPointFineSoil() {return wiltingPointFineSoil;}

	//voxel values (stone only) 
	public double getFieldCapacityStone() {return fieldCapacityStone;}
	public double getWiltingPointStone() {return wiltingPointStone;}
	
	
	// gt - 12.11.2009 - added this method used in SafeVoxel.countWaterUptakePotential
	public double getTheta(double p){			
		return(SafePedotransferUtil.getTheta(p,this.getThetaSat(),this.getAlpha(),this.getN()));		
	}
	
	public void razVoxel() {
		this.voxelList = new ArrayList<SafeVoxel> ();
	}
	public void addVoxel (SafeVoxel v) {
		if (this.voxelList == null)
			this.voxelList = new ArrayList<SafeVoxel>  ();
		voxelList.add(v);
	}

	public int getNbVoxels () {
		return voxelList.size();
	}
	public double getLayerVolume () {
		return this.thickness * getNbVoxels ();
	}
	
	public double getLayerEvaporation () {
		double evaporation = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			evaporation = evaporation + v.getEvaporation();
		}
		return  evaporation;
	}
	
	public double getLayerWaterStock () {
		double waterStock = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			waterStock = waterStock + v.getWaterStock();
		}
		return waterStock;
	}

	public double getLayerNitrogenNo3Stock () {
		double nitrogenNo3Stock = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			nitrogenNo3Stock = nitrogenNo3Stock + v.getNitrogenNo3Stock();
		}
		return nitrogenNo3Stock;
	}

	public double getLayerNitrogenNh4Stock () {
		double nitrogenNh4Stock = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			nitrogenNh4Stock = nitrogenNh4Stock + v.getNitrogenNh4Stock();
		}
		return nitrogenNh4Stock;
	}
	
	public double getLayerTreeRootDensity () {
		double treeRootDensity = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			treeRootDensity = treeRootDensity + v.getTotalTreeRootsDensity();
		}
		return treeRootDensity;
	}

	public double getLayerCropRootDensity () {
		double cropRootDensity = 0;
		Iterator<SafeVoxel>  itr = voxelList.iterator();
		while (itr.hasNext()) {
			SafeVoxel v = itr.next();
			cropRootDensity = cropRootDensity + v.getCropRootsDensity();
		}
		return cropRootDensity;
	}
	
	//for export
	public int getIdLayer() {return getId();}
}
