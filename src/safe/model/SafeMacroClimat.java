/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Gr�goire Talbot 1, Nicolas Barbault 1, 
 * Gr�goire Vincent 2 , Rachmat Mulia 3, Fran�ois Bussi�re 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Herv� Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Universit� Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d��tude de la for�t, Universit� du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d�H�res, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import capsis.defaulttype.plotofcells.PlotOfCells;
import safe.stics.SafeSticsClimat;

/**
 * MACRO CLIMAT parameters 
 * Weather data for a simulation  (day by day)
 * Each day is an instance of SafeDailyClimat
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */

public class SafeMacroClimat  implements  Serializable {

	/** Calendar to convert dates */
	private static  GregorianCalendar calendar;
	/** Map of weather data */
	private Map<String, SafeDailyClimat> weather;		
	/** Map of max water table depth */
	private Map<Integer, Integer> maxWaterTable;
	/** Beams set (created once) mixing diffuse and direct information */
	private SafeBeamSet<SafeBeam> beamSet;				
	/** Direct beams set  (created at each direct lighting process) in case DirectLightMethod=True */
	private SafeBeamSet<SafeBeam> directBeamSet;			

	/**	
	 *	Constructor 
	 */
	public SafeMacroClimat () {
		weather = new Hashtable<String, SafeDailyClimat> ();
		maxWaterTable = new Hashtable<Integer, Integer> ();
	}

	/**
	 * Return if the given year is leap year.
     * @param year  the four digit year
     * @return true if year is leap year 
	 */
	public boolean isLeapYear(int year) {
		if (calendar == null) {
			calendar = new GregorianCalendar();
		}
		return calendar.isLeapYear(year);
	}
	
 /**
   * Return the last day of a month
   *
   * @param year  the four digit year
   * @param month the two digit month
   * @return the last day of the specified month
   */
  public static int getLastDay(int year, int month) {
  
	if (calendar == null) {
		calendar = new GregorianCalendar();
	}
    
    // adjust the month for a zero based index
    month = month - 1;
    
    // set the date of the calendar to the date provided
    calendar.set(year, month, 1);
    
    return calendar.getActualMaximum(GregorianCalendar.DAY_OF_MONTH);
    
  } 
	  

	/**	
	 *	Add a new daily weather in macro climat
	 * @param generalParameters Reference to SafeGeneralParameters object
	 * @param latitude Latitude of the plot
	 * @param julian Julian day of the climatic entry
	 * @param year Year of the climatic entry
	 * @param realYear Real year of the climatic entry
	 * @param month Month of the climatic entry
	 * @param day Day of the climatic entry
	 * @param tmin Temperature min of the climatic entry (degree)
	 * @param tmax Temperature max of the climatic entry (degree)
	 * @param rhmin Relative humidity min of the climatic entry (%)
	 * @param rhmax Relative humidity max of the climatic entry (%)
	 * @param globalRad Global radiation of the climatic entry (MJ m-2)
	 * @param rain Rain of the climatic entry (mm)
	 * @param windSpeed Wind speed (m s-1)
	 * @param waterTableDepth Water table depth of the climatic entry (m) 
	 * @param co2 CO2 concentration of the climatic entry (ppm) 
	 */
	public void createDailyClimat (SafeGeneralParameters generalParameters, 
									float latitude,
									int julian, int year, int realYear, int month, int day,
									float tmin, float tmax,
									float rhmin, float rhmax, float globalRad,
									float rain, float windSpeed, float waterTableDepth, float co2) {

		SafeDailyClimat s = new SafeDailyClimat (generalParameters, latitude, julian, year, realYear, month, day,
												tmin, tmax, rhmin, rhmax,
												globalRad, rain, windSpeed, waterTableDepth, co2);

		String sjulian = new Integer(julian).toString();
		String syear   = new Integer(year).toString();
		String key    = syear+"|"+sjulian;
		weather.put (key, s);
		return;
	}
	/**
	 * Return the weather for a day
	 * @param year Year 
	 * @param julian Julian day 
	 */
	public SafeDailyClimat getDailyWeather (int year, int julian)  {

		if (julian == 0) return null;
		
		String sjulian = new Integer(julian).toString();
		String syear = new Integer(year).toString();
		String key = syear+"|"+sjulian;
	 
		SafeDailyClimat s = (SafeDailyClimat) weather.get (key);
		if (s != null) 	{
			if (s.getWaterTableDepth()>0) {
				System.out.println("Water table depth have to be a negative value : "+key);
				System.out.println("CLIMAT getDailyWeather problem ");
				System.exit(1);
			}
			else return s;
		}
		else {
			

			System.out.println("Cannot find day in climate file : "+key);
			System.out.println("CLIMAT getDailyWeather problem ");
			System.exit(1);

		}
		return null;
	}
	
	/**
	 * Load climate in STICS object between 2 dates 
	 * This code replace the subroutine Iniclim.f90 in STICS fortran code 
	 * @param sticsClimat Reference to the  SafeSticsClimat object
	 * @param yearStart Year start
	 * @param dayStart Julian start 
	 * @param dayEnd Julian day end
	 */
	public void loadClimate (SafeSticsClimat sticsClimat, int yearStart, int dayStart, int dayEnd) throws Exception  {
			  
		int indice = 0;
		int yearFin = yearStart;

		//leap year
		int nbDayMax = 365;
		if (this.isLeapYear(yearStart)){
			nbDayMax = 366; 
		}
		for (int i=dayStart; i<=dayEnd; i++) {
			int climateDay = i;
			int climatYear = yearStart;
			if (i>nbDayMax) {
				climateDay=climateDay-nbDayMax;
				climatYear = climatYear+1;
			}

			try {
				SafeDailyClimat dayClimat  = this.getDailyWeather(climatYear, climateDay);
				
				if (indice < 366) {
					sticsClimat.tmin[indice]	= dayClimat.getMinTemperature ();	//tmin
					sticsClimat.tmax[indice]	= dayClimat.getMaxTemperature ();	//tmax
					sticsClimat.trg[indice]	= dayClimat.getGlobalRadiation ();	//RG
					sticsClimat.tetp[indice]  = dayClimat.getEtpPenman();
			
					//il 31-10-2017 rain on cell = rain + snowMelted
					sticsClimat.trr[indice]	= dayClimat.getRain () + dayClimat.getMeltedSnow () ;				
					sticsClimat.tvent[indice]	= dayClimat.getWindSpeed ();
					sticsClimat.co2[indice]	= dayClimat.getCO2Concentration();
					sticsClimat.tpm[indice]	= dayClimat.getAirVapourPressure ();
	

					//STICS HISAFE COMPARAISON
					// il faut arrondi � une decimale
			
				//	sticsClimat.tmin[indice] = (float)Math.round(dayClimat.getMinTemperature() * 10) / 10 ;
				//	sticsClimat.tmax[indice] = (float)Math.round(dayClimat.getMaxTemperature() * 10) / 10 ;
				//	sticsClimat.trg[indice] = (float)Math.round(dayClimat.getGlobalRadiation() * 10) / 10 ;
				//	sticsClimat.tetp[indice] = (float)Math.round(dayClimat.getEtpPenman() * 10) / 10 ;
				//	v.trr[indice] = (float)Math.round((dayClimat.getRain () + dayClimat.getMeltedSnow ()) * 10) / 10 ;
				
				}
				
				yearFin = dayClimat.getYear();
				indice++;
				
			} catch (Throwable e) {
				throw new Exception ("Unrecognized day in climate file ");
			}

		}
		sticsClimat.julzero = dayStart;
		sticsClimat.julfin = dayEnd; 
		sticsClimat.anneezero = yearStart; 
		sticsClimat.anneefin = yearFin;		
		sticsClimat.nometp = 1; //default etp pennam 
		return;
	}
	
	/**
	 * Computation of stemflow and rain interception by trees in mm
	 * Stored rain in trees will be evaporated in SafeTree.computeWaterDemand module
	 * Hypothesis : Tree LAI is homogenous above all covered cells, stemflow is calculated first, no umbrella effect
	 * @param stand Reference on SafeStand object
	 * @param dailyClimat Reference on SafeDailyClimat object
	 */
	public static void rainTreatement (SafeStand stand, SafeDailyClimat dailyClimat) {
	
		double cellSurface = stand.getPlot().getPlotSettings().cellSurface;
		int nbTrees = stand.getPlot().getPlotSettings().nbTrees;
		double [] storedRain = new double [nbTrees];

		//for each tree (with leaf area > 0 !)
		for (Iterator i=stand.getTrees().iterator(); i.hasNext(); ) {
			SafeTree tree = (SafeTree) i.next();
			if (tree.getTotalLeafArea() > 0) {

				//Search for stored rain of the day before in mm
				storedRain[tree.getId()-1] = tree.getStoredRain() / cellSurface;

				//Calculate lai with cell surface below
				tree.setLaiAboveCells (tree.getTotalLeafArea() / (tree.getNbCellsBellow() * cellSurface));
			}
		}

		//For each cell
		PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
		
		for (Iterator iter=plotc.getCells().iterator(); iter.hasNext(); ) {

			SafeCell cell = (SafeCell) iter.next();

			//Above trees searching order by tree height max to min
			Collection<SafeTree> treeAbove = cell.getTreeAbove ();
			double rainForStemFlow = dailyClimat.getRain();
			double rainForInterception = dailyClimat.getRain();
			double snowForInterception = dailyClimat.getSnow();
			double totalWaterOnCell = dailyClimat.getWaterEnteringSoil();

			//For each tree above this cell, compute stemflow and rain interception
			for (Iterator i=treeAbove.iterator(); i.hasNext(); ) {
				SafeTree t = (SafeTree) i.next();

				//if there is tree above with leaf area > 0
				if ((t != null) && (t.getTotalLeafArea () != 0)) {

					int treeIndex = t.getId()-1;

					//*************************************
					// STEMFLOW
					//*************************************
					double stemflow = cellStemflow (t, rainForStemFlow);

					
					//stemflow is decreasing for the next tree bellow
					rainForStemFlow -= stemflow;
					rainForStemFlow = Math.max (rainForStemFlow, 0); //to avoid very small negative values due to rounding
					if (stemflow > 0) {
						//stemflow is decreasing rain entry for interception by the same tree
						rainForInterception -= stemflow;
						rainForInterception = Math.max (rainForInterception, 0); //to avoid very small negative values due to rounding
	
						//update total available water for the cell
						totalWaterOnCell -= stemflow;
						
						//update tree stemflow
						t.addStemflow   (stemflow * cellSurface);
					}

					//*************************************
					// RAIN INTERCEPTION
					//*************************************
					double interceptedRain = cellRainInterception (t, rainForInterception, storedRain[treeIndex]);

					//interception is decreasing rain entry for next tree
					rainForInterception -= interceptedRain;
					rainForInterception = Math.max (rainForInterception, 0); //to avoid very small negative values due to rounding

					if (interceptedRain > 0) {
						//stemflow is decreasing for the next tree bellow
						rainForStemFlow -= interceptedRain;
						rainForStemFlow = Math.max (rainForStemFlow, 0); //to avoid very small negative values due to rounding
						
						//update total available water for the cell
						totalWaterOnCell -= interceptedRain;
						
						//update tree state variables in liters				
						t.addInterceptedRain (interceptedRain * cellSurface);
						t.addStoredRain (interceptedRain * cellSurface);
						cell.addRainInterceptedByTrees(interceptedRain);

					}
					
					//*************************************
					// SNOW INTERCEPTION
					//*************************************
					double interceptedSnow = cellRainInterception (t, snowForInterception, storedRain[treeIndex]);

					
					//interception is decreasing rain entry for next tree
					snowForInterception -= interceptedSnow;
					snowForInterception = Math.max (interceptedSnow, 0); //to avoid very small negative values due to rounding
					if (interceptedSnow > 0) {
						
						//update tree state variables in liters				
						t.addInterceptedRain (interceptedSnow * cellSurface);
						t.addStoredRain (interceptedSnow * cellSurface);
						
						cell.addRainInterceptedByTrees(interceptedSnow);
					}

				}
			}

			//Distribution of water on the cell below these trees
			//we add daily rain + daily snowMelted - stemflow - interceptedRain for all trees
			cell.setRainTransmittedByTrees (totalWaterOnCell);			//mm

		}

		//Add cumulated stemflow in the cell where trees are planted
		//This should not be used by the crop but should go directly in the soil profile :  Y1[717] Precip !!!!!!!
		for (Iterator i=stand.getTrees().iterator(); i.hasNext(); ) {
			SafeTree tree = (SafeTree) i.next();
			double stemFlow = tree.getStemflow();
			SafeCell cell = (SafeCell) tree.getCell();
			cell.setStemFlowByTrees (stemFlow);		//mm
		}

	}


	/**
	 * For one cell above the tree, computation of rainfall interception by trees in mm
	 * @param tree Reference on SafeTree object
	 * @param rain Rain value of the day (mm) 
	 * @param storedRain Stored rain value of the day before (mm) 
	 */
	public static double cellRainInterception (SafeTree tree, double rain, double storedRain) {

		//wettability parameter in mm lai-1
		double wettability = tree.getTreeSpecies ().getWettability();
		//interception  in mm
		double interceptedRain = 0; 

		if ((wettability * tree.getLaiAboveCells()) - storedRain > 0) {
			interceptedRain =  Math.min ((wettability * tree.getLaiAboveCells()) - storedRain
										, rain);
		}

		return (interceptedRain);
	}

	/**
	 * For one cell above the tree, computation of stemflow by trees in mm
	 * @param tree Reference on SafeTree object
	 * @param rain Rain value of the day (mm) 
	 */
	private static double cellStemflow (SafeTree tree, double rain) {

		//stemflow parameters
		double stemFlowCoefficient = tree.getTreeSpecies ().getStemFlowCoefficient();
		double stemFlowMax = tree.getTreeSpecies ().getStemFlowMax();

		//stemflow for this tree in mm
		double stemflow = 0;
		stemflow = rain * stemFlowMax
					* (1 - Math.exp (-stemFlowCoefficient * tree.getLaiAboveCells()));

		return (stemflow);
	}	
	
	/**	
	 *	Add a new max water table for a year in macro climat
	 * @param year Year of the climatic entry
	 * @param julian Julian day of the water table max observation
	 */
	public void addWaterTableMax (int year, int julian) {

		Integer ijulian = new Integer(julian);
		Integer iyear   = new Integer(year);
		maxWaterTable.put (iyear, ijulian);
		return;
	}
	
	/**	
	 *	Get the max water table day for a year in macro climat
	 * @param year Year of the climatic entry
	 * @return julian Julian day of the water table max observation
	 */
	public int getWaterTableMax (int year) {

		Integer iyear   = new Integer(year);
		Integer iday = maxWaterTable.get (iyear);
		return (int) iday;
	}
	
	/**
	 * Return the daily weather list
	 */
	public Collection getList() {
		return  weather.values();
	}
	/**
	 * Create beam set
	 */
	public void setBeamSet (SafeBeamSet<SafeBeam> bs) {
		beamSet = new SafeBeamSet<SafeBeam>();
		beamSet = bs;
	}
	/**
	 * Create collection of direct beam set
	 */
	public void setDirectBeamSet  (SafeBeamSet<SafeBeam> bs) {
		directBeamSet = null;
		directBeamSet = new SafeBeamSet<SafeBeam>();
		directBeamSet = bs;
	}
	/**
	 * Return collection of beam set (direct and diffuse)
	 */
	public SafeBeamSet<SafeBeam> getBeamSet () {return beamSet;}
	/**
	 * Return collection of direct beam set
	 */
	public SafeBeamSet<SafeBeam> getDirectBeamSet ()  {return directBeamSet;}


}


