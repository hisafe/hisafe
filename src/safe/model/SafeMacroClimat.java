package safe.model;

import java.io.Serializable;
import java.util.Collection;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.Map;
import java.util.Iterator;
import java.lang.Math;
import java.math.BigDecimal;

import capsis.defaulttype.plotofcells.PlotOfCells;
import safe.stics.*;

/**
 * Wheather data for a simulation  (day by day)
 * Each day is an instance of SafeDailyClimat
 *
 * @author Isabelle LECOMTE - January 2002
 */

public class SafeMacroClimat  implements  Serializable {

	private Map weather;						//weather data
	private SafeBeamSet beamSet;				//beam set (created once) mixing diffuse and direct information
	private SafeBeamSet directBeamSet;			//direct beam set  (created at each direct lighting process) in case DirectLightMethod=True

	private static GregorianCalendar calendar;
	
	public SafeMacroClimat () {
		weather = new Hashtable ();
	}

	/**
	 * Tells if the given year is leap year.
     * @param year  the four digit year
     *
     * @return      true if year is leap year 
	 */
	public boolean isLeapYear(int year) {
		if (calendar == null) {
			calendar = new GregorianCalendar();
		}
		return calendar.isLeapYear(year);
	}
	
 /**
   * A method to get the last day of a month
   *
   * @param year  the four digit year
   * @param month the two digit month
   *
   * @return      the last day of the specified month
   */
  public static int getLastDay(int year, int month) {
  
    // get a calendar object
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
	 * Add a new daily weather (day j) in macro climat
	 */
	public void createDailyClimat (SafeInitialParameters settings, 
									double latitude,
									int j, int y, int m, int d,
									float tmin, float tmax,
									float rhmin, float rhmax, float globalRad,
									float rain, float windSpeed, float waterTableDepth, float co2) {

		SafeDailyClimat s = new SafeDailyClimat (settings,  j, latitude, y, m, d,
												tmin, tmax, rhmin, rhmax,
												globalRad, rain, windSpeed, waterTableDepth, co2);

		String julian = new Integer(j).toString();
		String year   = new Integer(y).toString();
		String key    = year+"|"+julian;
		weather.put (key, s);
		return;
	}
	/**
	 * Return the weather for a day
	 */
	public SafeDailyClimat getDailyWeather (int y, int d) throws Exception {

		String julian = new Integer(d).toString();
		String year = new Integer(y).toString();
		String key = year+"|"+julian;
		if (d == 0) return null; 
		SafeDailyClimat s = (SafeDailyClimat) weather.get (key);
		if (s != null) 	return s;
		else {
				System.out.println("Unrecognized day in climate file : "+key);
				throw new Exception ("Unrecognized day in climate file : "+key);

		}
	}
	
	/**
	 * Load climate in STICS object for a new year (365 days) 
	 * This code replace the subroutine Iniclim.f90 in STICS fortran code 
	 */
	public void loadClimate (SafeSticsClimat c, int yearStart, int dayStart, int dayEnd) throws Exception  {
			  

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
				c.tmin[indice]	= dayClimat.getMinTemperature ();	//tmin
				c.tmax[indice]	= dayClimat.getMaxTemperature ();	//tmax
				c.trg[indice]	= dayClimat.getGlobalRadiation ();	//RG
				
				//je force cet arrondi pour valider les resultats avec STICS
				BigDecimal bd = new BigDecimal(dayClimat.getEtpPenman());
				bd= bd.setScale(4,BigDecimal.ROUND_DOWN);
				c.tetp[indice] = bd.floatValue();
		
				//il 31-10-2017 rain on cell = rain + snowMelted
				c.trr[indice]	= dayClimat.getRain () + dayClimat.getMeltedSnow () ;				
				c.tvent[indice]	= dayClimat.getWindSpeed ();
				c.co2[indice]	= dayClimat.getCO2Concentration();
				
				//Pour le moment r�sultats de airVP different entre hisafe et stics
				c.tpm[indice]	= dayClimat.getAirDayVapourPressure ();
				c.tpm[indice]   = -999;
				
				yearFin = dayClimat.getYear();
				indice++;
				
			} catch (Throwable e) {
				throw new Exception ("Unrecognized day in climate file ");
			}

		}
    	c.julzero = dayStart;
    	c.julfin = dayEnd; 
    	c.anneezero = yearStart; 
    	c.anneefin = yearFin;		
    	c.nometp = 1; //default etp pennam 
		return;
	}
	
	
	/**
	 * Computation of rainfall stemflow interception by trees in mm
	 * Everyday, stored rain is evaporated in SafeTree.computeWaterDemand module
	 *
	 *  Hypothesis :
	 *   1) Tree LAI is homogenous above all covered cells
	 *   2) Stemflow is calculated first
	 *   3) No umbrella effect
	 */
	public static void rainTreatement (SafeInitialParameters safeSettings, SafeStand stand, SafeDailyClimat dailyClimat) {
	
		double cellSurface = ((SafePlot) stand.getPlot()).getCellSurface();
		int nbTrees = safeSettings.nbTrees;
		double [] storedRain = new double [nbTrees];

		//for each tree (with leaf area > 0 !)
		for (Iterator i=stand.getTrees().iterator(); i.hasNext(); ) {
			SafeTree tree = (SafeTree) i.next();
			if (tree.getLeafArea() > 0) {

				//Search for stored rain of the day before in mm
				storedRain[tree.getId()-1] = tree.getStoredRain() / cellSurface;

				//Calculate lai with cell surface below
				tree.setLaiAboveCells (tree.getLeafArea() / (tree.getNbCellsBellow() * cellSurface));
			}
		}

		//For each cell

		PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
		
		for (Iterator iter=plotc.getCells().iterator(); iter.hasNext(); ) {

			SafeCell cell = (SafeCell) iter.next();

			//Above trees searching order by tree height max to min
			Collection treeAboves = cell.treeAboveComputation();

			double rainForStemFlow = dailyClimat.getRain();
			double rainForInterception = dailyClimat.getRain();
			double snowForInterception = dailyClimat.getSnow();
			double totalWaterOnCell = dailyClimat.getWaterEnteringSoil();

			//For each tree above this cell, compute stemflow and rain interception
			for (Iterator i=treeAboves.iterator(); i.hasNext(); ) {
				SafeTree treeAbove = (SafeTree) i.next();

				//if there is tree above with leaf area > 0
				if ((treeAbove != null) && (treeAbove.getLeafArea () != 0)) {

					int treeIndex = treeAbove.getId()-1;

					//*************************************
					// STEMFLOW
					//*************************************
					double stemflow = cellStemflow (safeSettings, treeAbove, rainForStemFlow);

					
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
						treeAbove.addStemflow   (stemflow * cellSurface);
					}

					//*************************************
					// RAIN INTERCEPTION
					//*************************************
					double interceptedRain = cellRainInterception (safeSettings, treeAbove, rainForInterception, storedRain[treeIndex]);

					//interception is decreasing rain entry for next tree
					rainForInterception -= interceptedRain;
					rainForInterception = Math.max (rainForInterception, 0); //to avoid very small negative values due to rounding

					if (interceptedRain > 0) {
						//stemflow is decreasing for the next tree bellow
						rainForStemFlow -= interceptedRain;
						rainForStemFlow = Math.max (rainForStemFlow, 0); //to avoid very small negative values due to rounding
						
						//update total available water for the cell
						totalWaterOnCell -= interceptedRain;
						//storedRain[treeIndex] += interceptedRain;
						
						//update tree state variables in liters				
						treeAbove.addInterceptedRain (interceptedRain * cellSurface);
						treeAbove.addStoredRain (interceptedRain * cellSurface);
						cell.addRainInterceptedByTrees(interceptedRain);
						
						
					}
					
					//*************************************
					// SNOW INTERCEPTION
					//*************************************
					double interceptedSnow = cellRainInterception (safeSettings, treeAbove, snowForInterception, storedRain[treeIndex]);

					
					//interception is decreasing rain entry for next tree
					snowForInterception -= interceptedSnow;
					snowForInterception = Math.max (interceptedSnow, 0); //to avoid very small negative values due to rounding
					if (interceptedSnow > 0) {
						//storedRain[treeIndex] += interceptedSnow;
						
						//update tree state variables in liters				
						treeAbove.addInterceptedRain (interceptedSnow * cellSurface);
						treeAbove.addStoredRain (interceptedSnow * cellSurface);
						
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
	 * For one cell, computation of rainfall interception by trees in mm
	 */
	public static double cellRainInterception (SafeInitialParameters safeSettings, SafeTree tree, double rain, double storedRain) {

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
	 * For one cell, computation of stemflow by trees in mm
	 */
	private static double cellStemflow (SafeInitialParameters safeSettings, SafeTree tree, double rain) {

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
	 * Return the daily weather list
	 */
	public Collection getList() {
		return  weather.values();
	}
	/**
	 * Create beam set
	 */
	public void setBeamSet (SafeBeamSet bs) {
		beamSet = new SafeBeamSet();
		beamSet = bs;
	}
	/**
	 * Create direct beam set
	 */
	public void setDirectBeamSet  (SafeBeamSet bs) {
		directBeamSet = null;
		directBeamSet = new SafeBeamSet();
		directBeamSet = bs;
	}

	public SafeBeamSet getBeamSet () {return beamSet;}
	public SafeBeamSet getDirectBeamSet ()  {return directBeamSet;}


}


