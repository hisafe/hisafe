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
 * 6 INRA (UMR-PIAF), University of Clermont Auvergne, 63000 Clermont-Ferrand, France
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
 * Climatic entries for one day
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 * @author : Hervé SINOQUET - INRA (UMR-PIAF), University of Clermont-Ferrand, France
 */

public class SafeDailyClimat implements  Serializable {
		
	// Input values
	/** Year YYYY */
	private int year;				
	/** Real year YYYY (in case it is a copy of a missing climate measurement)  */
	private int realYear;				
	/** Month MM (1 to 12) */
	private int month;				
	/** Day DD (1 to 31)  */
	private int day;					
	/** Number of the day in the year (1 to 365) */
	private int julianDay;	
	/** Temperature min (degree) */
	private float minTemperature;	
	/** Temperature max (degree) */
	private float maxTemperature;		
	/** Relative humidity min (%)*/
	private float minRelativeHumidity;	
	/** Relative humidity max (%) */
	private float maxRelativeHumidity;	
	/** Global radiation (MJ m-2) */
	private float globalRadiation;				
	/** Precipitation (mm)*/
	private float precipitation;		
	/** Wind speed (m s-1) */
	private float windSpeed;		
	/** water table depth (m) */
	private float waterTableDepth;	
	/** CO2 concentration (ppm) */
	private float cO2Concentration;		

	// Calculated values
	/** Global PAR direct+diffuse (Moles m-2) */
	private float globalPar; 
	/** Diffuse PAR  (Moles m-2) */
	private float diffusePar; 					
	/** Sun declination (radian) */
	private float sunDeclination; 			
	/** Day length (hours) */
	private float dayLength; 					
	/** Extra terrestrial radiation (MJ m-2 day-1) */
	private float extraTerrestrialRadiation; 
	/** Atmospheric long-wave radiation (Watts m-2) */
	private float infraRedRadiation;
	/** Air vapor pressure (mbar or hPa) */
	private float airVapourPressure;			
	/** Slope of the saturation vapour pressure curve of the air */
	private float delta;						
	/** Etp penman calculated (mm) */
	private float etpPenman;								
	/** Rain (precipitation - snow + meltedSnow) (mm)  */
	private float rain;							
	/** Snow (mm) */
	private float snow;							
	/** snow stocked from previous days(mm) */
	private float stockedSnow;					
	/** Snow melted (mm)  */
	private float meltedSnow;				
	/** Rain capacity stocked in snow (mm)  */
	private float rainCapacityinSnow;			
	/** Rain captured by snow (mm) */
	private float rainCapturedBySnow;			
	
	/**	
	 *	Constructor 
	 */
	public SafeDailyClimat() {}
	
	/**	
	 *	Constructor 
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
	 * @param globalRadiation Global radiation of the climatic entry (MJ m-2)
	 * @param rain Rain of the climatic entry (mm)
	 * @param windSpeed Wind speed (m s-1)
	 * @param waterTableDepth Water table depth of the climatic entry (m) 
	 * @param co2 CO2 concentration of the climatic entry (ppm) 
	 */	
	public SafeDailyClimat (SafeGeneralParameters generalParameters, 
							float latitude, 
							int julian, int year, int realYear, int month, int day,
							float tmin, float tmax,	float rhmin, float rhmax,
							float globalRadiation,  float rain,  float windSpeed, 
							float waterTableDepth, float co2) {

		julianDay = julian;
		this.year = year;
		this.realYear = realYear;
		this.month = month;
		this.day = day;
		this.minTemperature = tmin;
		this.maxTemperature = tmax;
		this.minRelativeHumidity = rhmin;
		this.maxRelativeHumidity = rhmax;
		this.globalRadiation = globalRadiation;
		//convertion of global radiation (MJ m-2) in PAR (Moles m-2)
		this.globalPar = (float) ((globalRadiation * generalParameters.parGlobalCoefficient) / generalParameters.molesParCoefficient);
		this.precipitation = rain;		
		this.windSpeed = windSpeed;
		this.waterTableDepth = waterTableDepth;
		this.cO2Concentration = co2;
		//snow module (il 31-10-2017)
		this.rain = rain;
		this.snow = 0;
		this.meltedSnow = 0;
		this.stockedSnow = 0;
		this.rainCapacityinSnow = 0;
		this.rainCapturedBySnow = 0;
			
		//VPD STICS FOR STICS HISAFE COMPARAISON
		//this.airVapourPressure = (float) (vpSatFunction (getMinTemperature () - 1));
		
		//VPD HISAFE
		double airTemperature = getMeanTemperature ();
		this.airVapourPressure = (float) (vpSatFunction (airTemperature)
					  	      			* (minRelativeHumidity+maxRelativeHumidity) / 2 / 100) ;
		this.delta = (float) (deltaFunction (airTemperature));
		
		//sun declination in radian
		double om = 0.017202 * (((float) julianDay) - 3.244);
		double teta = om + 0.03344 * Math.sin(om) * (1 + 0.021 * Math.cos(om)) - 1.3526;        // Celestial longitude of the sun
		double sidec = 0.3978 * Math.sin(teta);			// Sine of sun declination
		sunDeclination = (float) (Math.asin(sidec));	//in radian

		//day length calculation in hours
		double codec = Math.cos(sunDeclination);				// cosine of sun declination
		double silat = Math.sin(Math.toRadians(latitude));
		double colat = Math.cos(Math.toRadians(latitude));
		double sinR = 0.01064; // Here is the French touch, ie there is another number for the other definitions
		double AA = (-sinR - sidec * silat) / (codec * colat);
		dayLength = (float) ((24/Math.PI) * Math.acos(AA));

		// extra Terrestrial Radiation calculation
		double CC = 1370 * 3600	* 1.e-6;			//Solar constant, in MJ m-2 hour-1
		CC = CC * (1 + 0.033 * Math.cos(2 * Math.PI * ((float)(julianDay)-4)/366)); //Solar constant, with sun-earth distance correction
		double G0 = silat * sidec * dayLength;
		G0 = G0 + colat * codec * (24/Math.PI) * Math.sin((Math.PI/12)*(dayLength/2));
		extraTerrestrialRadiation = (float) (G0 * CC);

		//	Coefficients aDG and bDG of the relationship : D/G = a - b G/G0
		//	(where D, G, G0 are diffuse incident, global incident and extraterrestrial radiation at daily time step, respectively.
		double aDG = generalParameters.diffuseCoeffA;
		double bDG = generalParameters.diffuseCoeffB;

		diffusePar = (float) (globalPar * (aDG - bDG * (globalRadiation/extraTerrestrialRadiation)));
		if (diffusePar >= globalPar) {
			diffusePar = (float) (0.99*globalPar);	//IL 06.07.2018 to avoid direct=0
		}
		if (diffusePar <= (0.1*globalPar)) diffusePar = (float) (globalPar * 0.1);

		//Calculation of ETP Penman (mm)
		double fracinsol = ((globalRadiation/extraTerrestrialRadiation)-generalParameters.aangst)/generalParameters.bangst;	//insolation fraction
		double visibleSky = 1;
		double dsat = vpSatFunction(getMeanTemperature()) - getAirVapourPressure();
		double L = (2500840-2358.6*getMeanTemperature())/1000000;

		double var1 = Math.pow((getMeanTemperature() + 273.16), 4)/1000000000;
		double var2 =(0.1 + 0.9*fracinsol);
		double var3 = 0.56 - 0.08*Math.sqrt(getAirVapourPressure());
		double rglo = 4.9 * var1 * var2 * var3;
		double rnetp =(1-0.2)*getGlobalRadiation()-rglo*visibleSky;
		double etp =  (rnetp/L*getDelta()/(getDelta()+generalParameters.psychrometricConstant)+(generalParameters.psychrometricConstant/(delta+generalParameters.psychrometricConstant))
							 *(0.26*(1+0.54*getWindSpeed()))*dsat);
		etpPenman = (float) Math.max (etp,0);


		//Calculation of infra red radiation of the day (W.m-2) 
		//Infra red radiation is (atmospheric radiation)-(system radiation, assuming T=Tair)
		//Brutsaert's formula, from Stics formalism
		double t = (minTemperature+maxTemperature)/2+2*(maxTemperature-minTemperature)*Math.sin(Math.PI*dayLength/24)/dayLength;		// GT 1/02/2008
		double eabrut = 1.24*Math.pow(airVapourPressure/(t+273.15),1/7);
		double emissa = eabrut+(1-fracinsol)*(1-eabrut)*(1-4*11/(t+273.15));	//sky emissivity
		infraRedRadiation = (float) (generalParameters.stefanBoltzmanConstant*Math.pow((t+273.15),4)*(1-emissa));				// W.m-2

	}

	/**
	 * Calculation of saturated vapor pressure of air (mbar)
	 * @param airTemperature Air temperature (degree)
     * @return value of saturated vapor pressure of air (mbar)
	*/
	public double vpSatFunction (double airTemperature)
	{
		return (6.107 * Math.pow (
					(1 + Math.sqrt(2)*Math.sin(Math.PI*airTemperature/3d/180d)),8.827));
	}

	/**
	 * Calculation of slope of the saturation vapor pressure curve of the air
	 * @param airTemperature Air temperature (degree)
     * @return value of slope of the saturation vapor pressure curve of the air
	 */
	public double deltaFunction (double airTemperature)
	{
		return (vpSatFunction(airTemperature+0.5) - vpSatFunction(airTemperature-0.5));
	}
	

	/**
	 * Calculation of rain transformed in snow of the day
	 * @author Christian DUPRAZ - INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
	 * @param generalParameters Reference to SafeGeneralParameters object
	 * @param yesterday Reference on SafeDailyClimat objet : climatic data of the day before today 
	*/
	public void calculateSnow (SafeGeneralParameters settings, SafeDailyClimat yesterday) {
		
		//values from tomorrow 
		if (yesterday != null) {
			this.stockedSnow = yesterday.getStockedSnow();
			this.rainCapacityinSnow = yesterday.getRainCapacityinSnow();
		}
		
		//It is snowing 	
		if ((this.getMeanTemperature () < settings.minTempSnow) && (maxTemperature < settings.maxTempSnow)) {
			this.snow = this.rain;
			this.rainCapacityinSnow = this.rainCapacityinSnow + this.snow;
			this.stockedSnow = this.stockedSnow + this.snow;
			this.rain = 0;
		}
		//it is not snowing 
		else {			
			if (yesterday != null) {
				
				//it is raining on snow
				if ((this.rain > 0) && (yesterday.getRainCapacityinSnow() > 0)) {
					this.rainCapturedBySnow = Math.min(this.rain,yesterday.getRainCapacityinSnow());
					this.rainCapacityinSnow = this.rainCapacityinSnow - this.rainCapturedBySnow;
					this.stockedSnow = this.stockedSnow + this.rainCapturedBySnow;
				}

				//calculation of melt 
				if  (yesterday.getStockedSnow() > 0)  {

					//max de fonte si tmoy > maxTempSnowMelt
					if (this.getMeanTemperature () > settings.maxTempSnowMelt)  {
						this.meltedSnow = Math.min ((float) settings.maxDailySnowMelt, yesterday.getStockedSnow());
					}	
					//pas de fonte si tmoy < minTempSnowMelt
					else {
						if (this.getMeanTemperature () < settings.minTempSnowMelt)  {
							this.meltedSnow = 0;
						}
						//interpolation entre maxTempSnowMelt et minTempSnowMelt
						else
						{
							this.meltedSnow = Math.min ((float) (settings.maxDailySnowMelt * ((this.getMeanTemperature () -settings.minTempSnowMelt) / (settings.maxTempSnowMelt-settings.minTempSnowMelt))), yesterday.getStockedSnow());
						}
					}	
					this.stockedSnow = this.stockedSnow - this.meltedSnow;
				}
			}
		}
	}
	/**
	 * Return the year of the climatic entry (YYYY)
	 **/
	public int getYear () {return year;}
	/**
	 * Return the real year of the climatic entry (YYYY)
	 **/
	public int getRealYear () {return realYear;}
	/**
	 * Return the month of the climatic entry (MM)
	 **/
	public int getMonth () {return month;}
	/**
	 * Return the day of the climatic entry (DD)
	 **/
	public int getDay () {return day;}
	/**
	 * Return the julian day of the climatic entry (0-365)
	 **/
	public int getJulianDay () {return julianDay;}
	/**
	 * Return the mean temperature of the climatic entry (degree)
	 **/
	public float getMeanTemperature () {return (maxTemperature+minTemperature)/2;}
	/**
	 * Return the min temperature of the climatic entry (degree)
	 **/
	public float getMinTemperature () {return minTemperature;}
	/**
	 * Return the max temperature of the climatic entry (degree)
	 **/
	public float getMaxTemperature () {return maxTemperature;}
	/**
	 * Return the min relative humidity of the climatic entry (%) 
	 **/
	public float getMinRelativeHumidity() {return minRelativeHumidity;}
	/**
	 * Return the max relative humidity of the climatic entry (%) 
	 **/
	public float getMaxRelativeHumidity () {return maxRelativeHumidity;}
	/**
	 * Return the ETP Prennam of the climatic entry (mm)
	 **/
	public float getEtpPenman () {return etpPenman;}
	/**
	 * Return the global radiation of the climatic entry (MJ m-2)
	 **/
	public float getGlobalRadiation () {return globalRadiation;}
	/**
	 * Return the global PAR of the climatic entry (Moles m-2)
	 **/
	public float getGlobalPar () {return globalPar;}
	/**
	 * Return the diffuse PAR of the climatic entry (Moles m-2)
	 **/
	public float getDiffusePar () {return diffusePar;}
	/**
	 * Return the direct PAR of the climatic entry (Moles m-2)
	 **/
	public float getDirectPar () {return globalPar - diffusePar;}
	/**
	 * Return the proportion of diffuse PAR of the climatic entry (%)
	 **/
	public float getDiffuseProp() {return (diffusePar/globalPar);}
	/**
	 * Return the precipitation (rain+snow) of the climatic entry (mm)
	 **/
	public float getPrecipitation () {return precipitation;}
	/**
	 * Return the wind speed of the climatic entry (m s-1)
	 **/
	public float getWindSpeed () {return windSpeed;}
	/**
	 * Return the co2 concentration day of the climatic entry (ppm)
	 **/
	public float getCO2Concentration () {return cO2Concentration;}
	/**
	 * Return the sun declination of the climatic entry (radian)
	 **/
	public float getSunDeclination () {return sunDeclination;}
	/**
	 * Return the lenght of the day of the climatic entry (hours)
	 **/
	public float getDayLength () {return dayLength;}
	/**
	 * Return rhe extra terrestrial radiation of the climatic entry (MJ m-2 day-1)
	 **/
	public float getExtraTerrestrialRadiation () {return extraTerrestrialRadiation;}
	/**
	 * Return the atmospheric long-wave radiation of the climatic entry  (Watts m-2)
	 **/
	public float getInfraRedRadiation() {return infraRedRadiation;}
	/**
	 * Return the air vapor pressure of the climatic entry (mbar)
	 **/	
	public float getAirVapourPressure () {return airVapourPressure;}
	/**
	 * Return the slope of the saturation vapor pressure curve of the air
	 **/
	public float getDelta () {return delta;}
	/**
	 * Return the water table depth (negative value) of the climatic entry  (m) 
	 **/
	public float getWaterTableDepth () {
		if (waterTableDepth == 0) return (float)(-0.1);
		return (waterTableDepth);
	}
	/**
	 * Return the rain value of the climatic entry (mm) 
	 **/
	public float getRain () {return rain;}
	/**
	 * Return the snow value of the climatic entry (mm) 
	 **/
	public float getSnow () {return snow;}
	/**
	 * Return the snow value stocked  (mm) 
	 **/
	public float getStockedSnow () {return stockedSnow;}
	/**
	 * Return the snow melted value (mm) 
	 **/
	public float getMeltedSnow () {return meltedSnow;}
	/**
	 * Return the rain capacity in snow (mm) 
	 **/
	public float getRainCapacityinSnow() {return rainCapacityinSnow;}
	/**
	 * Return the rain captured in snow (mm) 
	 **/
	public float getRainCapturedBySnow() {return rainCapturedBySnow;}
	/**
	 * Return the water entering the soil (mm) 
	 **/
	public float getWaterEnteringSoil() {return rain + meltedSnow - rainCapturedBySnow;}

}
