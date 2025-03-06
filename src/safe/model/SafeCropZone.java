/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
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
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.model;

import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;

import safe.stics.SafeSticsCrop;
import safe.stics.SafeSticsCropFormat;
import safe.stics.SafeSticsItk;
import safe.stics.SafeSticsItkFormat;
import safe.stics.SafeSticsParameters;
import safe.stics.SafeSticsTransit;

/**
 * Crop zone description : A collection of cell with the crop species
 *
 * @author Isabelle Lecomte - INRAE Montpellier - September 2022
 */

public class SafeCropZone implements Serializable {
	
	private static final long serialVersionUID = 1L;
	private int id;							//id of the ZONE
	private String simulationDir;			//name of the simulation directory
	private double area;					//area of the ZONE
	private String name;					//name of the ZONE
	private int itkIndex;					//index of the current itk 
	private List<SafeCell> cellList;		//List of cells in the ZONE
	private List<String> itkList;			//List of tec name for the zone 
	public SafeSticsItk sticsItk;			//Crop intervention in STICS for the current simulation
	private SafeCropSpecies cropSpecies;	//Crop species for the current simulation

	private int simulationDay;				//SIMULATION day index 
	private int sticsDay;					//STICS day index (can be different of simulation day) 
	private boolean simulationFinished;		//if TRUE simulation is finish (this crop rotation) 
	
	// crop initialization values (for perenial crops) 
	private int initialCropStage = 1; 								// Crop stage used at the beginning of simulation (P_stade0) 
	private double initialCropLai = 0;						 		// Initial leaf area index (P_lai0) m2 m-2 
	private double initialCropBiomass = 0; 							// Initial biomass (P_masec0) t ha-1
	private double initialCropRootsDepth = 0; 						// Initial depth of root front  (P_zrac0) m
	private double initialCropGrainBiomass = 0; 					// Initial grain dry weight (P_magrain0)  g m-2
	private double initialCropNitrogen = 0; 						// Initial nitrogen amount in the plant (P_QNplante0) kg ha-1 
	private double initialCropReserveBiomass = 0; 					// Initial reserve biomass (P_resperenne0)  t ha-1
	private double[] initialCropRootsDensity = {0, 0, 0, 0, 0}; 	// Table of initial root density of the 5 horizons of soil for fine earth (P_densinitial)  cm cm-3


	public SafeCropZone (int id, String name) {
		this.id = id;
		this.name = name;
	}
	/**
	 * Crop zone creation
	 **/
	public SafeCropZone (SafeEvolutionParameters ep, int id, String name, List<SafeCell> cells, List<String> itk, double cellSurface) {
		this.id = id;
		this.name = name;
		this.cellList = cells;
		this.area = cells.size() * cellSurface;
		this.itkIndex = 0;
		this.simulationDir = ep.simulationDir;
		this.simulationFinished = false; 
		itkList = new ArrayList<String>();
		for (Iterator<String> i = itk.iterator(); i.hasNext();) {
			String c = (String) i.next();
			c = this.simulationDir + "/cropInterventions/" + c;
			itkList.add(c);
		}
	}
	/**
	 * Set the zone to each cell
	 **/
	public void initCells () {
		for (Iterator<SafeCell> i = cellList.iterator(); i.hasNext();) {
			SafeCell c = (SafeCell) i.next();
			c.setCropZone (this);
		}
	}
	/**
	 * Check if a date is the first day of the current crop itk
	 **/
	public boolean isDayStart (int year, int month, int day) {
		if (this.getSticsItk().getYearstart() == year && this.getSticsItk().getMonthstart() == month && this.getSticsItk().getDaystart() == day) return true;
		return false;
	}
	/**
	 * Check if a date is the last day of the current crop itk
	 **/
	public boolean isDayEnd (int year, int month, int day) {
		if (this.getSticsItk().getYearend() == year && this.getSticsItk().getMonthend() == month && this.getSticsItk().getDayend() == day) return true;
		return false;
	}
	/**
	 * Return the first day (julian day) of the current crop itk
	 **/
	public int getJulianDayStart (int year) {
	
		int day = this.getSticsItk().getDaystart();
		int month = this.getSticsItk().getMonthstart();
		
		GregorianCalendar startDate= new GregorianCalendar();
		startDate.set(year,month-1,day);
		return( startDate.get(GregorianCalendar.DAY_OF_YEAR));

	}
	/**
	 * Return the last day (julian day) of the current crop itk
	 **/
	public int getJulianDayEnd (int year) {

		int day = this.getSticsItk().getDayend();
		int month = this.getSticsItk().getMonthend();
		int julianstart = getJulianDayStart(year);
		GregorianCalendar endDate= new GregorianCalendar();
		endDate.set(year,month-1,day);

		int julianend = endDate.get(GregorianCalendar.DAY_OF_YEAR);
		if (julianend <= julianstart) {
			julianend=julianend+365;
			if (endDate.isLeapYear(year+1)) {
				julianend++;
			}
		}
		return julianend;
	}
	
	/**
	 * Load the FIRST itk file and the crop species file for the zone
	 **/
	public void loadFirstItk (int year) {

		itkIndex = 0;
		String itkFileName = itkList.get(itkIndex);
		String cropSpeciespathName="";
		String cropSpeciesfileName="";
		SafeCell firstCell = this.getFirstCell();

		// Loading crop intervention file 
		try {
			this.sticsItk = new SafeSticsItk();
			cropSpeciesfileName = new SafeSticsItkFormat (itkFileName).load (this, sticsItk, year);
			this.sticsItk.setYearstart(year);
			this.sticsItk.setYearend(year);
			if (this.sticsItk.getMonthend()<this.sticsItk.getMonthstart()) this.sticsItk.setYearend(year+1);
			if (this.sticsItk.getMonthend()==this.sticsItk.getMonthstart() && this.sticsItk.getDayend()<=this.sticsItk.getDaystart()) 
				this.sticsItk.setYearend(year+1);
			
		} catch (Exception e2) {
			System.out.println("CROP ZONE "+this.name+" ITK "+(itkIndex+1)+" initialisation problem... simulation is canceled !");
			System.exit(1);
		}

		// Loading crop species file attached to this itk
		try {
			cropSpeciespathName = this.simulationDir + "/cropSpecies/" + cropSpeciesfileName;
			this.cropSpecies = new SafeCropSpecies();
			SafeCrop crop = new SafeCrop(firstCell);	
			
			String cropSpeciesName = new SafeSticsCropFormat (cropSpeciespathName).load (cropSpecies, crop.getSticsCrop());
			this.cropSpecies.setFileName(cropSpeciesfileName);
			crop.setCropSpeciesName(cropSpeciesName);
			firstCell.setCrop(crop);
	

		} catch (Exception e1) {

			System.out.println("FIRST ITK CROP SPECIES PARAMETERS  "+cropSpeciesfileName+" problem... simulation is canceled !");
			System.exit(1);
		}
		simulationFinished=false;
	}
	/**
	 * Load the NEXT itk file and the crop species file for the zone
	 **/
	public void loadNextItk (int year) {
		
		//if itk missing, we restart from the first one
		itkIndex++;
		if (itkIndex>itkList.size()-1) itkIndex = 0;

		String itkFileName = itkList.get(itkIndex);
		String cropSpeciespathName="";
		String cropSpeciesfileName="";
		SafeCell firstCell = this.getFirstCell();

		// Loading crop intervention file 
		try {
			this.sticsItk = new SafeSticsItk();
			cropSpeciesfileName = new SafeSticsItkFormat (itkFileName).load (this, sticsItk, year);			
			this.sticsItk.setYearstart(year);
			this.sticsItk.setYearend(year);
			if (this.sticsItk.getMonthend()<this.sticsItk.getMonthstart()) this.sticsItk.setYearend(year+1);
			if (this.sticsItk.getMonthend()==this.sticsItk.getMonthstart() && this.sticsItk.getDayend()<=this.sticsItk.getDaystart()) 
				this.sticsItk.setYearend(year+1);
			
		} catch (Exception e2) {
			System.out.println("CROP ZONE "+this.name+" ITK "+(itkIndex+1)+" initialisation problem... simulation is canceled !");

			System.exit(1);
		}


		// Loading crop species file attached to this itk
		try {
	
			//STICS initialisation for first cell
			SafeCrop firstCrop = firstCell.getCrop();
			cropSpeciespathName = this.simulationDir + "/cropSpecies/" + cropSpeciesfileName;
			this.cropSpecies = new SafeCropSpecies();
			String cropSpeciesName = new SafeSticsCropFormat (cropSpeciespathName).load (cropSpecies, firstCrop.getSticsCrop());
			this.cropSpecies.setFileName(cropSpeciesfileName);
			firstCrop.setCropSpeciesName(cropSpeciesName);
			firstCell.setCrop(firstCrop);
	
		
			//STICS initialisation for each cell
			for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeCrop crop = cell.getCrop();
				crop.setCropSpeciesName(cropSpeciesName);
				crop.sticsCrop 		= new SafeSticsCrop(firstCrop.sticsCrop);
				crop.setCropSpeciesName(firstCrop.getCropSpeciesName());
			}

		} catch (Exception e1) {

			System.out.println("CROP SPECIES PARAMETERS  "+cropSpeciesfileName+" problem... simulation is canceled !");
			System.exit(1);
		}
		simulationFinished=false;
	}
	
	/**
	 * STICS crop initialization (First simulation) 
	 */
    public void initialiseSticsCrop (SafeTestJNA jna, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
    								SafeGeneralParameters safeSettings, 
    								SafePlotSettings plotSettings, 
    								SafeEvolutionParameters evolutionParameters,  
    								SafeSoil  soil) throws Exception {

		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next ();

			//First cell of the zone
			if (cell==this.getFirstCell()) {
				cell.getCrop().cropInitialisation (jna,
											sticsParam, 
											sticsTransit, 
											soil, 
											this, 
											plotSettings,
											evolutionParameters.exportDir,											 
											safeSettings.laiFileName);
			}
			// Copy of STICS initialisation in other cells of the zone
			else {

				SafeCrop crop = new SafeCrop(cell);
				crop.cropInitialisationCopy (this.getFirstCell().getCrop());
				cell.setCrop(crop);
			}

			cell.getCrop().sticsCrop.ipl = 1;	
		}

		//init some STICS variables 
		this.sticsItk.ipl = 1;
		if (this.sticsItk.P_ressuite == 0) this.sticsItk.P_ressuite = 1;

	    if(sticsTransit.P_option_thinning== 1)
	    	this.sticsItk.flag_eclairmult=true;
	    else
	    	this.sticsItk.flag_eclairmult=false;

	    if(sticsTransit.P_option_engrais_multiple == 1 )
	    	this.sticsItk.flag_plusieurs_engrais=true;
	    else
	    	this.sticsItk.flag_plusieurs_engrais=false;
	
	    if(sticsTransit.P_option_pature == 1 )
	    	this.sticsItk.flag_pature=true;
	    else
	    	this.sticsItk.flag_pature=false;
	    
	}
    
    /**
	 * STICS crop initialization (other simulation, NOT THE FIRST) 
	 */
    public void reinitialiseSticsCrop (SafeTestJNA jna, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
    								SafeGeneralParameters safeSettings, 
    								SafePlotSettings plotSettings, 
    								SafeEvolutionParameters evolutionParameters,
    								SafeSoil  soil) throws Exception {

    	String cropSpeciesName = this.getCropSpecies().getName();
 
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next ();

			//if it is perenial crop and the same species 
			if (this.getCropSpecies().getName().equals(cropSpeciesName) && cell.getCrop().isPerennial ()) {

				cell.getCrop().cropPerenialReload (jna,
						sticsParam, 
						sticsTransit, 
						this,
						evolutionParameters.exportDir);				
			}
			else {

				cell.getCrop().cropReload (jna,
						sticsParam, 
						sticsTransit, 
						soil,
						this,
						evolutionParameters.exportDir);
			}

			//IL 25-04-2018
			//repris de STICS verifPlante (si on le zappe ben ça marche plus) 
			cell.getCrop().sticsCrop.ipl = 1;
	
		}
		
		//init some STICS variables 
		this.sticsItk.ipl = 1;
		if (this.sticsItk.P_ressuite == 0) this.sticsItk.P_ressuite = 1;
		
	    if(sticsTransit.P_option_thinning== 1)
	    	this.sticsItk.flag_eclairmult=true;
	    else
	    	this.sticsItk.flag_eclairmult=false;

	    if(sticsTransit.P_option_engrais_multiple == 1 )
	    	this.sticsItk.flag_plusieurs_engrais=true;
	    else
	    	this.sticsItk.flag_plusieurs_engrais=false;
	
	    if(sticsTransit.P_option_pature == 1 )
	    	this.sticsItk.flag_pature=true;
	    else
	    	this.sticsItk.flag_pature=false;
	}   

	public int 	  getId () {return id;}
	public int getIdZone() {return getId();}	//for export
	public String getName () {return name;}
	public double getArea(){return area;}		//m2
	public SafeCropSpecies getCropSpecies(){return cropSpecies;}
	public String getCropSpeciesName(){return cropSpecies.getName();}
	public List<SafeCell> getCellList(){return cellList;}	
	public SafeCell getFirstCell () { 	
		if (cellList!=null) return cellList.get(0);
		else return null;
	}
	public int getFirstCellId () {
		if (cellList!=null)return getFirstCell().getId();
		else return 0;
	}
	
	public List<String> getItkList(){return itkList;}	
	public SafeSticsItk getSticsItk() {
		return sticsItk;
	}
	
	public int getSimulationDay(){return simulationDay;}
	public void setSimulationDay(int d){simulationDay=d;}
	public void addSimulationDay(){simulationDay+=1;}
	
	public int getSticsDay(){return sticsDay;}
	public void setSticsDay(int d){sticsDay=d;}
	public void addSticsDay(){sticsDay+=1;}

	public boolean getSimulationFinished(){return simulationFinished;}
	public void setSimulationFinished(boolean d){simulationFinished=d;}

	public int getInitialCropStage(){return initialCropStage;}
	public void setInitialCropStage(int d){initialCropStage=d;}
	public double getInitialCropLai(){return initialCropLai;}
	public void setInitialCropLai(double d){initialCropLai=d;}
	public double getInitialCropBiomass(){return initialCropBiomass;}
	public void setInitialCropBiomass(double d){initialCropBiomass=d;}	
	public double getInitialCropRootsDepth(){return initialCropRootsDepth;}
	public void setInitialCropRootsDepth(double d){initialCropRootsDepth=d;}	
	public double getInitialCropGrainBiomass (){return initialCropGrainBiomass ;}
	public void setInitialCropGrainBiomass (double d){initialCropGrainBiomass =d;}	
	public double getInitialCropNitrogen (){return initialCropNitrogen ;}
	public void setInitialCropNitrogen (double d){initialCropNitrogen =d;}	
	public double getInitialCropReserveBiomass(){return initialCropReserveBiomass ;}
	public void setInitialCropReserveBiomass (double d){initialCropReserveBiomass =d;}	
	public double getInitialCropRootsDensity(int i){return initialCropRootsDensity[i] ;}
	public void setInitialCropRootsDensity (int i, double d){initialCropRootsDensity[i] =d;}
	
	//****************************************
	//Total and mean values for EXPORT
	//****************************************
	public double getTotalCropBiomass() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getBiomass(); //// Aboveground dry matter (masec) t.ha-1
		}
		return total; //t.ha-1
	}
	
	public double getMeanCropBiomass() {
		return getTotalCropBiomass()  / getNbCells (); //t.ha-1
	}
	
	public double getMeanPeakCropBiomass() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getBiomassMax();
		}
		return total / getNbCells (); //t.ha-1
	}
	
	public double getTotalCropYield() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getYield();
		}
		return total;	//t.ha-1
	}

	public double getMeanCropYield() {
		return getTotalCropYield() / getNbCells ();	//t.ha-1
	}

	public double getMeanPeakCropYield() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getYieldMax();
		}
		return (total / getNbCells ());	//t.ha-1
	}
	
	public double getTotalCropLai() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getLai();		// m2.m-2
		}
		return total;
	}
	public double getMeanCropLai() {
		return getTotalCropLai()/ getNbCells ();	// m2.m-2
	}

	public double getMeanPeakCropLai() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getLaiMax();	// m2.m-2
		}
		return (total / getNbCells ());	// m2.m-2
	}

	public double getTotalCropEai() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getEai();	// m2.m-2
		}
		return total;	// m2.m-2
	}	
	public double getMeanCropEai() {
		return getTotalCropEai() / getNbCells ();	// m2.m-2
	}
	
	public double getMeanPeakCropEai() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getEaiMax();	// m2.m-2
		}
		return (total / getNbCells ());	// m2.m-2
	}
	
	public double getTotalCropHeight() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getHeight();	// Height of canopy (hauteur) mm
		}
		return total;	//m
	}

	public double getMeanCropHeight() {
		return getTotalCropHeight() / getNbCells ();		//m
	}

	public double getMeanCropGrainNumber() {
		double total = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropYield = cell.getCrop().getGrainBiomass();
			if (cropYield != 0) {
				total += cell.getCrop().getGrainNumber();		// nbr m-2
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		// nbr m-2
	}

	public double getMeanCropGrainWeight() {
		double total = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropYield = cell.getCrop().getGrainBiomass();
			if (cropYield != 0) {
				total += cell.getCrop().getGrainWeight();		//g
				count++;
			}

		}
		if (count > 0)
			total /= count;
		return total;			//g
	}

	public double getMeanCropPlantDensity() {
		double total = 0;
		int count = 0;
		double cropBiomass = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropBiomass = cell.getCrop().getBiomass();
			if (cropBiomass != 0) {
				total += cell.getCrop().getPlantDensity();		// nbr m-2
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		// nbr m-2
	}
	
	public double getMeanCropSla() {
		double total = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
				cropLai = cell.getCrop().getLai();
				if (cropLai != 0) {
					total += cell.getCrop().getSla();		// cm2 g-1;
					count++;
				}
		}
		if (count > 0)
			total /= count;	// cm2 g-1;
		return total;
	}

	public double getMeanCropTemperature() {
		double total = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCropTemperature();		//degree C;
		}

		return total / getNbCells ();			//degree C;
	}

	public double getMeanSoilSurfaceTemperature() {
		double total = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getSoilSurfaceTemperature();		//degree C;
		}

		return total / getNbCells ();			//degree C;
	}
	
	public double getMeanCropRootsLenght() {
		double total = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getTotalRootsLength();		//m
		}
		return total / getNbCells ();		//m
	}
	public double getMeanCropRootsDepth() {
		double total = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				total += cell.getCrop().getRootsDepth();		//m
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		//m
	}
	
	public double getMeanCropQngrain() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getQNgrain();	//Amount of nitrogen in harvested organs (grains / fruits)  kgN ha-1
		}
		return total / this.getNbCells();	//kgN ha-1
	}

	public double getMeanCropQnplante() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getQNplante();		//Amount of nitrogen taken up by the plant   kgN.ha-1
		}
		return total / this.getNbCells();	//kgN ha-1
	}

	public double getMeanCropCngrain() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCNgrain();		//Nitrogen concentration of grains %
		}
		return total / this.getNbCells();				//%
	}

	public double getMeanCropCnplante() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCNplante();		//Nitrogen concentration of entire plant %
		}
		return total / this.getNbCells();		//%
	}


	//LIGHT
	public double getTotalParIncident() {
		double total = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += (cell.getDirectParIncident() +   cell.getDiffuseParIncident() );	
		}
		return total;		//moles.m-2
	}
	
	public double getMeanParIncident() {
		return getTotalParIncident() / this.getNbCells();		//moles.m-2
	}
	
	public double getTotalParInterceptedByCrop() {
		double total = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeCrop crop = cell.getCrop();
			total += (crop.getDiffuseParIntercepted() + crop.getDirectParIntercepted());
		}
		return total;		//Moles PAR m-2
	}
	
	public double getMeanParInterceptedByCrop() {
		return getTotalParInterceptedByCrop() / this.getNbCells();		//Moles PAR m-2
	}
		

		
	//STRESSES
	public double getMeanCropHisafeWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				waterStress += cell.getCrop().getHisafeWaterStress();
				count++;
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}

	public double getMeanCropHisafeNitrogenStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getHisafeNitrogenStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}
	
	public double getMeanCropNitrogenLaiStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenLaiStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMeanCropNitrogenBiomassStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenBiomassStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMeanCropNitrogenSenescenceStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenSenescenceStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}
	
	
	
	//WATER
	public double getTotalWaterStock() {
		double waterStock = 0;

		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				waterStock += voxel[i].getWaterStock();
							
			}
		}
		return waterStock;	//liters
	}



	
	public double getTotalWaterUptakeByCrop() {
		double waterUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake +=cell.getCrop().getWaterUptake() * cell.getArea();	//convert mm to liters
		}
		return waterUptake; //liters
	}
	
	public double getTotalWaterUptakeInSaturationByCrop() {
		double waterUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake += cell.getWaterUptakeInSaturationByCrop()* cell.getArea();	//convert mm to liters
		}
		return waterUptake; //liters
	}
	
	public double getTotalWaterUptakeByTrees() {
		double waterUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
			waterUptake +=voxels[i].getTotalTreeWaterUptake();	//liters
			}
		
		}
		return waterUptake;	//liters
	}
	
	public double getTotalWaterUptakeInSaturationByTrees() {
		double waterUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake += cell.getWaterUptakeInSaturationByTrees();//liters
		}
		return waterUptake;	//liters
	}
	
	public double getTotalSoilEvaporation() {
		double waterEvap = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterEvap += cell.getCrop().getSoilEvaporation() *cell.getArea();//convert mm to liters
		}
		return waterEvap;//liters
	}

	public double getTotalIrrigation() {
		double irrig = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			irrig += cell.getCrop().getIrrigation() * cell.getArea();//convert mm to liters
		}
		return irrig;//liters
	}

	public double getTotalRainTransmittedByCrop() {
		double rain = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();	
			rain += cell.getRainTransmittedByCrop() * cell.getArea();//convert mm to liters
		}
		return rain;//liters	
	}
	
	public double getTotalRainInterceptedByCrop() {
		double intWater = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			intWater += cell.getRainInterceptedByCrop() * cell.getArea();//convert mm to liters
		}
		return intWater;//liters	
	}

	
	public double getTotalCropWaterDemand() {
		double wdem = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			wdem += cell.getCrop().getWaterDemand() * cell.getArea();//convert mm to liters
		}
		return wdem; //liters
	}

	public double getTotalCropWaterDemandReduced() {
		double wdem = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			wdem += cell.getCrop().getWaterDemandReduced()* cell.getArea();//convert mm to liters
		}
		return wdem; //liters
	}

	public double getTotalSurfaceRunOff() {
		double runOff = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			runOff += cell.getCrop().getSurfaceRunOff()* cell.getArea();//convert mm to liters
		}
		return runOff;//liters
	}
	
	public double getTotalDrainageBottom() {
		
		double drainage = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			drainage += cell.getCrop().getDrainageBottom()* cell.getArea();//convert mm to liters
		}
		return drainage; //liters
	}

	public double getTotalDrainageArtificial() {
		
		double drainage = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			drainage += cell.getCrop().getDrainageArtificial()* cell.getArea();//convert mm to liters
		}
		return drainage; //liters
	}
	
	public double getTotalWaterAddedByWaterTable() {
		double water = 0;

		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			water += cell.getWaterAddedByWaterTable()* cell.getArea();//convert mm to liters
		}
		return water; //liters
	}
	
	public double getTotalWaterToDesaturation() {
		double waterDesat = 0;

		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterDesat += cell.getWaterTakenByDesaturation()* cell.getArea();//convert mm to liters
		}
		return waterDesat;//liters
	}
	
	//NITROGEN
	public double getTotalCropNitrogenDemand() {
		double temp = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			temp += cell.getCrop().getNitrogenDemand() / 10000 * cell.getArea(); //kgN ha-1 to KgN
		}

		return temp;//kgN ;
	}	

	public double getTotalNitrogenUptakeByCrop() {
		double nitrogenUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
				nitrogenUptake +=voxels[i].getCropNitrogenUptake();	//gr
			}
		
		}
		return nitrogenUptake/1000;	//kg N
	}
	
	public double getTotalNitrogenUptakeByTrees() {
		double nitrogenUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
				nitrogenUptake +=voxels[i].getTotalTreeNitrogenUptake();	//gr
			}
		
		}
		return nitrogenUptake/1000;	//kg N
	}

	public double getTotalNitrogenUptakeInSaturationByCrop() {
		double nitrogenUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogenUptake += cell.getNitrogenUptakeInSaturationByCrop();
		}
		return nitrogenUptake; 	//kg N
	}
	

	
	public double getTotalNitrogenUptakeInSaturationByTrees() {
		double nitrogenUptake = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogenUptake += cell.getNitrogenUptakeInSaturationByTrees(); 
		}
		return nitrogenUptake;	//kg N
	}
	
	public double getTotalNitrogenFertilisationMineral() {
		double nitrogen = 0; 
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFertilisationMineral(); 
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenFertilisationOrganic() {
		double nitrogen = 0; 
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFertilisationOrganic(); 
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenIrrigation() {
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenIrrigation();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenRain() {
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenRain();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenRunOff() {
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getNitrogenRunOff();
		}
		return nitrogen;//kg N ha-1
	}
	
	

	
	public double getTotalNitrogenFixation() {
		
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFixation();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenHumusMineralisation() {
		
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenHumusMineralisation();
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenResiduMineralisation() {
		
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenResiduMineralisation();
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenDenitrification() {
		
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenDenitrification();
		}
		return nitrogen;//kg N ha-1
	}

	

	public double getTotalNitrogenLeachingBottom() {
		
		double nitrogen = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenLeachingBottom();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenLeachingWaterTable() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenLeachingWaterTable();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenAddedByWaterTable() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenAddedByWaterTable();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenImmobilisation() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenImmobilisation();
		}
		return v;//kg N ha-1
	}
	
	public double getTotalNitrogenVolatilisation() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenVolatilisation();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenVolatilisationOrganic() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenVolatilisationOrganic();
		}
		return v;//kg N ha-1
	}


	
	//RESIDUS
	public double getTotalBiomassRestitution() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getBiomassRestitution(); // quantity of aerial residues from the previous crop // t.ha-1
		}
		return v;	// t.ha-1
	}

	public double getTotalCarbonResidus() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getCarbonResidus();
		}
		return v;	//kgC.ha-1
	}

	public double getTotalNitrogenResidus() {
		
		double v = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenResidus();
		}
		return v;	//kgN.ha-1
	}
	
	//LITTERS
	public double getTotalTreeCarbonFoliageLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFoliageLitter();	
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenFoliageLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFoliageLitter();
		}
		return temp;	//kg N ha-1
	}

	
	public double getTotalTreeCarbonBranchesLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonBranchesLitter();	
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenBranchesLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenBranchesLitter();
		}
		return temp;	//kg N ha-1
	}
	
	public double getTotalTreeCarbonFruitLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFruitLitter();	
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenFruitLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFruitLitter();
		}
		return temp;	//kg N ha-1
	}
	public double getTotalTreeCarbonFineRootsLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFineRootsLitter();
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenFineRootsLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFineRootsLitter();
		}
		return temp;	//kg N ha-1
	}
	
	public double getTotalTreeCarbonCoarseRootsLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonCoarseRootsLitter();
		}
		return temp; //kg C ha-1
	}

	public double getTotalTreeNitrogenCoarseRootsLitter() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenCoarseRootsLitter();
		}
		return temp;	//kg N ha-1
	}	

	
	
	public double getTotalCarbonHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalNitrogenHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalInactiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalActiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalInactiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalActiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator<SafeCell>  c = this.cellList.iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}	


				
	//POUR FRANCESCO
	public double getMeanCropTempStressLue() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
		SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				ftemp += cell.getCrop().getSticsCrop().ftemp;
				count++;
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}
	
	public double getMeanCropTempStressGrainFilling() {
		double ftempremp = 0;
		int count = 0;
		double stade = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			stade = cell.getCrop().getPhenologicStageVegetative();
			//entre stade DRP et MAT
			if ((stade >= 2) && (stade <= 5)) {
				ftempremp += cell.getCrop().getSticsCrop().ftempremp;
				count++;
			}
		}
		if (count > 0)
			ftempremp /= count;
		return ftempremp;
	}

	public double getMeanCropFrostStressPlantDensity() {
		double ftemp = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			ftemp += cell.getCrop().getSticsCrop().fgellev;
		}
		return ftemp / getNbCells();
	}

	
	public double getMeanCropFrostStressFoliage() {
		double ftemp = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			ftemp += cell.getCrop().getSticsCrop().fstressgel;
		}

		return ftemp / getNbCells();
	}

	public double getMeanCropFrostStressReprod() {
		double ftemp = 0;
		for (Iterator<SafeCell> c = this.cellList.iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			ftemp += cell.getCrop().getSticsCrop().fgelflo;
		}
		return ftemp / getNbCells();
	}
	

	public int getNbCells () {
		int nb  = 0;
		if (cellList!=null) nb = this.cellList.size();
		return nb;
	}
	public String toString(){
		String str = "";
		str = "CropZone id="+id+" name="+name+" area="+area+" nbCells="+getNbCells();
		if (this.getFirstCell()!=null) {
			if (this.getFirstCell().getCrop()!=null)
				str = str+" crop ="+this.getFirstCell().getCrop().getCropSpeciesName();
		}

		return str;
	}
	public void printVerif (Path myfile) throws IOException {
		
		String line = System.getProperty("line.separator")+"CROP = " +this.getCropSpeciesName();

		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);

		
		line = System.getProperty("line.separator")+"Date start = " +this.getSticsItk().getMonthstart()+"-"+ this.getSticsItk().getDaystart();
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		
		line = System.getProperty("line.separator")+"Sowing = " + getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_iplt0);
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		
		if ( this.getSticsItk().P_nbjres > 0) {
			line = System.getProperty("line.separator")+"Residue incorporation = ";
			for(int i=0; i< this.getSticsItk().P_nbjres; i++){
				line = line+ getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_julres[i]) + " ";
			}
			
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		}
		if ( this.getSticsItk().P_nbjtrav > 0) {
			line = System.getProperty("line.separator")+"Tillage = ";
			for(int i=0; i< this.getSticsItk().P_nbjtrav; i++){
				line = line+ getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_jultrav[i]) + " ";
			}
			
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		}
		
		if ( this.getSticsItk().nap > 0) {
			line = System.getProperty("line.separator")+"Irrigation = ";
			for(int i=0; i< this.getSticsItk().nap; i++){
				line = line+ getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_julapI[i]) + " ";
			}
			
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		}
	
		if ( this.getSticsItk().napN > 0) {
			line = System.getProperty("line.separator")+"Fertilization = ";
			for(int i=0; i< this.getSticsItk().napN; i++){
				line = line+ getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_julapN[i]) + " ";
			}
			
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		}
	
		if ( this.getSticsItk().nbcoupe > 0) {
			line = System.getProperty("line.separator")+"Cutting = ";
			for(int i=0; i< this.getSticsItk().nbcoupe; i++){
				line = line+ getMMDD(this.getSticsItk().getYearstart(), this.getSticsItk().P_julfauche[i]) + " ";
			}
			
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		}
		
		
		line = System.getProperty("line.separator")+"Date end = " + this.getSticsItk().getMonthend()+"-"+ this.getSticsItk().getDayend();
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		
		line = System.getProperty("line.separator")+"##===============================================";
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		
	}
	
	public String getMMDD  (int year, int julianDay)
	{

		GregorianCalendar date = new GregorianCalendar();
		date.set(GregorianCalendar.YEAR, year);
		date.set(GregorianCalendar.DAY_OF_YEAR, julianDay);
		 
		int mm = date.get(GregorianCalendar.MONTH)+1;
		int dd = date.get(GregorianCalendar.DAY_OF_MONTH);
		return ""+mm+"-"+dd;
	}
}
