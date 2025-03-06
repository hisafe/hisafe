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
package safe.stics;

import java.util.GregorianCalendar;
import java.util.Iterator;

import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.model.*;


/**
 * Format to read STICS crop management parameters file 
 * 
 * @author Isabelle Lecomte - December 2016
 */

public class SafeSticsItkFormat extends RecordSet {

	private static final long serialVersionUID = 1L;

		// Soil residus incorporation record is described here
		@Import
		static public class SoilResidusRecord extends Record {
			private static final long serialVersionUID = 1L;
			public String 	name;    		// to define parameter name
			public String 	dateres;		//julres in MM-JJ (IL 25/05/2023)
			public int 		coderes;
			public float 	qres;
			public float 	Crespc;
			public float 	CsurNres;
			public float 	Nminres;
			public float 	eaures;

			public SoilResidusRecord () {super ();}
			public SoilResidusRecord (String line) throws Exception {super (line);}

		}
		// Soil management record is described here
		@Import
		static public class SoilManagementRecord extends Record {
			private static final long serialVersionUID = 1L;
			public String 	name;    		// to define parameter name
			public String 	datetrav;		//jultrav in MM-JJ (IL 25/05/2023)
			public float 	profres;
			public float 	proftrav;


			public SoilManagementRecord () {super ();}
			public SoilManagementRecord (String line) throws Exception {super (line);}

		}
		// Irrigation and Fertilisation record is described here
		@Import
		static public class IrrigationAndFertilisationRecord extends Record {
			public String 	name;    		// to define parameter name
			public String 	dateap;			//julap in MM-JJ (IL 25/05/2023)
			public float 	dose;

			public IrrigationAndFertilisationRecord () {super ();}
			public IrrigationAndFertilisationRecord (String line) throws Exception {super (line);}

		}

		//Safe crop irrigation record is described here
		@Import
		 static public class CuttingRecord extends Record {
			private static final long serialVersionUID = 1L;
			public String 	name;    		// to define parameter name
			public String 	datefauche;		//julfauche in MM-JJ (IL 25/05/2023)
			public float 	hautcoupe;		//m
			public float 	lairesiduel;	//m2 leaf m-2 soil
			public float 	msresiduel;		//t ha-1
			public float 	anitcoupe;		//kg N ha-1 

			public CuttingRecord () { super (); }
			public CuttingRecord (String line) throws Exception { super (line); }

		 }


		public SafeSticsItkFormat (String fileName) throws Exception {
			prepareImport (fileName);
		}

		/**
		* Saving data in a recordSet
		*/
		public SafeSticsItkFormat (SafeSticsItk sci)
							throws Exception {createRecordSet (sci);}

		public void createRecordSet (SafeSticsItk sci) throws Exception {

				
		}
		//julian days replaced by MM-JJ (IL 25/05/2023)
		public int getJulianDay (String dateMMDD, int year, int dayStart) throws Exception
		{
			if (dateMMDD.equals ("999")) return 999;
			if (dateMMDD.equals ("0")) return 0;
			if (dateMMDD.length()!=5) 	throw new CancellationException();	// abort
			if (!dateMMDD.contains("-")) 	throw new CancellationException();	// abort
			String [] part1 = dateMMDD.split("-");
			GregorianCalendar date = new GregorianCalendar();
			date.set(year,Integer.parseInt(part1[0])-1,Integer.parseInt(part1[1]) );
			int jul =  date.get(GregorianCalendar.DAY_OF_YEAR);
			if (jul < dayStart) jul=jul+365;
			return jul;
		}
		/**
		* Read data from a recordSet
		*/
		public String load (SafeCropZone cropZone, SafeSticsItk cropItk, int year) throws Exception {


			String cropSpeciesFileName = ""; 
			int indexSoilResidus = 0;
			int indexSoilManagement = 0;
			int indexIrrigation = 0;
			int indexFertilisation = 0;
			int indexCutting = 0;   
			int julianDayStart = 0;
			int julianDayEnd = 0;
			
			
			for (Iterator i = this.iterator (); i.hasNext ();) {

				Record record = (Record) i.next ();

				if (record instanceof SafeSticsItkFormat.SoilResidusRecord) {

					SafeSticsItkFormat.SoilResidusRecord r =
							(SafeSticsItkFormat.SoilResidusRecord) record;	// cast to precise type
					try {
						if(indexSoilResidus < cropItk.P_nbjres){
							//julian days replaced by MM-JJ (IL 25/05/2023)
							cropItk.P_julres[indexSoilResidus] = getJulianDay (r.dateres, year, julianDayStart);				
							cropItk.P_coderes[indexSoilResidus] = r.coderes;
							cropItk.P_qres[indexSoilResidus]	= r.qres;
							cropItk.P_Crespc[indexSoilResidus] 	= r.Crespc;
							cropItk.P_CsurNres[indexSoilResidus] = r.CsurNres;
							cropItk.P_Nminres[indexSoilResidus] = r.Nminres;
							cropItk.P_eaures[indexSoilResidus]	= r.eaures;
							
							if (cropItk.P_julres[indexSoilResidus]  < julianDayStart || cropItk.P_julres[indexSoilResidus] > julianDayEnd) {
								System.out.println ("RESIDUE julres:"+cropItk.P_julres[indexSoilResidus]  +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
								throw new Exception ("Itk error");
							}

							indexSoilResidus++; 
						}
					} catch(Exception e){
						System.out.println("Wrong itk crop file format parameters : RESIDUE julres");
						throw new CancellationException();	// abort
					}
				}
				else if (record instanceof SafeSticsItkFormat.SoilManagementRecord) {

					SafeSticsItkFormat.SoilManagementRecord r =
							(SafeSticsItkFormat.SoilManagementRecord) record;	// cast to precise type
					
					try {
						if(indexSoilManagement < cropItk.P_nbjtrav){
							//julian days replaced by MM-JJ (IL 25/05/2023)
							cropItk.P_jultrav[indexSoilManagement] = getJulianDay (r.datetrav, year, julianDayStart);	
							cropItk.P_profres[indexSoilManagement] 	= r.profres;
							cropItk.P_proftrav[indexSoilManagement] = r.proftrav;
							
							if (cropItk.P_jultrav[indexSoilManagement]  < julianDayStart || cropItk.P_jultrav[indexSoilManagement] > julianDayEnd) {
								System.out.println ("TILLAGE jultrav:"+cropItk.P_jultrav[indexSoilManagement]   +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
								throw new Exception ("Itk error");
							}
							
							indexSoilManagement++; 
						}
					} catch(Exception e){
						System.out.println("Wrong itk crop file format parameters : TILLAGE jultrav");
						throw new CancellationException();	// abort
					}
				}				
				else if (record instanceof SafeSticsItkFormat.IrrigationAndFertilisationRecord){
					SafeSticsItkFormat.IrrigationAndFertilisationRecord r =
							(SafeSticsItkFormat.IrrigationAndFertilisationRecord) record;	// cast to precise type

					if (r.name.equals("IRRIGATION")){
						try {
							if(indexIrrigation < cropItk.nap){
	
								if (cropItk.P_codedateappH2O == 2) {//jour julian
									//julian days replaced by MM-JJ (IL 25/05/2023)
									cropItk.P_julapI[indexIrrigation] = getJulianDay (r.dateap, year, julianDayStart);	
								}
								else	//degres jour
									cropItk.P_upvttapI[indexIrrigation] 	= Integer.parseInt(r.dateap);
								
								cropItk.P_doseI[indexIrrigation] = r.dose;
								
								if (cropItk.P_julapI[indexIrrigation]  < julianDayStart || cropItk.P_julapI[indexIrrigation] > julianDayEnd) {
									System.out.println ("IRRIGATION julapI:"+cropItk.P_julapI[indexIrrigation]  +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
								
								
								indexIrrigation++; 
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : IRRIGATION julapI");
							throw new CancellationException();	// abort
						}
					}
					if (r.name.equals("FERTILIZATION")){
						try {
							if(indexFertilisation < cropItk.napN) {
	
								if (cropItk.P_codedateappN == 2) { //jour julian
									//julian days replaced by MM-JJ (IL 25/05/2023)
									cropItk.P_julapN[indexFertilisation]  = getJulianDay (r.dateap, year, julianDayStart);	
								}
								else  //degres jour
									cropItk.P_upvttapN[indexFertilisation] 	=  Integer.parseInt(r.dateap);
								
							
								if (cropItk.P_codefracappN == 1)  //dose entiere							
									cropItk.P_doseN[indexFertilisation] 	= r.dose;
								else //fraction
									cropItk.P_fracN[indexFertilisation] 	= r.dose;
									
								if (cropItk.P_julapN[indexFertilisation]  < julianDayStart || cropItk.P_julapN[indexFertilisation] > julianDayEnd) {
									System.out.println ("FERTILIZATION julapN:"+cropItk.P_julapN[indexFertilisation]  +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
								
								indexFertilisation++; 
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : FERTILIZATION julapN");
							throw new CancellationException();	// abort
						}
					}
				}
				else if (record instanceof SafeSticsItkFormat.CuttingRecord){
					SafeSticsItkFormat.CuttingRecord r =
							(SafeSticsItkFormat.CuttingRecord) record;	// cast to precise type
					try {
						if(indexCutting < cropItk.nbcoupe) {
	
							//julian days replaced by MM-JJ (IL 25/05/2023)
							cropItk.P_julfauche[indexCutting] = getJulianDay (r.datefauche, year, julianDayStart);
		
							//ici il faut decaller sinon cela deconne 
							cropItk.P_hautcoupe [indexCutting+1] 	= r.hautcoupe;
							cropItk.P_lairesiduel [indexCutting+1] = r.lairesiduel;
							cropItk.P_msresiduel [indexCutting+2] = r.msresiduel;
							cropItk.P_anitcoupe [indexCutting+1] 	= r.anitcoupe;
							
							if (cropItk.P_julfauche[indexCutting]  < julianDayStart || cropItk.P_julfauche[indexCutting] > julianDayEnd) {
								System.out.println ("CUTTING julfauche:"+cropItk.P_julfauche[indexCutting]  +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
								throw new Exception ("Itk error");
							}
							
							
							indexCutting++; 
						}
					} catch(Exception e){
						System.out.println("Wrong itk crop file format parameters : CUTTING julfauche");
						throw new CancellationException();	// abort
					}

				} //KEY  Records
				else if (record instanceof SafeSticsItkFormat.KeyRecord) {
					SafeSticsItkFormat.KeyRecord r =
							(SafeSticsItkFormat.KeyRecord) record;	// cast to precise type
					
					if (r.key.equals ("species")) {
						cropSpeciesFileName = r.value;
					}
					else if (r.key.equals ("variete")) {
						cropItk.P_variete = r.getIntValue ();
					}			
					else if (r.key.equals ("daystart")) {
						String [] part1 = r.value.split("-");
						cropItk.setMonthstart(Integer.parseInt(part1[0]));
						cropItk.setDaystart(Integer.parseInt(part1[1]));
						GregorianCalendar date = new GregorianCalendar();
						date.set(year,Integer.parseInt(part1[0])-1,Integer.parseInt(part1[1]) );
						julianDayStart =  date.get(GregorianCalendar.DAY_OF_YEAR);
					}
					else if (r.key.equals ("dayend")) {
						String [] part1 = r.value.split("-");
						cropItk.setMonthend(Integer.parseInt(part1[0]));
						cropItk.setDayend(Integer.parseInt(part1[1]));
						GregorianCalendar date = new GregorianCalendar();
						date.set(year,Integer.parseInt(part1[0])-1,Integer.parseInt(part1[1]) );
						julianDayEnd =  date.get(GregorianCalendar.DAY_OF_YEAR);
						if (julianDayEnd <= julianDayStart) julianDayEnd = julianDayEnd + 365;
						
					}					
					else if (r.key.equals ("nbjres")) {
						cropItk.P_nbjres 	= r.getIntValue ();
					}
					else if (r.key.equals ("nbjtrav")) {
						cropItk.P_nbjtrav 	= r.getIntValue ();
					}					
					else if (r.key.equals ("nap")) {
						cropItk.nap 		= r.getIntValue ();
					}
					else if (r.key.equals ("napN")) {
						cropItk.napN		= r.getIntValue ();
					}
					else if (r.key.equals ("nbcoupe")) {
						cropItk.nbcoupe = r.getIntValue();
					}					
					else if (r.key.equals ("iplt0")) {						
					
						try {
							//julian days replaced by MM-JJ (IL 25/05/2023)
							cropItk.P_iplt0 = getJulianDay (r.value, year, julianDayStart);	
							if (cropItk.P_iplt0 != 0 && cropItk.P_iplt0 != 999) {
								if (cropItk.P_iplt0 < julianDayStart || cropItk.P_iplt0 > julianDayEnd) {
									System.out.println ("SOWING iplt0:"+cropItk.P_iplt0 +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : iplt0");
							throw new CancellationException();	// abort
						}
					}				
					else if (r.key.equals ("profsem")) {
						cropItk.P_profsem = r.getFloatValue ();
					}
					else if (r.key.equals ("densitesem")) {
						cropItk.P_densitesem = r.getFloatValue ();
					}
					else if (r.key.equals ("codetradtec")) {
						cropItk.P_codetradtec = r.getIntValue ();
					}
					else if (r.key.equals ("interrang")) {
						cropItk.P_interrang = r.getFloatValue ();
					}
					else if (r.key.equals ("orientrang")) {
						cropItk.P_orientrang = r.getFloatValue ();
					}
					else if (r.key.equals ("codedecisemis")) {
						cropItk.P_codedecisemis = r.getIntValue ();
					}
					else if (r.key.equals ("nbjmaxapressemis")) {
						cropItk.P_nbjmaxapressemis = r.getIntValue ();
					}		
					else if (r.key.equals ("nbjseuiltempref")) {
						cropItk.P_nbjseuiltempref = r.getIntValue ();
					}	
					else if (r.key.equals ("codestade")) {
						cropItk.P_codestade = r.getIntValue ();
					}	
					//julian days replaced by MM-JJ (IL 25/05/2023)
					else if (r.key.equals ("ilev")) {
						try {
							cropItk.P_ilev = getJulianDay (r.value, year, julianDayStart);	
							if (cropItk.P_ilev != 999) {
								if (cropItk.P_ilev < julianDayStart || cropItk.P_ilev > julianDayEnd) {
									System.out.println ("STAGE FORCING ilev:"+cropItk.P_ilev +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : ilev");
							throw new CancellationException();	// abort
						}

					}
					else if (r.key.equals ("iamf")) {			
						try {
							cropItk.P_iamf = getJulianDay (r.value, year, julianDayStart);
						
							if (cropItk.P_iamf != 999) {
								if (cropItk.P_iamf < julianDayStart || cropItk.P_iamf > julianDayEnd) {
									System.out.println ("STAGE FORCING iamf:"+cropItk.P_iamf +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : iamf");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("ilax")) {
						try {
					
							cropItk.P_ilax = getJulianDay (r.value, year, julianDayStart);	
							if (cropItk.P_ilax != 999) {
								if (cropItk.P_ilax < julianDayStart || cropItk.P_ilax > julianDayEnd) {
									System.out.println ("STAGE FORCING ilax:"+cropItk.P_ilax +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : ilax");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("isen")) {
						try {
							cropItk.P_isen = getJulianDay (r.value, year, julianDayStart);	
							if (cropItk.P_isen != 999) {
								if (cropItk.P_isen < julianDayStart || cropItk.P_isen > julianDayEnd) {
									System.out.println ("STAGE FORCING isen:"+cropItk.P_isen +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : isen");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("ilan")) {
						try {
							cropItk.P_ilan = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_ilan != 999) {
								if (cropItk.P_ilan < julianDayStart || cropItk.P_ilan > julianDayEnd) {
									System.out.println ("STAGE FORCING ilan:"+cropItk.P_ilan +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : ilan");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("iflo")) {
						try {
							cropItk.P_iflo = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_iflo != 999) {
								if (cropItk.P_iflo < julianDayStart || cropItk.P_iflo > julianDayEnd) {
									System.out.println ("STAGE FORCING iflo:"+cropItk.P_iflo +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : iflo");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("idrp")) {
						try {
							cropItk.P_idrp = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_idrp != 999) {
								if (cropItk.P_idrp < julianDayStart || cropItk.P_idrp > julianDayEnd) {
									System.out.println ("STAGE FORCING idrp:"+cropItk.P_idrp +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : idrp");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("imat")) {
						try {
							cropItk.P_imat = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_imat != 999) {
								if (cropItk.P_imat < julianDayStart || cropItk.P_imat > julianDayEnd) {
									System.out.println ("STAGE FORCING imat:"+cropItk.P_imat +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : imat");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("irec")) {
						try {
							cropItk.P_irec = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_irec != 999) {
								if (cropItk.P_irec < julianDayStart || cropItk.P_irec > julianDayEnd) {
									System.out.println ("STAGE FORCING irec:"+cropItk.P_irec +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : irec");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("irecbutoir")) {
						try {
							cropItk.P_irecbutoir = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_irecbutoir != 999) {
								if (cropItk.P_irecbutoir < julianDayStart || cropItk.P_irecbutoir > julianDayEnd) {
									System.out.println ("STAGE FORCING irecbutoir:"+cropItk.P_irecbutoir +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : irecbutoir");
							throw new CancellationException();	// abort
						}

					}
					else if (r.key.equals ("effirr")) {
						cropItk.P_effirr = r.getFloatValue ();
					}
					else if (r.key.equals ("codecalirrig")) {
						cropItk.P_codecalirrig = r.getIntValue ();
					}
					else if (r.key.equals ("ratiol")) {
						cropItk.P_ratiol = r.getFloatValue ();
					}
					else if (r.key.equals ("dosimx")) {
						cropItk.P_dosimx = r.getFloatValue ();
					}
					else if (r.key.equals ("doseirrigmin")) {
						cropItk.P_doseirrigmin = r.getFloatValue ();
					}					
					else if (r.key.equals ("codedateappH2O")) {
						cropItk.P_codedateappH2O = r.getIntValue ();
					}
					else if (r.key.equals ("codlocirrig")) {
						cropItk.P_codlocirrig = r.getIntValue ();
					}		
					else if (r.key.equals ("locirrig")) {
						cropItk.P_locirrig = r.getIntValue ();
					}						
					else if (r.key.equals ("profmes")) {
						cropItk.P_profmes = r.getFloatValue ();
					}

					else if (r.key.equals ("codecalferti")) {
						cropItk.P_codecalferti = r.getIntValue ();
					}	
					else if (r.key.equals ("codetesthumN")) {
						cropItk.P_codetesthumN = r.getIntValue ();
					}	
					else if (r.key.equals ("dosimxN")) {
						cropItk.P_dosimxN = r.getFloatValue ();
					}	
					else if (r.key.equals ("ratiolN")) {
						cropItk.P_ratiolN = r.getFloatValue ();
					}
					else if (r.key.equals ("engrais")) {
						cropItk.P_engrais[0] = r.getIntValue();
					}
					else if (r.key.equals ("concirr")) {
						cropItk.P_concirr = r.getFloatValue ();
					}	
					else if (r.key.equals ("codedateappN")) {
						cropItk.P_codedateappN = r.getIntValue();
					}		
					else if (r.key.equals ("codefracappN")) {
						cropItk.P_codefracappN = r.getIntValue();
					}
					else if (r.key.equals ("Qtot_N")) {
						cropItk.P_Qtot_N = r.getIntValue();
					}
					else if (r.key.equals ("codlocferti")) {
						cropItk.P_codlocferti = r.getIntValue();
					}
					else if (r.key.equals ("locferti")) {
						cropItk.P_locferti = r.getIntValue();
					}					
					else if (r.key.equals ("ressuite")) {
						String ressuite = r.value;
						if (ressuite.equals("roots"))
							cropItk.P_ressuite = 1;
						else if (ressuite.equals("whole_crop"))
							cropItk.P_ressuite = 2;
						else if (ressuite.equals("straw+roots"))
							cropItk.P_ressuite = 3;	
						else if (ressuite.equals("stubble+roots"))
							cropItk.P_ressuite = 4;
						else if (ressuite.equals("stubble_of_residu_type_9+roots"))
							cropItk.P_ressuite = 5;	
						else if (ressuite.equals("stubble_of_residu_type_10+roots"))
							cropItk.P_ressuite = 6;	
						else if (ressuite.equals("prunings"))
							cropItk.P_ressuite = 7;							
					}					
		
					else if (r.key.equals ("ressuite")) {
						cropItk.P_ressuite = r.getIntValue ();
					}
					else if (r.key.equals ("codceuille")) {
						cropItk.P_codcueille = r.getIntValue ();
					}
					//attention erreur de nom dans les fichier parametre itk de STICS (nbceuille au lieu de nbcueille)
					else if (r.key.equals ("nbceuille")) {
						cropItk.P_nbcueille = r.getIntValue ();
					}
					else if (r.key.equals ("cadencerec")) {
						cropItk.P_cadencerec = r.getIntValue ();
					}		
					else if (r.key.equals ("codrecolte")) {
						cropItk.P_codrecolte = r.getIntValue ();
					}
					else if (r.key.equals ("codeaumin")) {
						cropItk.P_codeaumin = r.getIntValue ();
					}					
					else if (r.key.equals ("h2ograinmin")) {
						cropItk.P_h2ograinmin = r.getFloatValue ();
					}
					else if (r.key.equals ("h2ograinmax")) {
						cropItk.P_h2ograinmax = r.getFloatValue ();
					}
					else if (r.key.equals ("sucrerec")) {
						cropItk.P_sucrerec = r.getFloatValue ();
					}
					else if (r.key.equals ("CNgrainrec")) {
						cropItk.P_CNgrainrec = r.getFloatValue ();
					}
					else if (r.key.equals ("huilerec")) {
						cropItk.P_huilerec = r.getFloatValue ();
					}
					else if (r.key.equals ("coderecolteassoc")) {
						cropItk.P_coderecolteassoc = r.getIntValue ();
					}
					else if (r.key.equals ("codedecirecolte")) {
						cropItk.P_codedecirecolte = r.getIntValue ();
					}
					else if (r.key.equals ("nbjmaxapresrecolte")) {
						cropItk.P_nbjmaxapresrecolte = r.getIntValue ();
					}				
					else if (r.key.equals ("codefauche")) {
						cropItk.P_codefauche = r.getIntValue ();
					}
					else if (r.key.equals ("mscoupemini")) {
						cropItk.P_mscoupemini[0] = r.getFloatValue ();
					}
					else if (r.key.equals ("codemodfauche")) {
						cropItk.P_codemodfauche = r.getIntValue ();
					      if(cropItk.P_codemodfauche == 1) cropItk.lecfauche=false;
					      else cropItk.lecfauche=true;
					}
					else if (r.key.equals ("hautcoupedefaut")) {
						cropItk.P_hautcoupedefaut = r.getFloatValue ();
					}
					else if (r.key.equals ("stadecoupedf")) {
						String stadecoupedf = r.value;
						if (stadecoupedf.equals("dor"))
							cropItk.P_stadecoupedf= 3;
						else if (stadecoupedf.equals("amf"))
							cropItk.P_stadecoupedf= 6;
						else if (stadecoupedf.equals("flo"))
							cropItk.P_stadecoupedf= 7;
						else if (stadecoupedf.equals("drp"))
							cropItk.P_stadecoupedf= 8;	
						else if (stadecoupedf.equals("des"))
							cropItk.P_stadecoupedf= 9;	
						else if (stadecoupedf.equals("mat"))
							cropItk.P_stadecoupedf= 10;							
						else if (stadecoupedf.equals("rec"))
							cropItk.P_stadecoupedf= 11;	
						else if (stadecoupedf.equals("lax"))
							cropItk.P_stadecoupedf= 12;							
						else if (stadecoupedf.equals("sen"))
							cropItk.P_stadecoupedf= 13;	
						else if (stadecoupedf.equals("lan"))
							cropItk.P_stadecoupedf= 15;	
	
					}
					else if (r.key.equals ("codepaillage")) {
						cropItk.P_codepaillage = r.getIntValue ();
					}
					else if (r.key.equals ("couvermulchplastique")) {
						cropItk.P_couvermulchplastique = r.getFloatValue ();
					}
					else if (r.key.equals ("albedomulchplastique")) {
						cropItk.P_albedomulchplastique = r.getFloatValue ();
					}					
					else if (r.key.equals ("codrognage")) {
						cropItk.P_codrognage = r.getIntValue ();
					}					
					else if (r.key.equals ("largrogne")) {
						cropItk.P_largrogne = r.getFloatValue ();
					}					
					else if (r.key.equals ("hautrogne")) {
						cropItk.P_hautrogne = r.getFloatValue ();
					}					
					else if (r.key.equals ("biorognem")) {
						cropItk.P_biorognem = r.getFloatValue ();
					}	
					//julian days replaced by MM-JJ (IL 25/05/2023)
					else if (r.key.equals ("julrogne")) {
						try {
							cropItk.P_julrogne = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_julrogne != 0 && cropItk.P_julrogne != 999) {
								if (cropItk.P_julrogne < julianDayStart || cropItk.P_julrogne > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS julrogne:"+cropItk.P_julrogne +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : julrogne");
							throw new CancellationException();	// abort
						}
					}	
					else if (r.key.equals ("juleclair")) {
						try {
							cropItk.P_juleclair[0] = getJulianDay (r.value, year, julianDayStart);	
							if (cropItk.P_juleclair[0]  != 0 && cropItk.P_juleclair[0]  != 999) {
								if (cropItk.P_juleclair[0]  < julianDayStart || cropItk.P_juleclair[0]  > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS juleclair:"+cropItk.P_juleclair[0] +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : juleclair");
							throw new CancellationException();	// abort
						}
					}
					
					else if (r.key.equals ("codcalrogne")) {
						cropItk.P_codcalrogne = r.getIntValue ();
					}						
					else if (r.key.equals ("margerogne")) {
						cropItk.P_margerogne = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeclaircie")) {
						cropItk.P_codeclaircie = r.getIntValue ();
					}		
					else if (r.key.equals ("codeffeuil")) {
						cropItk.P_codeffeuil = r.getIntValue ();
					}
					else if (r.key.equals ("codhauteff")) {
						cropItk.P_codhauteff = r.getIntValue ();
					}
					else if (r.key.equals ("codcaleffeuil")) {
						cropItk.P_codcaleffeuil = r.getIntValue ();
					}
					else if (r.key.equals ("laidebeff")) {
						cropItk.P_laidebeff = r.getFloatValue ();
					}
					else if (r.key.equals ("effeuil")) {
						cropItk.P_effeuil = r.getFloatValue ();
					}
					//julian days replaced by MM-JJ (IL 25/05/2023)
					else if (r.key.equals ("juleffeuil")) {
						try {
							cropItk.P_juleffeuil = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_juleffeuil  != 0 && cropItk.P_juleffeuil  != 999) {
								if (cropItk.P_juleffeuil  < julianDayStart || cropItk.P_juleffeuil > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS juleffeuil:"+cropItk.P_juleffeuil +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : juleffeuil");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("jultaille")) {
						try {
							cropItk.P_jultaille = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_jultaille  != 0 && cropItk.P_jultaille  != 999) {
								if (cropItk.P_jultaille  < julianDayStart || cropItk.P_jultaille  > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS jultaille:"+cropItk.P_jultaille +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : jultaille");
							throw new CancellationException();	// abort
						}
					}					
					else if (r.key.equals ("laieffeuil")) {
						cropItk.P_laieffeuil = r.getFloatValue ();
					}
					else if (r.key.equals ("codetaille")) {
						cropItk.P_codetaille = r.getIntValue ();
					}

					else if (r.key.equals ("codepalissage")) {
						cropItk.P_codepalissage = r.getIntValue ();
					}
					else if (r.key.equals ("hautmaxtec")) {
						cropItk.P_hautmaxtec = r.getFloatValue ();
					}
					else if (r.key.equals ("largtec")) {
						cropItk.P_largtec = r.getFloatValue ();
					}
					else if (r.key.equals ("codabri")) {
						cropItk.P_codabri = r.getIntValue ();
					}	
					else if (r.key.equals ("transplastic")) {
						cropItk.P_transplastic = r.getFloatValue ();
					}
					else if (r.key.equals ("surfouvre1")) {
						cropItk.P_surfouvre1 = r.getFloatValue ();
					}

					else if (r.key.equals ("surfouvre2")) {
						cropItk.P_surfouvre2 = r.getFloatValue ();
					}
					else if (r.key.equals ("surfouvre3")) {
						cropItk.P_surfouvre3 = r.getFloatValue ();
					}
					//julian days replaced by MM-JJ (IL 25/05/2023)
					else if (r.key.equals ("julouvre2")) {
						try {
							cropItk.P_julouvre2 = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_julouvre2  != 0 && cropItk.P_julouvre2  != 999) {
								if (cropItk.P_julouvre2  < julianDayStart || cropItk.P_julouvre2  > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS julouvre2:"+cropItk.P_julouvre2 +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : julouvre2");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("julouvre3")) {
							try {
							cropItk.P_julouvre3 = getJulianDay (r.value, year, julianDayStart);
							if (cropItk.P_julouvre3  != 0 && cropItk.P_julouvre3  != 999) {
								if (cropItk.P_julouvre3  < julianDayStart || cropItk.P_julouvre3  > julianDayEnd) {
									System.out.println ("OTHER INTERVENTIONS julouvre3:"+cropItk.P_julouvre3 +" is not between daystart:"+julianDayStart+" and dayend:" +julianDayEnd);
									throw new Exception ("Itk error");
								}
							}
						} catch(Exception e){
							System.out.println("Wrong itk crop file format parameters : julouvre3");
							throw new CancellationException();	// abort
						}
					}
					else if (r.key.equals ("codeDST")) {
						cropItk.P_codeDST = r.getIntValue ();
					}					
					else if (r.key.equals ("dachisel")) {
						cropItk.P_dachisel = r.getFloatValue ();
					}	
					else if (r.key.equals ("dalabour")) {
						cropItk.P_dalabour = r.getFloatValue ();
					}	
					else if (r.key.equals ("rugochisel")) {
						cropItk.P_rugochisel = r.getFloatValue ();
					}	
					else if (r.key.equals ("rugolabour")) {
						cropItk.P_rugolabour = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeDSTtass")) {
						cropItk.P_codeDSTtass = r.getIntValue ();
					}	
					else if (r.key.equals ("profhumsemoir")) {
						cropItk.P_profhumsemoir = r.getFloatValue ();
					}	
					else if (r.key.equals ("dasemis")) {
						cropItk.P_dasemis = r.getFloatValue ();
					}	
					else if (r.key.equals ("profhumrecolteuse")) {
						cropItk.P_profhumrecolteuse = r.getFloatValue ();
					}	
					else if (r.key.equals ("P_darecolte")) {
						cropItk.P_darecolte = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeDSTnbcouche")) {
						cropItk.P_codeDSTnbcouche = r.getIntValue ();
					
					
					// CROP INIT	
					
						// CROP INIT	
						
					} else if (r.key.equals("initialCropStage")) {
				
						cropZone.setInitialCropStage(r.getIntValue());
				
					} else if (r.key.equals("initialCropLai")) {
				
						cropZone.setInitialCropLai(Double.parseDouble(r.value));
			
					} else if (r.key.equals("initialCropBiomass")) {
						
						cropZone.setInitialCropBiomass(Double.parseDouble(r.value));
					
					} else if (r.key.equals("initialCropRootDepth")) {
					
						cropZone.setInitialCropRootsDepth(Double.parseDouble(r.value));
					
					} else if (r.key.equals("initialCropGrainBiomass")) {
					
						cropZone.setInitialCropGrainBiomass(Double.parseDouble(r.value));
				
					} else if (r.key.equals("initialCropNitrogen")) {
					
						cropZone.setInitialCropNitrogen(Double.parseDouble(r.value));
					
					} else if (r.key.equals("initialCropReserveBiomass")) {
					
						cropZone.setInitialCropReserveBiomass(Double.parseDouble(r.value));
					
					} else if (r.key.equals("initialCropRootDensity")) {
	
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
						
							cropZone.setInitialCropRootsDensity(k,Double.parseDouble(st[k]));
					
						}
					}

				}
				else {
					System.out.println ("Unrecognized record : "+record);	// automatic toString () (or null)
					throw new Exception ("Unrecognized record");	// automatic toString () (or null)
				}
			}
			return cropSpeciesFileName;
		}
	}
