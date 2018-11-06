package safe.stics;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.model.*;




public class SafeSticsItkFormat extends RecordSet {

	// Soil residus incorporation record is described here
		@Import
		static public class SoilResidusRecord extends Record {
			public int 		julres;
			public int 		coderes;
			public float 	qres;
			public float 	Crespc;
			public float 	CsurNres;
			public float 	Nminres;
			public float 	eaures;

			public SoilResidusRecord () {super ();}
			public SoilResidusRecord (String line) throws Exception {super (line);}
			//public String getSeparator () {return ";";}	// to change default "\t" separator
		}
		// Soil management record is described here
		@Import
		static public class SoilManagementRecord extends Record {
			public int 		jultrav;
			public float 	profres;
			public float 	proftrav;


			public SoilManagementRecord () {super ();}
			public SoilManagementRecord (String line) throws Exception {super (line);}
			//public String getSeparator () {return ";";}	// to change default "\t" separator
		}
		// Irrigation and Fertilisation record is described here
		@Import
		static public class IrrigationAndFertilisationRecord extends Record {
			public int 		jul;
			public float 	dose;

			public IrrigationAndFertilisationRecord () {super ();}
			public IrrigationAndFertilisationRecord (String line) throws Exception {super (line);}
			//public String getSeparator () {return ";";}	// to change default "\t" separator
		}

		//Safe crop irrigation record is described here
		@Import
		 static public class CuttingRecord extends Record {
			public int 		julfauche;
			public float 	hautcoupe;	//m
			public float 	lairesiduel;	//m2 leaf m-2 soil
			public float 	msresiduel;	//t ha-1
			public float 	anitcoupe;	//kg N ha-1 

			public CuttingRecord () { super (); }
			public CuttingRecord (String line) throws Exception { super (line); }
			 //public String getSeparator () {return ";";}	// to change default "\t" separator
		 }

		//CONSTRUCTEUR
		public SafeSticsItkFormat (String fileName) throws Exception {
			createRecordSet (fileName);
		}

		/**
		* Saving data in a recordSet
		*/
		public SafeSticsItkFormat (SafeSticsItk sci)
							throws Exception {createRecordSet (sci);}

		public void createRecordSet (SafeSticsItk sci) throws Exception {

				
		}

		/**
		* Read data from a recordSet
		*/
		public void load (SafeSticsItk cropItk, SafeInitialValues initialValues, boolean isMainCropSpecies) throws Exception {

			String typeIntervention="";
			int indexSoilResidus = 0;
			int indexSoilManagement = 0;
			int indexIrrigation = 0;
			int indexFertilisation = 0;
			int indexCutting= 0;   
			
			
			initialValues.mainCropStage = initialValues.MAIN_CROP_STAGE; 
			initialValues.interCropStage = initialValues.MAIN_CROP_STAGE; 
			
			
			for (Iterator i = this.iterator (); i.hasNext ();) {

				Record record = (Record) i.next ();

				if (record instanceof SafeSticsItkFormat.SoilResidusRecord) {

					SafeSticsItkFormat.SoilResidusRecord r =
							(SafeSticsItkFormat.SoilResidusRecord) record;	// cast to precise type
				
					if(indexSoilResidus < cropItk.P_nbjres){
						cropItk.P_julres[indexSoilResidus] 	= r.julres;
						cropItk.P_coderes[indexSoilResidus] = r.coderes;
						cropItk.P_qres[indexSoilResidus]	= r.qres;
						cropItk.P_Crespc[indexSoilResidus] 	= r.Crespc;
						cropItk.P_CsurNres[indexSoilResidus] = r.CsurNres;
						cropItk.P_Nminres[indexSoilResidus] = r.Nminres;
						cropItk.P_eaures[indexSoilResidus]	= r.eaures;
						indexSoilResidus++; 
					}
				}
				else if (record instanceof SafeSticsItkFormat.SoilManagementRecord) {

					SafeSticsItkFormat.SoilManagementRecord r =
							(SafeSticsItkFormat.SoilManagementRecord) record;	// cast to precise type
					if(indexSoilManagement < cropItk.P_nbjtrav){
						cropItk.P_jultrav[indexSoilManagement] = r.jultrav;
						cropItk.P_profres[indexSoilManagement] 	= r.profres;
						cropItk.P_proftrav[indexSoilManagement] = r.proftrav;
						indexSoilManagement++; 
					}
				}				
				else if (record instanceof SafeSticsItkFormat.IrrigationAndFertilisationRecord){
					SafeSticsItkFormat.IrrigationAndFertilisationRecord r =
							(SafeSticsItkFormat.IrrigationAndFertilisationRecord) record;	// cast to precise type

					if (typeIntervention.equals("nbIrrigation")){
						if(indexIrrigation < cropItk.nap){
							if (cropItk.P_codedateappH2O == 2) //jour julian
								cropItk.P_julapI[indexIrrigation] = r.jul;
							else	//degres jour
								cropItk.P_upvttapI[indexFertilisation] 	= r.jul;
							
							cropItk.P_doseI[indexIrrigation] = r.dose;
							indexIrrigation++; 
						}
					}
					else if (typeIntervention.equals("nbFertilisation")){
						if(indexFertilisation < cropItk.napN) {
							
							if (cropItk.P_codedateappN == 2) //jour julian
								cropItk.P_julapN[indexFertilisation] 	= r.jul;
							else  //degres jour
								cropItk.P_upvttapN[indexFertilisation] 	= r.jul;	
							
						
							if (cropItk.P_codefracappN == 1)  //dose entière							
								cropItk.P_doseN[indexFertilisation] 	= r.dose;
							else //fraction
								cropItk.P_fracN[indexFertilisation] 	= r.dose;
								
							indexFertilisation++; 
						}
					}
				}
				else if (record instanceof SafeSticsItkFormat.CuttingRecord){
					SafeSticsItkFormat.CuttingRecord r =
							(SafeSticsItkFormat.CuttingRecord) record;	// cast to precise type

					cropItk.P_julfauche[indexCutting] = r.julfauche;
					//là il faut decaller sinon ça deconne 
					cropItk.P_hautcoupe [indexCutting+1] 	= r.hautcoupe;
					cropItk.P_lairesiduel [indexCutting+1] = r.lairesiduel;
					cropItk.P_msresiduel [indexCutting+2] = r.msresiduel;
					cropItk.P_anitcoupe [indexCutting+1] 	= r.anitcoupe;
					indexCutting++; 

				} //KEY  Records
				else if (record instanceof SafeSticsItkFormat.KeyRecord) {
					SafeSticsItkFormat.KeyRecord r =
							(SafeSticsItkFormat.KeyRecord) record;	// cast to precise type
					
					if (r.key.equals ("nbjres")) {
						cropItk.P_nbjres 	= r.getIntValue ();
					}
					if (r.key.equals ("nbjtrav")) {
						cropItk.P_nbjtrav 	= r.getIntValue ();
					}					
					else if (r.key.equals ("nap")) {
						typeIntervention = "nbIrrigation";
						cropItk.nap 		= r.getIntValue ();
					}
					else if (r.key.equals ("napN")) {
						typeIntervention = "nbFertilisation";
						cropItk.napN		= r.getIntValue ();
					}
					else if (r.key.equals ("nbcoupe")) {
						cropItk.nbcoupe = r.getIntValue();
					}
					else if (r.key.equals ("iplt0")) {
						cropItk.P_iplt0 = r.getIntValue ();
					}				
					else if (r.key.equals ("profsem")) {
						cropItk.P_profsem = r.getFloatValue ();
					}
					else if (r.key.equals ("densitesem")) {
						cropItk.P_densitesem = r.getFloatValue ();
					}
					else if (r.key.equals ("variete")) {
						cropItk.P_variete = r.getIntValue ();
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
					else if (r.key.equals ("ilev")) {
						cropItk.P_ilev = r.getIntValue ();
					}
					else if (r.key.equals ("iamf")) {
						cropItk.P_iamf = r.getIntValue ();
					}
					else if (r.key.equals ("ilax")) {
						cropItk.P_ilax = r.getIntValue ();
					}
					else if (r.key.equals ("isen")) {
						cropItk.P_isen = r.getIntValue ();
					}
					else if (r.key.equals ("ilan")) {
						cropItk.P_ilan = r.getIntValue ();
					}
					else if (r.key.equals ("iflo")) {
						cropItk.P_iflo = r.getIntValue ();
					}
					else if (r.key.equals ("idrp")) {
						cropItk.P_idrp = r.getIntValue ();
					}
					else if (r.key.equals ("imat")) {
						cropItk.P_imat = r.getIntValue ();
					}
					else if (r.key.equals ("irec")) {
						cropItk.P_irec = r.getIntValue ();
					}
					else if (r.key.equals ("irecbutoir")) {
						cropItk.P_irecbutoir = r.getIntValue ();
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
							cropItk.P_stadecoupedf= 5;
						else if (stadecoupedf.equals("lax"))
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
						else if (stadecoupedf.equals("sen"))
							cropItk.P_stadecoupedf= 12;	
						else if (stadecoupedf.equals("lan"))
							cropItk.P_stadecoupedf= 13;	
	
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
					else if (r.key.equals ("codcalrogne")) {
						cropItk.P_codcalrogne = r.getIntValue ();
					}																				
					else if (r.key.equals ("julrogne")) {
						cropItk.P_julrogne = r.getIntValue ();
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
					else if (r.key.equals ("juleffeuil")) {
						cropItk.P_juleffeuil = r.getIntValue ();
					}
					else if (r.key.equals ("laieffeuil")) {
						cropItk.P_laieffeuil = r.getFloatValue ();
					}
					else if (r.key.equals ("codetaille")) {
						cropItk.P_codetaille = r.getIntValue ();
					}
					else if (r.key.equals ("jultaille")) {
						cropItk.P_jultaille = r.getIntValue ();
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
					else if (r.key.equals ("julouvre2")) {
						cropItk.P_julouvre2 = r.getIntValue ();
					}
					else if (r.key.equals ("julouvre3")) {
						cropItk.P_julouvre3 = r.getIntValue ();
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
					
					} else if (r.key.equals("initialCropStage")) {
						if (isMainCropSpecies)
							initialValues.mainCropStage = r.getIntValue();
						else
							initialValues.interCropStage = r.getIntValue();
					} else if (r.key.equals("initialCropLai")) {
						if (isMainCropSpecies)
							initialValues.mainCropLai = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropLai = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropBiomass")) {
						if (isMainCropSpecies)
							initialValues.mainCropBiomass = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropBiomass = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropRootDepth")) {
						if (isMainCropSpecies)
							initialValues.mainCropRootDepth = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropRootDepth = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropGrainBiomass")) {
						if (isMainCropSpecies)
							initialValues.mainCropGrainBiomass = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropGrainBiomass = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropNitrogen")) {
						if (isMainCropSpecies)
							initialValues.mainCropNitrogen = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropNitrogen = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropReserveBiomass")) {
						if (isMainCropSpecies)
							initialValues.mainCropReserveBiomass = new Double(Double.parseDouble(r.value));
						else
							initialValues.interCropReserveBiomass = new Double(Double.parseDouble(r.value));
					} else if (r.key.equals("initialCropRootDensity")) {
	
						String[] st = (r.value).split(",");
						for (int k = 0; k < st.length; k++) {
							if (isMainCropSpecies)
								initialValues.mainCropRootDensity[k] = Double.parseDouble(st[k]);
							else
								initialValues.interCropRootDensity[k] = Double.parseDouble(st[k]);
						}
					}

				}
				else {
					throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
				}
			}
		}
	}
