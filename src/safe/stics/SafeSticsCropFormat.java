package safe.stics;

import java.util.Iterator;

import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.model.*;



public class SafeSticsCropFormat extends RecordSet {

	// Crop varieties record is described here
		@Import
		static public class CropVarieties extends Record {
			public int idvar;
			public String P_codevar;    // variety name 
			public int P_stlevamf;      // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
			public int P_stamflax;      // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
			public int P_stlevdrp;      // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
			public int P_stflodrp;      // phasic duration between FLO and DRP (only for indication) // degrés.jours // PARPLT // 1
			public int P_stdrpdes;      // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1
			public float P_pgrainmaxi;  // Maximum weight of one grain (at 0% water content) // g // PARPLT // 1							
			public float P_adens;       // Interplant competition parameter // SD // PARPLT // 1
			public float P_croirac;     // Growth rate of the root front  // cm degree.days-1 // PARPLT // 1
			public float P_durvieF;     // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
			public int P_jvc;           // Number of vernalizing days // day // PARPLT // 1
			public float P_sensiphot;     // photoperiod sensitivity (1=insensitive) // SD // PARPLT // 1
			public int P_stlaxsen;      // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
			public int P_stsenlan;      // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
			public float P_nbgrmax;       // Maximum number of grain // grains m-2 // PARPLT // 1
			public int P_stdrpmat;      // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
			public float P_afruitpot;   // maximal number of set fruits per degree.day (indeterminate growth) // nbfruits degree.day-1 // PARPLT // 1
			public float P_dureefruit;  // total growth period of a fruit at the setting stage to the physiological maturity // degree.days // PARPLT // 1
	
			public CropVarieties () {super ();}
			public CropVarieties (String line) throws Exception {super (line);}
			//public String getSeparator () {return ";";}	// to change default "\t" separator
		}
		// Soil management record is described here
		
		//CONSTRUCTEUR
		public SafeSticsCropFormat (String fileName) throws Exception {
			createRecordSet (fileName);
		}

		/**
		* Saving data in a recordSet
		*/
		public SafeSticsCropFormat (SafeSticsCrop sc)
							throws Exception {createRecordSet (sc);}

		public void createRecordSet (SafeSticsCrop sc) throws Exception {

				
		}

		/**
		* Read data from a recordSet
		*/
		public void load (SafeSticsCrop crop, SafeCropSpecies species) throws Exception {


			int nbVarieties = 0;
			
			for (Iterator i = this.iterator (); i.hasNext ();) {

				Record record = (Record) i.next ();

				if (record instanceof SafeSticsCropFormat.CropVarieties) {

					SafeSticsCropFormat.CropVarieties r =
							(SafeSticsCropFormat.CropVarieties) record;	// cast to precise type
				
					if(nbVarieties < 30){
						crop.P_codevar[nbVarieties] 	= r.idvar;
						crop.P_stamflax[nbVarieties]	= (float) r.P_stamflax;
						crop.P_stlevamf[nbVarieties]	= (float) r.P_stlevamf;
						crop.P_stlevdrp[nbVarieties]	= (float) r.P_stlevdrp;				
						crop.P_stflodrp[nbVarieties]	= (float) r.P_stflodrp;
						crop.P_stdrpdes[nbVarieties]	= (float) r.P_stdrpdes;
						crop.P_pgrainmaxi[nbVarieties]	= r.P_pgrainmaxi;
						crop.P_adens[nbVarieties] = r.P_adens;
						crop.P_croirac[nbVarieties] 	= r.P_croirac;
						crop.P_durvieF[nbVarieties] = r.P_durvieF;
						crop.P_jvc[nbVarieties]	= (float) r.P_jvc;						
						crop.P_nbgrmax[nbVarieties]	=  r.P_nbgrmax;
						crop.P_stlaxsen[nbVarieties]	= (float) r.P_stlaxsen;
						crop.P_stsenlan[nbVarieties]	= (float) r.P_stsenlan;
						crop.P_stdrpmat[nbVarieties]	= (float) r.P_stdrpmat;
						crop.P_sensiphot[nbVarieties]	= (float) r.P_sensiphot;	
						crop.P_afruitpot[nbVarieties]	= r.P_afruitpot;
						crop.P_dureefruit[nbVarieties] = r.P_dureefruit;
						nbVarieties++; 
						crop.nbVariete = nbVarieties;
					}

				} //KEY  Records
				else if (record instanceof SafeSticsCropFormat.KeyRecord) {
					SafeSticsCropFormat.KeyRecord r =
							(SafeSticsCropFormat.KeyRecord) record;	// cast to precise type

			
					if (r.key.equals ("codeplante")) {
						String cropName = r.value;
						if (cropName.equals("snu")) crop.P_codeplante 	= 1;
						else if (cropName.equals("fou")) crop.P_codeplante 	= 2;
						else if (cropName.equals("ban")) crop.P_codeplante 	= 3;
						else if (cropName.equals("esc")) crop.P_codeplante 	= 4;
						else if (cropName.equals("mai")) crop.P_codeplante 	= 5;
						else if (cropName.equals("ble")) crop.P_codeplante 	= 6;
						else if (cropName.equals("fet")) crop.P_codeplante 	= 7;
						else if (cropName.equals("lin")) crop.P_codeplante 	= 8;
						else if (cropName.equals("sal")) crop.P_codeplante 	= 9;
						else if (cropName.equals("mou")) crop.P_codeplante 	= 10;
						else if (cropName.equals("poi")) crop.P_codeplante 	= 11;
						else if (cropName.equals("pdt")) crop.P_codeplante 	= 12;
						else if (cropName.equals("col")) crop.P_codeplante 	= 13;
						else if (cropName.equals("rgi")) crop.P_codeplante 	= 14;
						else if (cropName.equals("sor")) crop.P_codeplante 	= 15;
						else if (cropName.equals("soj")) crop.P_codeplante 	= 16;
						else if (cropName.equals("fra")) crop.P_codeplante 	= 17;
						else if (cropName.equals("bet")) crop.P_codeplante 	= 18;
						else if (cropName.equals("can")) crop.P_codeplante 	= 19;
						else if (cropName.equals("tou")) crop.P_codeplante 	= 20;
						else if (cropName.equals("tom")) crop.P_codeplante 	= 21;
						else if (cropName.equals("vig")) crop.P_codeplante 	= 22;
						else if (cropName.equals("pom")) crop.P_codeplante 	= 23;
						else if (cropName.equals("men")) crop.P_codeplante 	= 24;
						else if (cropName.equals("qui")) crop.P_codeplante 	= 25;


					}
					
					if (r.key.equals ("codemonocot")) {
						crop.P_codemonocot= r.getIntValue ();
					}					
					else if (r.key.equals ("alphaco2")) {
						crop.P_alphaco2 = r.getFloatValue ();
					}
					else if (r.key.equals ("tdmin")) {
						crop.P_tdmin = r.getFloatValue();
					}
					else if (r.key.equals ("tdmax")) {
						crop.P_tdmax = r.getFloatValue ();
					}				
					else if (r.key.equals ("codetemp")) {
						crop.P_codetemp= r.getIntValue ();
					}
					else if (r.key.equals ("codegdh")) {
						crop.P_codegdh = r.getIntValue ();
					}
					else if (r.key.equals ("coeflevamf")) {
						crop.P_coeflevamf = r.getFloatValue ();
					}	
					else if (r.key.equals ("coefamflax")) {
						crop.P_coefamflax = r.getFloatValue ();
					}
					else if (r.key.equals ("coeflaxsen")) {
						crop.P_coeflaxsen = r.getFloatValue ();
					}
					else if (r.key.equals ("coefsenlan")) {
						crop.P_coefsenlan = r.getFloatValue ();
					}
					else if (r.key.equals ("coeflevdrp")) {
						crop.P_coeflevdrp = r.getFloatValue ();
					}
					else if (r.key.equals ("coefdrpmat")) {
						crop.P_coefdrpmat = r.getFloatValue ();
					}		
					else if (r.key.equals ("coefflodrp")) {
						crop.P_coefflodrp = r.getFloatValue ();
					}	
					else if (r.key.equals ("codephot")) {
						crop.P_codephot = r.getIntValue ();
					}						
					else if (r.key.equals ("phobase")) {
						crop.P_phobase = r.getFloatValue ();
					}
					else if (r.key.equals ("phosat")) {
						crop.P_phosat = r.getFloatValue ();
					}
					else if (r.key.equals ("coderetflo")) {
						crop.P_coderetflo = r.getIntValue ();
					}
					else if (r.key.equals ("stressdev")) {
						crop.P_stressdev = r.getFloatValue ();
					}
					else if (r.key.equals ("codebfroid")) {
						crop.P_codebfroid = r.getIntValue ();
					}
					else if (r.key.equals ("jvcmini")) {
						crop.P_jvcmini = r.getFloatValue ();
					}
					else if (r.key.equals ("julvernal")) {
						crop.P_julvernal = r.getIntValue ();
					}
					else if (r.key.equals ("tfroid")) {
						crop.P_tfroid = r.getFloatValue ();
					}
					else if (r.key.equals ("ampfroid")) {
						crop.P_ampfroid = r.getFloatValue ();
					}
					else if (r.key.equals ("stdordebour")) {
						crop.P_stdordebour = r.getFloatValue ();
					}
					else if (r.key.equals ("tdmindeb")) {
						crop.P_tdmindeb = r.getFloatValue ();
					}
					else if (r.key.equals ("tdmaxdeb")) {
						crop.P_tdmaxdeb = r.getFloatValue ();
					}
					else if (r.key.equals ("codedormance")) {
						crop.P_codedormance = r.getIntValue ();
					}
					else if (r.key.equals ("ifindorm")) {
						crop.P_ifindorm = r.getIntValue ();
					}
					else if (r.key.equals ("q10")) {
						crop.P_q10 = r.getFloatValue ();
					}					
					else if (r.key.equals ("idebdorm")) {
						crop.P_idebdorm= r.getIntValue ();
					}
					else if (r.key.equals ("codegdhdeb")) {
						crop.P_codegdhdeb = r.getIntValue ();
					}		
					else if (r.key.equals ("codeperenne")) {
						crop.P_codeperenne = r.getIntValue ();
					}						
					else if (r.key.equals ("codegermin")) {
						crop.P_codegermin = r.getIntValue ();
					}
					else if (r.key.equals ("tgmin")) {
						crop.P_tgmin = r.getFloatValue();
					}
					else if (r.key.equals ("stpltger")) {
						crop.P_stpltger = r.getFloatValue ();
					}	
					else if (r.key.equals ("potgermi")) {
						crop.P_potgermi = r.getFloatValue();
					}		
					else if (r.key.equals ("nbjgerlim")) {
						crop.P_nbjgerlim = r.getIntValue();
					}
					else if (r.key.equals ("propjgermin")) {
						crop.P_propjgermin = r.getFloatValue();
					}
					else if (r.key.equals ("codehypo")) {
						crop.P_codehypo = r.getIntValue();
					}
					else if (r.key.equals ("belong")) {
						crop.P_belong = r.getFloatValue();
					}					
					else if (r.key.equals ("celong")) {
						crop.P_celong = r.getFloatValue ();
					}
					else if (r.key.equals ("elmax")) {
						crop.P_elmax = r.getFloatValue ();
					}
					else if (r.key.equals ("nlevlim1")) {
						crop.P_nlevlim1= r.getIntValue ();
					}
					else if (r.key.equals ("nlevlim2")) {
						crop.P_nlevlim2 = r.getIntValue ();
					}		
					else if (r.key.equals ("vigueurbat")) {
						crop.P_vigueurbat= r.getFloatValue ();
					}
					else if (r.key.equals ("laiplantule")) {
						crop.P_laiplantule = r.getFloatValue ();
					}					
					else if (r.key.equals ("nbfeuilplant")) {
						crop.P_nbfeuilplant = r.getIntValue ();
					}
					else if (r.key.equals ("masecplantule")) {
						crop.P_masecplantule = r.getFloatValue ();
					}
					else if (r.key.equals ("zracplantule")) {
						crop.P_zracplantule = r.getFloatValue ();
					}
					else if (r.key.equals ("phyllotherme")) {
						crop.P_phyllotherme = r.getFloatValue ();
					}
					else if (r.key.equals ("bdens")) {
						crop.P_bdens = r.getFloatValue ();
					}
					else if (r.key.equals ("laicomp")) {
						crop.P_laicomp = r.getFloatValue ();
					}
					else if (r.key.equals ("hautbase")) {
						crop.P_hautbase = r.getFloatValue ();
					}
					else if (r.key.equals ("hautmax")) {
						crop.P_hautmax = r.getFloatValue ();
					}				
					else if (r.key.equals ("tcxstop")) {
						crop.P_tcxstop = r.getFloatValue ();
					}
					else if (r.key.equals ("codelaitr")) {
						crop.P_codelaitr = r.getIntValue ();
					}
					else if (r.key.equals ("vlaimax")) {
						crop.P_vlaimax = r.getFloatValue ();
					}
					else if (r.key.equals ("pentlaimax")) {
						crop.P_pentlaimax = r.getFloatValue ();
					}
					else if (r.key.equals ("udlaimax")) {
						crop.P_udlaimax = r.getFloatValue ();
					}
					else if (r.key.equals ("ratiodurvieI")) {
						crop.P_ratiodurvieI= r.getFloatValue ();
					}
					else if (r.key.equals ("tcmin")) {
						crop.P_tcmin = r.getFloatValue ();
					}
					else if (r.key.equals ("tcmax")) {
						crop.P_tcmax = r.getFloatValue ();
					}					
					else if (r.key.equals ("ratiosen")) {
						crop.P_ratiosen = r.getFloatValue ();
					}					
					else if (r.key.equals ("abscission")) {
						crop.P_abscission = r.getFloatValue ();
					}					
					else if (r.key.equals ("parazofmorte")) {
						crop.P_parazofmorte = r.getFloatValue ();
					}					
					else if (r.key.equals ("innturgmin")) {
						crop.P_innturgmin = r.getFloatValue ();
					}					
					else if (r.key.equals ("dlaimin")) {
						crop.P_dlaimin = r.getFloatValue ();
					}																				
					else if (r.key.equals ("codlainet")) {
						crop.P_codlainet = r.getIntValue ();
					}					
					else if (r.key.equals ("dlaimax")) {
						crop.P_dlaimax = r.getFloatValue ();
					}	
					else if (r.key.equals ("tustressmin")) {
						crop.P_tustressmin = r.getFloatValue ();
					}		
					else if (r.key.equals ("dlaimaxbrut")) {
						crop.P_dlaimaxbrut = r.getFloatValue ();
					}
					else if (r.key.equals ("durviesupmax")) {
						crop.P_durviesupmax = r.getFloatValue ();
					}
					else if (r.key.equals ("innsen")) {
						crop.P_innsen = r.getFloatValue ();
					}
					else if (r.key.equals ("rapsenturg")) {
						crop.P_rapsenturg = r.getFloatValue ();
					}
					else if (r.key.equals ("codestrphot")) {
						crop.P_codestrphot = r.getIntValue ();
					}
					else if (r.key.equals ("phobasesen")) {
						crop.P_phobasesen = r.getFloatValue ();
					}
					else if (r.key.equals ("dltamsmaxsen")) {
						crop.P_dltamsmaxsen = r.getFloatValue ();
					}
					else if (r.key.equals ("dltamsminsen")) {
						crop.P_dltamsminsen = r.getFloatValue ();
					}
					else if (r.key.equals ("alphaphot")) {
						crop.P_alphaphot = r.getFloatValue ();
					}
					else if (r.key.equals ("tauxrecouvmax")) {
						crop.P_tauxrecouvmax = r.getFloatValue ();
					}
					else if (r.key.equals ("tauxrecouvkmax")) {
						crop.P_tauxrecouvkmax = r.getFloatValue ();
					}
					else if (r.key.equals ("pentrecouv")) {
						crop.P_pentrecouv = r.getFloatValue ();
					}
					else if (r.key.equals ("infrecouv")) {
						crop.P_infrecouv = r.getFloatValue ();
					}	
					else if (r.key.equals ("codetransrad")) {
						crop.P_codetransrad = r.getIntValue ();
					}
					else if (r.key.equals ("extin")) {
						crop.P_extin = r.getFloatValue ();
					}
					else if (r.key.equals ("ktrou")) {
						crop.P_ktrou = r.getFloatValue ();
					}
					else if (r.key.equals ("forme")) {
						crop.P_forme = r.getIntValue ();
					}
					else if (r.key.equals ("rapforme")) {
						crop.P_rapforme = r.getFloatValue ();
					}
					else if (r.key.equals ("adfol")) {
						crop.P_adfol= r.getFloatValue ();
					}
					else if (r.key.equals ("dfolbas")) {
						crop.P_dfolbas = r.getFloatValue ();
					}					
					else if (r.key.equals ("dfolhaut")) {
						crop.P_dfolhaut = r.getFloatValue ();
					}	
					else if (r.key.equals ("temin")) {
						crop.P_temin = r.getFloatValue ();
					}	
					else if (r.key.equals ("temax")) {
						crop.P_temax = r.getFloatValue ();
					}	
					else if (r.key.equals ("teopt")) {
						crop.P_teopt = r.getFloatValue ();
					}	
					else if (r.key.equals ("teoptbis")) {
						crop.P_teoptbis = r.getFloatValue ();
					}	
					else if (r.key.equals ("efcroijuv")) {
						crop.P_efcroijuv = r.getFloatValue ();
					}	
					else if (r.key.equals ("efcroiveg")) {
						crop.P_efcroiveg = r.getFloatValue ();
					}	
					else if (r.key.equals ("efcroirepro")) {
						crop.P_efcroirepro = r.getFloatValue ();
					}	
					else if (r.key.equals ("remobres")) {
						crop.P_remobres = r.getFloatValue ();
					}	
					else if (r.key.equals ("coefmshaut")) {
						crop.P_coefmshaut = r.getFloatValue ();
					}		
					else if (r.key.equals ("slamax")) {
						crop.P_slamax = r.getFloatValue ();
					}	
					else if (r.key.equals ("slamin")) {
						crop.P_slamin = r.getFloatValue ();
					}	
					else if (r.key.equals ("tigefeuil")) {
						crop.P_tigefeuil = r.getFloatValue ();
					}	
					else if (r.key.equals ("envfruit")) {
						crop.P_envfruit = r.getFloatValue ();
					}	
					else if (r.key.equals ("sea")) {
						crop.P_sea = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeindetermin")) {
						crop.P_codeindetermin = r.getIntValue ();
					}	
					else if (r.key.equals ("nbjgrain")) {
						crop.P_nbjgrain = r.getIntValue ();
					}	
					else if (r.key.equals ("cgrain")) {
						crop.P_cgrain = r.getFloatValue ();
					}	
					else if (r.key.equals ("cgrainv0")) {
						crop.P_cgrainv0 = r.getFloatValue ();
					}	
					else if (r.key.equals ("nbgrmin")) {
						crop.P_nbgrmin = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeir")) {
						crop.P_codeir = r.getIntValue ();
					}	
					else if (r.key.equals ("vitircarb")) {
						crop.P_vitircarb = r.getFloatValue ();
					}	
					else if (r.key.equals ("irmax")) {
						crop.P_irmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("vitircarbT")) {
						crop.P_vitircarbT = r.getFloatValue ();
					}	
					else if (r.key.equals ("nboite")) {
						crop.P_nboite = r.getIntValue ();
					}	
					else if (r.key.equals ("allocfrmax")) {
						crop.P_allocfrmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("afpf")) {
						crop.P_afpf = r.getFloatValue ();
					}	
					else if (r.key.equals ("bfpf")) {
						crop.P_bfpf = r.getFloatValue ();
					}	
					else if (r.key.equals ("cfpf")) {
						crop.P_cfpf = r.getFloatValue ();
					}	
					else if (r.key.equals ("dfpf")) {
						crop.P_dfpf = r.getFloatValue ();
					}
					else if (r.key.equals ("stdrpnou")) {
						crop.P_stdrpnou = r.getFloatValue ();
					}	
					else if (r.key.equals ("spfrmin")) {
						crop.P_spfrmin = r.getFloatValue ();
					}	
					else if (r.key.equals ("spfrmax")) {
						crop.P_spfrmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("splaimin")) {
						crop.P_splaimin = r.getFloatValue ();
					}
					else if (r.key.equals ("splaimax")) {
						crop.P_splaimax = r.getFloatValue ();
					}	
					else if (r.key.equals ("codcalinflo")) {
						crop.P_codcalinflo = r.getIntValue ();
					}	
					else if (r.key.equals ("nbinflo")) {
						crop.P_nbinflo = r.getFloatValue ();
					}	
					else if (r.key.equals ("inflomax")) {
						crop.P_inflomax = r.getFloatValue ();
					}	
					else if (r.key.equals ("pentinflores")) {
						crop.P_pentinflores = r.getFloatValue ();
					}	
					else if (r.key.equals ("codetremp")) {
						crop.P_codetremp = r.getIntValue ();
					}	
					else if (r.key.equals ("tminremp")) {
						crop.P_tminremp = r.getFloatValue ();
					}
					else if (r.key.equals ("tmaxremp")) {
						crop.P_tmaxremp = r.getFloatValue ();
					}	
					else if (r.key.equals ("vitpropsucre")) {
						crop.P_vitpropsucre = r.getFloatValue ();
					}	
					else if (r.key.equals ("vitprophuile")) {
						crop.P_vitprophuile = r.getFloatValue ();
					}	
					else if (r.key.equals ("vitirazo")) {
						crop.P_vitirazo = r.getFloatValue ();
					}	
					else if (r.key.equals ("sensanox")) {
						crop.P_sensanox = r.getFloatValue ();
					}	

					else if (r.key.equals ("stoprac")) {
						String stoprac = r.value;
						if (stoprac.equals("lax"))
							crop.P_stoprac= 6;
						else if (stoprac.equals("flo"))
							crop.P_stoprac= 7;
						else if (stoprac.equals("mat"))
							crop.P_stoprac= 10;
						else if (stoprac.equals("rec"))
							crop.P_stoprac= 13;
						else if (stoprac.equals("sen"))
							crop.P_stoprac= 11;			
					}	
					else if (r.key.equals ("sensrsec")) {
						crop.P_sensrsec = r.getFloatValue ();
					}
					else if (r.key.equals ("contrdamax")) {
						crop.P_contrdamax = r.getFloatValue ();
					}	
					else if (r.key.equals ("codetemprac")) {
						crop.P_codetemprac = r.getIntValue ();
					}	
					else if (r.key.equals ("coderacine")) {
						crop.P_coderacine = r.getIntValue ();
					}	
					else if (r.key.equals ("zlabour")) {
						crop.P_zlabour = r.getFloatValue ();
					}	
					else if (r.key.equals ("zpente")) {
						crop.P_zpente = r.getFloatValue ();
					}	
					else if (r.key.equals ("zprlim")) {
						crop.P_zprlim = r.getFloatValue ();
					}	
					else if (r.key.equals ("draclong")) {
						crop.P_draclong = r.getFloatValue ();
					}
					else if (r.key.equals ("debsenrac")) {
						crop.P_debsenrac = r.getFloatValue ();
					}	
					else if (r.key.equals ("lvfront")) {
						crop.P_lvfront= r.getFloatValue ();
					}	
					else if (r.key.equals ("longsperac")) {
						crop.P_longsperac = r.getFloatValue ();
					}	
					else if (r.key.equals ("codazorac")) {
						crop.P_codazorac= r.getIntValue ();
					}		
					else if (r.key.equals ("minefnra")) {
						crop.P_minefnra = r.getFloatValue ();
					}	
					else if (r.key.equals ("minazorac")) {
						crop.P_minazorac = r.getFloatValue ();
					}
					else if (r.key.equals ("maxazorac")) {
						crop.P_maxazorac = r.getFloatValue ();
					}	
					else if (r.key.equals ("codtrophrac")) {
						crop.P_codtrophrac = r.getIntValue ();
					}	
					else if (r.key.equals ("repracpermax")) {
						crop.P_repracpermax = r.getFloatValue ();
					}	
					else if (r.key.equals ("repracpermin")) {
						crop.P_repracpermin = r.getFloatValue ();
					}	
					else if (r.key.equals ("krepracperm")) {
						crop.P_krepracperm = r.getFloatValue ();
					}	
					else if (r.key.equals ("repracseumax")) {
						crop.P_repracseumax = r.getFloatValue ();
					}
					else if (r.key.equals ("repracseumin")) {
						crop.P_repracseumin = r.getFloatValue ();
					}	
					else if (r.key.equals ("krepracseu")) {
						crop.P_krepracseu= r.getFloatValue ();
					}	
					else if (r.key.equals ("tletale")) {
						crop.P_tletale = r.getFloatValue ();
					}	
					else if (r.key.equals ("tdebgel")) {
						crop.P_tdebgel = r.getFloatValue ();
					}	
					else if (r.key.equals ("codgellev")) {
						crop.P_codgellev = r.getIntValue ();
					}	
					else if (r.key.equals ("nbfgellev")) {
						crop.P_nbfgellev = r.getIntValue ();
					}
					else if (r.key.equals ("tgellev10")) {
						crop.P_tgellev10 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tgellev90")) {
						crop.P_tgellev90 = r.getFloatValue ();
					}	
					else if (r.key.equals ("codgeljuv")) {
						crop.P_codgeljuv = r.getIntValue ();
					}	
					else if (r.key.equals ("tgeljuv10")) {
						crop.P_tgeljuv10 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tgeljuv90")) {
						crop.P_tgeljuv90 = r.getFloatValue ();
					}	
					else if (r.key.equals ("codgelveg")) {
						crop.P_codgelveg = r.getIntValue ();
					}
					else if (r.key.equals ("tgelveg10")) {
						crop.P_tgelveg10 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tgelveg90")) {
						crop.P_tgelveg90 = r.getFloatValue ();
					}	
					else if (r.key.equals ("codgelflo")) {
						crop.P_codgelflo = r.getIntValue ();
					}	
					else if (r.key.equals ("tgelflo10")) {
						crop.P_tgelflo10 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tgelflo90")) {
						crop.P_tgelflo90 = r.getFloatValue ();
					}	
					else if (r.key.equals ("psisto")) {
						crop.P_psisto= r.getFloatValue ();
					}
					else if (r.key.equals ("psiturg")) {
						crop.P_psiturg = r.getFloatValue ();
					}	
					else if (r.key.equals ("h2ofeuilverte")) {
						crop.P_h2ofeuilverte = r.getFloatValue ();
					}	
					else if (r.key.equals ("h2ofeuiljaune")) {
						crop.P_h2ofeuiljaune = r.getFloatValue ();
					}	
					else if (r.key.equals ("h2otigestruc")) {
						crop.P_h2otigestruc = r.getFloatValue ();
					}	
					else if (r.key.equals ("h2oreserve")) {
						crop.P_h2oreserve = r.getFloatValue ();
					}	
					else if (r.key.equals ("h2ofrvert")) {
						crop.P_h2ofrvert= r.getFloatValue ();
					}	
					else if (r.key.equals ("deshydbase")) {
						crop.P_deshydbase = r.getFloatValue ();
					}
					else if (r.key.equals ("tempdeshyd")) {
						crop.P_tempdeshyd = r.getFloatValue ();
					}	
					else if (r.key.equals ("codebeso")) {
						crop.P_codebeso= r.getIntValue ();
					}	
					else if (r.key.equals ("kmax")) {
						crop.P_kmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("rsmin")) {
						crop.P_rsmin = r.getFloatValue ();
					}	
					else if (r.key.equals ("codeintercept")) {
						crop.P_codeintercept = r.getIntValue ();
					}	
					else if (r.key.equals ("mouillabil")) {
						crop.P_mouillabil = r.getFloatValue ();
					}
					else if (r.key.equals ("stemflowmax")) {
						crop.P_stemflowmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("kstemflow")) {
						crop.P_kstemflow = r.getFloatValue ();
					}	
					else if (r.key.equals ("Vmax1")) {
						crop.P_Vmax1= r.getFloatValue ();
					}	
					else if (r.key.equals ("Kmabs1")) {
						crop.P_Kmabs1 = r.getFloatValue ();
					}					
					else if (r.key.equals ("Vmax2")) {
						crop.P_Vmax2= r.getFloatValue ();
					}	
					else if (r.key.equals ("Kmabs2")) {
						crop.P_Kmabs2 = r.getFloatValue ();
					}	
					else if (r.key.equals ("adil")) {
						crop.P_adil = r.getFloatValue ();
					}
					else if (r.key.equals ("bdil")) {
						crop.P_bdil = r.getFloatValue ();
					}	
					else if (r.key.equals ("masecNmax")) {
						crop.P_masecNmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("INNmin")) {
						crop.P_INNmin = r.getFloatValue ();
					}	
					else if (r.key.equals ("INNimin")) {
						crop.P_INNimin = r.getFloatValue ();
					}	
					else if (r.key.equals ("inngrain1")) {
						crop.P_inngrain1 = r.getFloatValue ();
					}	
					else if (r.key.equals ("inngrain2")) {
						crop.P_inngrain2 = r.getFloatValue ();
					}
					else if (r.key.equals ("codeplisoleN")) {
						crop.P_codeplisoleN = r.getIntValue ();
					}	
					else if (r.key.equals ("adilmax")) {
						crop.P_adilmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("bdilmax")) {
						crop.P_bdilmax = r.getFloatValue ();
					}	
					else if (r.key.equals ("Nmeta")) {
						crop.P_Nmeta= r.getFloatValue ();
					}	
					else if (r.key.equals ("masecmeta")) {
						crop.P_masecmeta = r.getFloatValue ();
					}	
					else if (r.key.equals ("Nreserve")) {
						crop.P_Nreserve = r.getFloatValue ();
					}
					else if (r.key.equals ("codeINN")) {
						crop.P_codeINN = r.getIntValue ();
					}	
					else if (r.key.equals ("codelegume")) {
						crop.P_codelegume = r.getIntValue ();
					}	
					else if (r.key.equals ("stlevdno")) {
						crop.P_stlevdno = r.getFloatValue ();
					}	
					else if (r.key.equals ("stdnofno")) {
						crop.P_stdnofno = r.getFloatValue ();
					}
					else if (r.key.equals ("stfnofvino")) {
						crop.P_stfnofvino = r.getFloatValue ();
					}	
					else if (r.key.equals ("vitno")) {
						crop.P_vitno= r.getFloatValue ();
					}
					else if (r.key.equals ("profnod")) {
						crop.P_profnod = r.getFloatValue ();
					}	
					else if (r.key.equals ("concNnodseuil")) {
						crop.P_concNnodseuil = r.getFloatValue ();
					}	
					else if (r.key.equals ("concNrac0")) {
						crop.P_concNrac0 = r.getFloatValue ();
					}	
					else if (r.key.equals ("concNrac100")) {
						crop.P_concNrac100 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tempnod1")) {
						crop.P_tempnod1 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tempnod2")) {
						crop.P_tempnod2 = r.getFloatValue ();
					}	
					else if (r.key.equals ("tempnod3")) {
						crop.P_tempnod3 = r.getFloatValue ();
					}
					else if (r.key.equals ("tempnod4")) {
						crop.P_tempnod4 = r.getFloatValue ();
					}	
					else if (r.key.equals ("codefixpot")) {
						crop.P_codefixpot = r.getIntValue ();
					}	
					else if (r.key.equals ("fixmax")) {
						crop.P_fixmax = r.getFloatValue ();
					}		
					else if (r.key.equals ("fixmaxveg")) {
						crop.P_fixmaxveg= r.getFloatValue ();
					}	
					else if (r.key.equals ("fixmaxgr")) {
						crop.P_fixmaxgr = r.getFloatValue ();
					}	
					else if (r.key.equals ("codazofruit")) {
						crop.P_codazofruit = r.getIntValue ();
					}
					//Ne sert plus 
					else if (r.key.equals ("stadebbchplt")) {
					}	
					else if (r.key.equals ("stadebbchger")) {
					}	
					else if (r.key.equals ("stadebbchlev")) {
					}				
					else if (r.key.equals ("stadebbchamf")) {
					}
					else if (r.key.equals ("stadebbchlax")) {
					}
					else if (r.key.equals ("stadebbchsen")) {
					}
					else if (r.key.equals ("stadebbchflo")) {
					}
					else if (r.key.equals ("stadebbchdrp")) {
					}
					else if (r.key.equals ("stadebbchnou")) {
					}
					else if (r.key.equals ("stadebbchdebdes")) {
					}
					else if (r.key.equals ("stadebbchmat")) {
					}
					else if (r.key.equals ("stadebbchrec")) {
					}
					else if (r.key.equals ("stadebbchfindorm")) {
					}		
					
					//HISAFE Specific parameters
					else if (r.key.equals ("speciesName")) {
						String v = r.value;
						species.setName(v);
					}					
					else if (r.key.equals ("cropRootDiameter")) {
						species.setCropRootDiameter(r.getDoubleValue());
					}		
					else if (r.key.equals ("cropRootConductivity")) {
						species.setCropRootConductivity(r.getDoubleValue());
					}	
					else if (r.key.equals ("cropAlpha")) {
						species.setCropAlpha(r.getDoubleValue());
					}	
					else if (r.key.equals ("cropMinTranspirationPotential")) {
						species.setCropMinTranspirationPotential(r.getDoubleValue());
					}	
					else if (r.key.equals ("cropMaxTranspirationPotential")) {
						species.setCropMaxTranspirationPotential(r.getDoubleValue());
					}	
					else if (r.key.equals ("cropBufferPotential")) {
						species.setCropBufferPotential(r.getDoubleValue());
					}
					else if (r.key.equals ("cropLongitudinalResistantFactor")) {
						species.setCropLongitudinalResistantFactor(r.getDoubleValue());
					}					
				}
				else {
					throw new Exception ("Unrecognized record : "+record);	// automatic toString () (or null)
				}
			}
		}
	}
