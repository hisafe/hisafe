package safe.model;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.stics.*;

/**
 * Records description for  GENERAL parameters
 *
 * @author : Isabelle LECOMTE  - INRA SYSTEM Montpellier (March 2003)
 */
public class SafeGeneralParameterFormat extends RecordSet {

	public SafeGeneralParameterFormat (String fileName) throws Exception {
		createRecordSet (fileName);}

	/**
	 * Load general parameters
	 */
	public  void load (SafeInitialParameters settings, SafeSticsStation sticsStation) throws Exception {

		Set<String> requiredParameters = new HashSet<>();

		requiredParameters.add("diffuseCoeffA");
		requiredParameters.add("diffuseCoeffB");
		requiredParameters.add("SOC");
		requiredParameters.add("UOC");
		requiredParameters.add("turtleOption");
		requiredParameters.add("timeStep");
		requiredParameters.add("nbTimeStepMax");
		requiredParameters.add("diffuseAngleStep");
		requiredParameters.add("declinationThreshold");
		requiredParameters.add("leafAreaThreshold");
		requiredParameters.add("nbImpactMultiplication");
		requiredParameters.add("parGlobalCoefficient");
		requiredParameters.add("molesParCoefficient");
		requiredParameters.add("aangst");
		requiredParameters.add("bangst");
		requiredParameters.add("priestleyTaylorCoeff");
		requiredParameters.add("sigma");
		requiredParameters.add("gamma");
		requiredParameters.add("harmonicWeightedMean");
		requiredParameters.add("integrationStep");
		requiredParameters.add("maxPhiPF");
		requiredParameters.add("maxTempSnow");
		requiredParameters.add("minTempSnow");
		requiredParameters.add("maxDailySnowMelt");
		requiredParameters.add("maxTempSnowMelt");	
		requiredParameters.add("minTempSnowMelt");							
		requiredParameters.add("nitrogenDiffusionConstant");													
		requiredParameters.add("nitrogenEffectiveDiffusionA0");												
		requiredParameters.add("nitrogenEffectiveDiffusionA1");										
		requiredParameters.add("no3AbsorptionConstant");
		requiredParameters.add("nh4AbsorptionConstant");													
		requiredParameters.add("no3Fraction");
		requiredParameters.add("fmin1");
		requiredParameters.add("fmin2");
		requiredParameters.add("fmin3");
		requiredParameters.add("zr");														
		requiredParameters.add("NH3ref");												
		requiredParameters.add("patm");												
		requiredParameters.add("aclim");												
		requiredParameters.add("codeetp");												
		requiredParameters.add("codeclichange");												
		requiredParameters.add("codaltitude");											
		requiredParameters.add("altistation");												
		requiredParameters.add("gradtn");												
		requiredParameters.add("gradtx");												
		requiredParameters.add("altinversion");												
		requiredParameters.add("gradtninv");												
		requiredParameters.add("cielclair");												
		requiredParameters.add("codadret");												
		requiredParameters.add("ombragetx");												
		requiredParameters.add("ra");												
		requiredParameters.add("albveg");												
		requiredParameters.add("corecTrosee");												
		requiredParameters.add("codecaltemp");												
		requiredParameters.add("codernet");												
		requiredParameters.add("coefdevil");												
		requiredParameters.add("aks");												
		requiredParameters.add("bks");												
		requiredParameters.add("cvent");												
		requiredParameters.add("phiv0");												
		requiredParameters.add("coefrnet");				
				
		
		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

		 	if (record instanceof SafeGeneralParameterFormat.KeyRecord) {

		 		SafeGeneralParameterFormat.KeyRecord r = (SafeGeneralParameterFormat.KeyRecord) record;

				if (r.key.equals ("diffuseCoeffA")) {
					settings.diffuseCoeffA = r.getDoubleValue ();
					requiredParameters.remove("diffuseCoeffA");
					
				} else if  (r.key.equals ("diffuseCoeffB")) {
					settings.diffuseCoeffB  = r.getDoubleValue ();
					requiredParameters.remove("diffuseCoeffB");
					
				} else if  (r.key.equals ("parGlobalCoefficient")) {
					settings.parGlobalCoefficient  = r.getDoubleValue ();
					requiredParameters.remove("parGlobalCoefficient");
					
				} else if  (r.key.equals ("molesParCoefficient")) {
					settings.molesParCoefficient  = r.getDoubleValue ();
					requiredParameters.remove("molesParCoefficient");
					
				} else if  (r.key.equals ("aangst")) {
					settings.aangst  = r.getDoubleValue ();
					requiredParameters.remove("aangst");
					
				} else if  (r.key.equals ("bangst")) {
					settings.bangst  = r.getDoubleValue ();
					requiredParameters.remove("bangst");
					
				} else if  (r.key.equals ("priestleyTaylorCoeff")) {
					settings.priestleyTaylorCoeff  = r.getDoubleValue ();
					requiredParameters.remove("priestleyTaylorCoeff");
					
				} else if  (r.key.equals ("sigma")) {
					settings.sigma  = r.getDoubleValue ();
					requiredParameters.remove("sigma");
					
				} else if (r.key.equals ("gamma")) {
					settings.gamma  = r.getDoubleValue ();	
					requiredParameters.remove("gamma");
								
				} else if (r.key.equals ("timeStep")) {
					settings.timeStep  = r.getDoubleValue ();
					requiredParameters.remove("timeStep");
					
				} else if (r.key.equals ("nbTimeStepMax")) {
					settings.nbTimeStepMax  = r.getIntValue ();	
					requiredParameters.remove("nbTimeStepMax");
					
				} else if (r.key.equals ("diffuseAngleStep")) {
					settings.diffuseAngleStep  = r.getDoubleValue ();
					requiredParameters.remove("diffuseAngleStep");
					
				} else if (r.key.equals ("declinationThreshold")) {
					settings.declinationThreshold  = r.getDoubleValue ();
					requiredParameters.remove("declinationThreshold");
					
				} else if (r.key.equals ("leafAreaThreshold")) {
					settings.leafAreaThreshold  = r.getDoubleValue ();	
					requiredParameters.remove("leafAreaThreshold");
					
				} else if (r.key.equals ("nbImpactMultiplication")) {
					settings.nbImpactMultiplication  = r.getIntValue ();
					requiredParameters.remove("nbImpactMultiplication");
						
				} else if (r.key.equals("SOC")) {
					int ri = r.getIntValue();
					settings.SOC = true;
					if (ri == 0) settings.SOC = false;
					if (ri == 1) settings.SOC = true;
					requiredParameters.remove("SOC");
					
				} else if (r.key.equals("UOC")) {
					int ri = r.getIntValue();
					settings.UOC = false;
					if (ri == 0) settings.UOC = false;
					if (ri == 1) settings.UOC = true;	
					requiredParameters.remove("UOC");
					
				} else if (r.key.equals("turtleOption")) {
					int ri = r.getIntValue();
					settings.turtleOption = false;
					if (ri == 0) settings.turtleOption = false;	
					if (ri == 1) settings.turtleOption = true;	
					requiredParameters.remove("turtleOption");
					
				} else if (r.key.equals("cropLightMethod")) {
					int ri = r.getIntValue();
					settings.cropLightMethod = false;
					if (ri == 0) settings.cropLightMethod = false;
					if (ri == 1) settings.cropLightMethod = true;
					
				} else if  (r.key.equals ("harmonicWeightedMean")) {
					settings.harmonicWeightedMean  = r.getDoubleValue ();
					requiredParameters.remove("harmonicWeightedMean");
					
				} else if  (r.key.equals ("integrationStep")) {
					settings.integrationStep  = r.getDoubleValue ();
					requiredParameters.remove("integrationStep");
					
				} else if  (r.key.equals ("maxPhiPF")) {
					settings.maxPhiPF  = r.getDoubleValue ();
					requiredParameters.remove("maxPhiPF");
				
				} else if  (r.key.equals ("maxTempSnow")) {
					settings.maxTempSnow  = r.getDoubleValue ();
					requiredParameters.remove("maxTempSnow");
					
				} else if  (r.key.equals ("minTempSnow")) {
					settings.minTempSnow  = r.getDoubleValue ();
					requiredParameters.remove("minTempSnow");
					
				} else if  (r.key.equals ("maxDailySnowMelt")) {
					settings.maxDailySnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("maxDailySnowMelt");
					
				} else if  (r.key.equals ("maxTempSnowMelt")) {
					settings.maxTempSnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("maxTempSnowMelt");
					
				} else if  (r.key.equals ("minTempSnowMelt")) {
					settings.minTempSnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("minTempSnowMelt");
					
				} else if (r.key.equals("sticsWaterExtraction")) {
					int ri = r.getIntValue();
					settings.sticsWaterExtraction = false;
					if (ri == 1) settings.sticsWaterExtraction = true;	

				} else if (r.key.equals("laiFileName")) {
					settings.laiFileName  = settings.dataPath+"/"+r.value;
				}

				else if (r.key.equals ("codecaltemp")) {
					sticsStation.P_codecaltemp = r.getIntValue ();
					requiredParameters.remove("codecaltemp");
					
				} else if (r.key.equals ("codernet")) {
					sticsStation.P_codernet = r.getIntValue ();	
					requiredParameters.remove("codernet");
					
				} else if (r.key.equals ("codeclichange")) {
					sticsStation.P_codeclichange = r.getIntValue ();	
					requiredParameters.remove("codeclichange");
					
				} else if (r.key.equals ("zr")) {
					sticsStation.P_zr = r.getFloatValue ();
					requiredParameters.remove("zr");
					
				} else if (r.key.equals ("ra")) {
					sticsStation.P_ra = r.getFloatValue ();
					requiredParameters.remove("ra");
					
				} else if (r.key.equals ("NH3ref")) {
					sticsStation.P_NH3ref = r.getFloatValue ();
					requiredParameters.remove("NH3ref");

				} else if (r.key.equals ("coefdevil")) {
					sticsStation.P_coefdevil = r.getFloatValue ();
					requiredParameters.remove("coefdevil");
					
				} else if (r.key.equals ("albveg")) {
					sticsStation.P_albveg = r.getFloatValue ();
					requiredParameters.remove("albveg");
					
				} else if (r.key.equals ("altistation")) {
					sticsStation.P_altistation = r.getFloatValue ();
					requiredParameters.remove("altistation");
					
				} else if (r.key.equals ("gradtn")) {
					sticsStation.P_gradtn = r.getFloatValue ();
					requiredParameters.remove("gradtn");
					
				} else if (r.key.equals ("gradtx")) {
					sticsStation.P_gradtx = r.getFloatValue ();
					requiredParameters.remove("gradtx");
					
				} else if (r.key.equals ("altinversion")) {
					sticsStation.P_altinversion = r.getFloatValue ();
					requiredParameters.remove("altinversion");
					
				} else if (r.key.equals ("gradtninv")) {
					sticsStation.P_gradtninv = r.getFloatValue ();
					requiredParameters.remove("gradtninv");
					
				} else if (r.key.equals ("cielclair")) {
					sticsStation.P_cielclair = r.getFloatValue ();
					requiredParameters.remove("cielclair");
					
				} else if (r.key.equals ("ombragetx")) {
					sticsStation.P_ombragetx = r.getFloatValue ();
					requiredParameters.remove("ombragetx");
					
				} else if (r.key.equals ("aks")) {
					sticsStation.P_aks = r.getFloatValue ();
					requiredParameters.remove("aks");
					
				} else if (r.key.equals ("bks")) {
					sticsStation.P_bks = r.getFloatValue ();
					requiredParameters.remove("bks");
					
				} else if (r.key.equals ("cvent")) {
					sticsStation.P_cvent = r.getFloatValue ();
					requiredParameters.remove("cvent");
					
				} else if (r.key.equals ("phiv0")) {
					sticsStation.P_phiv0 = r.getFloatValue ();
					requiredParameters.remove("phiv0");
					
				} else if (r.key.equals ("coefrnet")) {
					sticsStation.P_coefrnet = r.getFloatValue ();	
					requiredParameters.remove("coefrnet");
					
				} else if (r.key.equals ("patm")) {
					sticsStation.P_patm = r.getFloatValue ();
					requiredParameters.remove("patm");
					
				} else if (r.key.equals ("corecTrosee")) {
					sticsStation.P_corecTrosee = r.getFloatValue ();
					requiredParameters.remove("corecTrosee");
					
				} else if (r.key.equals ("codeetp")) {
					sticsStation.P_codeetp = r.getIntValue ();
					requiredParameters.remove("codeetp");
			
				} else if (r.key.equals ("codaltitude")) {
					sticsStation.P_codaltitude = r.getIntValue ();
					requiredParameters.remove("codaltitude");
					
				} else if (r.key.equals ("codadret")) {
					sticsStation.P_codadret = r.getIntValue ();
					requiredParameters.remove("codadret");
					
				} else if (r.key.equals ("aclim")) {
					sticsStation.P_aclim = r.getFloatValue ();					
					requiredParameters.remove("aclim");	
				
				} else if (r.key.equals ("nitrogenDiffusionConstant")) {
					settings.nitrogenDiffusionConstant = r.getDoubleValue ();
					requiredParameters.remove("nitrogenDiffusionConstant");
					
				} else if (r.key.equals ("nitrogenEffectiveDiffusionA0")) {
					settings.nitrogenEffectiveDiffusionA0 = r.getDoubleValue ();
					requiredParameters.remove("nitrogenEffectiveDiffusionA0");
					
				} else if (r.key.equals ("nitrogenEffectiveDiffusionA1")) {
					settings.nitrogenEffectiveDiffusionA1 = r.getDoubleValue ();
					requiredParameters.remove("nitrogenEffectiveDiffusionA1");
					
				} else if (r.key.equals ("no3AbsorptionConstant")) {
					settings.no3AbsorptionConstant = r.getDoubleValue ();
					requiredParameters.remove("no3AbsorptionConstant");
					
				} else if (r.key.equals ("nh4AbsorptionConstant")) {
					settings.nh4AbsorptionConstant = r.getDoubleValue ();
					requiredParameters.remove("nh4AbsorptionConstant");
					
				} else if (r.key.equals ("no3Fraction")) {
					settings.no3Fraction = r.getDoubleValue ();	
					requiredParameters.remove("no3Fraction");
					
				} else if (r.key.equals ("fmin1")) {
					settings.fmin1 = r.getDoubleValue ();
					requiredParameters.remove("fmin1");
					
				} else if (r.key.equals ("fmin2")) {
					settings.fmin2 = r.getDoubleValue ();
					requiredParameters.remove("fmin2");
					
				} else if (r.key.equals ("fmin3")) {
					settings.fmin3 = r.getDoubleValue ();
					requiredParameters.remove("fmin3");
				}
		 	}
		}
		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing general parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}
	}
}
