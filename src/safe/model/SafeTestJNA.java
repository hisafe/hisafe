package safe.model;

import safe.stics.*;

import java.io.Serializable;
import java.math.BigDecimal;
import com.sun.jna.Library;
import com.sun.jna.Native;

/**
 * SafeTestJNA is the class for calling native functions in STICS via JNA 
 * 
 * Available native functions are : 
 * =================================
 * verifParam : general parameters initialization verification
 * verifPlante : plants and itk parameters initialization verification
 * initClimat : climatic data initialization
 * initBoucleAnnuelle : annual loop initialization
 * apport : add carbon litters from tree
 * boucleJour1 : daily loop part 1
 * boucleJour2 : daily loop part 2
 * finBoucleAnnuelle : annual loop end process
 * 
 * @author Isabelle Lecomte - November 2016
 */


public class SafeTestJNA implements  Serializable {


	public interface TestJNA extends Library {

		// nb-26.01.2018
		// Loads the gfortran library.
		// Needed in order the functions of SticsV8 library that depend on gfortran library can be called.
		// Indeed, without this instruction, the gfortran library should have been placed in classical paths (/lib or 
		// /usr/lib for Linux) or the LD_LIBRARY_PATH environment variable (for Linux) or the PATH variable (for 
		// Windows) should have contained the path to the directory containing the gfortran library (capsis4/ext for example).
		// Windows) should have contained the path to the directory containing the gfortran library (capsis4/ext for example). 
//		TestJNA INSTANCE_GFORTRAN = (TestJNA) Native.loadLibrary("gfortran", TestJNA.class);

//		if ( System.getProperty("os.name").equals("Linux")  ) {
//			TestJNA INSTANCE_GFORTRAN = (TestJNA) Native.loadLibrary("gfortran", TestJNA.class);	 
//		}
		

		// Loads the SticsV8.so library (libSticsV8.so under Linux, SticsV8.dll under Windows, libSticsV8.dylib under MacOS).
		TestJNA INSTANCE_STICS = (TestJNA) Native.loadLibrary("SticsV8", TestJNA.class);

		 void verifParam (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsSoil soil, SafeSticsCommun sc, int lg, String outputDir);
		 void verifPlante (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsCommun sc,  SafeSticsItk itk, SafeSticsCrop p, boolean main, int lg, String outputDir);
		 void initClimat (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c);
		 void initBoucleAnnuelle (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int cellId, int lg, String outputDir);
		 void apport (SafeSticsParameters pg,  SafeSticsCommun sc, SafeSticsCrop p, SafeSticsItk itk, float profmax, float carbonLitter, float cnLitter, float cfeupc, float waterLitter, int typeLitter);
		 void boucleJour1 (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk);
		 void boucleJour2 (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int hisafeInfluence, float cellTrg, float cellVisibleSky);
		 void finBoucleAnnuelle (SafeSticsParameters pg, SafeSticsTransit t, SafeSticsStation sta, SafeSticsClimat c, SafeSticsCommun sc, SafeSticsSoil soil,  SafeSticsCrop p,SafeSticsItk itk, int cellId);
	}

	/**
	 * verifParam : general parameters initialization verification
	 */
	 
    public static void verifParam(SafeSticsParameters pg, SafeSticsTransit t, SafeSticsSoil soil, SafeSticsCommun sc, String exportDir ) { 
    	
    	TestJNA.INSTANCE_STICS.verifParam (pg, t, soil, sc, exportDir.length(), exportDir);

		return;
   } 
    
    /**
     * verifPlante : plants and itk parameters initialization verification
     */ 
    public static void verifPlante(SafeSticsParameters pg, SafeSticsTransit t, SafeSticsCommun sc, 
						    		SafeSticsItk itk, SafeSticsCrop p, 
						    		boolean main,
						    		String exportDir) { 
    	
   	 	TestJNA.INSTANCE_STICS.verifPlante (pg, t, sc, itk, p, main, exportDir.length(), exportDir);

		return; 
   } 
    
    /**
     * initClimat : climatic data initialization
     */
    public static void initClimat(SafeSticsParameters pg, SafeSticsTransit t,
					    		SafeSticsStation sta, SafeSticsClimat c)  { 
    	
    	
    	TestJNA.INSTANCE_STICS.initClimat (pg, t, sta, c); 

		return;
   }   
    
    /**
     * initBoucleAnnuelle : annual loop initialization
     */   
 
    public static void initBoucleAnnuelle(SafeSticsParameters pg, SafeSticsTransit t,
    								     SafeSticsStation sta, SafeSticsClimat c, 
    								     SafeSticsCommun sc, SafeSticsSoil soil,
    								     SafeSticsCrop p, SafeSticsItk itk,
    								     int dayStart, int dayEnd, 
    								     int cellId,
    								     String exportDir,
    								     boolean sticsReport
    								     ) { 
    	
    	

    	sc.P_iwater = dayStart;	
    	sc.P_ifwater = dayEnd;	
    	sc.ifwater_courant =  dayEnd; 
    	sc.P_culturean = 1;
    	//culture sur 2 ans
    	if ((dayStart>1) && (dayStart+dayEnd>366)) sc.P_culturean = 0;
    	
    	//pas de sorties BILAN STICS par defaut 
    	pg.P_flagEcriture = 0;
    	if (sticsReport) pg.P_flagEcriture = 31;

    	TestJNA.INSTANCE_STICS.initBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId, exportDir.length(), exportDir);

		return;     
   } 
    /**
     * apport : add carbon litters from tree
     */
    public static void apport(SafeSticsParameters pg, SafeSticsCommun sc, 
    						  SafeSticsCrop p, SafeSticsItk itk,
    						  float profMax,
							  float	carbonLitter, 							   
							  float	cnLitter, 
							  float cfeupc, 
							  float  waterLitter,
							  int    typeLitter) { 
    	
        TestJNA.INSTANCE_STICS.apport (pg, sc, p, itk, profMax, carbonLitter, cnLitter, cfeupc, waterLitter, typeLitter);
    
    }
    /**
     * boucleJour1 : daily loop part 1
     */
    public static void boucleJour1(SafeSticsParameters pg, SafeSticsTransit t,
							     SafeSticsStation sta, SafeSticsClimat c, 
							     SafeSticsCommun sc, SafeSticsSoil soil,
							     SafeSticsCrop p, SafeSticsItk itk,
							     int sticsDay, 
							     int julianDay,
							     double	cellRad, 							   
							     double	cellRain, 
							     double cellEtp) { 

		//n=1 to 365
    	//julianDay = 1 to 364 or 290 to 654
    	//day = 1 to 365 
		
       sc.n = sticsDay;
       sc.jjul = julianDay;
       sc.jul = julianDay;
       if (sc.jul > 365) sc.jul = sc.jul - 365;
       sc.numdate = sc.jul;
		
       	//Tree influence on cell data

   		c.trg[sticsDay-1]    = (float) cellRad; 				  /* RG re�u par la culture  */		 
		c.trr[sticsDay-1]    = (float) cellRain;                  /* pluie re�ue par la culture (mm) */
		c.tetp[sticsDay-1]   = (float) cellEtp;                  /* ETP re�ue par la culture (mm) */
		

		//RAZ Emulch (bug in STICS) 
		sc.Emulch=0;
		sc.drain=0;
		p.offrenod[1]=0;
		
       TestJNA.INSTANCE_STICS.boucleJour1 (pg, t, sta, c, sc, soil, p, itk);

		return;
    } 
    
    /**
     * boucleJour2 : daily loop part 2
     */ 
    public static void boucleJour2(SafeSticsParameters pg, SafeSticsTransit t,
		     SafeSticsStation sta, SafeSticsClimat c, 
		     SafeSticsCommun sc, SafeSticsSoil soil,
		     SafeSticsCrop p, SafeSticsItk itk,
		     int sticsDay, int julianDay,
		     int hisafeInfluence,
		     double cellTrg,
		     double cellVisibleSky
		    ) { 


	       	//Tree influence on cell data
			
			float trg = (float) cellTrg;					//% of GlobalRad (1=100%)
			float visibleSky = (float) cellVisibleSky;		//% of visible sky (1=100%)
			
	    	TestJNA.INSTANCE_STICS.boucleJour2 (pg, t, sta, c, sc, soil, p, itk, hisafeInfluence, trg, visibleSky);

    		return;
  		}		

		


    /**
     * finBoucleAnnuelle : annual loop end process
     */   
    
    public static void finBoucleAnnuelle(SafeSticsParameters pg, SafeSticsTransit t,
								     SafeSticsStation sta, SafeSticsClimat c, 
								     SafeSticsCommun sc, SafeSticsSoil soil,
								     SafeSticsCrop p, SafeSticsItk itk,
								     int cellId) { 

		TestJNA.INSTANCE_STICS.finBoucleAnnuelle (pg, t, sta, c, sc, soil, p, itk, cellId);

		return;
    }   
  
}
