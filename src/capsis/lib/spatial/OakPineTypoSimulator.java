/*
 * Spatial library for Capsis4.
 *
 * Copyright (C) 2001-2006 Francois Goreaud & Ngo Bieng Marie ANge.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.lib.spatial;

//~ import capsis.gui.*;
//~ import  capsis.util.*;
import java.io.FileReader;
import java.io.IOException;
import java.io.StreamTokenizer;
import java.util.Random;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * OakPineTypoSimulator : utilities to simulate virtual stand.
 *
 * @author Francois Goreaud & Ngo Bieng Marie Ange 10/7/2002 - 1/3/2007
 */
    public class OakPineTypoSimulator {
   //checked for c4.1.1_09 - fc - 5.2.2003
      static {
      //~ System.out.println ("OakPineTypoSimulator *** loaded");
      }
      static Random R = new Random();

   /**
   * One strata virtual stand, only D & H.
   * Uses a VirtualParameters set of parameters
   * and returns a VirtualStand + an error code. x,y random
   *
   * @author Francois Goreaud 10/7/2002 - 30/3/2007
   */
   // fc - 5.2.2003 - replaced dialog messaging by Log messaging to make it usable in script mode.
       static public int simulateOneStrataDH (AmapDialog dial, VirtualParameters vParam, VirtualStand vStand) {

      // Return code 0 means no error,
      // other int correspond to specific errors.

      // 1 : D & H.
      //---------------------------------------------

         if (vParam.virtualStandD==5) {	// common file for n,x,y,D,H
         //int error = LoadDHFile(vParam.virtualStandnDHFile, vStand.treeNumber, vStand.h, vStand.d);

         // Checks...
            if (!Check.isFile (vParam.virtualStandnxyDHFile)) {
            //~ JOptionPane.showMessageDialog (dial,
            		//~ Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile"),
            		//~ Translator.swap ("Shared.warning"),
            		//~ JOptionPane.WARNING_MESSAGE );
               Log.println (Log.ERROR, "OakPineTypoSimulator.simulateOneStrata ()",
                  Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile")+" (nxyDH)");
               return 112;
            }

         // Loads n, x,y, D & H.
            try {
               FileReader file=new FileReader(vParam.virtualStandnxyDHFile);
               StreamTokenizer st = new StreamTokenizer(file);

            // Compute treeNumber.
               vStand.treeNumber=0;
               double bof;
               st.nextToken();
               while(st.ttype!=st.TT_EOF) {
                  bof = st.nval;
                  st.nextToken();
                  bof = st.nval;
                  st.nextToken();
                  bof = st.nval;
                  st.nextToken();
                  bof = st.nval;
                  st.nextToken();
                  bof = st.nval;
                  st.nextToken();
                  vStand.treeNumber+=1;
               }
               file.close();

            // Read data.
               file=new FileReader(vParam.virtualStandnxyDHFile);
               st = new StreamTokenizer(file);
               vStand.x = new double[vStand.treeNumber+1];
               vStand.y = new double[vStand.treeNumber+1];
               vStand.h = new double[vStand.treeNumber+1];
               vStand.d = new double[vStand.treeNumber+1];

               for(int i=1; i<=vStand.treeNumber; i++) {
                  st.nextToken();
                  bof = st.nval;
                  st.nextToken();
                  vStand.x[i] = st.nval;
                  st.nextToken();
                  vStand.y[i] = st.nval;
                  st.nextToken();
                  vStand.d[i] = st.nval;
                  st.nextToken();
                  vStand.h[i] = st.nval;
               }
               file.close();

            }
                catch (Exception exc) {
                  vStand.treeNumber=0;
               //~ JOptionPane.showMessageDialog (dial,
               	//~ Translator.swap ("OakPineTypoSimulator.exceptionDuringStandLoad")
               	//~ +"\n"+exc.getMessage (),
               	//~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                  Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                     "Exception caught (nxyDH file): ",exc);	// fc 4.0
                  return 113;
               }
         //~ Log.println("nDH loaded");
            return 0;
         // 1.1. common file for n,D,H.
         }
         else
         {
            if (vParam.virtualStandD==1) {	// common file for n,D,H
            //int error = LoadDHFile(vParam.virtualStandnDHFile, vStand.treeNumber, vStand.h, vStand.d);

            // Checks...
               if (!Check.isFile (vParam.virtualStandnDHFile)) {
               //~ JOptionPane.showMessageDialog (dial,
               	//~ Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile"),
               	//~ Translator.swap ("Shared.warning"),
               	//~ JOptionPane.WARNING_MESSAGE );
                  Log.println (Log.ERROR, "OakPineTypoSimulator.simulateOneStrata ()",
                     Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile")+" (nDH)");
                  return 112;
               }

            // Loads n, D & H.
               try {
                  FileReader file=new FileReader(vParam.virtualStandnDHFile);
                  StreamTokenizer st = new StreamTokenizer(file);

               // Compute treeNumber.
                  vStand.treeNumber=0;
                  double bof;
                  st.nextToken();
                  while(st.ttype!=st.TT_EOF) {
                     bof = st.nval;
                     st.nextToken();
                     bof = st.nval;
                     st.nextToken();
                     bof = st.nval;
                     st.nextToken();
                     vStand.treeNumber+=1;
                  }
                  file.close();

               // Read data.
                  file=new FileReader(vParam.virtualStandnDHFile);
                  st = new StreamTokenizer(file);
                  vStand.h = new double[vStand.treeNumber+1];
                  vStand.d = new double[vStand.treeNumber+1];

                  for(int i=1; i<=vStand.treeNumber; i++) {
                     st.nextToken();
                     bof = st.nval;
                     st.nextToken();
                     vStand.d[i] = st.nval;
                     st.nextToken();
                     vStand.h[i] = st.nval;
                  }
                  file.close();

               }
                   catch (Exception exc) {
                     vStand.treeNumber=0;
                  //~ JOptionPane.showMessageDialog (dial,
                  //~ Translator.swap ("OakPineTypoSimulator.exceptionDuringStandLoad")
                  //~ +"\n"+exc.getMessage (),
                  //~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                     Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                        "Exception caught (nDH file): ",exc);	// fc 4.0
                     return 113;
                  }
            //~ Log.println("nDH loaded");

            // 1.2. Separate simulation of D and then H.
            }
            else if ((vParam.virtualStandD>=2)&(vParam.virtualStandD<=4)) {	// separate simulation of D and then H

            // 1.2.1. D first.
               if (vParam.virtualStandD==2) {	// file for n and D

               // Checks...
                  if (!Check.isFile (vParam.virtualStandnDFile)) {
                  //~ JOptionPane.showMessageDialog (dial,
                  	//~ Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile"),
                  	//~ Translator.swap ("Shared.warning"),
                  	//~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "OakPineTypoSimulator.simulateOneStrata ()",
                        Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile")+" (nD)");
                     return 1212;
                  }

               // Loads n, D.
                  try {
                     FileReader file=new FileReader(vParam.virtualStandnDFile);
                     StreamTokenizer st = new StreamTokenizer(file);

                  // Compute treeNumber.
                     vStand.treeNumber=0;
                     double bof;
                     st.nextToken();
                     while(st.ttype!=st.TT_EOF) 	{
                        bof = st.nval;
                        st.nextToken();
                        bof = st.nval;
                        st.nextToken();
                        vStand.treeNumber+=1;
                     }
                     file.close();

                  // Read data.
                     file=new FileReader(vParam.virtualStandnDFile);
                     st = new StreamTokenizer(file);
                     vStand.d = new double[vStand.treeNumber+1];

                     for(int i=1; i<=vStand.treeNumber; i++) {
                        st.nextToken();
                        bof = st.nval;
                        st.nextToken();
                        vStand.d[i] = st.nval;
                     }
                     file.close();

                  }
                      catch (Exception exc) {
                        vStand.treeNumber=0;
                     //~ JOptionPane.showMessageDialog (dial,
                     //~ Translator.swap ("OakPineTypoSimulator.exceptionDuringStandLoad")
                     //~ +"\n"+exc.getMessage (),
                     //~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.simulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.exceptionDuringStandLoad")+" (nD)", exc);
                        return 1213;
                     }
               //~ Log.println("nD loaded");

               }
               else if (vParam.virtualStandD==3) {	// histogram file for D

               // Checks...
                  if (!Check.isFile (vParam.virtualStandDHistFile)) {
                  //~ JOptionPane.showMessageDialog (dial,
                  	//~ Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile"),
                  	//~ Translator.swap ("Shared.warning"),
                  	//~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "OakPineTypoSimulator.simulateOneStrata ()",
                        Translator.swap ("OakPineTypoSimulator.fileNameIsNotFile")+" (DHist)");
                     return 1215;
                  }

               // Loads DHist and computes D.
                  try {
                     FileReader file=new FileReader(vParam.virtualStandDHistFile);
                     StreamTokenizer st = new StreamTokenizer(file);

                  // Compute treeNumber.
                     vStand.treeNumber=0;
                     int classNumber=0;
                     double bof;
                     int bof2;
                     st.nextToken();
                     while(st.ttype!=st.TT_EOF) {
                        bof = st.nval;
                        st.nextToken();
                        bof2 = (int) st.nval;
                        st.nextToken();
                        vStand.treeNumber+=bof2;
                        classNumber+=1;
                     }
                     file.close();

                  // Read data.
                     file=new FileReader(vParam.virtualStandDHistFile);
                     st = new StreamTokenizer(file);
                     vStand.d = new double[vStand.treeNumber+1];
                     int k=0;

                     for(int i=1; i<=classNumber; i++) {
                        st.nextToken();
                        bof = st.nval;
                        st.nextToken();
                        bof2 = (int) st.nval;
                        if (bof2>0) {
                           for (int j=1; j<=bof2; j=j+1) {
                              k=k+1;
                              vStand.d[k]=bof;
                           }
                        }
                     }
                     file.close();

                  }
                      catch (Exception exc) {
                        vStand.treeNumber=0;
                     //~ JOptionPane.showMessageDialog (dial,
                     //~ Translator.swap ("OakPineTypoSimulator.exceptionDuringStandLoad")
                     //~ +"\n"+exc.getMessage (),
                     //~ Translator.swap ("Shared.error"),
                     //~ JOptionPane.ERROR_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           "Exception caught (DHist file): ",exc);	// fc 4.0
                        return 1216;
                     }
               //~ Log.println("DHist loaded");

               }
               else {	// gaussian distribution for D

                  try {
                     vStand.treeNumber = vParam.virtualStandTreeNumber;
                     if (vStand.treeNumber<1) {
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap ("OakPineTypoSimulator.treeNumber"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.treeNumber"));
                        return 1217;
                     }
                     if (vParam.virtualStandDMean<0) {
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap ("OakPineTypoSimulator.DMean"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.DMean"));
                        return 1218;
                     }
                     if (vParam.virtualStandDDeviation<0) {
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap ("OakPineTypoSimulator.DDeviation"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.DDeviation"));
                        return 1219;
                     }

                     vStand.d = new double[vStand.treeNumber+1];
                     RandomPattern.simulateGaussianM(vStand.treeNumber, vStand.d, vParam.virtualStandDMean, vParam.virtualStandDDeviation);

                  	// here we add a test to verify that d> Dmin	FG 1/3/2007
                     for (int i=1;i<=vStand.treeNumber;i++)
                     {
                        if (vStand.d[i]<vParam.virtualStandDMin)
                        {	vStand.d[i]=vParam.virtualStandDMin;
                        }
                     }
                  }
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial,
                     //~ Translator.swap("OakPineTypoSimulator.simulationProblemD"),
                     //~ Translator.swap ("Shared.warning"),
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.simulationProblemD"), exc);
                        return 999;
                     }
               }

            // 1.2.2. Then H.

               long seed1=System.currentTimeMillis();
               while (System.currentTimeMillis()<seed1+vStand.treeNumber) {
               // we wait until we have a different random serie
               }

               if (vParam.virtualStandH==1) {	// H=f(D) curve
                  try {
                     vStand.h = new double[vStand.treeNumber+1];

                     if (vParam.virtualStandHCurve==1) {	// linear model
                        HCurve.hLinearModel(vStand.treeNumber, vStand.h, vStand.d, vParam.virtualStandH1a,
                           vParam.virtualStandH1b, vParam.virtualStandH1c,
                           vParam.virtualStandH1d  );
                     }
                     else if (vParam.virtualStandHCurve==2) {	// logistic model
                        HCurve.hLogisticModel(vStand.treeNumber, vStand.h, vStand.d, vParam.virtualStandH2a,
                           vParam.virtualStandH2K, vParam.virtualStandH2b,
                           vParam.virtualStandH2d  );
                     }
                     else if (vParam.virtualStandHCurve==3) {	// hyperbolic model
                        HCurve.hHyperbolicModel(vStand.treeNumber, vStand.h, vStand.d, vParam.virtualStandH3a,
                           vParam.virtualStandH3b, vParam.virtualStandH3c,
                           vParam.virtualStandH3d  );
                     }
                     else {	// error
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap("OakPineTypoSimulator.simulationProblemHCurve"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.simulationProblemHCurve"));
                        return 999;
                     }
                  // No return because simulation will be done anyway.
                  }
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial,
                     //~ Translator.swap("OakPineTypoSimulator.simulationProblemHCurve"),
                     //~ Translator.swap ("Shared.warning"),
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.simulationProblemHCurve"), exc);
                        return 999;
                     }
               }
               else {	// gaussian distribution
                  try {
                     vStand.h = new double[vStand.treeNumber+1];
                     if (vParam.virtualStandHMean<0) {
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap ("OakPineTypoSimulator.HMean"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.HMean"));
                        return 1218;
                     }
                     if (vParam.virtualStandHDeviation<0) {
                     //~ JOptionPane.showMessageDialog (dial,
                     	//~ Translator.swap ("OakPineTypoSimulator.HDeviation"),
                     	//~ Translator.swap ("Shared.warning"),
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.HDeviation"));
                        return 1219;
                     }
                     RandomPattern.simulateGaussianM(vStand.treeNumber, vStand.h, vParam.virtualStandHMean, vParam.virtualStandHDeviation);

                  	// here we add a test to verify that h> Hmin	FG 1/3/2007
                     for (int i=1;i<=vStand.treeNumber;i++)
                     {
                        if (vStand.h[i]<vParam.virtualStandHMin)
                        {	vStand.h[i]=vParam.virtualStandHMin;
                        }
                     }

                  }
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial,
                     //~ Translator.swap("OakPineTypoSimulator.simulationProblemH"),
                     //~ Translator.swap ("Shared.warning"),
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                           Translator.swap ("OakPineTypoSimulator.simulationProblemH"));
                        return 999;
                     }
               }

            }
            else {	// error for D&H
            // "à traiter mieux que ca !" (i.e. "to do better than this" ;-)
            //~ JOptionPane.showMessageDialog (dial,
            	//~ Translator.swap ("OakPineTypoSimulator.simulationProblemD"),
            	//~ Translator.swap ("Shared.warning"),
            	//~ JOptionPane.WARNING_MESSAGE );
               Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                  Translator.swap ("OakPineTypoSimulator.simulationProblemD"));
               return 1;
            }


         // 2 : X & Y. : here only random,
         //---------------------------------------------

            try {
               vStand.x = new double[vStand.treeNumber+1];
               vStand.y = new double[vStand.treeNumber+1];
               RandomPattern.simulateXY(vStand.treeNumber, vStand.x, vStand.y,
                     vParam.virtualStandXmin, vParam.virtualStandXmax,
                     vParam.virtualStandYmin, vParam.virtualStandYmax,
                     vParam.virtualStandPrecision);
            }
                catch (Exception exc) {
                  //~ JOptionPane.showMessageDialog (dial,
                  //~ Translator.swap ("OakPineTypoSimulator.XYRandom"),
                  //~ Translator.swap ("Shared.warning"),
                  //~ JOptionPane.WARNING_MESSAGE );
                  Log.println (Log.ERROR, "OakPineTypoSimulator.SimulateOneStrata ()",
                        Translator.swap ("OakPineTypoSimulator.XYRandom"), exc);
                  return 999;
               }
         }
         return 0;
      } // end simulateOneStrataDH

       static public int simulateStand (AmapDialog dial, VirtualParametersOakPineTypo vParamOPT, VirtualStand vStand)throws IOException {

      // Return code 0 means no error,
      // other int correspond to specific errors.

      // we first simulate oak and pine in the canopy

         int TotalNbTree =0;
      	// ensure coherence
         vParamOPT.OakParam.virtualStandXmin=vParamOPT.virtualStandXmin;
         vParamOPT.OakParam.virtualStandXmax=vParamOPT.virtualStandXmax;
         vParamOPT.OakParam.virtualStandYmin=vParamOPT.virtualStandYmin;
         vParamOPT.OakParam.virtualStandYmax=vParamOPT.virtualStandYmax;
         vParamOPT.OakParam.virtualStandTreeNumber = vParamOPT.numberOak;

         vParamOPT.PineParam.virtualStandXmin=vParamOPT.virtualStandXmin;
         vParamOPT.PineParam.virtualStandXmax=vParamOPT.virtualStandXmax;
         vParamOPT.PineParam.virtualStandYmin=vParamOPT.virtualStandYmin;
         vParamOPT.PineParam.virtualStandYmax=vParamOPT.virtualStandYmax;
         vParamOPT.PineParam.virtualStandTreeNumber = vParamOPT.numberPine;


      // D&H for Oaks
         VirtualStand vsOak=new VirtualStand ();
         System.out.print("- pop : Oaks canopy");
         int test=simulateOneStrataDH(null, vParamOPT.OakParam, vsOak);
            // we must add a test here
         System.out.println(" -> "+test+"("+vsOak.treeNumber+")");
         TotalNbTree = TotalNbTree+vsOak.treeNumber;

      // D&H for Pines
         VirtualStand vsPine=new VirtualStand ();
         System.out.print("- pop : Pines canopy");
         test=simulateOneStrataDH(null, vParamOPT.PineParam, vsPine);
            // we must add a test here
         System.out.println(" -> "+test+"("+vsPine.treeNumber+")");
         TotalNbTree = TotalNbTree+vsPine.treeNumber;

		double surf=(vParamOPT.virtualStandXmax-vParamOPT.virtualStandXmin)*(vParamOPT.virtualStandYmax-vParamOPT.virtualStandYmin)/10000;


      // spatial structure...
         if ( vParamOPT.type==1)
         {	test=simulateXYNS3(vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.Type1NbAgOak),(int)Math.round(surf*vParamOPT.Type1NbAgPine),
               			vParamOPT.Type1ROak,vParamOPT.Type1RPine,
               			vParamOPT.Type1DistIntertype,vParamOPT.Type1DistPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
         else if ( vParamOPT.type==2)
         {	test=simulateXYNS3(vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.Type2NbAgOak),(int)Math.round(surf*vParamOPT.Type2NbAgPine),
               			vParamOPT.Type2ROak,vParamOPT.Type2RPine,
               			vParamOPT.Type2DistIntertype,vParamOPT.Type2DistPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
         else if ( vParamOPT.type==3)
         {	test=simulateXYHC2d(vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.Type3NbAgPine),vParamOPT.Type3RPine,
               			vParamOPT.Type3DistIntertype,vParamOPT.Type3Proba,
               			vParamOPT.Type3DistPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
         else if ( vParamOPT.type==4)
         {	test=simulateXYHC2b(vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.Type4NbAgPine),vParamOPT.Type4RPine,
               			vParamOPT.Type4DistIntertype,vParamOPT.Type4DistPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }

      // we then simulate oak and pine in the understorey

      	// ensure coherence
         vParamOPT.OakParamUnder.virtualStandXmin=vParamOPT.virtualStandXmin;
         vParamOPT.OakParamUnder.virtualStandXmax=vParamOPT.virtualStandXmax;
         vParamOPT.OakParamUnder.virtualStandYmin=vParamOPT.virtualStandYmin;
         vParamOPT.OakParamUnder.virtualStandYmax=vParamOPT.virtualStandYmax;
         vParamOPT.OakParamUnder.virtualStandTreeNumber = vParamOPT.numberOakUnder;

         vParamOPT.PineParamUnder.virtualStandXmin=vParamOPT.virtualStandXmin;
         vParamOPT.PineParamUnder.virtualStandXmax=vParamOPT.virtualStandXmax;
         vParamOPT.PineParamUnder.virtualStandYmin=vParamOPT.virtualStandYmin;
         vParamOPT.PineParamUnder.virtualStandYmax=vParamOPT.virtualStandYmax;
         vParamOPT.PineParamUnder.virtualStandTreeNumber = vParamOPT.numberPineUnder;


      // D&H for Oaks
         VirtualStand vsOakUnder=new VirtualStand ();
         System.out.print("- pop : Oaks understorey");
         test=simulateOneStrataDH(null, vParamOPT.OakParamUnder, vsOakUnder);
            // we must add a test here
         System.out.println(" -> "+test+"("+vsOakUnder.treeNumber+")");
         TotalNbTree = TotalNbTree+vsOakUnder.treeNumber;

      // D&H for Pines
         VirtualStand vsPineUnder=new VirtualStand ();
         //if (vParamOPT.numberPineUnder>0)
         //{
            System.out.print("- pop : Pines understorey");
            test=simulateOneStrataDH(null, vParamOPT.PineParamUnder, vsPineUnder);
            // we must add a test here
            System.out.println(" -> "+test+"("+vsPineUnder.treeNumber+")");
            TotalNbTree = TotalNbTree+vsPineUnder.treeNumber;
         //}


      // spatial structure...
      	// for Oak understorey
         if ( vParamOPT.typeUnder==1)
         {	test=simulateXYSE7(vsOakUnder.treeNumber,vsOakUnder.x, vsOakUnder.y,
               			vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int) Math.round(surf*vParamOPT.UnderType1NbAgOak),vParamOPT.UnderType1ROak,
               			vParamOPT.UnderType1DistRepOak,vParamOPT.UnderType1DistRepPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
         else if ( vParamOPT.typeUnder==2)
         {	test=simulateXYSE6(vsOakUnder.treeNumber,vsOakUnder.x, vsOakUnder.y,
               			vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.UnderType2NbAgOak),vParamOPT.UnderType2ROak,
               			vParamOPT.UnderType2DistAttOak,vParamOPT.UnderType2DistRepPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
         else if ( vParamOPT.typeUnder==3)
         {	test=simulateXYSE5(vsOakUnder.treeNumber,vsOakUnder.x, vsOakUnder.y,
               			vsOak.treeNumber,vsOak.x, vsOak.y,
               			vsPine.treeNumber,vsPine.x,vsPine.y,
               			(int)Math.round(surf*vParamOPT.UnderType3NbAgOak),vParamOPT.UnderType3ROak,
               			vParamOPT.UnderType3DistRepOak,vParamOPT.UnderType3DistAttPine,
               		vParamOPT.virtualStandXmin, vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         }
      	// for pine understorey
         //if (vParamOPT.numberPineUnder>0)
         //{
            simulerRandom(vsPineUnder.treeNumber, vsPineUnder.x,vsPineUnder.y,vParamOPT.virtualStandXmin,vParamOPT.virtualStandXmax, vParamOPT.virtualStandYmin, vParamOPT.virtualStandYmax);
         //}


      // If everything is ok, we have calculated the total number of trees
         System.out.println("Nb total : "+TotalNbTree);

      // we still have to cerate the final vStand,
      // and to simulate the finale intertype structure
      // (because we need to handle a unique array)
         vStand.h = new double[TotalNbTree+1];
         vStand.d = new double[TotalNbTree+1];
         vStand.x = new double[TotalNbTree+1];
         vStand.y = new double[TotalNbTree+1];
         vStand.e = new int[TotalNbTree+1];
         vStand.p = new int[TotalNbTree+1];
         vStand.treeNumber=TotalNbTree;
         int indice=0;

       // oaks	canopy
         for (int i=1;i<=vsOak.treeNumber;i++)
         {	indice=indice+1;
            vStand.h[indice]=vsOak.h[i];
            vStand.d[indice]=vsOak.d[i];
            vStand.x[indice]=vsOak.x[i];
            vStand.y[indice]=vsOak.y[i];
            vStand.p[indice]=1;
            vStand.e[indice]=0;	// oaks = 0
         }

      	// pines canopy
         for (int i=1;i<=vsPine.treeNumber;i++)
         {	indice=indice+1;
            vStand.h[indice]=vsPine.h[i];
            vStand.d[indice]=vsPine.d[i];
            vStand.x[indice]=vsPine.x[i];
            vStand.y[indice]=vsPine.y[i];
            vStand.p[indice]=2;
            vStand.e[indice]=1;	// pines = 1
         }

         // oaks	understorey
         for (int i=1;i<=vsOakUnder.treeNumber;i++)
         {	indice=indice+1;
            vStand.h[indice]=vsOakUnder.h[i];
            vStand.d[indice]=vsOakUnder.d[i];
            vStand.x[indice]=vsOakUnder.x[i];
            vStand.y[indice]=vsOakUnder.y[i];
            vStand.p[indice]=3;
            vStand.e[indice]=0;	// oaks = 0
         }

        	// pines canopy
         for (int i=1;i<=vsPineUnder.treeNumber;i++)
         {	indice=indice+1;
            vStand.h[indice]=vsPineUnder.h[i];
            vStand.d[indice]=vsPineUnder.d[i];
            vStand.x[indice]=vsPineUnder.x[i];
            vStand.y[indice]=vsPineUnder.y[i];
            vStand.p[indice]=4;
            vStand.e[indice]=1;	// pines = 1
         }



         return 0;  // to be improved


      } // end simulateMixedStand

      ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus SE7 (anciennement SE2)
   	// correspondant à : S NS (NbAgS, RS)
   	//		avec répulsion partielle aux chenes de la canopee (l2 : proba linéaire de 0 en dist=0 à 1 en dist=l2)
   	//		et répulsion partielle aux pins de la canopée (l3 : proba linéaire de 0 en dist=0 à 1 en dist=l3 )
   	// simule S points de type S,  ranges dans xs,ys

       static public int simulateXYSE7(int S, double xs[], double ys[], int N, double x[], double y[], int P, double w[], double z[],
       int NbAgS, double RS, double l2, double l3,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         int erreur=0;
         double xag[];
         double yag[];
         xag = new double[NbAgS+1];
         yag = new double[NbAgS+1];

      // pour chaque point s, on s'autorise 100 essais, sinon c'est que la realisation est impossible
         int NbEssai2=0;

         // 1. créer le semis de points des centres d'agrégats
         for(int i=1; i<=NbAgS; i++)
         {
            // je simule les coordonnées xag et yag du point i, centre d'agrégats du sous étage
            xag[i]=xmin + R.nextDouble()*(xmax-xmin);
            yag[i]=ymin + R.nextDouble()*(ymax-ymin);

         }

         // 2. simulation des points du sous étage, dans le rayon Rs des agrégats et loin des semis A et B
         // on simule S points à une distance <ou = à d; d'au moins un des centres i d'agrégats
         for(int j=1;j<=S;j++)
         {	// pour chaque point j du sous étage, le nombre des points du ss etage étant S
            double xstemp=xmin + R.nextDouble()*(xmax-xmin);
            double ystemp=ymin + R.nextDouble()*(ymax-ymin);

            // on calcule la distance du point au centre i des agregats
            // et on teste si on est assez pres
            int ok=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for( int i=1;i<=NbAgS;i++)
            {
               double distance2=(xag[i]-xstemp)*(xag[i]-xstemp)+(yag[i]-ystemp)*(yag[i]-ystemp)  ;
               if (distance2<=RS*RS)	// ca marche !
               {	ok=1;
                  i=NbAgS;
               }
            }

            if (ok==0)	// le point est mauvais : il faut recommencer
            {	j=j-1;
            }
            else			// on peut passer aux autres test
            {

            // 3. 2ème test : est il loin des points A de la canopée ?
               int ok2=0;	// drapeau si = 0 : on le garde (soit loin de A, soit proba); si = 1 : pres de A et on le rejette

               // je cherche la distance minimale au  semis de points A de la canopée pour calculer la proba
               // pour gagner du temps on fait le test sur la distance au carré
               double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
               for(int i=1;i<=N;i++)
               {
                  double distance2=(x[i]-xstemp)*(x[i]-xstemp)+(y[i]-ystemp)*(y[i]-ystemp)  ;
                  if (distance2<distmin2)	//c'est plus petit : on memorise
                  {	distmin2=distance2;
                  }
               }	// quand je sors, j'ai la distance au plus proche voisin A

               // calcul de la proba
               double distmin=Math.sqrt(distmin2);
               if (distmin<l2)	// des voisins a une distance inferieure a l2 : on ne garde que avec une certaine proba
               {
                  if (R.nextDouble()<=distmin/l2)		// la proba de garder le point vaut distmin/l
                  {	ok2=0; // on le garde
                  }
                  else		// on rejete le point
                  {  ok2=1;
                  }
               }
               else	// on le garde dans tous les cas si il est au dela de l2
               {	ok2=0;
               }

               if (ok2!=0)	// le point est mauvais : il faut recommencer
               {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai2=NbEssai2+1;
                  if (NbEssai2>100) // situation bloquee : on arrete tout !
                  {
                     return 1;
                  }
               }
               else			// ca marche pour A, on peut passer au dernier test
               {
               // 4. 3ième test : est il loin des points B de la canopée ?
                  int ok3=0;	// drapeau si = 0 : on le garde (soit loin de B, soit proba); si = 1 : pres de B et on le rejette

                  // je cherche la distance minimale au  semis de points B de la canopée pour calculer la proba
                  // pour gagner du temps on fait le test sur la distance au carré
                  distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
                  for(int i=1;i<=P;i++)
                  {
                     double distance2=(w[i]-xstemp)*(w[i]-xstemp)+(z[i]-ystemp)*(z[i]-ystemp)  ;
                     if (distance2<distmin2)	//c'est plus petit : on memorise
                     {	distmin2=distance2;
                     }
                  }	// quand je sors, j'ai la distance au plus proche voisin B

                  // calcul de la proba
                  distmin=Math.sqrt(distmin2);
                  if (distmin<l3)	// des voisins a une distance inferieure a l : on ne garde que avec une certaine proba
                  {
                     if (R.nextDouble()<=distmin/l3)		// la proba de garder le point vaut distmin/l
                     {	ok3=0;	// on le garde
                     }
                     else		// on rejete le point
                     { 	ok3=1;
                     }
                  }
                  else // si il est assez loin : on garde dans tous les cas
                  {	ok3=0;
                  }

                  if (ok3!=0)	// le point est mauvais : il faut recommencer
                  {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                     NbEssai2=NbEssai2+1;
                     if (NbEssai2>100)
                     {
                        return 1;
                     }
                  }
                  else		// on garde le point !!!!
                  {
                     NbEssai2=0;
                     xs[j]=xstemp;
                     ys[j]=ystemp;
                  }
               }
            }
         }
         return erreur;
      }

	   ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus SE6 (anciennement SE8)
   	// correspondant à : S NS (NbAgS, RS)
   	//		avec attraction aux chenes de la canopee (l2 = distance maximum entre S et A
   	//		et répulsion partielle aux pins de la canopée (l3 : proba linéaire de 0 en dist=0 à 1 en dist=l3 )
   	// simule S points de type S,  ranges dans xs,ys

       static public int simulateXYSE6(int S, double xs[], double ys[], int N, double x[], double y[], int P, double w[], double z[],
       int NbAgS, double RS, double l2, double l3,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         int erreur=0;
         double xag[];
         double yag[];
         xag = new double[NbAgS+1];
         yag = new double[NbAgS+1];

      // pour chaque point s, on s'autorise 100 essais, sinon c'est que la realisation est impossible
         int NbEssai2=0;

         // 1. créer le semis de points des centres d'agrégats
         for(int i=1; i<=NbAgS; i++)
         {
            // je simule les coordonnées xag et yag du point i, centre d'agrégats du sous étage
            xag[i]=xmin + R.nextDouble()*(xmax-xmin);
            yag[i]=ymin + R.nextDouble()*(ymax-ymin);

         }

         // 2. simulation des points du sous étage, dans le rayon Rs des agrégats, pres de A et loin de B
         // on simule S points à une distance <ou = à d; d'au moins un des centres i d'agrégats
         for(int j=1;j<=S;j++)
         {	// pour chaque point j du sous étage, le nombre des points du ss etage étant S
            double xstemp=xmin + R.nextDouble()*(xmax-xmin);
            double ystemp=ymin + R.nextDouble()*(ymax-ymin);

            // on calcule la distance du point au centre i des agregats
            // et on teste si on est assez pres
            int ok=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for( int i=1;i<=NbAgS;i++)
            {
               double distance2=(xag[i]-xstemp)*(xag[i]-xstemp)+(yag[i]-ystemp)*(yag[i]-ystemp)  ;
               if (distance2<=RS*RS)	// ca marche !
               {	ok=1;
                  i=NbAgS;
               }
            }

            if (ok==0)	// le point est mauvais : il faut recommencer
            {	j=j-1;
            }
            else			// on peut passer aux autres test
            {

            // 3. 2ème test : est il loin des points A de la canopée ?
               int ok2=0;	// drapeau si = 0 : on le rejette (loin de A); si = 1 : pres de A et on le garde

               // on regarde tous les A jusqu'à en trouver un proche
               for(int i=1;i<=N;i++)
               {
                  double distance2=(x[i]-xstemp)*(x[i]-xstemp)+(y[i]-ystemp)*(y[i]-ystemp)  ;
                  if (distance2<=l2*l2)	// ca marche !
                  {	ok2=1;
                     i=N;
                  }
               }

               if (ok2!=1)	// le point est mauvais : il faut recommencer
               {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai2=NbEssai2+1;
                  if (NbEssai2>100) // situation bloquee : on arrete tout !
                  {
                     return 1;
                  }
               }
               else			// ca marche pour A, on peut passer au dernier test
               {
               // 4. 3ième test : est il loin des points B de la canopée ?
                  int ok3=0;	// drapeau si = 0 : on le garde (soit loin de B, soit proba); si = 1 : pres de B et on le rejette

                  // je cherche la distance minimale au  semis de points B de la canopée pour calculer la proba
                  // pour gagner du temps on fait le test sur la distance au carré
                  double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
                  for(int i=1;i<=P;i++)
                  {
                     double distance2=(w[i]-xstemp)*(w[i]-xstemp)+(z[i]-ystemp)*(z[i]-ystemp)  ;
                     if (distance2<distmin2)	//c'est plus petit : on memorise
                     {	distmin2=distance2;
                     }
                  }	// quand je sors, j'ai la distance au plus proche voisin B

                  // calcul de la proba
                  double distmin=Math.sqrt(distmin2);
                  if (distmin<l3)	// des voisins a une distance inferieure a l3 : on ne garde que avec une certaine proba
                  {
                     if (R.nextDouble()<=distmin/l3)		// la proba de garder le point vaut distmin/l3
                     {	ok3=0;	// on le garde
                     }
                     else		// on rejete le point
                     { 	ok3=1;
                     }
                  }
                  else // si il est assez loin : on garde dans tous les cas
                  {	ok3=0;
                  }

                  if (ok3!=0)	// le point est mauvais : il faut recommencer
                  {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                     NbEssai2=NbEssai2+1;
                     if (NbEssai2>100)
                     {
                        return 1;
                     }
                  }
                  else		// on garde le point !!!!
                  {
                     NbEssai2=0;
                     xs[j]=xstemp;
                     ys[j]=ystemp;
                  }
               }
            }
         }
         return erreur;
      }


      ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus SE5 (anciennement SE4)
   	// correspondant à : S NS (NbAgS, RS)
   	//		avec répulsion partielle aux chenes de la canopee (l2 : proba linéaire de 0 en dist=0 à 1 en dist=l2)
   	//		et attraction aux pins de la canopée (l3 : distance maximum entre S et A )
   	// simule S points de type S,  ranges dans xs,ys

       static public int simulateXYSE5(int S, double xs[], double ys[], int N, double x[], double y[], int P, double w[], double z[],
       int NbAgS, double RS, double l2, double l3,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         int erreur=0;
         double xag[];
         double yag[];
         xag = new double[NbAgS+1];
         yag = new double[NbAgS+1];

      // pour chaque point s, on s'autorise 100 essais, sinon c'est que la realisation est impossible
         int NbEssai2=0;

         // 1. créer le semis de points des centres d'agrégats
         for(int i=1; i<=NbAgS; i++)
         {
            // je simule les coordonnées xag et yag du point i, centre d'agrégats du sous étage
            xag[i]=xmin + R.nextDouble()*(xmax-xmin);
            yag[i]=ymin + R.nextDouble()*(ymax-ymin);

         }

         // 2. simulation des points du sous étage, dans le rayon Rs des agrégats et loin des semis A et B
         // on simule S points à une distance <ou = à d; d'au moins un des centres i d'agrégats
         for(int j=1;j<=S;j++)
         {	// pour chaque point j du sous étage, le nombre des points du ss etage étant S
            double xstemp=xmin + R.nextDouble()*(xmax-xmin);
            double ystemp=ymin + R.nextDouble()*(ymax-ymin);

            // on calcule la distance du point au centre i des agregats
            // et on teste si on est assez pres
            int ok=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for( int i=1;i<=NbAgS;i++)
            {
               double distance2=(xag[i]-xstemp)*(xag[i]-xstemp)+(yag[i]-ystemp)*(yag[i]-ystemp)  ;
               if (distance2<=RS*RS)	// ca marche !
               {	ok=1;
                  i=NbAgS;
               }
            }

            if (ok==0)	// le point est mauvais : il faut recommencer
            {	j=j-1;
            }
            else			// on peut passer aux autres test
            {

            // 3. 2ème test : est il loin des points A de la canopée ?
               int ok2=0;	// drapeau si = 0 : on le garde (soit loin de A, soit proba); si = 1 : pres de A et on le rejette

               // je cherche la distance minimale au  semis de points A de la canopée pour calculer la proba
               // pour gagner du temps on fait le test sur la distance au carré
               double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
               for(int i=1;i<=N;i++)
               {
                  double distance2=(x[i]-xstemp)*(x[i]-xstemp)+(y[i]-ystemp)*(y[i]-ystemp)  ;
                  if (distance2<distmin2)	//c'est plus petit : on memorise
                  {	distmin2=distance2;
                  }
               }	// quand je sors, j'ai la distance au plus proche voisin A

               // calcul de la proba
               double distmin=Math.sqrt(distmin2);
               if (distmin<l2)	// des voisins a une distance inferieure a l2 : on ne garde que avec une certaine proba
               {
                  if (R.nextDouble()<=distmin/l2)		// la proba de garder le point vaut distmin/l
                  {	ok2=0; // on le garde
                  }
                  else		// on rejete le point
                  {  ok2=1;
                  }
               }
               else	// on le garde dans tous les cas si il est au dela de l2
               {	ok2=0;
               }

               if (ok2!=0)	// le point est mauvais : il faut recommencer
               {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai2=NbEssai2+1;
                  if (NbEssai2>100) // situation bloquee : on arrete tout !
                  {
                     return 1;
                  }
               }
               else			// ca marche pour A, on peut passer au dernier test
               {
               // 4. 3ième test : est il pres des points B de la canopée ?
                  int ok3=0;	// drapeau si = 0 : on le rejette (loin de B); si = 1 : pres de B et on le garde

               	// on regarde tous les B jusqu'à en trouver un proche
                  for(int i=1;i<=P;i++)
                  {
                     double distance2=(w[i]-xstemp)*(w[i]-xstemp)+(z[i]-ystemp)*(z[i]-ystemp)  ;
                     if (distance2<=l3*l3)	// ca marche !
                     {	ok3=1;
                        i=P;
                     }
                  }

                  if (ok3!=1)	// le point est mauvais : il faut recommencer
                  {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                     NbEssai2=NbEssai2+1;
                     if (NbEssai2>100)
                     {
                        return 1;
                     }
                  }
                  else		// on garde le point !!!!
                  {
                     NbEssai2=0;
                     xs[j]=xstemp;
                     ys[j]=ystemp;
                  }
               }
            }
         }
         return erreur;
      }


      ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus NS3
   	// correspondant à : A NS (NbAgA, RA)
   	//							B NS (NbAgB, RB) avec régularité partielle (ll : proba linéaire de 0 en dist)0 à 1 en dist=ll)
   	//													et répulsion partielle (l : proba linéaire de 0 en dist=0 à 1 en dist=l )
   	// simule N points de type A,  ranges dans x,y
   	// et P points de type B,  ranges dans w,z

       static public int simulateXYNS3(int N, double x[], double y[], int P, double w[], double z[],
       int NbAgA,int NbAgB, double RA,double RB, double l, double ll,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         double xag[];
         double yag[];
         double wag[];
         double zag[];
         xag = new double[NbAgA+1];
         yag = new double[NbAgA+1];
         wag = new double[NbAgB+1];
         zag = new double[NbAgB+1];
         int erreur=0;

      	/// simuler le semis A
      	//////////////////////

        // création du semis de points des centres d'agrégats
         simulerRandom(NbAgA, xag, yag, xmin, xmax, ymin, ymax);

         // simulation des agregats : on simule N points dans la surface d'analyse
         // on simule N points à une distance <ou = à RA; d'au moins un des centres d'agrégats

         for(int i=1;i<=N;i++)
         {	// pour chaque point i

            double xtemp=xmin + R.nextDouble()*(xmax-xmin);
            double ytemp=ymin + R.nextDouble()*(ymax-ymin);

                     // on calcule la distance du point au centre des agregats
                     // et on teste si on est assez pres
            int caMarche=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for(int a=1;a<=NbAgA;a++)
            {
               double distance2=(xag[a]-xtemp)*(xag[a]-xtemp)+(yag[a]-ytemp)*(yag[a]-ytemp)  ;
               if (distance2<=RA*RA)	// ca marche !
               {	caMarche=1;
                  a=NbAgA;
               }
            }
                     // on regarde si ca  a marche, c'est a dire si le point est proche d'un agregat qq
            if (caMarche==1)	// le point est bon
            {	x[i]=xtemp;
               y[i]=ytemp;
                        //  System.out.println(i+" "+ x[i]+" "+y[i]);
            }

                        // sinon il faut recommencer ce point
            else
            {//	System.out.println(i+"annule ");
                        // il faut que je teste ce nouveau point
               i=i-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
            }
         }

       	// simulation du 2eme semis de points
         /////////////////////////////////////

         // creer tous les points p à une distance l des points du 1ers semis et dans le rayon des 2èmes agrégats
        // System.out.println("\n(b) simulation du 2ème semis de points");

         // créer le semis de points des centres d'agrégats
         // creer tous les centres d'agrégats c : à une distance l des 1erssemis de points
      	// pour chaque point, on s'autorise 100 essais, sinon c'est que la realisation est impossible
         int NbEssai=0;
         for(int c=1; c<=NbAgB; c++)
         {
            // je simule les coordonnées w et z du point c, centre d'agrégats du 2ème semis
            wag[c]=xmin + R.nextDouble()*(xmax-xmin);
            zag[c]=ymin + R.nextDouble()*(ymax-ymin);

         }

         // simulation des points du 2ème semis de points, dans le rayon des agrégats et loin des 1er semis

         //b.on simule P points dans la surface d'analyse
         // on simule P points à une distance <ou = à d; d'au moins un des centres c d'agrégats
         for(int j=1;j<=P;j++)
         {	// pour chaque point i

            double wtemp=xmin + R.nextDouble()*(xmax-xmin);
            double ztemp=ymin + R.nextDouble()*(ymax-ymin);

            // on calcule la distance du point au centre c des agregats
            // et on teste si on est assez pres
            int ok=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for( int c=1;c<=NbAgB;c++)
            {
               double distance2=(wag[c]-wtemp)*(wag[c]-wtemp)+(zag[c]-ztemp)*(zag[c]-ztemp)  ;
               if (distance2<=RB*RB)	// ca marche !
               {	ok=1;
                  c=NbAgB;
               }
            }

            if (ok==0)	// le point est mauvais : il faut recommencer
            {	j=j-1;
            }
            else			// on peut passer aux autres test
            {

            // d'abord : est il loin des autres points B ?

               int okB=1;	// drapeau si = 0 : pres des autres B, si = 1 : loin
               if (j>1)
               {
               // je cherche la distance minimale au autres points B pour calculer la proba
               // pour gagner du temps on fait le test sur la distance au carré
                  double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
                  for(int i=1;i<j;i++)
                  {
                     double distance2=(w[i]-wtemp)*(w[i]-wtemp)+(z[i]-ztemp)*(z[i]-ztemp)  ;
                     if (distance2<distmin2)	//c'est plus petit : on memorise
                     {	distmin2=distance2;
                     }
                  }	// quand je sors, j'ai la distance au plus proche voisin B
               // calcul de la proba
                  double distmin=Math.sqrt(distmin2);

                  if (R.nextDouble()<=distmin/ll)		// la proba de garder le point vaut distmin/l
                  {	okB=1;
                  }
                  else		// on rejete le point
                  {  okB=0; // raté
                  }
               }

               if (okB==0)		// raté !
               {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai=NbEssai+1;
                  if (NbEssai>100)
                  {
                     return 1;
                  }
               }
               else
               {
               // dernier test : est il loin des points A ?

               	// je cherche la distance minimale au 1ers semis de points pour calculer la proba
               	// pour gagner du temps on fait le test sur la distance au carré
                  double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
                  for(int i=1;i<=N;i++)
                  {
                     double distance2=(x[i]-wtemp)*(x[i]-wtemp)+(y[i]-ztemp)*(y[i]-ztemp)  ;
                     if (distance2<distmin2)	//c'est plus petit : on memorise
                     {	distmin2=distance2;
                     }
                  }	// quand je sors, j'ai la distance au plus proche voisin
               	// calcul de la proba
                  double distmin=Math.sqrt(distmin2);
                  if (distmin<l)	// des voisins a une distance inferieure a l : on ne garde que avec une certaine proba
                  {
                     if (R.nextDouble()<=distmin/l)		// la proba de garder le point vaut distmin/l
                     {	NbEssai=0;
                        w[j]=wtemp;
                        z[j]=ztemp;
                     }
                     else		// on rejete le point
                     {  j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                        NbEssai=NbEssai+1;
                        if (NbEssai>100)
                        {
                           return 1;
                        }
                     }
                  }
                  else		// pas de voisins a distance inferieure a l : on garde le point dans tous les cas
                  {
                     NbEssai=0;
                     w[j]=wtemp;
                     z[j]=ztemp;

                  }
               }
            }
         }
         return erreur;
      }

      ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus HC2d
   	// correspondant à : B NS (NbAgB, RB) avec régularité partielle (ll : proba linéaire de 0 en dist=0 à 1 en dist=ll)
   	//							A HC intertype partiel (l, pa : proba =pa en dist<=l, 1 apres )
   	// simule N points de type A,  ranges dans x,y
   	// et P points de type B,  ranges dans w,z

       static public int simulateXYHC2d(int N, double x[], double y[], int P, double w[], double z[],
       int NbAgB, double RB, double l, double pa, double ll,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         double wag[];
         double zag[];
         wag = new double[NbAgB+1];
         zag = new double[NbAgB+1];
         int erreur=0;

      	/// simuler le semis B
      	//////////////////////

         int NbEssai=0;
       // création du semis de points des centres d'agrégats
         simulerRandom(NbAgB, wag, zag, xmin, xmax, ymin, ymax);

         // simulation des points du 2ème semis de points, dans le rayon des agrégats et avec une legere regularite

         //b.on simule P points dans la surface d'analyse
         // on simule P points à une distance <ou = à d; d'au moins un des centres c d'agrégats
         for(int j=1;j<=P;j++)
         {	// pour chaque point i

            double wtemp=xmin + R.nextDouble()*(xmax-xmin);
            double ztemp=ymin + R.nextDouble()*(ymax-ymin);

            // on calcule la distance du point au centre c des agregats
            // et on teste si on est assez pres
            int ok=0;	// drapeau si = 0 : loin des agregats, si = 1 : pres d'un agregat
            for( int c=1;c<=NbAgB;c++)
            {
               double distance2=(wag[c]-wtemp)*(wag[c]-wtemp)+(zag[c]-ztemp)*(zag[c]-ztemp)  ;
               if (distance2<=RB*RB)	// ca marche !
               {	ok=1;
                  c=NbAgB;
               }
            }

            if (ok==0)	// le point est mauvais : il faut recommencer
            {	j=j-1;
            }
            else			// on peut passer à l'autre test, de régularité
            {

            //est il loin des autres points B ?

               int okB=1;	// drapeau si = 0 : pres des autres B, si = 1 : loin
               if (j>1)
               {
               // je cherche la distance minimale au autres points B pour calculer la proba
               // pour gagner du temps on fait le test sur la distance au carré
                  double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
                  for(int i=1;i<j;i++)
                  {
                     double distance2=(w[i]-wtemp)*(w[i]-wtemp)+(z[i]-ztemp)*(z[i]-ztemp)  ;
                     if (distance2<distmin2)	//c'est plus petit : on memorise
                     {	distmin2=distance2;
                     }
                  }	// quand je sors, j'ai la distance au plus proche voisin B
               // calcul de la proba
                  double distmin=Math.sqrt(distmin2);

                  if (R.nextDouble()<=distmin/ll)		// la proba de garder le point vaut distmin/l
                  {	okB=1;
                  }
                  else		// on rejete le point
                  {  okB=0; // raté
                  }
               }

               if (okB==0)		// raté !
               {	j=j-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai=NbEssai+1;
                  if (NbEssai>100)
                  {
                     return 1;
                  }
               }
               else		// pas de voisins a distance inferieure a l : on garde le point dans tous les cas
               {
                  NbEssai=0;
                  w[j]=wtemp;
                  z[j]=ztemp;

               }
            }
         }

       	// simulation des  points A
         /////////////////////////////////////


         // creer les N points A à une distance (max) l des points du 1ers semis
         int NbEssai2=0;
         for(int i=1; i<=N; i++)
         {
            // je simule les coordonnées x et y du point i du semis A
            double xtemp=xmin + R.nextDouble()*(xmax-xmin);
            double ytemp=ymin + R.nextDouble()*(ymax-ymin);


            // on passe au test de répulsion : est il loin des points B ?

               	// je cherche la distance minimale au semis de points B pour calculer la proba
               	// pour gagner du temps on fait le test sur la distance au carré
            double distmin2=(xmax-xmin)*(xmax-xmin)+(ymax-ymin)*(ymax-ymin); // la plus grande distance possible
            for(int j=1;j<=P;j++)
            {
               double distance2=(w[j]-xtemp)*(w[j]-xtemp)+(z[j]-ytemp)*(z[j]-ytemp);
               if (distance2<distmin2)	//c'est plus petit : on memorise
               {	distmin2=distance2;
               }
            }	// quand je sors, j'ai la distance au plus proche voisin
               	// calcul de la proba
            double distmin=Math.sqrt(distmin2);
            if (distmin<l)	// des voisins a une distance inferieure a l : on ne garde que avec une certaine proba
            {
               if (R.nextDouble()<=pa)		// la proba de garder vaut pa
               {	NbEssai2=0;
                  x[i]=xtemp;
                  y[i]=ytemp;
               }
               else		// on rejete le point
               {  i=i-1; // => dans la boucle, je recule d'un cran, donc je vais retester ce point
                  NbEssai2=NbEssai2+1;
                  if (NbEssai2>100)
                  {
                     return 1;
                  }
               }
            }
            else		// pas de voisins a distance inferieure a l : on garde le point dans tous les cas
            {
               NbEssai2=0;
               x[i]=xtemp;
               y[i]=ytemp;
            }
         }



         return erreur;
      }


        ///////////////////////////////////////////////////////////////////
   	// simulation d'une realisation du processus HC2b
   	// correspondant à : B NS (NbAgB, RB) avec régularité partielle (ll : proba linéaire de 0 en dist=0 à 1 en dist=ll)
   	//							A HC intertype partiel (l : proba =0 en dist<=l, 1 apres )
   	// simule N points de type A,  ranges dans x,y
   	// et P points de type B,  ranges dans w,z

       static public int simulateXYHC2b(int N, double x[], double y[], int P, double w[], double z[],
       int NbAgB, double RB, double l,double ll,double xmin, double xmax, double ymin, double ymax) throws IOException
      {
         return  simulateXYHC2d(N,x,y,P,w,z,NbAgB,RB,l,0,ll,xmin,xmax,ymin,ymax);
      }

       static public int simulerRandom (int nb, double xx[], double yy[], double xmi, double xma, double ymi, double yma) throws IOException
      {

      // 2 niveaux a droite
      // on simule le semis de Poisson
         for(int i=1;i<=nb;i++)
         {	xx[i]=xmi + R.nextDouble()*(xma-xmi);
            yy[i]=ymi + R.nextDouble()*(yma-ymi);
         }
         return 0;
      }




   }	// end
