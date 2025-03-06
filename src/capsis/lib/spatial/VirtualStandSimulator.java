/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2006 Francois Goreaud.
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
import java.io.StreamTokenizer;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * VirtualStandSimulator : utilities to simulate virtual stand.
 * 
 * @author Francois Goreaud 10/7/2002 - 1/3/2007
 */
    public class VirtualStandSimulator {
   //checked for c4.1.1_09 - fc - 5.2.2003
      static {
      //~ System.out.println ("VirtualStandSimulator *** loaded");
      }
   
   /**
   * One strata virtual stand.
   * Uses a VirtualParameters set of parameters 
   * and returns a VirtualStand + an error code.
   * 
	* @author Francois Goreaud 10/7/2002 - 1/3/2007
   */
   // fc - 5.2.2003 - replaced dialog messaging by Log messaging to make it usable in script mode.
       static public int simulateOneStrata (AmapDialog dial, VirtualParameters vParam, VirtualStand vStand) {
      
      // Return code 0 means no error,
      // other int correspond to specific errors.
      
      // 1 : D & H.
      //---------------------------------------------
      
         if (vParam.virtualStandD==5) {	// common file for n,x,y,D,H
         //int error = LoadDHFile(vParam.virtualStandnDHFile, vStand.treeNumber, vStand.h, vStand.d);
         
         // Checks...
            if (!Check.isFile (vParam.virtualStandnxyDHFile)) {
            //~ JOptionPane.showMessageDialog (dial, 
            		//~ Translator.swap ("VirtualStandSimulator.fileNameIsNotFile"),
            		//~ Translator.swap ("Shared.warning"), 
            		//~ JOptionPane.WARNING_MESSAGE );
               Log.println (Log.ERROR, "VirtualStandSimulator.simulateOneStrata ()", 
                  Translator.swap ("VirtualStandSimulator.fileNameIsNotFile")+" (nxyDH)");
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
               	//~ Translator.swap ("VirtualStandSimulator.exceptionDuringStandLoad")
               	//~ +"\n"+exc.getMessage (),
               	//~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                  Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
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
               	//~ Translator.swap ("VirtualStandSimulator.fileNameIsNotFile"),
               	//~ Translator.swap ("Shared.warning"), 
               	//~ JOptionPane.WARNING_MESSAGE );
                  Log.println (Log.ERROR, "VirtualStandSimulator.simulateOneStrata ()", 
                     Translator.swap ("VirtualStandSimulator.fileNameIsNotFile")+" (nDH)");
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
                  //~ Translator.swap ("VirtualStandSimulator.exceptionDuringStandLoad")
                  //~ +"\n"+exc.getMessage (),
                  //~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
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
                  	//~ Translator.swap ("VirtualStandSimulator.fileNameIsNotFile"),
                  	//~ Translator.swap ("Shared.warning"), 
                  	//~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.simulateOneStrata ()", 
                        Translator.swap ("VirtualStandSimulator.fileNameIsNotFile")+" (nD)");
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
                     //~ Translator.swap ("VirtualStandSimulator.exceptionDuringStandLoad")
                     //~ +"\n"+exc.getMessage (),
                     //~ Translator.swap ("Shared.error"), JOptionPane.ERROR_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.simulateOneStrata ()", 
                           Translator.swap ("VirtualStandSimulator.exceptionDuringStandLoad")+" (nD)", exc);
                        return 1213;
                     }
               //~ Log.println("nD loaded");
               
               } 
               else if (vParam.virtualStandD==3) {	// histogram file for D
               
               // Checks...
                  if (!Check.isFile (vParam.virtualStandDHistFile)) {
                  //~ JOptionPane.showMessageDialog (dial, 
                  	//~ Translator.swap ("VirtualStandSimulator.fileNameIsNotFile"),
                  	//~ Translator.swap ("Shared.warning"), 
                  	//~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.simulateOneStrata ()", 
                        Translator.swap ("VirtualStandSimulator.fileNameIsNotFile")+" (DHist)");
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
                     //~ Translator.swap ("VirtualStandSimulator.exceptionDuringStandLoad")
                     //~ +"\n"+exc.getMessage (),
                     //~ Translator.swap ("Shared.error"), 
                     //~ JOptionPane.ERROR_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
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
                     	//~ Translator.swap ("VirtualStandSimulator.treeNumber"),
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.treeNumber"));
                        return 1217;
                     }
                     if (vParam.virtualStandDMean<0) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     	//~ Translator.swap ("VirtualStandSimulator.DMean"),
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.DMean"));
                        return 1218;
                     }
                     if (vParam.virtualStandDDeviation<0) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     	//~ Translator.swap ("VirtualStandSimulator.DDeviation"),
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.DDeviation"));
                        return 1219;
                     }
                  
                     vStand.d = new double[vStand.treeNumber+1];
                     RandomPattern.simulateGaussianM(vStand.treeNumber, vStand.d, vParam.virtualStandDMean, vParam.virtualStandDDeviation);	
  							
							// here we add a test to verify that d> Dmin	FG 1/3/2007
							for (int i=1;i<=vStand.treeNumber;i++)
							{	if (vStand.d[i]<vParam.virtualStandDMin)
								{	vStand.d[i]=vParam.virtualStandDMin;
								}
  							}
                  } 
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     //~ Translator.swap("VirtualStandSimulator.simulationProblemD"), 
                     //~ Translator.swap ("Shared.warning"), 
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.simulationProblemD"), exc);
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
                     	//~ Translator.swap("VirtualStandSimulator.simulationProblemHCurve"), 
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.simulationProblemHCurve"));
                        return 999;
                     }
                  // No return because simulation will be done anyway.
                  } 
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     //~ Translator.swap("VirtualStandSimulator.simulationProblemHCurve"), 
                     //~ Translator.swap ("Shared.warning"), 
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.simulationProblemHCurve"), exc);
                        return 999;
                     }
               } 
               else {	// gaussian distribution
                  try {
                     vStand.h = new double[vStand.treeNumber+1];
                     if (vParam.virtualStandHMean<0) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     	//~ Translator.swap ("VirtualStandSimulator.HMean"),
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.HMean"));
                        return 1218;
                     }
                     if (vParam.virtualStandHDeviation<0) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     	//~ Translator.swap ("VirtualStandSimulator.HDeviation"),
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.HDeviation"));
                        return 1219;
                     }
                     RandomPattern.simulateGaussianM(vStand.treeNumber, vStand.h, vParam.virtualStandHMean, vParam.virtualStandHDeviation);

							// here we add a test to verify that h> Hmin	FG 1/3/2007
							for (int i=1;i<=vStand.treeNumber;i++)
							{	if (vStand.h[i]<vParam.virtualStandHMin)
								{	vStand.h[i]=vParam.virtualStandHMin;
								}
  							}

                  } 
                      catch (Exception exc) {
                     //~ JOptionPane.showMessageDialog (dial, 
                     //~ Translator.swap("VirtualStandSimulator.simulationProblemH"), 
                     //~ Translator.swap ("Shared.warning"), 
                     //~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.simulationProblemH"));
                        return 999;
                     }
               }
            
            } 
            else {	// error for D&H
            // "à traiter mieux que ca !" (i.e. "to do better than this" ;-)
            //~ JOptionPane.showMessageDialog (dial, 
            	//~ Translator.swap ("VirtualStandSimulator.simulationProblemD"), 
            	//~ Translator.swap ("Shared.warning"), 
            	//~ JOptionPane.WARNING_MESSAGE );
               Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                  Translator.swap ("VirtualStandSimulator.simulationProblemD"));
               return 1;
            } 
         
         
         // 2 : X & Y.
         //---------------------------------------------
         
            if (vParam.virtualStandXY==1) {	// random pattern
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
                  //~ Translator.swap ("VirtualStandSimulator.XYRandom"), 
                  //~ Translator.swap ("Shared.warning"), 
                  //~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                        Translator.swap ("VirtualStandSimulator.XYRandom"), exc);
                     return 999;
                  }
            } 
            else if (vParam.virtualStandXY==2) {	// Neyman Scott pattern
               try {
                  vStand.x = new double[vStand.treeNumber+1];
                  vStand.y = new double[vStand.treeNumber+1];
                  NeymanScottPattern.simulateXY(vStand.treeNumber, vParam.virtualStandClusterNumber,
                     vParam.virtualStandClusterRadius, vStand.x, vStand.y,
                     vParam.virtualStandXmin, vParam.virtualStandXmax,
                     vParam.virtualStandYmin, vParam.virtualStandYmax,
                     vParam.virtualStandPrecision); 
               } 
                   catch (Exception exc) {
                  //~ JOptionPane.showMessageDialog (dial, 
                  //~ Translator.swap ("VirtualStandSimulator.XYNeymanScott"), 
                  //~ Translator.swap ("Shared.warning"), 
                  //~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                        Translator.swap ("VirtualStandSimulator.XYNeymanScott"), exc);
                     return 999; 
                  }
            
            } 
            else if (vParam.virtualStandXY==3) {	// Gibbs process
               try {
                  int intervalNumber = vParam.virtualStandGibbsInterval;
                  double[] intervalRadius = new double[4];
                  double[] intervalCost = new double[4];
               
                  if (intervalNumber<=0) {
                  //~ JOptionPane.showMessageDialog (dial, 
                  	//~ Translator.swap ("DInitStand.simulationProblemXY")
                  	//~ +"intervalNumber<=0", 
                  	//~ Translator.swap ("Shared.warning"), 
                  	//~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.WARNING, "VirtualStandSimulator.SimulateOneStrata ()",
                        Translator.swap ("VirtualStandSimulator.simulationProblemXY")+" (intervalNumber<=0)");
                     intervalNumber=1;
                     intervalRadius[1]=1;
                     intervalCost[1]=0;
                  } 
                  else {
                     intervalRadius[1]=vParam.virtualStandGibbsR1;
                     intervalCost[1]=vParam.virtualStandGibbsCost1;
                     if (intervalRadius[1]<=0) {	
                     //~ JOptionPane.showMessageDialog (dial, 
                     	//~ Translator.swap ("DInitStand.simulationProblemXY")+"R1<=0", 
                     	//~ Translator.swap ("Shared.warning"), 
                     	//~ JOptionPane.WARNING_MESSAGE );
                        Log.println (Log.WARNING, "VirtualStandSimulator.SimulateOneStrata ()",
                           Translator.swap ("VirtualStandSimulator.simulationProblemXY")+" (R1<=0)");
                        intervalNumber=1;
                        intervalRadius[1]=1;
                        intervalCost[1]=0;
                     }
                  
                     if (intervalNumber >1) {
                        intervalRadius[2]=vParam.virtualStandGibbsR2;
                        intervalCost[2]=vParam.virtualStandGibbsCost2;
                        if (intervalRadius[2]<=intervalRadius[1]) {	
                        //~ JOptionPane.showMessageDialog (dial, 
                        	//~ Translator.swap ("DInitStand.simulationProblemXY")+"R2<=R1", 
                        	//~ Translator.swap ("Shared.warning"), 
                        	//~ JOptionPane.WARNING_MESSAGE );
                           Log.println (Log.WARNING, "VirtualStandSimulator.SimulateOneStrata ()",
                              Translator.swap ("VirtualStandSimulator.simulationProblemXY")+" (R2<=R1)");
                           intervalNumber=1;
                           intervalRadius[2]=0;
                        }
                     
                     } 
                     else {
                        intervalRadius[2]=0;
                     }
                  
                     if (intervalNumber >2) {
                        intervalRadius[3]=vParam.virtualStandGibbsR3;
                        intervalCost[3]=vParam.virtualStandGibbsCost3;
                        if (intervalRadius[3]<=intervalRadius[2]) {	
                        //~ JOptionPane.showMessageDialog (dial, 
                        	//~ Translator.swap ("DInitStand.simulationProblemXY")+"R3<=R2", 
                        	//~ Translator.swap ("Shared.warning"), 
                        	//~ JOptionPane.WARNING_MESSAGE );
                           Log.println (Log.WARNING, "VirtualStandSimulator.SimulateOneStrata ()",
                              Translator.swap ("VirtualStandSimulator.simulationProblemXY")+" (R3<=R2)");
                           intervalNumber=2;
                           intervalRadius[3]=0;
                        }
                     
                     } 
                     else {
                        intervalRadius[3]=0;
                     }
                  }	
                  vStand.x = new double[vStand.treeNumber+1];
                  vStand.y = new double[vStand.treeNumber+1];
                  GibbsPattern.simulateXY(vStand.treeNumber, vStand.x, vStand.y,
                     vParam.virtualStandXmin, vParam.virtualStandXmax,
                     vParam.virtualStandYmin, vParam.virtualStandYmax, 
                     vParam.virtualStandPrecision, intervalNumber,
                     intervalRadius, intervalCost, vParam.virtualStandGibbsIteration);
               } 
                   catch (Exception exc) {
                  //~ JOptionPane.showMessageDialog (dial, 
                  //~ Translator.swap ("VirtualStandSimulator.XYGibbs"), 
                  //~ Translator.swap ("Shared.warning"), 
                  //~ JOptionPane.WARNING_MESSAGE );
                     Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                        Translator.swap ("VirtualStandSimulator.XYGibbs"), exc);
                     return 999;
                  }
            } 
            else {	// error for XY
            // to be improved
            //~ JOptionPane.showMessageDialog (dial, 
            	//~ Translator.swap ("VirtualStandSimulator.XY"), 
            	//~ Translator.swap ("Shared.warning"), 
            	//~ JOptionPane.WARNING_MESSAGE );
               Log.println (Log.ERROR, "VirtualStandSimulator.SimulateOneStrata ()",
                  Translator.swap ("VirtualStandSimulator.XY"));
               return 2;
            }
         } 
         return 0;
      } // end simulateOneStrata
   
       static public int simulateMixedStand (AmapDialog dial, VirtualParametersMixedStand vParamMS, VirtualStand vStand) {
      
      // Return code 0 means no error,
      // other int correspond to specific errors.
      
      // There are different sub populations, 
      // we first simulate each of them as a OneStrata
      
         VirtualStand[] vs;
         vs=new VirtualStand[vParamMS.numberOfPopulation+2];
         int TotalNbTree =0;
      
         for (int pop=1;pop<=vParamMS.numberOfPopulation;pop++)
         {	System.out.print("- pop : "+pop);
            VirtualParameters vParam=vParamMS.param[pop];
            VirtualStand vstand=new VirtualStand();
         // we first simulate specis and D, H, and XY when independance 
         // if there are spatial intertype interactions,
			// we simulate random independance and we will modify it later
            if (vParam.virtualStandXYmode == 2)
            {	vParam.virtualStandXY=1;
            }
            int test=simulateOneStrata(null, vParam, vstand);
            vs[pop]=vstand;
         
         // we must add a test here
            System.out.println(" -> "+test+"("+vstand.treeNumber+")");
         
            TotalNbTree = TotalNbTree+vstand.treeNumber;
         }
      
      // WARNING : the intertype structure has not yet been simulated at this point 
      
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
      
         for (int pop=1;pop<=vParamMS.numberOfPopulation;pop++)
         {	VirtualParameters vp=vParamMS.param[pop];	
            for (int i=1;i<=vs[pop].treeNumber;i++)
            {	indice=indice+1;
               vStand.h[indice]=vs[pop].h[i];
               vStand.d[indice]=vs[pop].d[i];
               vStand.x[indice]=vs[pop].x[i];
               vStand.y[indice]=vs[pop].y[i];
               vStand.p[indice]=pop;				
               vStand.e[indice]=vp.virtualStandSpeciesCode;				
            }
         // now we can simulate the intertype structure if necessary
            if (vp.virtualStandXYmode == 2)
            {	GibbsPattern.evolveXYInteraction(pop,indice, vStand.x, vStand.y, vStand.p,
                  vParamMS.virtualStandXmin, vParamMS.virtualStandXmax,
                  vParamMS.virtualStandYmin, vParamMS.virtualStandYmax, 
                  vp.virtualStandPrecision, vp.virtualStandInteractionNumber,
                  vp.virtualStandInteractionPop, vp.virtualStandInteractionR,
                  vp.virtualStandInteractionCost, vp.virtualStandInteractionIteration);
            
            }
         	
         }
      
         return 0;  // to be improved
      
      
      } // end simulateMixedStand
   
   }	// end
