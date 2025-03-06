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

//~ import  capsis.util.*; 
import jeeb.lib.util.Log;

/**
 * Ripley K(r) and Besag L(r) functions (analysis of the spatial structure).
 * corresponding to the last V7 version of C++ codes
 * taking into account edge effect corrections
 * for rectangular and complex shape plots
 * 
 * @author Francois Goreaud 29/3/2001 - 24/5/2006
 */
public class Ripley {
   
      static double epsilon =0.0001; // admitted imprecision for the relative location points/triangles in case of complex shape plot
   
      static {
      //~ System.out.println ("Ripley *** loaded");
      }
   
	
	// ------------------------------------------------------------------
	// Here are various methods
	// to estimate Ripley's L(r) function for a point pattern  
	// in a rectangular or complex shape plot
	// with its confidence interval
	// for CSR null hypothesis
	// These methods call specific edge effect correction tools 
	// that are also implemented here
	//
	// WARNING : arrays (x,y,l,ax,ay,... are ALL numbered from 1 to N !!! (and NOT from 0 to N-1)
	//
	// The subroutines return 0 if everything was ok, -1 if there were an error
	// -------------------------------------------------------------------

	   
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   // Main subroutines, called by Capsis or PASS
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   

	   
   /** computeLRect
   *
   * This method computes Besag's (1978) L(r) function 
	* for a point pattern defined by x[] and y[] 
	* in a rectangular plot (xmi, xma, ymi, yma)
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * This routine directly calls computeNormalisedKRect, which estimates K(r)
   * 
	* Results are put in the array (tabl) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   ******************************/
   
       static public int computeLRect (double tabl[], double x[], double y[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma,
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabk[] = new double[intervalNumber+1];
			int erreur=0;
         
			try 
         {	//System.out.println("Début computeLRect n="+pointNumber);
         
         	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {	tabk[i]=0;
               tabl[i]=0;
            }
   	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {	intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // K computation.
				erreur=computeNormalisedKRect(tabk,x,y,pointNumber,xmi,xma,ymi,yma,intervalNumber,intervalWidth);
 
       	// Computing L
				if (erreur==0)		// only if we have computed K !
				{	for (int i = 1; i <= intervalNumber; i++) 
					{	//tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               	tabl[i]=Math.sqrt(tabk[i]/Math.PI)-i*intervalWidth;
     		      }
				}
				else
				{	//System.out.println ("L(r) computation error");
					Log.println (Log.ERROR, "Ripley.computeLRect ()", "L(r) computation error");
 	           	for (int i=0; i<tabl.length; i++) 
            	{	tabl[i]=0;
            	}
            	return -1;
				}
        	} 
         catch (Exception exc) 
         {
            //System.out.println ("L(r) computation error");
            Log.println (Log.ERROR, "Ripley.computeLRect ()", "L(r) computation error");
            for (int i=0; i<tabl.length; i++) 
            {	tabl[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
	
   /** computeNormalisedKRect
   *
   * This method computes Ripley's (1978) K(r) function 
	* for a point pattern defined by x[] and y[]
	* in a rectangular plot (xmi, xma, ymi, yma)
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * The subroutine uses g (pair density function, but here not normalised);
   * as intermediate variables to compute K.
	* Initially an unnormalised version of this routine was used only for LIC, because it avoided to compute L
	* Now it is also called by computeLRect.
	*
	* Results are put in the array (tabk) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   ******************************/
   
       static public int computeNormalisedKRect (double tabk[], 
		 	double x[], double y[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma,
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables 
         double tabg[] = new double[intervalNumber+1];
         double cin;
			double surface=0;
			double densite=0;
      
         try 
         {	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {	tabg[i]=0;
               tabk[i]=0;
            }
            surface=(xma-xmi)*(yma-ymi);
            densite=((double) pointNumber)/surface;
   	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {	intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // g computation.
         // Considering couples (i,j) and (j,i) : thus for i>j only 
            for (int i=2; i<=pointNumber; i++) 
            {  double x1=x[i];
               double y1=y[i];
               for (int j=1; j<i; j++) 
               {	double x2=x[j];		// x : m
                  double y2=y[j];		// y : m
               	// distance between these 2 points ?
                  double d = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
               	// which range class does it belong ? (max value excluded)
                  int interval = (int) (d / intervalWidth)+1;
               	// if it is short enought, we count it
                  if (interval <= intervalNumber) 
                  {	
                  ///// for [i,j] :
                  	// edge effect correction
                     cin=perimInRect(x1,y1,d,xmi,xma,ymi,yma);
                     if (cin<0)
                     {	//System.out.print("\ncin<0 on i BEFORE");
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on i BEFORE");
								return -1;
                     }
                     tabg [interval] +=2*Math.PI/cin;
							
						///// for [j,i] :
            			// edge effect correction
							cin=perimInRect(x2,y2,d,xmi,xma,ymi,yma);
							if (cin<0)
							{	//System.out.print("\ncin<0 sur j AVANT");
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on j BEFORE");
   			         	return -1;
      			      }
                     tabg [interval] +=2*Math.PI/cin;
                  }
               }
            }	// end of the double loop on the couples of points
                  
         // Normalisation & co :
	         // averaging -> density
            for (int i = 1; i <= intervalNumber; i++) {
               tabg[i]=tabg[i]/((double) pointNumber);
            }
         	// integrating g to obtain k
            tabk[1]=tabg[1];
            for (int i = 2; i <= intervalNumber; i++) {
               tabk[i]=tabk[i-1]+tabg[i];
            }
         	// normalisation of k
            for (int i = 1; i <= intervalNumber; i++) {
            //tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               tabk[i]=tabk[i]/densite;
            }
   ///// WARNING : we have only normalised K,
   ///// g is here only an intermediate variable
   ///// if we want to estimate g(t) then we still have to normalise it 
     
         } 
         catch (Exception exc) 
         {
            //System.out.println ("K(r) computation error");
            Log.println (Log.ERROR, "Ripley.computeKRect ()", "K(r) computation error");
            for (int i=0; i<tabk.length; i++) 
            {	tabk[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
   	
	/** computeLICRect
   *
   * This method computes the confidence interval for Besag L(r) function 
	* for a point pattern defined by x[] and y[]
	* in a rectangular plot (xmi, xma, ymi, yma)
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * For each range r, the confidence interval is estimated, 
	* for a given risk (risk) , (risk=1 for alpha of 1%)
   * by order statistics on (simulationNumber) Monte Carlo simulations 
	* corresponding to CSR null hypothesis
	*
	* Results are put in the arrays (lic1) and (lic2) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
	*
	* imp is the choice of what is printed on screen : 0 : nothing, 1 : everything, 2 : minimum
	*
   ******************************/
       static public int computeLICRect (int imp, double lic1[], double lic2[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma, 
      	int intervalNumber, double intervalWidth,
       	int simulationNumber, double risk, double precision) 

		{	// intermediate variables
			double x[];
			double y[];
			double k[];
			double kic[][];
			double mer;
			int cro;
			
			// initialisation of results array
			for (int i=0; i<=intervalNumber; i++) 
			{	 lic1[i]=0;
             lic2[i]=0;
         }


			// handle here the case for the simplified estimator
			
			if (simulationNumber < 1) 		// default estimation	
			{	double surface=(xma-xmi)*(yma-ymi);
         	for (int i=1; i<=intervalNumber; i++) 
				{	if (pointNumber>0) {
               	lic1[i]=-1.68*Math.sqrt(surface)/(pointNumber+0.0);
               	lic2[i]=1.68*Math.sqrt(surface)/(pointNumber+0.0);
            	} 
            	else 
					{	lic1[i]=0;
               	lic2[i]=0;
            	}
            }
				return 0;
			}
			
			// ----------------------------------------------------------------------------
			// Otherwise : estimation of  confidence interval through Monte Carlo --------------
			
			try 
         {
           	// Compute the minimum size of the array.
    			/// défining i0 : index where the estimate of CI will be put 
				// risk in %... if alpha 1%, risk=1.
              	double temp=((double) simulationNumber)*((double)risk)/((double)200.0);          // i0=0.005*nbiter;
               int i0= (int) temp;   	
               if (i0<1) {i0=1;}
					// we must redefine the number of simulations as a function of i0;
					simulationNumber=i0*200;
               if (imp>0) {	Log.println ("Monte Carlo, "+simulationNumber+" simulations, i0="+i0);}
            
				// initialisation
 		        	x = new double[pointNumber+1];
   		      y = new double[pointNumber+1];
     		    	k = new double[intervalNumber+1];
               kic = new double[intervalNumber+1][2*i0+2];
  			
	///////////////// main loop for MC
					for(int i=1;i<=simulationNumber;i++)
					{	if (imp==1)	{	Log.println("Iteration "+i);}
						if (imp==2)	{	Log.println("["+i+"]");	}

      			////// simulating CSR null hypothesis
                 	int erreur= RandomPattern.simulateXY(pointNumber,x,y,xmi,xma,ymi,yma,precision);
						if (erreur==0)
	               {	if (imp==1)	{	Log.println(" .");}
						}
						else
						{	if (imp>0)	{	Log.println(" [ERROR XY]");}
						}

			      ////// estimating K
			      /// only if no error !
			      /// we call the normalised routine [for intertri the number of points can vary]
						if (erreur==0)
						{	
							erreur=computeNormalisedKRect(k,x,y,pointNumber,xmi,xma,ymi,yma,intervalNumber, intervalWidth);
   						if (erreur==0)
	               	{	if (imp==1)	{	Log.println(" K");}
							}
							else
							{	if (imp>0)	{	Log.println(" [ERROR K]");}
							}
						}
 
       			////// if there is an error : a new simulation
         			if (erreur!=0)
         			{	i=i-1;
         				if (imp>0)	{Log.println( "ERROR");}
		         	}
  			      	else
         			{

      				////// handle the results
      					if (imp==1)	{Log.println(" T");}

         			// we put the 2i0+1 first values and sort them
           		
                  	if (i<=2*i0+1) 
   	      			{	if (imp==1)	{Log.println(" stock");}      // new values in i
      	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
         					{	kic[tt][i]=k[tt];
               			}

						// from second to (2i0+1)th value : we sort the new value directly 
            				if (i>1)
		            		{  
		           				// buble sorting towards lower values
									if (imp==1)	{	Log.println(" buble sorting directly");}
   	   	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
    		     					{ 
										if (kic[tt][i-1]>kic[tt][i])
       		        	  			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i],kic[tt][i-1]);
         	        					mer=kic[tt][i];
          	    						cro=i-1;
    	      	    					while ((cro>0)&&(kic[tt][cro]>mer))
      	     	   					{	kic[tt][cro+1]=kic[tt][cro];
         	   	           			//printf(" %d -> %d",cro,cro+1);
            	 	 						cro=cro-1;
              							}
           								kic[tt][cro+1]=mer;
  	            					}
   	           				}// end for tt buble sorting
								}// end if (i>1)
  		   		    	}// end if (i<=2*i0+1)
				
					// here, the (2io+1) first values are sorted...
				
						else  
					
					////// the array already contains (2i0+1) sorted values, we put the new value in i0+1

	    		     	{  if (imp==1)	{Log.println("(i0+1)");}
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{	kic[tt][i0+1]=k[tt];
 		           		}

   	         		// sorting the new value of k
    		        		if (imp==1)	{Log.println(" sorting(k) ");}
      	         	//printf("[%f ## %f ## %f] -> ",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{

 		          			// k goes down
   	         			if (kic[tt][i0+1]<kic[tt][i0])
    		        			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i0+1],kic[tt][i0]);
      	         			mer=kic[tt][i0+1];
       		        			cro=i0;
         	      			while ((cro>0)&&(kic[tt][cro]>mer))
          		     			{	kic[tt][cro+1]=kic[tt][cro];
            	   				cro=cro-1;
             		  			}
             					kic[tt][cro+1]=mer;
               				//printf("ok");
	            			}// end if k goes down

 								// k goes up
  		          			else if (kic[tt][i0+1]>kic[tt][i0+2])
    		        			{	//printf("\nk(%d)=%f> %f ",tt,kic[tt][i0+1],kic[tt][i0+2]);
      	         			mer=kic[tt][i0+1];
       		        			cro=i0+2;
         	      			while ((cro<2*i0+2)&&(kic[tt][cro]<mer))
          		     			{	kic[tt][cro-1]=kic[tt][cro];
            	    				cro=cro+1;
             		  			}
              					kic[tt][cro-1]=mer;
               				//printf("ok");
	            			}// end k goes up

 		              	}// end for tt sorting new value

   	            	//printf("[%f ## %f ## %f]",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);

 						}// end if iter sup 2i0+1, sorting
					
					} // end else from if (erreur!=0)
				
				} // end Monte Carlo loop 
     
		////////////////////////////////////// end main MC loop

      /// Handling results
  		    	int i1=i0;
    		  	int i2=i0+2;
      		for(int tt=1;tt<=intervalNumber;tt=tt+1)
	      	{	lic1[tt]=Math.sqrt(kic[tt][i1]/Math.PI)-tt*intervalWidth;
       		  	lic2[tt]=Math.sqrt(kic[tt][i2]/Math.PI)-tt*intervalWidth;
	      	}
	  			
			}
			catch (Exception exc) 
			{	Log.println (Log.ERROR, "Ripley.computeLICRect ()", "LIC(r) computation error");
            for (int i=0; i<=intervalNumber; i++) 
				{	lic1[i]=0;
               lic2[i]=0;
            }
				return -1;
         }
			return 0;
      }
   	

		   
   /** computeLTri
   *
   * This method computes Besag's (1978) L(r) function 
	* for a point pattern defined by x[] and y[]
	* in a plot of complex shape, defined by a rectangular plot (xmi, xma, ymi, yma)
	* and excluded triangles (ax, ay, bx, by, cx, cy).
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * This routine directly calls computeNormalisedKTri, which estimates K(r)
   * 
	* Results are put in the array (tabl) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
   ******************************/
   
       static public int computeLTri (double tabl[], double x[], double y[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabk[] = new double[intervalNumber+1];
			int erreur=0;
         
			try 
         {	//System.out.println("Début computeLTri n="+pointNumber);
         
         	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {
               tabk[i]=0;
               tabl[i]=0;
            }
   	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {
               intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // K computation.
				erreur=computeNormalisedKTri(tabk,x,y,pointNumber,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,intervalNumber,intervalWidth);
 
       	// Computing L
				if (erreur==0)		// only if we have computed K !
				{
    	        	for (int i = 1; i <= intervalNumber; i++) 
					{
     	       		//tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               	tabl[i]=Math.sqrt(tabk[i]/Math.PI)-i*intervalWidth;
     		      }
				}
				else
				{	//System.out.println ("L(r) computation error");
 					Log.println (Log.ERROR, "Ripley.computeLRect ()", "L(r) computation error");
 	           	for (int i=0; i<tabl.length; i++) 
            	{	tabl[i]=0;
            	}
           		return -1;
				}
        	} 
         catch (Exception exc) 
         {
            //System.out.println ("L(r) computation error");
            Log.println (Log.ERROR, "Ripley.computeLRect ()", "L(r) computation error");
            for (int i=0; i<tabl.length; i++) 
            {	tabl[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
   
	
   /** computeNormalisedKTri
   *
   * This method computes Ripley's (1978) K(r) function 
	* for a point pattern defined by x[] and y[]
	* in a plot of complex shape, defined by a rectangular plot (xmi, xma, ymi, yma)
	* and excluded triangles (ax, ay, bx, by, cx, cy).
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * The subroutine uses g (pair density function, but here not normalised);
   * as intermediate variables to compute K.
	* Initially an unnormalised version of this routine was used only for LIC, because it avoided to compute L
	* Now it is also called by computeLTri.
	*
	* Results are put in the array (tabl) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
   ******************************/
   
       static public int computeNormalisedKTri (double tabk[], double x[], double y[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabg[] = new double[intervalNumber+1];
         double cin;
			double surface=0;
			double densite=0;
      
         try 
         {	
			  	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {
               tabg[i]=0;
               tabk[i]=0;
            }
            surface=surfaceTri(xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy);
            densite=((double) pointNumber)/surface;
   	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {
               intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // g computation.
         // Considering couples (i,j) and (j,i) : thus for i>j only 
            for (int i=2; i<=pointNumber; i++) 
            {  double x1=x[i];
               double y1=y[i];
               for (int j=1; j<i; j++) 
               {	double x2=x[j];		// x : m
                  double y2=y[j];		// y : m
               	// distance between these 2 points ?
                  double d = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
               	// which range class does it belong ? (max value excluded)
                  int interval = (int) (d / intervalWidth)+1;
               	// if it is short enought, we count it
                  if (interval <= intervalNumber) 
                  {	
                  ///// for [i,j] :
                  	// edge effect correction
                     cin=perimInRect(x1,y1,d,xmi,xma,ymi,yma);
                     if (cin<0)
                     {	//System.out.print("\ncin<0 sur i AVANT");
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on i BEFORE");
                        return -1;
                     }
                     cin=cin-perimInTri(x1,y1,d,triangleNumber,ax,ay,bx,by,cx,cy);
                     if (cin<0)
                     {	//System.out.print("\ncin<0 sur i APRES : "+cin+" "+i);
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on i AFTER :"+cin+" "+i);
                        return -1;
                     }
                     tabg [interval] +=2*Math.PI/cin;
							
						///// for [j,i] :
            			// edge effect correction
								cin=perimInRect(x2,y2,d,xmi,xma,ymi,yma);
							if (cin<0)
							{	//System.out.print("\ncin<0 sur j AVANT");
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on j BEFORE");							
   			         	return -1;
      			      }
							cin=cin-perimInTri(x2,y2,d,triangleNumber,ax,ay,bx,by,cx,cy);
							if (cin<0)
							{	//System.out.print("\ncin<0 sur j APRES : "+cin+" "+j);
 								Log.println (Log.ERROR, "Ripley.computeKRect ()", "\ncin<0 on j AFTER :"+cin+" "+j);
								return -1;
							}
                     tabg [interval] +=2*Math.PI/cin;
                  }
               }
            }	// end of the double loop on the couples of points
                  
         // Normalisation & co :
	         // averaging -> density
            for (int i = 1; i <= intervalNumber; i++) {
               tabg[i]=tabg[i]/((double) pointNumber);
            }
         	// integrating g to obtain k
            tabk[1]=tabg[1];
            for (int i = 2; i <= intervalNumber; i++) {
               tabk[i]=tabk[i-1]+tabg[i];
            }
         	// normalisation of k
            for (int i = 1; i <= intervalNumber; i++) {
            //tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               tabk[i]=tabk[i]/densite;
            }
   ///// WARNING : we have only normalised K,
   ///// g is here only an intermediate variable
   ///// if we want to estimate g(t) then we still have to normalise it 
    
         } 
         catch (Exception exc) 
         {
            //System.out.println ("K(r) computation error");
            Log.println (Log.ERROR, "Ripley.computeKTri ()", "K(r) computation error");
            for (int i=0; i<tabk.length; i++) 
            {	tabk[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
   

	/** computeLICTri
   *
   * This method computes the confidence interval for Besag L(r) function 
	* for a point pattern defined by x[] and y[]
	* in a plot of complex shape, defined by a rectangular plot (xmi, xma, ymi, yma)
	* and excluded triangles (ax, ay, bx, by, cx, cy).
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * For each range r, the confidence interval is estimated, 
	* for a given risk (risk) , (risk=1 for alpha of 1%)
   * by order statistics on (simulationNumber) Monte Carlo simulations 
	* corresponding to CSR null hypothesis
	*
	* Results are put in the arrays (lic1) and (lic2) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
	*
	* imp is the choice of what is printed on screen : 0 : nothing, 1 : everything, 2 : minimum
	*
   ******************************/
       static public int computeLICTri (int imp, double lic1[], double lic2[], int pointNumber, 
       	double xmi, double xma, double ymi, double yma, 
			int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth,
       	int simulationNumber, double risk, double precision) 

		{	// intermediate variables
			double x[];
			double y[];
			double k[];
			double kic[][];
			double mer;
			int cro;
			
			// initialisation of results array
			for (int i=0; i<=intervalNumber; i++) 
			{	 lic1[i]=0;
             lic2[i]=0;
         }


			// handle here the case for the simplified estimator
			// unfortunately this is not known for complex shape plots
			
			if (simulationNumber < 1)	
			{
				return 0;
			}
			
			// ----------------------------------------------------------------------------
			// Otherwise : estimation of  confidence interval through Monte Carlo --------------
			
			try 
         {
           	// Compute the minimum size of the array.
    			/// défining i0 : index where the estimate of CI will be put 
				// risk in %... if alpha 1%, risk=1.
              	double temp=((double) simulationNumber)*((double)risk)/((double)200.0);          // i0=0.005*nbiter;
               int i0= (int) temp;   	
               if (i0<1) {i0=1;}
					// we must redefine the number of simulations as a function of i0;
					simulationNumber=i0*200;
               if (imp>0) {	Log.println ("Monte Carlo, "+simulationNumber+" simulations, i0="+i0);}
            
				// initialisation
 		        	x = new double[pointNumber+1];
   		      y = new double[pointNumber+1];
     		    	k = new double[intervalNumber+1];
               kic = new double[intervalNumber+1][2*i0+2];
  			
	///////////////// main loop for MC
					for(int i=1;i<=simulationNumber;i++)
					{	if (imp==1)	{	Log.println("Iteration "+i);}
						if (imp==2)	{	Log.println("["+i+"]");	}

      			////// simulating CSR null hypothesis
                 	int erreur= RandomPattern.simulateXYTri(pointNumber,x,y,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,precision);
						if (erreur==0)
	               {	if (imp==1)	{	Log.println(" .");}
						}
						else
						{	if (imp>0)	{	Log.println(" [ERROR XY]");}
						}

			      ////// estimating K
			      /// only if no error !
			      /// we call the normalised routine [for intertri the number of points can vary]
						if (erreur==0)
						{	
							erreur=computeNormalisedKTri(k,x,y,pointNumber,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,intervalNumber, intervalWidth);
   						if (erreur==0)
	               	{	if (imp==1)	{	Log.println(" K");}
							}
							else
							{	if (imp>0)	{	Log.println(" [ERROR K]");}
							}
						}
 
       			////// if there is an error : a new simulation
         			if (erreur!=0)
         			{	i=i-1;
         				if (imp>0)	{Log.println( "ERROR");}
		         	}
  			      	else
         			{

      				////// handle the results
      					if (imp==1)	{Log.println(" T");}

         			// we put the 2i0+1 first values and sort them
           		
                  	if (i<=2*i0+1) 
   	      			{	if (imp==1)	{Log.println(" stock");}      // new values in i
      	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
         					{	kic[tt][i]=k[tt];
               			}

						// from second to (2i0+1)th value : we sort the new value directly 
            				if (i>1)
		            		{  
		           				// buble sorting towards lower values
									if (imp==1)	{	Log.println(" buble sorting directly");}
   	   	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
    		     					{ 
										if (kic[tt][i-1]>kic[tt][i])
       		        	  			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i],kic[tt][i-1]);
         	        					mer=kic[tt][i];
          	    						cro=i-1;
    	      	    					while ((cro>0)&&(kic[tt][cro]>mer))
      	     	   					{	kic[tt][cro+1]=kic[tt][cro];
         	   	           			//printf(" %d -> %d",cro,cro+1);
            	 	 						cro=cro-1;
              							}
           								kic[tt][cro+1]=mer;
  	            					}
   	           				}// end for tt buble sorting
								}// end if (i>1)
  		   		    	}// end if (i<=2*i0+1)
				
					// here, the (2io+1) first values are sorted...
				
						else  
					
					////// the array already contains (2i0+1) sorted values, we put the new value in i0+1

	    		     	{  if (imp==1)	{Log.println("(i0+1)");}
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{	kic[tt][i0+1]=k[tt];
 		           		}

   	         		// sorting the new value of k
    		        		if (imp==1)	{Log.println(" sorting(k) ");}
      	         	//printf("[%f ## %f ## %f] -> ",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{

 		          			// k goes down
   	         			if (kic[tt][i0+1]<kic[tt][i0])
    		        			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i0+1],kic[tt][i0]);
      	         			mer=kic[tt][i0+1];
       		        			cro=i0;
         	      			while ((cro>0)&&(kic[tt][cro]>mer))
          		     			{	kic[tt][cro+1]=kic[tt][cro];
            	   				cro=cro-1;
             		  			}
             					kic[tt][cro+1]=mer;
               				//printf("ok");
	            			}// end if k goes down

 								// k goes up
  		          			else if (kic[tt][i0+1]>kic[tt][i0+2])
    		        			{	//printf("\nk(%d)=%f> %f ",tt,kic[tt][i0+1],kic[tt][i0+2]);
      	         			mer=kic[tt][i0+1];
       		        			cro=i0+2;
         	      			while ((cro<2*i0+2)&&(kic[tt][cro]<mer))
          		     			{	kic[tt][cro-1]=kic[tt][cro];
            	    				cro=cro+1;
             		  			}
              					kic[tt][cro-1]=mer;
               				//printf("ok");
	            			}// end k goes up

 		              	}// end for tt sorting new value

   	            	//printf("[%f ## %f ## %f]",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);

 						}// end if iter sup 2i0+1, sorting
					
					} // end else from if (erreur!=0)
				
				} // end Monte Carlo loop 
     
		////////////////////////////////////// end main MC loop

      /// Handling results
  		    	int i1=i0;
    		  	int i2=i0+2;
      		for(int tt=1;tt<=intervalNumber;tt=tt+1)
	      	{	lic1[tt]=Math.sqrt(kic[tt][i1]/Math.PI)-tt*intervalWidth;
       		  	lic2[tt]=Math.sqrt(kic[tt][i2]/Math.PI)-tt*intervalWidth;
	      	}
	  			
			}
			catch (Exception exc) 
			{	Log.println (Log.ERROR, "Ripley.computeLICRect ()", "LIC(r) computation error");
            for (int i=0; i<=intervalNumber; i++) 
				{	lic1[i]=0;
               lic2[i]=0;
            }
				return -1;
         }
			return 0;
      }
   	
	
	
	
	   
   
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   // Secondary subroutines, called by main subroutines 
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   
   
   /** perimInRect
   *
   * This method computes the edge effect correcting factor for a rectangular plot (Ripley, 1978)
   * ie (perimeter of Cij inside the plot)/ddd, where Cij is the circle of radius ddd,
   * centered in i(xxx,yyy); and the plot is defined by xmi, xma, ymi, yma.		
   * It deals with cases when 1 edge, 2 edgess or 3 edges of the plot are concerned.
   */
       static public double perimInRect(double xxx, double yyy, double ddd, 
       	double xmi, double xma, double ymi, double yma) {
      
         double d1,d2,d3,d4;
      
         if ((xxx>=xmi+ddd) && (yyy>=ymi+ddd) && (xxx<=xma-ddd) && (yyy<=yma-ddd)) {
            return 2*Math.PI;      
         } 
         else {
            d1=(xxx-xmi)/ddd;
            d2=(yyy-ymi)/ddd;
            d3=(xma-xxx)/ddd;
            d4=(yma-yyy)/ddd;
            if (d1>=1) {	
               if (d2>=1) {	
                  if (d3>=1) {
                     if (d4>=1) { // circle in rectangle
                        return 2*Math.PI;
                     }
                     else {		/* 1 edge in d4 */
                        return (2*(Math.PI-Math.acos(d4)));
                     }
                  } 
                  else {	
                     if (d4>=1)	{	/* 1 edge in d3 */
                        return (2*(Math.PI-Math.acos(d3)));
                     } 
                     else {		/* 2 edges in d3 and d4 */
                        if (d3*d3+d4*d4<1) {
                           return (1.5*Math.PI-Math.acos(d3)-Math.acos(d4));
                        } 
                        else {
                           return (2*(Math.PI-Math.acos(d3)-Math.acos(d4)));
                        }
                     }
                  }
               } 
               else {	
                  if (d3>=1) {	
                     if (d4>=1) {	/* 1 edge in d2 */
                        return (2*(Math.PI-Math.acos(d2)));
                     } 
                     else {		/* 2 edges in d2 and d4 */
                        return (2*(Math.PI-Math.acos(d2)-Math.acos(d4)));
                     }
                  } 
                  else {	
                     if (d4>=1) {	/* 2 edges in d2 and d3 */
                        if (d2*d2+d3*d3<1) {
                           return ((1.5*Math.PI-Math.acos(d2)-Math.acos(d3)));
                        } 
                        else {
                           return (2*(Math.PI-Math.acos(d2)-Math.acos(d3)));
                        }
                     } 
                     else {		/* 3 edges in d2,d3,d4 */
                        if (d2*d2+d3*d3<1) {	
                           if (d3*d3+d4*d4<1) {
                              return ((Math.PI-Math.acos(d2)-Math.acos(d4)));
                           } 
                           else {
                              return ((1.5*Math.PI-Math.acos(d2)-Math.acos(d3)-2*Math.acos(d4)));
                           }
                        } 
                        else {	
                           if (d3*d3+d4*d4<1) {
                              return ((1.5*Math.PI-2*Math.acos(d2)-Math.acos(d3)-Math.acos(d4)));
                           } 
                           else {
                              return (2*(Math.PI-Math.acos(d2)-Math.acos(d3)-Math.acos(d4)));
                           }
                        }
                     }
                  }
               }
            } 
            else {	
               if (d2>=1) {	
                  if (d3>=1) {	
                     if (d4>=1)	{		/* 1 edge in d1 */
                        return (2*(Math.PI-Math.acos(d1)));
                     } 
                     else {			/* 2 edges in d1 and d4 */
                        if (d1*d1+d4*d4<1) {
                           return ((1.5*Math.PI-Math.acos(d1)-Math.acos(d4)));
                        } 
                        else {
                           return (2*(Math.PI-Math.acos(d1)-Math.acos(d4)));
                        }
                     }
                  } 
                  else {	
                     if (d4>=1) {		/* 2 edges in d1 and d3 */
                        return (2*(Math.PI-Math.acos(d1)-Math.acos(d3)));
                     } 
                     else {			/* 3 edges in d1,d3,d4 */
                        if (d3*d3+d4*d4<1) {	
                           if (d4*d4+d1*d1<1) {
                              return ((Math.PI-Math.acos(d3)-Math.acos(d1)));
                           } 
                           else {
                              return ((1.5*Math.PI-Math.acos(d3)-Math.acos(d4)-2*Math.acos(d1)));
                           }
                        } 
                        else {	
                           if (d4*d4+d1*d1<1) {
                              return ((1.5*Math.PI-2*Math.acos(d3)-Math.acos(d4)-Math.acos(d1)));
                           } 
                           else {
                              return (2*(Math.PI-Math.acos(d3)-Math.acos(d4)-Math.acos(d1)));
                           }
                        }
                     }
                  }
               } 
               else {	
                  if (d3>=1) {	
                     if (d4>=1)	{			/* 2 edges in d1 and d2 */
                        if (d1*d1+d2*d2<1) {
                           return ((1.5*Math.PI-Math.acos(d1)-Math.acos(d2)));
                        } 
                        else {
                           return (2*(Math.PI-Math.acos(d1)-Math.acos(d2)));
                        }
                     } 
                     else {				/* 3 edges d1,d2,d4 */
                        if (d4*d4+d1*d1<1) {	
                           if (d1*d1+d2*d2<1) {
                              return ((Math.PI-Math.acos(d4)-Math.acos(d2)));
                           } 
                           else {
                              return ((1.5*Math.PI-Math.acos(d4)-Math.acos(d1)-2*Math.acos(d2)));
                           }
                        } 
                        else {	
                           if (d1*d1+d2*d2<1) {
                              return ((1.5*Math.PI-2*Math.acos(d4)-Math.acos(d1)-Math.acos(d2)));
                           } 
                           else {
                              return (2*(Math.PI-Math.acos(d4)-Math.acos(d1)-Math.acos(d2)));
                           }
                        }
                     }
                  } 
                  else {	
                     if (d4>=1) {			/* 3 edges d1,d2,d3 */
                        if (d1*d1+d2*d2<1) {	
                           if (d2*d2+d3*d3<1) {
                              return ((Math.PI-Math.acos(d1)-Math.acos(d3)));
                           } 
                           else {
                              return ((1.5*Math.PI-Math.acos(d1)-Math.acos(d2)-2*Math.acos(d3)));
                           }
                        } 
                        else {	
                           if (d2*d2+d3*d3<1) {
                              return ((1.5*Math.PI-2*Math.acos(d1)-Math.acos(d2)-Math.acos(d3)));
                           } 
                           else {
                              return (2*(Math.PI-Math.acos(d1)-Math.acos(d2)-Math.acos(d3)));
                           }
                        }
                     } 
                     else {				/* 4 edges : not allowed here */
                     	Log.println (Log.ERROR, "Ripley.perimInRect ()", "Error in perimInRect");	// fc - 10.10.2001
                        //System.out.println ("error in perimInRect (4 edges)");
                        return 0;
                     }
                  }
               }
            }
         }
      }
   
   
   /** PerimInTri   
   *              
   * This method is used for the edge effect correcting factor for a plot of complex shape.
   * It computes the sum of the angles corresponding to portions of the circle
   * (centered in (x,y) and radius d) which is inside the triangles.                         
   * For each triangle we consider the different possible cases.             
   ******************************************************************************/
   
       static public double perimInTri(double x,double y, double d, int triangle_nb, double ax[], double ay[], double bx[], double by[], double cx[], double cy[])
      {	double angle;
         double doa,dob,doc;
         int h,i;
      
         angle=0;
         for(h=1;h<=triangle_nb;h++)
         {	doa=Math.sqrt((x-ax[h])*(x-ax[h])+(y-ay[h])*(y-ay[h]));
            dob=Math.sqrt((x-bx[h])*(x-bx[h])+(y-by[h])*(y-by[h]));
            doc=Math.sqrt((x-cx[h])*(x-cx[h])+(y-cy[h])*(y-cy[h]));
            if (doa-d<-epsilon)
            {	
               if (dob-d<-epsilon)
               {	
                  if (doc-d<-epsilon)
                     i=1;		 /* the triangle is in the circle, OK */	
                  else if (doc-d>epsilon)
                     angle+=un_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
                  else
                     i=1;		 /* the triangle is in the circle, OK */	
               }
               else if (dob-d>epsilon)
               {	
                  if (doc-d<-epsilon)
                     angle+=un_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
                  else if (doc-d>epsilon)
                     angle+=deux_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
                  else
                     angle+=ununun_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
               }
               else /* b on the edge */
               {	
                  if (doc-d<-epsilon)
                     i=1;		 /* the triangle is in the circle, OK */	
                  else if (doc-d>epsilon)
                     angle+=ununun_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
                  else
                     i=1;		 /* the triangle is in the circle, OK */	
               }
            }
            else if (doa-d>epsilon)
            {	
               if (dob-d<-epsilon)
               {	
                  if (doc-d<-epsilon)
                     angle+=un_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
                  else if (doc-d>epsilon)
                     angle+=deux_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
                  else
                     angle+=ununun_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
               }
               else if (dob-d>epsilon)
               {	
                  if (doc-d<-epsilon)
                     angle+=deux_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
                  else if (doc-d>epsilon)
                     angle+=trois_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
                  else
                     angle+=deuxun_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
               }
               else /* b on the edge */
               {	
                  if (doc-d<-epsilon)
                     angle+=ununun_point(ax[h],ay[h],cx[h],cy[h],bx[h],by[h],x,y,d);
                  else if (doc-d>epsilon)
                     angle+=deuxun_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
                  else
                     angle+=deuxbord_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
               }
            }
            else /* a on the edge */
            {	
               if (dob-d<-epsilon)
               {	
                  if (doc-d<-epsilon)
                     i=1;		 /* the triangle is in the circle, OK */	
                  else if (doc-d>epsilon)
                     angle+=ununun_point(cx[h],cy[h],bx[h],by[h],ax[h],ay[h],x,y,d);
                  else
                     i=1;		 /* the triangle is in the circle, OK */	
               }
               else if (dob-d>epsilon)
               {	
                  if (doc-d<-epsilon)
                     angle+=ununun_point(bx[h],by[h],cx[h],cy[h],ax[h],ay[h],x,y,d);
                  else if (doc-d>epsilon)
                     angle+=deuxun_point(ax[h],ay[h],bx[h],by[h],cx[h],cy[h],x,y,d);
                  else
                     angle+=deuxbord_point(bx[h],by[h],ax[h],ay[h],cx[h],cy[h],x,y,d);
               }
               else /* b on the edge */
               {	
                  if (doc-d<-epsilon)
                     i=1;		 /* the triangle is in the circle, OK */
                  else if (doc-d>epsilon)
                     angle+=deuxbord_point(cx[h],cy[h],ax[h],ay[h],bx[h],by[h],x,y,d);
                  else
                     i=1;		 /* the triangle is in the circle, OK */	
               }
            }
         }	
         return angle;		
      }
   
   
   
      
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   // Third order subroutines, called by secondary subroutines 
   // ep : geometrical computations
   // -------------------------------------------------------------   
   // -------------------------------------------------------------   
   
   /** in_droite()
   *
   * This method return 1 if point x,y is on the same side of line (ab) than c.
   */
   
       public static int in_droite(double x, double y, double ax, double ay, double bx, double by, double cx, double cy)
      {	double vabx,vaby,vacx,vacy,vamx,vamy,pv1,pv2;
      
         vabx=bx-ax;
         vaby=by-ay;
         vacx=cx-ax;
         vacy=cy-ay;
         vamx=x-ax;
         vamy=y-ay;
         pv1=vabx*vacy-vaby*vacx;
         pv2=vabx*vamy-vaby*vamx;
      
         if (((pv1>0)&&(pv2>=0))||((pv1<0)&&(pv2<=0)))
            return 1;
         else
            return 0;
      }
   
   
   /** in_triangle()
   *
   * This method returns 1 if (x,y) is inside triangle abc or on its edges.
   */
   
       public static int in_triangle(double x, double y, double ax, double ay, double bx, double by, double cx, double cy)
      {	int res;
      
         res=0;
         if (in_droite(x, y, ax, ay, bx, by, cx, cy)==1)
            if (in_droite(x, y, bx, by, cx, cy, ax, ay)==1)
               if (in_droite(x, y, cx, cy, ax, ay, bx, by)==1)
                  res=1;
      
         return res;
      }
   	
   /** surfaceTri()
   *
   * This method computes the total area of a study zone defined by an initial rectangle
   * (xmi, xma, ymi, yma) and excluded triangles (triangleNumber, ax, ay, bx, by, cx, cy).
   **********/
   
       public static double surfaceTri(double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[])
      {	
      	// intermediate variables :
         double surface;
      
      	// computation
         surface=(xma-xmi)*(yma-ymi);
         for (int i=1;i<=triangleNumber;i++)
         {	surface=surface-0.5*Math.abs((bx[i]-ax[i])*(cy[i]-ay[i])-(by[i]-ay[i])*(cx[i]-ax[i]));
         }
         return surface;
      }
   	
      
   /******************************************************************************/
   /* Subroutines for Perim_in_triangle               */
   /******************************************************************************/
   
   //----------------------------------------------------------------------------
   /* a exterieur ; b et c interieur */
   
       static public double un_point(	double ax, double ay, double bx, double by, double cx, double cy, 
       double x, double y, double d)
      {	double alpha, beta, gamma, delta, ttt, ang;
         double ex,ey,fx,fy;
      
      /* premier point d'intersection */
      
         alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
         beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
         gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         if (delta<=0)
            Log.println("erreur1");
         ttt=(-beta-Math.sqrt(delta))/(2*alpha);
         if ((ttt<=0)||(ttt>=1))
            Log.println("erreur2");
         ex=ax+ttt*(bx-ax);
         ey=ay+ttt*(by-ay);
      
      /* deuxieme point d'intersection */
      
         alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
         beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
         delta=beta*beta-4*alpha*gamma;
         if (delta<=0)
            Log.println("erreur3");
         ttt=(-beta-Math.sqrt(delta))/(2*alpha);
         if ((ttt<=0)||(ttt>=1))
            Log.println("erreur4");
         fx=ax+ttt*(cx-ax);
         fy=ay+ttt*(cy-ay);
      
      /* calcul de l'angle */
      
         ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
         return ang;
      }
   
   
   //----------------------------------------------------------------------------
   /* a exterieur, b interieur, c sur le bord */
   
       static public double ununun_point(double ax, double ay, double bx, double by, double cx, double cy, 
       double x, double y, double d)
      {	double alpha, beta, gamma, delta, ttt, ang;
         double ex,ey,fx,fy;
      
      /* premier point d'intersection sur ab*/
      
         alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
         beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
         gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         if (delta<=0)
            Log.println("erreur1b");
         ttt=(-beta-Math.sqrt(delta))/(2*alpha);
         if ((ttt<=0)||(ttt>=1))
            Log.println("erreur2b");
         ex=ax+ttt*(bx-ax);
         ey=ay+ttt*(by-ay);
      
      /* deuxieme point d'intersection ac*/
      
         alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
         beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
         delta=beta*beta-4*alpha*gamma;
         ttt=1;
         if (delta>0)
         {	ttt=(-beta-Math.sqrt(delta))/(2*alpha);
            if ((ttt<=0)||(ttt>1))
               ttt=1;
            if (ttt<=0)
               Log.println("e3b");
         }
         fx=ax+ttt*(cx-ax);
         fy=ay+ttt*(cy-ay);
      
      /* calcul de l'angle */
      
         ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
         return ang;
      }
   
   
   //----------------------------------------------------------------------------
   /* a exterieur, b et c sur le bord */
   
       static public double deuxbord_point(double ax, double ay, double bx, double by, double cx, double cy,
       	double x, double y, double d)
      {	double alpha, beta, gamma, delta, te,tf, ang;
         double ex,ey,fx,fy;
      
      /* premier point d'intersection sur ab*/
      
         alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
         beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
         gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         te=1;
         if (delta>0)
         {	te=(-beta-Math.sqrt(delta))/(2*alpha);
            if ((te<=0)||(te>=1))
               te=1;
            if (te<=0)
               Log.println("e1t ");
         }
         ex=ax+te*(bx-ax);
         ey=ay+te*(by-ay);
      
      /* deuxieme point d'intersection ac*/
      
         alpha=(cx-ax)*(cx-ax)+(cy-ay)*(cy-ay);
         beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
         delta=beta*beta-4*alpha*gamma;
         tf=1;
         if (delta>0)
         {	tf=(-beta-Math.sqrt(delta))/(2*alpha);
            if ((tf<=0)||(tf>=1))
               tf=1;
            if (tf<=0)
               Log.println("e4t ");
         }
         fx=ax+tf*(cx-ax);
         fy=ay+tf*(cy-ay);
      
      /* calcul de l'angle */
      
         ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
         return ang;
      }
   
   
   //----------------------------------------------------------------------------
   /* a interieur , b et c exterieur */
   
       static public double deux_point(double ax, double ay, double bx, double by, double cx, double cy,
       double x, double y, double d)
      {	double alpha, beta, gamma, delta, ttt, ang;
         double ex=0;
         double ey=0;
         double fx=0;
         double fy=0;
         double gx=0;
         double gy=0;
         double hx=0;
         double hy=0;
         int cas;
      
      /* premier point d'intersection */
      
         alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
         beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
         gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         if (delta<=0)
            Log.println("erreur6");
         ttt=(-beta+Math.sqrt(delta))/(2*alpha);
         if ((ttt<=0)||(ttt>=1))
            Log.println("erreur7");
         ex=ax+ttt*(bx-ax);
         ey=ay+ttt*(by-ay);
      
      /* deuxieme point d'intersection */
      
         alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
         beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
         delta=beta*beta-4*alpha*gamma;
         if (delta<=0)
            Log.println("erreur8");
         ttt=(-beta+Math.sqrt(delta))/(2*alpha);
         if ((ttt<=0)||(ttt>=1))
            Log.println("erreur9");
         fx=ax+ttt*(cx-ax);
         fy=ay+ttt*(cy-ay);
      
      /* y a t il deux autres intersections? */
      
         cas=0;
         alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
         beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
         gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         if (delta>0)
         {	ttt=(-beta-Math.sqrt(delta))/(2*alpha);
            if ((ttt>=0)&&(ttt<=1))
            {	gx=bx+ttt*(cx-bx);
               gy=by+ttt*(cy-by);
               ttt=(-beta+Math.sqrt(delta))/(2*alpha);
               if ((ttt>=0)&&(ttt<=1))
               {	cas=1;
                  hx=bx+ttt*(cx-bx);
                  hy=by+ttt*(cy-by);
               }
               else
                  Log.println("erreur9bis");
            }
         }
      
      /* calcul de l'angle */
      
         if (cas==0)
         {	ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
         }
         else
         {	ang=Math.acos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
            ang+=Math.acos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));		
         }
      
         return ang;
      }
   
   
   //----------------------------------------------------------------------------
   /* a est le point sur le bord , b et c exterieur */
   
       static public double deuxun_point(double ax, double ay, double bx, double by, double cx, double cy,
       double x, double y, double d)
      {	double alpha, beta, gamma, delta, te,tf,tg,th, ang;
         double ex=0;
         double ey=0;
         double fx=0;
         double fy=0;
         double gx=0;
         double gy=0;
         double hx=0;
         double hy=0;
         int cas;
      
      /* premier point d'intersection */
      
         alpha=((bx-ax)*(bx-ax)+(by-ay)*(by-ay));
         beta=(2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay));
         gamma=((ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         te=0;
         if (delta>0)
         {	te=(-beta+Math.sqrt(delta))/(2*alpha);
            if ((te<0)||(te>=1))
               te=0;
            if (te>=1)
               Log.println("e15 ");
         }
         ex=ax+te*(bx-ax);
         ey=ay+te*(by-ay);
      
      /* deuxieme point d'intersection */
      
         alpha=((cx-ax)*(cx-ax)+(cy-ay)*(cy-ay));
         beta=(2*(ax-x)*(cx-ax)+2*(ay-y)*(cy-ay));
         delta=beta*beta-4*alpha*gamma;
         tf=0;
         if (delta>0)
         {	tf=(-beta+Math.sqrt(delta))/(2*alpha);
            if ((tf<0)||(tf>=1))
               tf=0;
            if (tf>=1)
               Log.println("e15 ");
         }
         fx=ax+tf*(cx-ax);
         fy=ay+tf*(cy-ay);
      
      /* y a t il deux autres intersections? */
      
         cas=0;
         alpha=((cx-bx)*(cx-bx)+(cy-by)*(cy-by));
         beta=(2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by));
         gamma=((bx-x)*(bx-x)+(by-y)*(by-y)-d*d);
         delta=beta*beta-4*alpha*gamma;
         if (delta>0)
         {	tg=(-beta-Math.sqrt(delta))/(2*alpha);
            if ((tg>=0)&&(tg<=1))
            {	gx=bx+tg*(cx-bx);
               gy=by+tg*(cy-by);
               th=(-beta+Math.sqrt(delta))/(2*alpha);
               if ((th>=0)&&(th<=1))
               {	cas=1;
                  hx=bx+th*(cx-bx);
                  hy=by+th*(cy-by);
               }
               else
                  Log.println("erreur9ter");
            }
         }
      
      /* calcul de l'angle */
      
         if (cas==0)
         {	
            if ((te==0)&&(tf==0))
               ang=0;
            else
               ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
         }
         else
         {	ang=Math.acos(((ex-x)*(gx-x)+(ey-y)*(gy-y))/(d*d));
            ang+=Math.acos(((fx-x)*(hx-x)+(fy-y)*(hy-y))/(d*d));		
         }
      
         return ang;
      }
   
   
   //----------------------------------------------------------------------------
   /* a,b et c exterieurs */
   
       static public double trois_point(double ax, double ay, double bx, double by, double cx, double cy, double x, double y, double d)
      {	
         double alpha, beta, gamma, delta, te,tf,tg,th,ti,tj, ang;
         double ex=0;
         double ey=0;
         double fx=0;
         double fy=0;
         double gx=0;
         double gy=0;
         double hx=0;
         double hy=0;
         double ix=0;
         double iy=0;
         double jx=0;
         double jy=0;
       
      /* premier segment ab */
      
         alpha=(bx-ax)*(bx-ax)+(by-ay)*(by-ay);
         beta=2*(ax-x)*(bx-ax)+2*(ay-y)*(by-ay);
         gamma=(ax-x)*(ax-x)+(ay-y)*(ay-y)-d*d;
         delta=beta*beta-4*alpha*gamma;
         if (delta<0)
         {	te=-1;
            tf=-1;
         }
         else
         {	te=(-beta-Math.sqrt(delta))/(2*alpha);
            tf=(-beta+Math.sqrt(delta))/(2*alpha);
            if ((te<0)||(te>=1)||(tf==0))
            {	te=-1;
               tf=-1;
            }
            else
            {	ex=ax+te*(bx-ax);
               ey=ay+te*(by-ay);
               fx=ax+tf*(bx-ax);
               fy=ay+tf*(by-ay);
               if ((tf<=0)||(tf>1))
                  Log.println("\npb te "+te+" tf "+tf);
            }
         }
      
      /* deuxieme segment bc */
      
         alpha=(cx-bx)*(cx-bx)+(cy-by)*(cy-by);
         beta=2*(bx-x)*(cx-bx)+2*(by-y)*(cy-by);
         gamma=(bx-x)*(bx-x)+(by-y)*(by-y)-d*d;
         delta=beta*beta-4*alpha*gamma;
         if (delta<0)
         {	tg=-1;
            th=-1;
         }
         else
         {	tg=(-beta-Math.sqrt(delta))/(2*alpha);
            th=(-beta+Math.sqrt(delta))/(2*alpha);
            if ((tg<0)||(tg>=1)||(th==0))
            {	tg=-1;
               th=-1;
            }
            else
            {	gx=bx+tg*(cx-bx);
               gy=by+tg*(cy-by);
               hx=bx+th*(cx-bx);
               hy=by+th*(cy-by);
               if ((th<=0)||(th>1))
                  Log.println("\npb tg "+tg+" th "+th);       
            }
         }
      
      /* troisieme segment ca */
      
         alpha=(ax-cx)*(ax-cx)+(ay-cy)*(ay-cy);
         beta=2*(cx-x)*(ax-cx)+2*(cy-y)*(ay-cy);
         gamma=(cx-x)*(cx-x)+(cy-y)*(cy-y)-d*d;
         delta=beta*beta-4*alpha*gamma;
         if (delta<0)
         {	ti=-1;
            tj=-1;
         }
         else
         {	ti=(-beta-Math.sqrt(delta))/(2*alpha);
            tj=(-beta+Math.sqrt(delta))/(2*alpha);
            if ((ti<0)||(ti>=1)||(tj==0))
            {	ti=-1;
               tj=-1;
            }
            else
            {	ix=cx+ti*(ax-cx);
               iy=cy+ti*(ay-cy);
               jx=cx+tj*(ax-cx);
               jy=cy+tj*(ay-cy);
               if ((tj<=0)||(tj>1))
                  Log.println("\npb ti "+ti+" tj "+tj);         
            }
         }
      
      /* quelle configuration ? */
      
         if (te<0)
         {	
            if (tg<0)
            {	
               if (ti<0)
               {	/* pas d'intersection... ouf!*/
                  ang=0;
               }
               else
               {	/* un seul cote (ca) coupe le cercle en i,j*/
                  ang=Math.acos(((ix-x)*(jx-x)+(iy-y)*(jy-y))/(d*d));
               }
            }
            else
            {	
               if (ti<0)
               {	/* un seul cote (bc) coupe le cercle en g,h*/
                  ang=Math.acos(((gx-x)*(hx-x)+(gy-y)*(hy-y))/(d*d));
               }
               else
               {	/* deux cotes (bc et ca) coupent le cercle en g,h,i,j */
                  ang=Math.acos(((gx-x)*(jx-x)+(gy-y)*(jy-y))/(d*d));
                  ang+=Math.acos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
               }
            }
         }
         else
         {	
            if (tg<0)
            {	
               if (ti<0)
               {	/* un seul cote (ab) coupe le cercle en e,f*/
                  ang=Math.acos(((ex-x)*(fx-x)+(ey-y)*(fy-y))/(d*d));
               }
               else
               {	/* deux cotes (ab et ca) coupent le cercle en e,f,i,j */
                  ang=Math.acos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
                  ang+=Math.acos(((fx-x)*(ix-x)+(fy-y)*(iy-y))/(d*d));
               }
            }
            else
            {	
               if (ti<0)
               {	/* deux cotes (ab et bc) coupent le cercle en e,f,g,h */
                  ang=Math.acos(((ex-x)*(hx-x)+(ey-y)*(hy-y))/(d*d));
                  ang+=Math.acos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
               }
               else
               {	/* les trois cotes coupent le cercle */
                  ang=Math.acos(((ex-x)*(jx-x)+(ey-y)*(jy-y))/(d*d));
                  ang+=Math.acos(((hx-x)*(ix-x)+(hy-y)*(iy-y))/(d*d));
                  ang+=Math.acos(((fx-x)*(gx-x)+(fy-y)*(gy-y))/(d*d));
               }
            }
         }	
         if ((ang<0)||(ang>Math.PI))
            Log.println("\nerreur12 : ang="+ang+" "+te+" "+tf+" "+tg+" "+th+" "+ti+" "+tj);
      
         return ang;
      }  
   }
