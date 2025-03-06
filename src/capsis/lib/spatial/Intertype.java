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
import java.util.Random;

import jeeb.lib.util.Log;

/**
 * Intertype K12(r) and L12(r) functions (analysis of the spatial structure).
 * corresponding to the last V7 version of C++ codes
 * corresponding to the last V7 version of C++ codes
 * taking into account edge effect corrections
 * for rectangular and complex shape plots
 * 
 * @author Francois Goreaud 29/3/2001 - 24/5/2006
 */
    public class Intertype {
   //checked for c4.1.1_09 - fc - 5.2.2003
   
      static {
      //~ System.out.println ("Intertype *** loaded");
      }
   
	
	// ------------------------------------------------------------------
	// Here are various methods
	// to estimate Intertype L12(r) function between two point patterns  
	// in a rectangular or complex shape plot
	// with its confidence interval
	// for two different null hypothesis :
	// 1 : population independance (two different species)
	// 2 : random labelling (death, illness)
	// These methods call specific edge effect correction tools 
	// that are implemented in Ripley.java class
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
	
	
	
   /** computeL12Rect
   *
   * This method computes the intertype L12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
	* in a rectangular plot (xmi, xma, ymi, yma)
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * This routine directly calls computeNormalisedK12Rect, which estimates K12(r)
	*
	* Results are put in the array (tabl12) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   ******************************/
   
       static public int computeL12Rect (double tabl12[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabk12[] = new double[intervalNumber+1];
         int erreur;
			
			try 
         {	//System.out.println("Début computeL12Rect n1="+pointNumber1+" n2="+pointNumber2);
         
         	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {	tabk12[i]=0;
               tabl12[i]=0;
            }
    	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {	intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // K12 computation.
				erreur=computeNormalisedK12Rect(tabk12,xx1,yy1,pointNumber1,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,intervalNumber,intervalWidth);

        	// Computing L12
				if (erreur==0)		// only if we have computed K12 !
				{	for (int i = 1; i <= intervalNumber; i++) 
					{	//tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               	tabl12[i]=Math.sqrt(tabk12[i]/Math.PI)-i*intervalWidth;
     		      }
				}
				else
				{	Log.println (Log.ERROR, "Ripley.computeL12Rect ()", "L12(r) computation error");
            	for (int i=0; i<tabl12.length; i++) 
            	{	tabl12[i]=0;
            	}
            	return -1;
				}     
         } 
         catch (Exception exc) 
         {
            Log.println (Log.ERROR, "Ripley.computeL12Rect ()", "L12(r) computation error");
            for (int i=0; i<tabl12.length; i++) 
            {	tabl12[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
   
   /** computeNormalisedK12Rect
   *
   * This method computes the intertype K12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
	* in a rectangular plot (xmi, xma, ymi, yma)
	* 
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * The subroutine uses g12 (intertype pair density function, but here not normalised);
   * as intermediate variables to compute K12.
	* Initially an unnormalised version of this routine was used only for L12IC, because it avoided to compute L12
	* Now it is also called by computeL12Rect.
	*
	* Results are put in the array (tabk12) given in parameter.
	*
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   ******************************/
   
       static public int computeNormalisedK12Rect (double tabk12[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabg12[] = new double[intervalNumber+1];
         double cin;
			double surface=0;
			double densite1=0;
			double densite2=0;
      
         try 
         {	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {	tabg12[i]=0;
               tabk12[i]=0;
            }
            surface=(xma-xmi)*(yma-ymi);
            densite1=((double) pointNumber1)/surface;
				//System.out.println("Densité 1="+densite1);
            densite2=((double) pointNumber2)/surface;
				//System.out.println("Densité 2="+densite2);
    	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {	intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // g12 computation.
         // Considering all couples (i,j) i species 1, j species 2 
            for (int i=1; i<=pointNumber1; i++) 
            {  double x1=xx1[i];
               double y1=yy1[i];
               for (int j=1; j<=pointNumber2; j++) 
               {	double x2=xx2[j];		// x : m
                  double y2=yy2[j];		// y : m
               	// distance between these 2 points ?
                  double d = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
               	// which range class does it belong ? (max value excluded)
                  int interval = (int) (d / intervalWidth)+1;
               	// if it is short enought, we count it
                  if (interval <= intervalNumber) 
                  {	
                  	// edge effect correction
                     cin=Ripley.perimInRect(x1,y1,d,xmi,xma,ymi,yma);
                     if (cin<0)
                     {	Log.println("\ncin<0 sur i AVANT");
            				for (int ii=0; i<tabk12.length; i++) 
            				{	tabk12[ii]=0;
            				}
	                     return -1;
                     }
                     tabg12 [interval] +=2*Math.PI/cin;						
                  }
               }
            }	// end of the double loop on the couples of points
                  
         // Normalisation & co :
	         // averaging -> density
            for (int i = 1; i <= intervalNumber; i++) {
               tabg12[i]=tabg12[i]/((double) pointNumber1);
            }
         	// integrating g12 to obtain k12
            tabk12[1]=tabg12[1];
            for (int i = 2; i <= intervalNumber; i++) {
               tabk12[i]=tabk12[i-1]+tabg12[i];
            }
         	// normalisation of k12, 
            for (int i = 1; i <= intervalNumber; i++) {
            	//tabg[i]=???;
               tabk12[i]=tabk12[i]/densite2;
            }
   ///// WARNING : we have only normalised K12,
   ///// g12 is here only an intermediate variable
   ///// if we want to estimate g12(t) then we still have to normalise it 
     
         } 
         catch (Exception exc) 
         {
            Log.println (Log.ERROR, "Ripley.computeK12Rect ()", "K12(r) computation error");
            for (int i=0; i<tabk12.length; i++) 
            {	tabk12[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   	
		
	/** computeL12ICRect
   *
   * This method computes the confidence interval for the intertype L12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
	* in a rectangular plot (xmi, xma, ymi, yma)
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * For each range r, the confidence interval is estimated, 
	* for a given risk (risk) , (risk=1 for alpha of 1%)
   * by order statistics on (simulationNumber) Monte Carlo simulations 
	* corresponding to the chosen null hypothesis (icNullHypothesis) :
	*	1 : population independance
	*	2 : random labelling
	*
	* Results are put in the arrays (l12ic1) and (l12ic2) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
	*
	* imp is the choice of what is printed on screen : 0 : nothing, 1 : everything, 2 : minimum
	*
   ******************************/
       static public int computeL12ICRect (int imp, double l12ic1[], double l12ic2[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int intervalNumber, double intervalWidth, 
       	int icNullHypothesis, int simulationNumber, double risk, double precision) 

		{	
		 	// Rq : the code is optimised to compute L12(r) only

			// intermediate variables
			double k12[];
			double k12ic[][];
			double mer;
			int cro;
			int erreur=0;
        	Random R=new Random();
			
			// for IC population independance (shifting)
			double dx;
			double dy;
			double dx2;
			double dy2;
			double transmax=0;
			double transmoy=0;
			double trans;
			
			// for IC random labelling (substitution)
			double x0[];
			double y0[];
			int pointNumber0=0;
			int type[];
						
			// initialisation of results array
			for (int i=0; i<=intervalNumber; i++) 
			{	 l12ic1[i]=0;
             l12ic2[i]=0;
         }


			// handle here the case for the simplified estimator
			// unfortunately this is not known for intertype
			
			if (simulationNumber < 1)	// no estimation of Confidance Interval	
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
               if (imp>0)	{Log.println("Monte Carlo, "+simulationNumber+" simulations, i0="+i0);}
            
				// initialisation
 		        	x0 = new double[pointNumber1+pointNumber2+1];
   		      y0 = new double[pointNumber1+pointNumber2+1];
     		    	k12 = new double[intervalNumber+1];
               k12ic = new double[intervalNumber+1][2*i0+2];
  			
	/////////////// initialisation before main loop

		// option 1 : population independance : nothing to prepare
					if (icNullHypothesis==1)
      			{	
 						if (imp==1)	{Log.println("\nshifting");}
         			transmax=0;
      				transmoy=0;
      				trans=0;
	     			}
   				else
      			{

		// option 2 : substitutions : stocking all points 
						if (imp==1)	{Log.println("\nsubstitution");}
      				for(int i=1;i<=pointNumber1;i++)
         			{	x0[i]=xx1[i];
         				y0[i]=yy1[i];
						}
						for(int i=1;i<=pointNumber2;i++)
         			{	x0[pointNumber1+i]=xx2[i];
							y0[pointNumber1+i]=yy2[i];
						}
					}
					
					
	///////////////// main loop for MC
					for(int i=1;i<=simulationNumber;i++)
					{	if (imp==1)	{	Log.println("\nIteration "+i);}
						if (imp==2)	{	Log.println("["+i+"]");	}

      			////// simulating null hypothesis
						if (imp==1)	{Log.println(" S");}


      /// option 1 : population independance (shifting)
         			if (icNullHypothesis==1)
         			{  // shifting type 1, type 2 stays unchanged
 							if (imp==1)	{Log.println(" PI");}
							pointNumber0=pointNumber1;
		 				// Warning,
						// R.nextInt(N) returns an int between 0 et N-1 (included)
 
							// first x 
                  	dx=R.nextInt((int)((xma-xmi)/precision+1))*precision;
            			for(int j=1;j<=pointNumber0;j++)
            			{  x0[j]=xx1[j]+dx;
               			if (x0[j]>xma)
               			{	x0[j]=x0[j]-(xma-xmi);
               			}
            			}

							// then y 
     	            	dy=R.nextInt((int)((yma-ymi)/precision+1))*precision;
			            for(int j=1;j<=pointNumber0;j++)
            			{	y0[j]=yy1[j]+dy;
               			if (y0[j]>yma)
               			{	y0[j]=y0[j]-(yma-ymi);
               			}
            			}

     							// computing the length of the translation
               			if (dx<=(xma-xmi)/2)
               			{	dx2=dx;
               			}
               			else
               			{  dx2=dx-(xma-xmi);
               			}
               			if (dy<=(yma-ymi)/2)
               			{	dy2=dy;
               			}
               			else
               			{  dy2=dy-(yma-ymi);
               			}
               			trans=Math.sqrt(dx2*dx2+dy2*dy2);
               			//printf("[%f]",trans);
               			transmoy+=trans;
               			if (transmax<trans)
            				{  //printf("[%f<]R",transmax);
               				transmax=trans;
            				}
               			else
            				{  //printf("[%f>=]",transmax);
             				}
          			}
						
						else
		/// option 2 : substitutions : random labelling of points
   			     {	
					  		type = new int[pointNumber1+pointNumber2+1];
					  		for(int j=1;j<=pointNumber1+pointNumber2;j++)
         				type[j]=2;

		 				// Warning,
						// R.nextInt(N) returns an int between 0 et N-1 (included)
						
            			// random drawing of pointNumber1 type 1 points -> in xx1
         				int j=1;
            			while (j<=pointNumber1)
            			{	int jj=1+R.nextInt(pointNumber1+pointNumber2);
            				while (type[jj]!=2)
               			{  jj=1+R.nextInt(pointNumber1+pointNumber2);
               			}
               			type[jj]=1;
               			xx1[j]=x0[jj];
               			yy1[j]=y0[jj];
               			j=j+1;
            			}
							
            			// verifying there are still pointNumber2 type 2 points -> in xx2
            			int jj=0;
            			for(j=1;j<=pointNumber1+pointNumber2;j++)
            			{	if (type[j]==2)
              				{	jj=jj+1;
                  			xx2[jj]=x0[j];
         						yy2[jj]=y0[j];
               			}
            			}
            			if (jj!=pointNumber2)
            			{	if (imp>0)	{Log.println("ERREUR substitution");}
               			erreur=1;
        					}
            			else
            			{	erreur=0;
            			}
         			}

			      ////// estimating K12
			      /// only if no error !
			      /// we call the normalised routine [for intertri the number of points can vary]

     					if (erreur==0)					// computing NormalisedK12
      				{
							if (icNullHypothesis==1)			// HO population independance
         				{  
        				 		erreur=computeNormalisedK12Rect(k12,x0,y0,pointNumber0,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,intervalNumber,intervalWidth);
      			   	}
      				 	else										// H0 random labelling
							{  
        				 		erreur=computeNormalisedK12Rect(k12,xx1,yy1,pointNumber1,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,intervalNumber,intervalWidth);
        				 	}
     				 	}
 
       			////// if there is an error : a new simulation
         			if (erreur!=0)
         			{	i=i-1;
         				if (imp>0)	{Log.println( "ERREUR Intertype\n");}
		         	}
  			      	else
         			{

      				////// handle the results
      					if (imp==1)	{Log.println(" T");}

         			// we put the 2i0+1 first values and sort them
           		
                  	if (i<=2*i0+1) 
   	      			{	if (imp==1)	{Log.println(" stock");}      // new values in i
      	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
         					{	k12ic[tt][i]=k12[tt];
               			}

						// from second to (2i0+1)th value : we sort the new value directly 
            				if (i>1)
		            		{  
		           				// buble sorting towards lower values
									if (imp==1)	{Log.println(" buble sorting directly");}
   	   	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
    		     					{ 
										if (k12ic[tt][i-1]>k12ic[tt][i])
       		        	  			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i],kic[tt][i-1]);
         	        					mer=k12ic[tt][i];
          	    						cro=i-1;
    	      	    					while ((cro>0)&&(k12ic[tt][cro]>mer))
      	     	   					{	k12ic[tt][cro+1]=k12ic[tt][cro];
         	   	           			//printf(" %d -> %d",cro,cro+1);
            	 	 						cro=cro-1;
              							}
           								k12ic[tt][cro+1]=mer;
  	            					}
   	           				}// end for tt buble sorting
								}// end if (i>1)
  		   		    	}// end if (i<=2*i0+1)
				
					// here, the (2io+1) first values are sorted...
				
						else  
					
					////// the array already contains (2i0+1) sorted values, we put the new value in i0+1

	    		     	{  if (imp==1)	{Log.println("(i0+1)");}
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{	k12ic[tt][i0+1]=k12[tt];
 		           		}

   	         		// sorting the new value of k12
    		        		if (imp==1)	{Log.println(" sorting(k12) ");}
      	         	//printf("[%f ## %f ## %f] -> ",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{

 		          			// k12 goes down
   	         			if (k12ic[tt][i0+1]<k12ic[tt][i0])
    		        			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i0+1],kic[tt][i0]);
      	         			mer=k12ic[tt][i0+1];
       		        			cro=i0;
         	      			while ((cro>0)&&(k12ic[tt][cro]>mer))
          		     			{	k12ic[tt][cro+1]=k12ic[tt][cro];
            	   				cro=cro-1;
             		  			}
             					k12ic[tt][cro+1]=mer;
               				//printf("ok");
	            			}// end if k12 goes down

 								// k12 goes up
  		          			else if (k12ic[tt][i0+1]>k12ic[tt][i0+2])
    		        			{	//printf("\nk(%d)=%f> %f ",tt,kic[tt][i0+1],kic[tt][i0+2]);
      	         			mer=k12ic[tt][i0+1];
       		        			cro=i0+2;
         	      			while ((cro<2*i0+2)&&(k12ic[tt][cro]<mer))
          		     			{	k12ic[tt][cro-1]=k12ic[tt][cro];
            	    				cro=cro+1;
             		  			}
              					k12ic[tt][cro-1]=mer;
               				//printf("ok");
	            			}// end k12 goes up

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
	      	{	l12ic1[tt]=Math.sqrt(k12ic[tt][i1]/Math.PI)-tt*intervalWidth;
       		  	l12ic2[tt]=Math.sqrt(k12ic[tt][i2]/Math.PI)-tt*intervalWidth;
	      	}
	  			
			}
			catch (Exception exc) 
			{	Log.println (Log.ERROR, "Ripley.computeL12ICRect ()", "L12IC(r) computation error");
            for (int i=0; i<=intervalNumber; i++) 
				{	l12ic1[i]=0;
               l12ic2[i]=0;
            }
				return -1;
         }
			return 0;
      }
   	



  
   /** computeL12Tri
   *
   * This method computes the intertype L12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
	* in a plot of complex shape, defined by a rectangular plot (xmi, xma, ymi, yma)
	* and excluded triangles (ax, ay, bx, by, cx, cy).
	*
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * This routine directly calls computeNormalisedK12Tri, which estimates K12(r)
   * 
	* Results are put in the array (tabl12) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
   ******************************/
   
       static public int computeL12Tri (double tabl12[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabk12[] = new double[intervalNumber+1];
         int erreur;
			
			try 
         {	//System.out.println("Début computeL12Tri n1="+pointNumber1+" n2="+pointNumber2);
         
         	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {
               tabk12[i]=0;
               tabl12[i]=0;
            }
    	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {
               intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
           // K12 computation.
				erreur=computeNormalisedK12Tri(tabk12,xx1,yy1,pointNumber1,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,intervalNumber,intervalWidth);

       	// Computing L12
				if (erreur==0)		// only if we have computed K12 !
				{
    	        	for (int i = 1; i <= intervalNumber; i++) 
					{
     	       		//tabg[i]=tabg[i]/(densite*(Math.PI*i*i*intervalWidth*intervalWidth-Math.PI*(i-1)*(i-1)*intervalWidth*intervalWidth));
               	tabl12[i]=Math.sqrt(tabk12[i]/Math.PI)-i*intervalWidth;
     		      }
				}
				else
				{	Log.println (Log.ERROR, "Ripley.computeL12Rect ()", "L12(r) computation error");
             	for (int i=0; i<tabl12.length; i++) 
            	{	tabl12[i]=0;
            	}
           		return -1;
				}     
         } 
         catch (Exception exc) 
         {
            Log.println (Log.ERROR, "Ripley.computeL12Tri ()", "L12(r) computation error");
            for (int i=0; i<tabl12.length; i++) 
            {	tabl12[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   
   
   /** computeNormalisedK12Tri
   *
   * This method computes the intertype K12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
	* in a plot of complex shape, defined by a rectangular plot (xmi, xma, ymi, yma)
	* and excluded triangles (ax, ay, bx, by, cx, cy).
	* 
   * Edge effect correction are done through Ripley's method,
	* i.e. the inverse of the proportion of the part of the circle inside the plot.
   * computations are done for the (intervalNumber) first intervals
   * of width (intervalWidth).
   * The subroutine uses g12 (intertype pair density function, but here not normalised);
   * as intermediate variables to compute K12.
	* Initially an unnormalised version of this routine was used only for L12IC, because it avoided to compute L12
	* Now it is also called by computeL12Tri.
	*
	* Results are put in the array (tabl12) given in parameter.
	*
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
   ******************************/
   
       static public int computeNormalisedK12Tri (double tabk12[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth) 
      	
      {	// intermediate variables
         double tabg12[] = new double[intervalNumber+1];
         double cin;
			double surface=0;
			double densite1=0;
			double densite2=0;
      
         try 
         {	//System.out.println("Début computeL12Tri n="+x.length+" ("+pointNumber+")");
         
         	// initialisation
            for (int i=0; i<=intervalNumber; i++) 
            {
               tabg12[i]=0;
               tabk12[i]=0;
            }
            surface=Ripley.surfaceTri(xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy);
            densite1=((double) pointNumber1)/surface;
            densite2=((double) pointNumber2)/surface;
   	
         // Test : is intervalNumber not too large ?.
            if (intervalNumber*intervalWidth>(0.5*Math.max((xma-xmi),(yma-ymi)))) 
            {
               intervalNumber=(int) ((0.5*Math.max((xma-xmi),(yma-ymi)))/intervalWidth-1);
            }
         	
         // g12 computation.
         // Considering all couples (i,j) i species 1, j species 2 
            for (int i=1; i<=pointNumber1; i++) 
            {  double x1=xx1[i];
               double y1=yy1[i];
               for (int j=1; j<=pointNumber2; j++) 
               {	double x2=xx2[j];		// x : m
                  double y2=yy2[j];		// y : m
               	// distance between these 2 points ?
                  double d = Math.sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
               	// which range class does it belong ? (max value excluded)
                  int interval = (int) (d / intervalWidth)+1;
               	// if it is short enought, we count it
                  if (interval <= intervalNumber) 
                  {	
                  	// edge effect correction
                     cin=Ripley.perimInRect(x1,y1,d,xmi,xma,ymi,yma);
                     if (cin<0)
                     {	Log.println("\ncin<0 sur i AVANT");
            				for (int ii=0; i<tabk12.length; i++) 
            				{	tabk12[ii]=0;
            				}
	                     return -1;
                     }
                     cin=cin-Ripley.perimInTri(x1,y1,d,triangleNumber,ax,ay,bx,by,cx,cy);
                     if (cin<0)
                     {	Log.println("\ncin<0 sur i APRES : "+cin+" "+i);
            				for (int ii=0; i<tabk12.length; i++) 
            				{	tabk12[ii]=0;
            				}							
                        return -1;
                     }
                     tabg12 [interval] +=2*Math.PI/cin;
                  }
               }
            }	// end of the double loop on the couples of points
                  
         // Normalisation & co :
	         // averaging -> density
            for (int i = 1; i <= intervalNumber; i++) {
               tabg12[i]=tabg12[i]/((double) pointNumber1);
            }
         	// integrating g12 to obtain k12
            tabk12[1]=tabg12[1];
            for (int i = 2; i <= intervalNumber; i++) {
               tabk12[i]=tabk12[i-1]+tabg12[i];
            }
         	// normalisation of k12, 
            for (int i = 1; i <= intervalNumber; i++) {
            	//tabg[i]=???;
               tabk12[i]=tabk12[i]/densite2;
            }
   ///// WARNING : we have only normalised K12,
   ///// g12 is here only an intermediate variable
   ///// if we want to estimate g12(t) then we still have to normalise it 
     
         } 
         catch (Exception exc) 
         {
            Log.println (Log.ERROR, "Ripley.computeK12Tri ()", "K12(r) computation error");
            for (int i=0; i<tabk12.length; i++) 
            {	tabk12[i]=0;
            }
				return -1;
         }
			return 0;      
      }
   	
	
	
	/** computeL12ICTri
   *
   * This method computes the confidence interval for the intertype L12(r) function 
	* for two populations x1[], y1[] and x2[], y2[] 
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
	* corresponding to the chosen null hypothesis (icNullHypothesis) :
	*	1 : population independance
	*	2 : random labelling
	*
	* Results are put in the arrays (l12ic1) and (l12ic2) given in parameter.
   *
   * WARNING : for x, y : the points are numbered from 1 to pointNumber included
   * and idem for triangles.
	*
	* imp is the choice of what is printed on screen : 0 : nothing, 1 : everything, 2 : minimum
	*
   ******************************/
       static public int computeL12ICTri (int imp, double l12ic1[], double l12ic2[],
		   double xx1[], double yy1[], int pointNumber1,
		 	double xx2[], double yy2[], int pointNumber2, 
       	double xmi, double xma, double ymi, double yma,
       	int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
       	int intervalNumber, double intervalWidth, 
       	int icNullHypothesis, int simulationNumber, double risk, double precision, double perte2) 

		{	
		 	// Rq : the code is optimised to compute L12(r) only

			// intermediate variables
			double k12[];
			double k12ic[][];
			double mer;
			int cro;
			int erreur;
			Random R=new Random();
			
			// for IC population independance (shifting)
			double dx;
			double dy;
			double dx2;
			double dy2;
			double transmax=0;
			double transmoy=0;
			double trans;
			double exclus;
			double exclusmax=0;
			double exclusmoy=0;
			long tentative=0;
			
			// for IC random labelling (substitution)
			double x0[];
			double y0[];
			int pointNumber0=0;
			int type[];
						
			// initialisation of results array
			for (int i=0; i<=intervalNumber; i++) 
			{	 l12ic1[i]=0;
             l12ic2[i]=0;
         }


			// handle here the case for the simplified estimator
			// unfortunately this is not known for intertype
				
			if (simulationNumber < 1)	// no estimation of Confidance Interval	
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
               if(imp>0)	{Log.println("Monte Carlo, "+simulationNumber+" simulations, i0="+i0);}
            
				// initialisation
 		        	x0 = new double[pointNumber1+pointNumber2+1];
   		      y0 = new double[pointNumber1+pointNumber2+1];
     		    	k12 = new double[intervalNumber+1];
               k12ic = new double[intervalNumber+1][2*i0+2];
  			
	/////////////// initialisation before main loop

		// option 1 : population independance : nothing to prepare
					if (icNullHypothesis==1)
      			{	
 						if (imp==1)	{Log.println("\nshifting");}
         			transmax=0;
      				transmoy=0;
      				trans=0;
      				exclus=0;
      				exclusmax=0;
      				exclusmoy=0;
      				tentative=0;
	     			}
   				else
      			{

		// option 2 : substitutions : stocking all points 
						if (imp==1)	{Log.println("\nsubstitution");}
      				for(int i=1;i<=pointNumber1;i++)
         			{	x0[i]=xx1[i];
         				y0[i]=yy1[i];
						}
						for(int i=1;i<=pointNumber2;i++)
         			{	x0[pointNumber1+i]=xx2[i];
							y0[pointNumber1+i]=yy2[i];
						}
					}
					
					
	///////////////// main loop for MC
					for(int i=1;i<=simulationNumber;i++)
					{	if (imp==1)	{	Log.println("\nIteration "+i);}
						if (imp==2)	{	Log.println("["+i+"]");	}
     					tentative=tentative+1;

      			////// simulating null hypothesis
						if (imp==1)	{Log.println(" S");}


      /// option 1 : population independance (shifting)
         			if (icNullHypothesis==1)
         			{  // shifting type 1, type 2 stays unchanged
 							if (imp==1)	{Log.println(" PI");}
							pointNumber0=pointNumber1;
		 				// Warning,
						// R.nextInt(N) returns an int between 0 et N-1 (included)
 
							// first x 
                  	dx=R.nextInt((int)((xma-xmi)/precision+1))*precision;
            			for(int j=1;j<=pointNumber0;j++)
            			{  x0[j]=xx1[j]+dx;
               			if (x0[j]>xma)
               			{	x0[j]=x0[j]-(xma-xmi);
               			}
            			}

							// then y 
     	            	dy=R.nextInt((int)((yma-ymi)/precision+1))*precision;
			            for(int j=1;j<=pointNumber0;j++)
            			{	y0[j]=yy1[j]+dy;
               			if (y0[j]>yma)
               			{	y0[j]=y0[j]-(yma-ymi);
               			}
            			}

            			// then we delete the points (of shifted pattern 1) that are in the triangles
							int ii=1;
							erreur=0;
            			exclus=0;
							while (ii<=pointNumber0)
							{	int jj=1;
								while ((jj<=triangleNumber)&&(erreur==0))
								{	if (Ripley.in_triangle(x0[ii],y0[ii],ax[jj],ay[jj],bx[jj],by[jj],cx[jj],cy[jj])==1)
									{	if (imp==1)	{Log.println(".");}
										erreur=1;
									}
									jj=jj+1;
								}
								if (erreur==1)			// if the point is in a triangle
								{	if (ii<pointNumber0)
									{	x0[ii]=x0[pointNumber0];
										y0[ii]=y0[pointNumber0];
										ii=ii-1;
                  			}
                  			exclus=exclus+1;
									pointNumber0=pointNumber0-1;
								}
								ii=ii+1;
               			erreur=0;
							}
							// here we have deleted from pattern 0 (shifted version of pattern 1) all points in a triangle
							erreur=0;
			         	if (imp==1)	{Log.println("-"+pointNumber0+"-");}
            			exclusmoy+=exclus;
            			if (exclus>exclusmax)
            			{	exclusmax=exclus;
            			}
							
							// if there are enought points left
							if (pointNumber0>=perte2)
   	      			{  if (imp==1)	{Log.println("ok");}
      	   				erreur=0;

     							// computing the length of the translation
               			if (dx<=(xma-xmi)/2)
               			{	dx2=dx;
               			}
               			else
               			{  dx2=dx-(xma-xmi);
               			}
               			if (dy<=(yma-ymi)/2)
               			{	dy2=dy;
               			}
               			else
               			{  dy2=dy-(yma-ymi);
               			}
               			trans=Math.sqrt(dx2*dx2+dy2*dy2);
               			//printf("[%f]",trans);
               			transmoy+=trans;
               			if (transmax<trans)
            				{  //printf("[%f<]R",transmax);
               				transmax=trans;
            				}
               			else
            				{  //printf("[%f>=]",transmax);
             				}
         				}
			         	else		// not enought points
         				{ 	if (imp==1)	{Log.println("!! Not enought points ("+pointNumber0+")- we do it again");}
         					erreur=1;
         				}
         			}
						
						else
		/// option 2 : substitutions : random labelling of points
   			     {	
					  		type = new int[pointNumber1+pointNumber2+1];
					  		for(int j=1;j<=pointNumber1+pointNumber2;j++)
         				type[j]=2;

		 				// Warning,
						// R.nextInt(N) returns an int between 0 et N-1 (included)
						
            			// random drawing of pointNumber1 type 1 points -> in xx1
         				int j=1;
            			while (j<=pointNumber1)
            			{	int jj=1+R.nextInt(pointNumber1+pointNumber2);
            				while (type[jj]!=2)
               			{  jj=1+R.nextInt(pointNumber1+pointNumber2);
               			}
               			type[jj]=1;
               			xx1[j]=x0[jj];
               			yy1[j]=y0[jj];
               			j=j+1;
            			}
							
            			// verifying there are still pointNumber2 type 2 points -> in xx2
            			int jj=0;
            			for(j=1;j<=pointNumber1+pointNumber2;j++)
            			{	if (type[j]==2)
              				{	jj=jj+1;
                  			xx2[jj]=x0[j];
         						yy2[jj]=y0[j];
               			}
            			}
            			if (jj!=pointNumber2)
            			{	if (imp>0)	{Log.println("ERREUR substitution");}
               			erreur=1;
        					}
            			else
            			{	erreur=0;
            			}
         			}

			      ////// estimating K12
			      /// only if no error !
			      /// we call the normalised routine [for intertri the number of points can vary]

     					if (erreur==0)					// computing NormalisedK12
      				{
							//printf(" R(%d)",point_nb);
							if (icNullHypothesis==1)			// HO population independance
         				{  
								// shifting with triangles : we should recompute density for pattern 0
         					// but fortunately it does not appears in the computation of K12.
         					//printf(" R(%d)",point_nb0);
        	 				  	//densite0=point_nb0/surface;
        				 		erreur=computeNormalisedK12Tri(k12,x0,y0,pointNumber0,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,intervalNumber,intervalWidth);
      			   	}
      				 	else										// H0 random labelling
							{  
								// densities haven't changed
         					//printf(" R(%d)",point_nb);
        				 		erreur=computeNormalisedK12Tri(k12,xx1,yy1,pointNumber1,xx2,yy2,pointNumber2,xmi,xma,ymi,yma,triangleNumber,ax,ay,bx,by,cx,cy,intervalNumber,intervalWidth);
        				 	}
      			    	//printf("ok");
     				 	}
 
       			////// if there is an error : a new simulation
         			if (erreur!=0)
         			{	i=i-1;
         				if (imp==1)	{Log.println( "ERREUR Intertype\n");}
		         	}
  			      	else
         			{

      				////// handle the results
      					if (imp==1)	{Log.println(" T");}

         			// we put the 2i0+1 first values and sort them
           		
                  	if (i<=2*i0+1) 
   	      			{	if (imp==1)	{Log.println(" stock");}      // new values in i
      	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
         					{	k12ic[tt][i]=k12[tt];
               			}

						// from second to (2i0+1)th value : we sort the new value directly 
            				if (i>1)
		            		{  
		           				// buble sorting towards lower values
									if (imp==1)	{Log.println(" buble sorting directly");}
   	   	   				for(int tt=1;tt<=intervalNumber;tt=tt+1)
    		     					{ 
										if (k12ic[tt][i-1]>k12ic[tt][i])
       		        	  			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i],kic[tt][i-1]);
         	        					mer=k12ic[tt][i];
          	    						cro=i-1;
    	      	    					while ((cro>0)&&(k12ic[tt][cro]>mer))
      	     	   					{	k12ic[tt][cro+1]=k12ic[tt][cro];
         	   	           			//printf(" %d -> %d",cro,cro+1);
            	 	 						cro=cro-1;
              							}
           								k12ic[tt][cro+1]=mer;
  	            					}
   	           				}// end for tt buble sorting
								}// end if (i>1)
  		   		    	}// end if (i<=2*i0+1)
				
					// here, the (2io+1) first values are sorted...
				
						else  
					
					////// the array already contains (2i0+1) sorted values, we put the new value in i0+1

	    		     	{  if (imp==1)	{Log.println("(i0+1)");}
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{	k12ic[tt][i0+1]=k12[tt];
 		           		}

   	         		// sorting the new value of k12
    		        		if (imp==1)	{Log.println(" sorting(k12) ");}
      	         	//printf("[%f ## %f ## %f] -> ",kic[3][i0],kic[3][i0+1],kic[3][i0+2]);
       		  			for(int tt=1;tt<=intervalNumber;tt=tt+1)
							{

 		          			// k12 goes down
   	         			if (k12ic[tt][i0+1]<k12ic[tt][i0])
    		        			{	//printf("\nk(%d)=%f< %f ",tt,kic[tt][i0+1],kic[tt][i0]);
      	         			mer=k12ic[tt][i0+1];
       		        			cro=i0;
         	      			while ((cro>0)&&(k12ic[tt][cro]>mer))
          		     			{	k12ic[tt][cro+1]=k12ic[tt][cro];
            	   				cro=cro-1;
             		  			}
             					k12ic[tt][cro+1]=mer;
               				//printf("ok");
	            			}// end if k12 goes down

 								// k12 goes up
  		          			else if (k12ic[tt][i0+1]>k12ic[tt][i0+2])
    		        			{	//printf("\nk(%d)=%f> %f ",tt,kic[tt][i0+1],kic[tt][i0+2]);
      	         			mer=k12ic[tt][i0+1];
       		        			cro=i0+2;
         	      			while ((cro<2*i0+2)&&(k12ic[tt][cro]<mer))
          		     			{	k12ic[tt][cro-1]=k12ic[tt][cro];
            	    				cro=cro+1;
             		  			}
              					k12ic[tt][cro-1]=mer;
               				//printf("ok");
	            			}// end k12 goes up

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
	      	{	l12ic1[tt]=Math.sqrt(k12ic[tt][i1]/Math.PI)-tt*intervalWidth;
       		  	l12ic2[tt]=Math.sqrt(k12ic[tt][i2]/Math.PI)-tt*intervalWidth;
	      	}
	  			
			}
			catch (Exception exc) 
			{	Log.println (Log.ERROR, "Ripley.computeL12ICTri ()", "L12IC(r) computation error");
            for (int i=0; i<=intervalNumber; i++) 
				{	l12ic1[i]=0;
               l12ic2[i]=0;
            }
				return -1;
         }
			return 0;
      }
   	
  	
	
   }


