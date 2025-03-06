/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2003 Francois Goreaud.
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
 * Clark & Evans index.
 * 
 * @author Francois Goreaud 30/4/2002 - 2/5/02
 */
public class ClarkEvans {
   //checked for c4.1.1_09 - fc - 5.2.2003
   
      static {
      //~ System.out.println ("ClarkEvans *** loaded");
      }
   
   /**
   * This method computes Clark & Evans index for a point pattern defined by x[] and y[] 
   * in a rectangular plot defined by xmi, xma, ymi, yma.
   */
       static public double computeCE (double x[], double y[], 
       int pointNumber, double xmi, double xma, double ymi, double yma) {	
      
         double d, dmin, dmean, ce, densite;
      
         try {
         // computation
            densite=pointNumber/((xma-xmi)*(yma-ymi));
            dmean=0;        
            for (int i=0; i<pointNumber; i++) {
               dmin=2*(xma+yma-xmi-ymi);
               for (int j=0; j<pointNumber; j++) {
                  if (j != i) {
                  //System.out.println ("*");
                     d = Math.sqrt((x[i]-x[j])*(x[i]-x[j])+(y[i]-y[j])*(y[i]-y[j]));
                     if (d<dmin) {
                        dmin=d;
                     }
                  }
               }
               dmean=dmean+dmin;
            }
            dmean=dmean/pointNumber;
            ce=2*Math.sqrt(densite)*dmean;
         
         } 
             catch (Exception exc) {
               Log.println (Log.ERROR, "ClarkEvans.computeCE ()", "CE computation error", exc);
               ce=0;
            }
         return ce;
      
      }
   
   /**
   * This method computes Clark & Evans Intertype index for a point pattern defined by x1[] y1[] x2[] y2[] 
   * in a rectangular plot defined by xmi, xma, ymi, yma.
   */
       static public double computeCE12 (double x1[], double y1[],int pointNumber1,
       double x2[], double y2[],int pointNumber2, double xmi, double xma, double ymi, double yma) {	
      
         double d, dmin, dmean, ce12, densite2;
      
         try {
         // computation
            densite2=pointNumber2/((xma-xmi)*(yma-ymi));
            dmean=0;        
            for (int i=0; i<pointNumber1; i++) {
               dmin=2*(xma+yma-xmi-ymi);
               for (int j=0; j<pointNumber2; j++) {
               	d = Math.sqrt((x1[i]-x2[j])*(x1[i]-x2[j])+(y1[i]-y2[j])*(y1[i]-y2[j]));
                  if (d<dmin) {
                     dmin=d;
                  }
               
               }
               dmean=dmean+dmin;
            }
            dmean=dmean/pointNumber1;
            ce12=2*Math.sqrt(densite2)*dmean;
         
         } 
             catch (Exception exc) {
               Log.println (Log.ERROR, "ClarkEvans.computeCE12 ()", "CE12 computation error", exc);
               ce12=0;
            }
         return ce12;
      
      }   
   }
