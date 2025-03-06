/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2006 Francois Goreaud & Marie ANge Ngo Bieng.
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

import java.util.Random;

/**
 * NeymanScott Patterns simulation.
 * Utilities to simulate NeymanScott (clustered) patterns.
 * 
 * @author Francois Goreaud & Marie ANge Ngo Bieng 31/08/06
 */
    public class NeymanScottPattern {
   //checked for c4.1.1_09 - fc - 5.2.2003
   
      static {
      //~ System.out.println ("NeymanScottPattern *** loaded");
      }
   	
   /**
   * This method simulates a NeymanScott pattern of pointNumber points,
   * with clusterNumber clusters of radius clusterRadius
   * within a rectangular plot defined by xmi, xma, ymi, yma
   * x and y co-ordinates are computed with a precision p.
   * In this version, each cluster is simulated successively with the exact number of points
   * so that there will be a higher density in the intersections between 2 clusters
   */
       static public void simulateXY (int pointNumber, int clusterNumber, double clusterRadius, 
       	double x[], double y[], double xmi, double xma, double ymi, double yma, double p) {
         int i, cluster;
         double xxx, yyy, dx, dy;
      
         Random R=new Random();
      
         double[] clusterx = new double[clusterNumber+1];
         double[] clustery = new double[clusterNumber+1];
         RandomPattern.simulateXY (clusterNumber, clusterx, clustery, xmi, xma, ymi, yma, p);
      
         cluster=0;
         for(i=1;i<=pointNumber;i=i+1) {
            cluster=cluster+1;
            if (cluster>clusterNumber) {cluster=1;}
            xxx=xmi-1;
            yyy=0;
            while((xxx<xmi)||(xxx>xma)||(yyy<ymi)||(yyy>yma)) {
               dx=R.nextInt((int) ((2*clusterRadius/p)+1))*p-clusterRadius;
               dy=R.nextInt((int) ((2*clusterRadius/p)+1))*p-clusterRadius;
               while ((dx*dx+dy*dy)>clusterRadius*clusterRadius) {
                  dx=R.nextInt((int) ((2*clusterRadius/p)+1))*p-clusterRadius;
                  dy=R.nextInt((int) ((2*clusterRadius/p)+1))*p-clusterRadius;
               }
               xxx=clusterx[cluster]+dx;
               yyy=clustery[cluster]+dy;
            }
            x[i]=xxx;
            y[i]=yyy;
         }
      
      }
   
   	
   /**
   * This method simulates a NeymanScott pattern of pointNumber points,
   * with clusterNumber clusters of radius clusterRadius
   * within a rectangular plot defined by xmi, xma, ymi, yma
   * x and y co-ordinates are computed with a precision p.
   * In this version, points are located within a certain distance of cluster centers
   * so that there is no higher density in the intersections between 2 clusters
   * but then the number of points per cluster is not constant
   * authors : MA Ngo Bieng, F Goreaud, 31/08/06
   */
       static public void simulateXYNS2 (int pointNumber, int clusterNumber, double clusterRadius, 
       	double x[], double y[], double xmi, double xma, double ymi, double yma, double p) {
         int i, caMarche;
         double xxx, yyy,distance;
      
         Random R=new Random();
      
         double[] clusterx = new double[clusterNumber+1];
         double[] clustery = new double[clusterNumber+1];
         RandomPattern.simulateXY (clusterNumber, clusterx, clustery, xmi, xma, ymi, yma, p);
      
         for(i=1;i<=pointNumber;i=i+1) {
         // a random point
            xxx=xmi+R.nextInt((int)((xma-xmi)/p+1))*p;
            yyy=ymi+R.nextInt((int)((yma-ymi)/p+1))*p;
         
         // is it within a cluster ?		
            caMarche=0;		// by default, we consider it is not in a cluster
            for(int a=1;a<=clusterNumber;a++)
            {
               distance=Math.sqrt((clusterx[a]-xxx)*(clusterx[a]-xxx)+(clustery[a]-yyy)*(clustery[a]-yyy))  ;
               if (distance<=clusterRadius)	//it works !
               {	caMarche=1;
               }			
            }
         	
         // if the point is inside a cluster, we keep it
            if (caMarche==1)	
            {
               x[i]=xxx;
               y[i]=yyy;
            }
			// else we must simulate it again
				else
				{
					i=i-1;
				}	
         }
      
      }
   
   }   
  
