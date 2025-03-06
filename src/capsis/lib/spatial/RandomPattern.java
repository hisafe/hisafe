/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2005 Francois Goreaud.
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
 * Random Patterns simulation.
 * Utilities to simulate random patterns.
 * 
 * @author Francois Goreaud - 3/7/2001 -> 22/5/2006 
 */
 
public class RandomPattern {

	static Random R;

	static {
		//~ System.out.println ("RandomPattern *** loaded");
				R=new Random();

	}
	
	/**
	* This method simulates a Poisson pattern of pointNumber points,
	* within a rectangular plot defined by xmi, xma, ymi, yma
	* x and y co-ordinates are computed with a precision p.
	* and are put in tables x[] and y[] (parameters)
	* from index 1 to pointNumber inclusive !!!! (no point in 0)
	*/
	static public int simulateXY (int pointNumber, double x[], double y[], 
			double xmi, double xma, double ymi, double yma, double p) {
		int i;
		
		try
		{
			for (i=1; i<=pointNumber; i=i+1) {
				x[i]=xmi+R.nextInt((int)((xma-xmi)/p+1))*p;
				y[i]=ymi+R.nextInt((int)((yma-ymi)/p+1))*p;
			}
			return 0;      
		}
		catch (Exception exc)
		{	return -1;
		}
	}
	
		
	/**
	* This method simulates a Poisson pattern of pointNumber points,
	* within a plot of complexe shape defined by xmi, xma, ymi, yma
	* and some excluded triangles triangleNumber, ax, ay, bx, by, cx, cy
	* x and y co-ordinates are computed with a precision p.
	* and are put in tables x[] and y[] (parameters)
	* from index 1 to pointNumber inclusive !!!! (no point in 0)
	*/
	static public int simulateXYTri (int pointNumber, double x[], double y[], 
			double xmi, double xma, double ymi, double yma,
			int triangleNumber, double ax[], double ay[], double bx[], double by[], double cx[], double cy[], 
 			double p) {
		int i;
		int j;
		int erreur;
		//Random R=new Random();
		
		try
		{
			i=1;
   		while (i<=pointNumber)
			{  // first point in the rectangle
				// WARNING
				// R.nextInt(N) returns an int between 0 and N-1 
				x[i]=xmi+R.nextInt((int)((xma-xmi)/p+1))*p;
				y[i]=ymi+R.nextInt((int)((yma-ymi)/p+1))*p;
		
  	 	   	// if the point is in no triangle, next point otherwise we do it again
   	   	erreur=0;
				j=1;
				while ((j<=triangleNumber)&&(erreur==0))
				{	if (Ripley.in_triangle(x[i],y[i],ax[j],ay[j],bx[j],by[j],cx[j],cy[j])==1)
					{	//System.out.print(".");
						erreur=1;
					}
					j=j+1;
				}
				if (erreur==0)
				{	i=i+1;
				}
			}
			return 0;
		}	
		catch (Exception exc)
		{	return -1;
		}
	}
	
	/**
	* This method simulates a Gaussian repartition for a quantitative mark m.
	* for pointNumber points.
	* The results are put in table m[] (parameters)
	* from index 1 to pointNumber inclusive !!!! (no point in 0)
	*/
	static public void simulateGaussianM (int pointNumber, double m[], 
			double mean, double deviation) {
		int i;
		//Random R=new Random();
		
		for (i=1; i<=pointNumber; i=i+1) {
			m[i]=mean +R.nextGaussian()*deviation;
		}      
	}
	
}   
  
