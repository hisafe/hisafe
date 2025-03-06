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

import java.util.Random;

/**
 * Gibbs Patterns simulation (utilities to simulate Gibbs (complex strucutre) patterns).
 * This method simulates a Gibbs pattern of pointNumber points,
 * within a rectangular plot defined by xmi, xma, ymi, yma
 * x and y co-ordinates are computed with a precision p.
 * iterationNumber iterations allows to converge towards a pattern
 * that minimize the cost function parametrized on intervalNumber intervals
 * by intervalRadius[] and intervalCost[].
 * 
 * @author Francois Goreaud - 13/8/2001 - 26/1/06
 */
public class GibbsPattern {
//checked for c4.1.1_09 - fc - 5.2.2003
	
	static {
		//~ System.out.println ("GibbsPattern *** loaded");
	}
	
	static public void simulateXY (int pointNumber, double x[], double y[], 
			double xmi, double xma, double ymi, double yma, double p,
			int intervalNumber, double intervalRadius[], double intervalCost[],
			int iterationNumber) {	
		int i,j,k,l,m;
		double xx,yy;
		double e1,e2,ee,dd;
		
		Random R=new Random();
		RandomPattern.simulateXY(pointNumber, x, y, xmi, xma, ymi, yma, p);
		
		for (i=1; i<=iterationNumber; i=i+1) {
			j=1+R.nextInt(pointNumber);
			e1=0;
			for (k=1; k<=pointNumber; k++) {
				if (k!=j) {
					dd=(x[k]-x[j])*(x[k]-x[j])+(y[k]-y[j])*(y[k]-y[j]);
					if (dd>0) {
						dd=Math.sqrt(dd);
					} else {
						dd=0;
					}
					l=intervalNumber;
					ee=0;
					while (l>0) {	
						if (dd<intervalRadius[l]) {
							ee=intervalCost[l];
							l=l-1;
						} else {
							l=0;
						}
					}
					e1=e1+ee;
				}
			}
			
			for (m=1; m<=4; m++) {
				e2=0;
				xx=xmi+R.nextInt((int)((xma-xmi)/p+1))*p;
				yy=ymi+R.nextInt((int)((yma-ymi)/p+1))*p;
				for (k=1; k<=pointNumber; k++) {	
					if (k!=j) {
						dd=Math.sqrt((x[k]-xx)*(x[k]-xx)+(y[k]-yy)*(y[k]-yy));
						l=intervalNumber;
						ee=0;
						while(l>0) {	
							if (dd<intervalRadius[l]) {
								ee=intervalCost[l];
								l=l-1;
							} else {
								l=0;
							}
						}
						e2=e2+ee;
					}
				}
				if (e2<e1) {
					x[j]=xx;
					y[j]=yy;
					e1=e2;
				}
			}
		}
	}

	static public void evolveXYInteraction (int pop, int pointNumber, double x[], double y[], 
			int po[], double xmi, double xma, double ymi, double yma, double p,
			int interactionNumber, int interactionPop [], double interactionRadius[], 
			double interactionCost[], int iterationNumber) {	
		int i,j,k,l,m;
		double xx,yy;
		double e1,e2,ee,dd;
		
		Random R=new Random();
		
		// we make iterationNumber modifications of points from population pop
		for (i=1; i<=iterationNumber; i=i+1) 
		{	// choosing the point at random
			j=1+R.nextInt(pointNumber);
			while(po[j]!=pop)			// is it a point from population pop ?
			{
				j=1+R.nextInt(pointNumber);			
			}

			// estimating the cost for old location
			e1=0;
			for (k=1; k<=pointNumber; k++) {
				if (k!=j) 
				{						// all neighbours
					dd=(x[k]-x[j])*(x[k]-x[j])+(y[k]-y[j])*(y[k]-y[j]);
					if (dd>0) 
					{
						dd=Math.sqrt(dd);
					} else {
						dd=0;
					}
					// do these two points interact ?
					l=interactionNumber;
					ee=0;
					while (l>0) 
					{	
						if ((dd<interactionRadius[l])&&(interactionPop[l]==po[k])) 
						{
							ee=interactionCost[l];
						}
						l=l-1;
					}
					e1=e1+ee;
				}
			}
			
			// we test 4 other locations
			for (m=1; m<=4; m++) 
			{
				e2=0;
				xx=xmi+R.nextInt((int)((xma-xmi)/p+1))*p;
				yy=ymi+R.nextInt((int)((yma-ymi)/p+1))*p;
				for (k=1; k<=pointNumber; k++) {	
					if (k!=j) 
					{
						dd=Math.sqrt((x[k]-xx)*(x[k]-xx)+(y[k]-yy)*(y[k]-yy));
					// do these two points interact ?
						l=interactionNumber;
						ee=0;
						while(l>0) 
						{	
							if ((dd<interactionRadius[l])&&(interactionPop[l]==po[k])) 
							{
								ee=interactionCost[l];
							}
							l=l-1;
						}
						e2=e2+ee;
					}
				}
				if (e2<e1) {
					x[j]=xx;
					y[j]=yy;
					e1=e2;
				}
			}
		}
	}
	
}   
	
