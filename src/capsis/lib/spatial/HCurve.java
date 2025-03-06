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

import java.util.Random;

/**
 * H curve simulation.
 * Utilities to simulate H from D values.
 * 
 * @author Francois Goreaud - 6/9/2001
 */
public class HCurve {
//checked for c4.1.1_09 - fc - 5.2.2003
	
	static {
		//~ System.out.println ("HCurve *** loaded");
	}
	
	/**
	* This method computes H from D,
	* using a linear model : H=a+bD+csqrt(D)+dN(0,1).
	*/
	static public void hLinearModel (int pointNumber, double h[], double d[], 
			double alpha, double beta, double gamma, double delta) {
		int i;
		Random R=new Random();
		
		for (i=1; i<=pointNumber; i=i+1) {
			h[i]=alpha+beta*d[i]+gamma*Math.sqrt(d[i])+delta*R.nextGaussian();
		}
	}
	
	/**
	* This method computes H from D,
	* using a logistic model : H=a+K(1-exp(-b*D))+dN(0,1).
	*/
	static public void hLogisticModel (int pointNumber, double h[], double d[], 
			double alpha, double K, double beta, double delta) {
		int i;
		Random R=new Random();
		
		for (i=1; i<=pointNumber; i=i+1) {
			h[i]=alpha+K*(1-Math.exp(-beta*d[i]))+delta*R.nextGaussian();
		}
	}
	
	/**
	* This method computes H from D,
	* using an hyperbolic model : H=a+D²/(b+c*D)²+dN(0,1).
	*/
	static public void hHyperbolicModel (int pointNumber, double h[], double d[], 
			double alpha, double beta, double gamma, double delta) {
		int i;
		Random R=new Random();
		
		for (i=1; i<=pointNumber; i=i+1) {
			h[i]=alpha+(d[i]*d[i])/((beta+gamma*d[i])*(beta+gamma*d[i]))+delta*R.nextGaussian();
		}
	}
	
}

