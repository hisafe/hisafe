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

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Random;

import jeeb.lib.sketch.scene.item.Polygon;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * Crown Avoidance Pattern simulation. This method simulates a pattern of
 * pointNumber points within a rectangular plot defined by xmi, xma, ymi, yma x
 * and y co-ordinates are computed with a precision p.
 * 
 * 
 * Tree positions are randomly selected one after one with the following
 * constraint: Crown diameter should not intersect one with another (with a
 * given probability) Notice that trees are sorted depending on their diameter
 * first (to place the biggest first)
 * 
 * If p=0, trees are taken randomly. If p big, no crown intersection is
 * possible. when is becomes too long (randIt>10stemNumber) p is divided by two
 * 
 * It should be noticed that crownProfile generally contains at least two radius
 * (at cbh and at h, for cylindric trees) However the method will work only if
 * ALL crownProfiles contains only one radius (considered at the radius of the
 * tree) In the case the weight will be a surface instead a volume
 * 
 * 
 * @author Francois Pimont - 6/2010
 */
public class CrownAvoidancePattern {
	
	static {
		// fc-7.10.2019 Added the labels translations, see Translator.swap () below
		Translator.addBundle("capsis.lib.spatial.SpatialLabels");
	}
	
	static public void simulateXY (int pointNumber, double x[], double y[], Polygon poly, 
			 double p,
			List<double[][]> treeRadius, Random rnd) throws Exception {
		double xmi = poly.getMin ().x;
		double ymi = poly.getMin ().y;
		double xma = poly.getMax ().x;
		double yma = poly.getMax ().y;
		
		System.out.println("simulateXY "+treeRadius.get(1)[0][0]+ " "+treeRadius.get(10)[0][0]);
		boolean radiusAllEqual0 = true;
		boolean sortTrees = true;

		// building lists of maxRadius (= crownRadius)
		double maxHeight = 0d;
		double minCbh = Double.MAX_VALUE;
		Double[] maxRadius = new Double[pointNumber];
		Double[] maxRadiusSorted = new Double[pointNumber];
		int nmin = Integer.MAX_VALUE; // max number of crown profile provided
		int nmax = 0; // min number of crown profile provided
		for(int i=0; i<pointNumber; i++) {
			maxRadius[i] = 0d;
			double[][] treeRadiusI = treeRadius.get(i);
			nmin = Math.min(nmin, treeRadiusI.length);
			nmax = Math.max(nmax, treeRadiusI.length);
			for (int j = 0; j < treeRadiusI.length; j++) {
				maxRadius[i] = Math.max(maxRadius[i], treeRadiusI[j][0]);

			}

			maxHeight = Math.max(maxHeight,
					treeRadiusI[treeRadiusI.length - 1][1]);
			minCbh = Math.min(minCbh, treeRadiusI[0][1]);
			radiusAllEqual0 = !(maxRadius[i] > 0);
			maxRadiusSorted[i] = maxRadius[i];
		}
		
		if (nmin == 0 || (nmin == 1 && nmax > 1)) {
			throw new Exception(
					"CrownAvoidancePattern.computeCrownIntersection: crownRadius array is not defined");
		}
		
		if (radiusAllEqual0 || p == 0d)
			sortTrees = false;
		// index is a converter of index to find
		// a point or treeRadius respecting sorting order:
		// maxRadiusSorted[i] = maxRadius[index[i]]
		int[] index = new int[pointNumber];
		
		// sorting trees from the biggest to the smallest and definition of
		// "index"
		if (sortTrees) {
			Arrays.sort(maxRadiusSorted, new Comparator<Double>() {
				public int compare(Double r1, Double r2) {
					if (r1 < r2) {
						return 1;
					} else {
						return -1;
					}
				}
			});
			for (int i = 0; i < pointNumber; i++) {
				int j = 0;
				while (maxRadiusSorted[i] != maxRadius[j]) {
					j = j + 1;
				}
				index[i] = j;
			}
		} else {
			for (int i = 0; i < pointNumber; i++)
				index[i] = i;
		}
		// System.out.println("radmax=" + maxRadius[index[0]] + ";radmax2="
		// + maxRadius[index[10]] + ";radmin="
		// + maxRadius[index[pointNumber - 1]]);
		// System.out.println("indmax=" + index[0] + ";indmax2=" + index[1]
		// + ";indmin=" + index[pointNumber - 1]);
		
		double totalWeight = 0d;
		Random R=rnd;
		if (R==null) {
			R=new Random();
		}
		int randIt = 0;
		int dispatcherIndex = 0;
		//int dispatcherIndexMax = (int) pointNumber/20; //every 5%
		int dispatcherIndexMax = (int) 3000; //every 3000 trees
		
		for(int i=0; i<pointNumber; i++) { 
			int i2 = index[i]; // index in non sorted arrays
           // I simulate coordinates  x and y of point i in seeds
           double xtemp=0;
           double ytemp=0; 
           boolean inPoly = false;
           while(!inPoly) {
        	   xtemp = xmi + R.nextDouble()*(xma-xmi);
        	   ytemp = ymi + R.nextDouble()*(yma-ymi);
        	   if (poly.contains (xtemp, ytemp)) {
        		   inPoly = true;
        	   }
           }
           	randIt++;
           	dispatcherIndex = i % dispatcherIndexMax;
           	if (dispatcherIndex == dispatcherIndexMax-1) {
          	    StatusDispatcher.print(Translator.swap("CrownAvoidancePattern.polygon")+" "+poly.getItemId()+": "+Translator
    				.swap("CrownAvoidancePattern.computingSpatialDistribution") + " "+
    				+ Math.round((double)(i*100)/(double) pointNumber) + "%");
           	 System.out.println(i+ " positions computed for "+pointNumber+ "trees ("+Math.round((double)(i*100)/(double) pointNumber) + "%)");
           	}
           	
			if (randIt % (int) (2d * pointNumber) == 0) {
				p = p / 2;
				System.out
						.println(randIt
								+ " random positions tested for trees, weight is now divided by 2: p="
								+ p);
			}
           double maxIntersectionVolume = 0d;
           double weight = 0d; // quadratic weight of intersections
			for (int j = 0; j < i; j++) {
        	   int j2 = index[j]; // index in non sorted arrays
				double d = Math.sqrt((x[j2] - xtemp) * (x[j2] - xtemp)
						+ (y[j2] - ytemp) * (y[j2] - ytemp));
				//System.out.println("SimulateXY calling computeCrownIntersection");
				//System.out.println("d="+d+"	i2="+i2+" j2="+j2+ " "+treeRadius.get(i2)[0][0]+ " "+treeRadius.get(j2)[0][0]);
				//System.out.println("d="+d+"	i2=22"+" j2=44"+ " "+treeRadius.get(22)[0][0]+ " "+treeRadius.get(44)[0][0]);
				double intersectionVolume = computeCrownIntersection(d,
						treeRadius.get(i2), treeRadius.get(j2));
				// if (intersectionVol>0d) { // if distance smaller that radius
				// sum
            	  // we memorize
				maxIntersectionVolume = Math.max(intersectionVolume,
						maxIntersectionVolume);
				weight += intersectionVolume;
				// }
           }
           
           double rand = R.nextDouble();
           double proba = 1d - Math.exp( -weight * p); 
           if (rand < proba) {
        	   // try a new position
        	   i = i - 1;
        	   // System.out.println("TRY AGAIN");
           } else {
        	   totalWeight += weight;
        	   x[i2] = xtemp;
				y[i2] = ytemp;
				// System.out.println("positioning tree:" + i + ";crownRadius="
				// + maxRadius[i2] + ";weight=" + weight
				// + " nb position tested=" + randIt);
           }
           
		}
		StatusDispatcher.print(Translator.swap("CrownAvoidancePattern.polygon")+" "+poly.getItemId()+": "+Translator
				.swap("CrownAvoidancePattern.computingSpatialDistribution") + "100 %");
       	 
		System.out.println(randIt + " random postion tested for " + pointNumber
				+ " trees");
		StatusDispatcher.print("Polygon "+poly.getItemId()+": "+Translator
				.swap("CrownAvoidancePattern.spatialDistributionComputed")+" ("+
				+ (int) (totalWeight
				/ (poly.getPolygon2 ().getPositiveArea () * (maxHeight - minCbh))
				* 100d)
				+ Translator.swap("CrownAvoidancePattern.percentageOfCrownIntersectionInTreeStrata")); //  % of crown intersection in tree strata
		System.out.println("crownIntersection % in tree strata=" + totalWeight
				/ (poly.getPolygon2 ().getPositiveArea () * (maxHeight - minCbh)) * 100);
		
	
	}

	/**
	 * This method computes the intersection of two crowns at distance d
	 * 
	 * @param d
	 * @param crownRadius1
	 *            array of crownRadius at a given height (should increase...)
	 * @param crownRadius2
	 * @throws Exception
	 */
	static public double computeCrownIntersection(double d,
			double[][] crownRadius1, double[][] crownRadius2) throws Exception {
		//System.out.println(crownRadius1[0][0]+" "+crownRadius1[0][1]+" "+crownRadius2[0][0]+" "+crownRadius2[0][1]);
		int n1 = crownRadius1.length;
		int n2 = crownRadius2.length;
		double res = 0d;
		// comparing minimum heights
		
		if (n1 == 1 && n2 == 1) // just one crown radius is provided
			return computeDiskAreaIntersection(d, crownRadius1[0][0],
					crownRadius2[0][0]);
		if (n1<=1 || n2 <=1) {
			throw new Exception("CrownAvoidancePattern.computeCrownIntersection: crownRadius array is not defined");
		}
		
		//za1 and zb1 are heights for i1 and i1+1 in crownRadius1
		
		int i1=0;
		double cbh1 = crownRadius1[i1][1];
		double za1 = crownRadius1[i1][1];
		double zb1 = crownRadius1[i1+1][1];
		int i2=0;
		double cbh2 = crownRadius2[i2][1];
		double za2 = crownRadius2[i2][1];
		double zb2 = crownRadius2[i2+1][1];
		
		// tree heights
		double h1 = crownRadius1[n1-1][1];
		double h2 = crownRadius2[n2-1][1];
		//System.out.println("cbh1="+cbh1+" h1="+h1+" cbh2="+cbh2+" h2= "+h2);
		if (h1<cbh2 || h2<cbh1) {
			return 0d;
		}
		
		// looking for max of crown base
		while (zb1 < cbh2) {
			i1++;
			za1 = crownRadius1[i1][1];
			zb1 = crownRadius1[i1+1][1];
		}
		while (zb2 < cbh1) {
			i2++;
			za2 = crownRadius2[i2][1];
			zb2 = crownRadius2[i2+1][1];
		}
		
		double zlow = Math.max(cbh1, cbh2);
		double ztop = zlow;
		
		
		double ra1 = crownRadius1[i1][0];
		double rb1 = crownRadius1[i1+1][0];
		if (zb1 <= za1) {
			throw new Exception(
					"CrownAvoidancePattern.computeCrownIntersection: height in crownRadius array should increase");
		}
		double r1low = (ra1 * (zb1 - zlow) + rb1 * (zlow - za1))
		/ (zb1 - za1);
		double r1top = r1low;
		
		double ra2 = crownRadius2[i2][0];
		double rb2 = crownRadius2[i2+1][0];
		if (zb2 <= za2) {
			throw new Exception(
					"CrownAvoidancePattern.computeCrownIntersection: height in crownRadius array should increase");
		}
		double r2low = (ra2 * (zb2 - zlow) + rb2 * (zlow - za2))
		/ (zb2 - za2);
		double r2top = r2low;
		double r1max=0d;
		double r2max=0d;
		
		// when i1=n1-1 or i2==n2-1, it means that we are at the top of a tree
		// for ztop, we can stop
		
		while (i1 < n1 - 1 && i2 < n2 - 1) {
			double r1mean = 0d;// this is the radius for tree1 between zlow and
								// ztop
			double r2mean = 0d;
			zlow = ztop;
			// computation of new ztop
			if (zb1<=zb2) {
				ztop = zb1;
				i1++;
				za1 = crownRadius1[i1][1];
				ra1 = crownRadius1[i1][0];
				if (i1==n1-1) {//treetop
					zb1 = za1+0.01;
					rb1 = ra1;
				} else {
					zb1 = crownRadius1[i1+1][1];
					rb1 = crownRadius1[i1+1][0];
				}
			}
			if (zb2<=zb1) {
				ztop = zb2;
				i2++;
				za2 = crownRadius2[i2][1];
				ra2 = crownRadius2[i2][0];
				if (i2==n2-1) {//treetop
					zb2 = za2+0.01;
					rb2 = ra2;
				} else {
					zb2 = crownRadius2[i2+1][1];
					rb2 = crownRadius2[i2+1][0];
				}
			}
			
			// computation of r1mean = mean de r1low et r1top
			r1low = r1top;
			if (zb1 <= za1) {
				throw new Exception(
						"CrownAvoidancePattern.computeCrownIntersection: height in crownRadius array should increase");
			}
			if (ztop<za1 || ztop>zb1) {
				throw new Exception(
				"CrownAvoidancePattern.computeCrownIntersection: 1: ztop,za,zb:"+ztop+" "+za1+" "+zb1);
			}
			r1top = (ra1 * (zb1 - ztop) + rb1 * (ztop - za1))
			/ (zb1 - za1);
			
			r1mean = 0.5 * (r1low+r1top);
			
			r2low = r2top; 
			if (zb2 <= za2) {
				throw new Exception(
						"CrownAvoidancePattern.computeCrownIntersection: height in crownRadius array should increase");
			}
			if (ztop<za2 || ztop>zb2) {
				throw new Exception(
				"CrownAvoidancePattern.computeCrownIntersection: 2: ztop,za,zb:"+ztop+" "+za2+" "+zb2);
			}
			//System.out.println("	r2mean:r2low="+r2low+" r2top="+r2top);
			//System.out.println("	r2mean:ra2="+ra2+" rb2="+rb2+" ztop="+ztop+" za2="+za2+" zb2="+zb2);
			r2top = (ra2 * (zb2 - ztop) + rb2 * (ztop - za2))
			/ (zb2 - za2);
			r2mean = 0.5 * (r2low+r2top);
			r1max = Math.max(r1max, r1mean);
			r2max = Math.max(r2max, r2mean);
			double tempres = (ztop - zlow)
			* computeDiskAreaIntersection(d, r1mean, r2mean);
			//System.out.println("	tempres="+tempres+": r1mean="+r1mean+" r2mean="+r2mean);
			res += tempres;
			//res += (ztop - zlow)
			//			* computeDiskAreaIntersection(d, r1mean, r2mean);
		}
	//	if (res>0d)
	//	System.out.println("crownVolumeIntersec="+res+"  d="+d+ " r1max="+r1max+" r2max="+r2max);
		return res;
		
	}
	/**
	 * This method computes the intersection of two crowns at distance d
	 * 
	 * @param d
	 * @param crownRadius1
	 *            array of crownRadius at a given height (should increase...)
	 * @param crownRadius2
	 * @throws Exception
	 */
	static public double computeCrownDistance(double d,
			double[][] crownRadius1, double[][] crownRadius2) throws Exception {
		double intersection = computeCrownIntersection(d, crownRadius1,
				crownRadius2);
		
		if (intersection > 0) {
			double r1 = crownRadius1[0][0];
			double r2 = crownRadius2[0][0];
			double r1max = r1;
			double r2max = r2;
			for (int i1 = 0; i1 < crownRadius1.length; i1++) {
				r1 = crownRadius1[i1][0];
				r1max=Math.max(r1, r1max);
				for (int i2 = 0; i2 < crownRadius2.length; i2++) {
				
					r2 = crownRadius2[i2][0];
					r2max=Math.max(r2, r2max);
				}
			}
			
			
			//System.out.println("int=" + intersection+"  d="+d+"  r1="+r1max+"  r2="+r2max);
			return -1;
		} else {
			int n1 = crownRadius1.length;
			int n2 = crownRadius2.length;
 

			if (n1 == 1 && n2 == 1) // just one crown radius is provided
				return d - crownRadius1[0][0] - crownRadius2[0][0];
			if (n1 <= 1 || n2 <= 1) {
				throw new Exception(
						"CrownAvoidancePattern.computeCrownIntersection: crownRadius array is not defined");
			}
			double h1 = crownRadius1[0][1];
			double h2 = crownRadius2[0][1];
			double r1 = crownRadius1[0][0];
			double r2 = crownRadius2[0][0];
			double r1max = r1;
			double r2max = r2;
			double dist = Math.sqrt((d - r1 - r2) * (d - r1 - r2) + (h1 - h2)
					* (h1 - h2));
			for (int i1 = 0; i1 < n1; i1++) {
				r1 = crownRadius1[i1][0];
				h1 = crownRadius1[i1][1];
				r1max=Math.max(r1, r1max);
				for (int i2 = 0; i2 < n2; i2++) {
					h2 = crownRadius2[i2][1];
					r2 = crownRadius2[i2][0];
					dist = Math.min(dist, Math.sqrt((d - r1 - r2)
							* (d - r1 - r2) + (h1 - h2) * (h1 - h2)));
					r2max=Math.max(r2, r2max);
				}
			}
			if (dist<= d - r1max -r2max) {
				throw new Exception(
				"CrownAvoidancePattern.computeCrownIntersection: dist bet crown lower than d-r1-r2");
			}
			return dist;
		}
	}

	/**
	 * This method computes the intersection of two disk which center are at
	 * distance d and of ray r1 and r2 // derived from
	 * http://mathworld.wolfram.com/Circle-CircleIntersection.html
	 * 
	 * @param d
	 * @param r1
	 * @param r2
	 * @return
	 */
	static private double computeDiskAreaIntersection(double d, double r1,
			double r2) {
		
		if (d > r1 + r2)
			return 0d;
		if (r1 + d <= r2 || r2 + d <= r1)
			return Math.PI * Math.pow(Math.min(r1, r2), 2d);
		if (r1 <= 0d || r2 <= 0d)
			return 0d;
		double temp = (d * d + r1 * r1 - r2 * r2) / (2d * d * r1);
		double res = r1 * r1 * Math.acos(temp);
		temp = (d * d + r2 * r2 - r1 * r1) / (2d * d * r2);
		res += r2 * r2 * Math.acos(temp);
		temp = (-d + r1 + r2) * (d + r1 - r2) * (d - r1 + r2) * (d + r1 + r2);
		res += -0.5 * Math.sqrt(temp);
		if (res == Double.NaN)
			System.out.println(" 	cdki:d=" + d + ";r1=" + r1 + ";r2=" + r2
				+ "   res=" + res);
		return res;
	}
	
	
}   
	
