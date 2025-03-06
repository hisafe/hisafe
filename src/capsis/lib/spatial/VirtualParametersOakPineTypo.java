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

import capsis.kernel.AbstractSettings;
import jeeb.lib.util.Automatable;
import jeeb.lib.util.annotation.Ignore;
import jeeb.lib.util.annotation.RecursiveParam;

/**
 * VirtualParametersOakPineTypo - List of parameters for the simulation of Oak Pine mixed stands from typology.
 * May have defaults or not. May be modified by user action during 
 * initialization process in interactive mode.
 *
 * @author F. Goreaud - 12/06/07
 */
    public class VirtualParametersOakPineTypo extends AbstractSettings implements Automatable {
   
   // Default values
   // Here we add all the parameters used to simulate virtual mixed stands.
   
      public boolean virtualStand = true;
      public double virtualStandXmin = 0;
      public double virtualStandXmax = 100;
      public double virtualStandYmin = 0;
      public double virtualStandYmax = 100;

	// canopy		
		public int numberOak = 50;
		public int numberPine = 100;
      public int type = 1;
		
	// for type 1 (NS3 model)
	@Ignore
   	public int Type1NbAgOak = 7;	// nb of agregates of oaks
	@Ignore
 		public double Type1ROak = 17;	// radius of agregates of oaks
	@Ignore
    	public int Type1NbAgPine = 13;	// nb of agregates of pines
	@Ignore
 		public double Type1RPine = 18;	// radius of agregates of pines
	@Ignore
 		public double Type1DistIntertype = 18; // l : distance of intertype repulsion
	@Ignore
 		public double Type1DistPine = 5; // ll : distance of regularity for pines

	// for type 2 (NS3 model)
 	@Ignore
   	public int Type2NbAgOak = 7;	// nb of agregates of oaks
	@Ignore
 		public double Type2ROak = 20;	// radius of agregates of oaks
 	@Ignore
   	public int Type2NbAgPine = 23;	// nb of agregates of pines
	@Ignore
 		public double Type2RPine = 16;	// radius of agregates of pines
	@Ignore
 		public double Type2DistIntertype = 6; // l : distance of intertype repulsion
	@Ignore
 		public double Type2DistPine = 3; // ll : distance of regularity for pines
	
	// for type 3 (HC2d model)
 	@Ignore
   	public int Type3NbAgPine = 38;	// nb of agregates of pines
	@Ignore
 		public double Type3RPine = 8;	// radius of agregates of pines
	@Ignore
 		public double Type3DistIntertype = 4; // l : distance of intertype repulsion
	@Ignore
 		public double Type3DistPine = 10; // ll : distance of regularity for pines
	@Ignore
 		public double Type3Proba = 0.15; // proba : repulsion probability for r<l

	// for type 4 (HC2b model)
 	@Ignore
   	public int Type4NbAgPine = 10;	// nb of agregates of pines
	@Ignore
 		public double Type4RPine = 14;	// radius of agregates of pines
	@Ignore
 		public double Type4DistIntertype = 12; // l : distance of intertype repulsion
	@Ignore
 		public double Type4DistPine = 4; // ll : distance of regularity for pines

	// D&H for Oak
	@RecursiveParam
	public VirtualParameters OakParam;	

	// D&H for Pine
	@RecursiveParam
	public VirtualParameters PineParam;	
 
 
 	// understorey		
		public int numberOakUnder = 100;
		public int numberPineUnder = 10;
      public int typeUnder = 1;

	// for type 1 (SE7 model)
	@Ignore
    	public int UnderType1NbAgOak = 15;	// nb of agregates of oaks
	@Ignore
 		public double UnderType1ROak = 10;	// radius of agregates of oaks
	@Ignore
 		public double UnderType1DistRepOak = 9; //l2 distance of intertype repulsion with oaks from canopy
	@Ignore
 		public double UnderType1DistRepPine = 2; //l3 distance of intertype repulsion with pines from canopy

	// for type 2 (SE6 model)
 	@Ignore
   	public int UnderType2NbAgOak = 37;	// nb of agregates of oaks
	@Ignore
 		public double UnderType2ROak = 12;	// radius of agregates of oaks
	@Ignore
 		public double UnderType2DistAttOak = 52; // distance of intertype attraction with oaks from canopy
	@Ignore
 		public double UnderType2DistRepPine = 2; // distance of intertype repulsion with pines from canopy

	// for type 3 (SE5 model)
 	@Ignore
   	public int UnderType3NbAgOak = 44;	// nb of agregates of oaks
	@Ignore
 		public double UnderType3ROak = 9;	// radius of agregates of oaks
	@Ignore
 		public double UnderType3DistRepOak = 4; // distance of intertype repulsion with oaks from canopy
	@Ignore
 		public double UnderType3DistAttPine = 14; // distance of intertype attraction with pines from canopy

	// D&H for Oak
	@RecursiveParam
	public VirtualParameters OakParamUnder;	

	// D&H for Pine
	@RecursiveParam
	public VirtualParameters PineParamUnder;	
  
   }



