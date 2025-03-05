/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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


 package capsis.extension.dataextractor;

 import capsis.util.methodprovider.MeanDbhIncrementProvider;
import capsis.util.methodprovider.DbhIncrementVariationCoefficientProvider;
import capsis.extension.dataextractor.superclass.DETimeYs;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import jeeb.lib.util.*;
import capsis.kernel.*;
import capsis.defaulttype.*;

import java.util.List;

 /**
  * Mean DBH Increment, versus Date.
  *
  * @author T.Cordonnier, V. Lafond - June 2012
  */
 public class DETimeMeanDbhIncrement2 extends DETimeYs {

     static {
         Translator.addBundle("capsis.extension.dataextractor.DETimeMeanDbhIncrement2");
     }

     // nb-06.08.2018
     //public static final String NAME = "DETimeMeanDbhIncrement2";
     //public static final String DESCRIPTION = "Calculate mean and variation ocefficient of DBH increment Over Time";
     //public static final String AUTHOR = "T.Cordonnier, V. Lafond";
     //public static final String VERSION = "1.1";


     static public boolean matchWith(Object referent) {
         if (!(referent instanceof GModel)) {return false;}
         GModel m = ((GModel)referent);
         MethodProvider p = m.getMethodProvider();
         if (!(p instanceof MeanDbhIncrementProvider))return false;
         if (!(p instanceof DbhIncrementVariationCoefficientProvider))return false;
         GScene s = ((Step) m.getProject().getRoot()).getScene();
         return s instanceof TreeList;
     }
     
    @Override
  	public String getName() {
  		return Translator.swap("DETimeMeanDbhIncrement2.name");
  	}
     
 	@Override
 	public String getAuthor() {
 		return "T.Cordonnier, V. Lafond";
 	}

 	@Override
 	public String getDescription() {
 		return Translator.swap("DETimeMeanDbhIncrement2.description");
 	}

 	@Override
 	public String getVersion() {
 		return "1.1";
 	}
 	
	 @Override
     public String[] getYAxisVariableNames() {
        return new String[] {"mean", "cv"}; // the names of the 2 variables
     }


    @Override
    protected Number getValue(GModel m, GScene stand, int date, int i) { // value for the 'i'th variable
         	TreeList tl = (TreeList) stand;

        if (i <= 0) {
			MeanDbhIncrementProvider p1 = (MeanDbhIncrementProvider)m.getMethodProvider();
         	double mean = p1.getMeanDbhIncrement(stand,tl.getTrees());
            return mean;
        } else {
         	DbhIncrementVariationCoefficientProvider p2 = (DbhIncrementVariationCoefficientProvider)m.getMethodProvider();
         	double cv = p2.getDbhIncrementVariationCoefficient(stand,tl.getTrees());
            return cv;
        }

    }

}

