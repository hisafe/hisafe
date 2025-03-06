/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype.dataextractor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jeeb.lib.util.Log;
import repicea.stats.estimates.MonteCarloEstimate;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.methodprovider.AverageRingWidthCmProvider;

public class DETimeRingWidth extends AbstractDETimeWithCI {

	private static enum MessageID implements TextableEnum {
		yLabel("cm/yr","cm/an"),
		xLabel("Time (yr)", "Temps (ann\u00E9es)"),
		ExtractorName("Average Ring Width / Time", 
				"Largeur de cerne moyenne / Temps"),
		Description("Average ring width (cm) by steps",
				"Largeur de cerne moyenne en fonction des pas de croissance")
		;

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
		
	}
	
	// nb-07.08.2018
	//public static final String AUTHOR = "M. Fortin";
	//public static final String VERSION = "1.0";

	public DETimeRingWidth() {
		super(Variable.AverageRingWidth);
	}
	
	public DETimeRingWidth(GenericExtensionStarter s) {
		super(s, Variable.AverageRingWidth);
	}
	
	@Override
	public List<String> getAxesNames() {
		List<String> v = new ArrayList<String> ();
		v.add(REpiceaTranslator.getString(MessageID.xLabel));
		v.add(getYAxisLabelName());
		return v;
	}

	@Override
	public String getText() {return MessageID.ExtractorName.toString();}

	@Override
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof ExtModel)) {return false;}
			ExtModel m = (ExtModel) referent;
			MethodProvider mp = m.getMethodProvider ();
			if (!(mp instanceof AverageRingWidthCmProvider)) {return false;}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DETimeRingWidth.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}
	
	@Override
	public String getName() {
		return REpiceaTranslator.getString(MessageID.ExtractorName);
	}

	@Override
	public String getAuthor() {
		return "M. Fortin";
	}
	
	public String getDescription() {
		return REpiceaTranslator.getString(MessageID.Description);
	}
	
	@Override
	public String getVersion() {
		return "1.0";
	}
	
	@Override
	protected String getYAxisLabelName() {return MessageID.yLabel.toString();}

	@Override
	protected MonteCarloEstimate getEstimate(GScene stand, Collection trees) {
		AverageRingWidthCmProvider provider = (AverageRingWidthCmProvider) methodProvider;
		return (MonteCarloEstimate) provider.getAverageRingWidthCm(stand, trees);
	}



}
