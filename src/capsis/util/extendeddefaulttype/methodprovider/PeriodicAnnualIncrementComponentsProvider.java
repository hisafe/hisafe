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
package capsis.util.extendeddefaulttype.methodprovider;

import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.kernel.GScene;

/**
 * This interface provides a method that calculates the periodic annual increment components for
 * basal area (e.g. Mortality, survivor growth and recruitment). 
 * @author Mathieu Fortin - July 2010.
 */
@Deprecated
interface PeriodicAnnualIncrementComponentsProvider {
	
	static enum GrowthComponent implements TextableEnum {
		Mortality("Mortality", "Mortalit\u00E9"), 
		SurvivorGrowth("Survivors", "Survivants"), 
		Recruitment("Recruitment", "Recrutement");
		
		GrowthComponent(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
	
		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
	}
	
	static enum Variable implements TextableEnum{
		N("Stem density", "Densit\u00E9 d'arbres"),
		G("Basal area", "Surface terri\u00E8re"),
		V("Volume", "Volume");

		Variable(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}

	}

	/**
	 * This method calculates the periodic annual increment (PAI) components for increment.
	 * Mortality is obtained through the TreeList.getTrees("dead") method and the recruitment is 
	 * obtained by screening the trees with age 0 in the getTrees() collection. Survivor growth is 
	 * calculated as the difference between the total and the sum of mortality and recruitment.
	 * NOTE : the method must return an empty map for the root step and the scenes that result from
	 * interventions.
	 */
	public double getPAIComponents(GScene stand, GrowthComponent component, Variable variable);

}
