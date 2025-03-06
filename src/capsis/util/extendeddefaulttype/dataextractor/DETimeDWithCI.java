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

import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.kernel.extensiontype.GenericExtensionStarter;

public class DETimeDWithCI extends AbstractDETimeWithCI {
	
	protected enum MessageID implements TextableEnum {
		Description("Dominant diameter with confidence intervals", "Diam\u00E8tre dominant avec intervalles de confiances"),
		Name("CI - D0", "IC - D0"),
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

	/**
	 * Phantom constructor. Only to ask for extension properties (authorName,
	 * version...).
	 */
	public DETimeDWithCI() {
		super(Variable.D);
	}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DETimeDWithCI(GenericExtensionStarter s) {
		super(s, Variable.D);
		methodProvider = step.getProject().getModel().getMethodProvider(); // fc-30.11.2011
	}

	@Override
	public String getText() {
		return text;
	}

	@Override
	protected String getYAxisLabelName() {
		return variable.toString();
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + MessageID.Name;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return MessageID.Description.toString();
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

}

