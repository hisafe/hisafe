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
package capsis.util.extendeddefaulttype;

import java.io.IOException;
import java.io.Serializable;
import java.security.InvalidParameterException;
import java.util.Map;
import java.util.TreeMap;

import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.util.EnumProperty;

/**
 * This AbstractConstantInitialParameters class serves for the heavy implementation
 * of constant parameters.
 * @author Mathieu Fortin - October 2013
 */
public abstract class ExtConstantInitialParameters implements Serializable {

	protected static enum MessageID implements TextableEnum {
		SpeciesGroup("Species group", "Groupe d'esp\u00E8ces");

		
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
		
	public Map<Integer, EnumProperty> groupTagMap = new TreeMap<Integer, EnumProperty>();
	public EnumProperty groupTagSpecimen;
	
	private String pathname;
	
	/**
	 * Protected constructor.
	 * @param pathname the path for the constant parameters
	 * @throws IOException 
	 */
	protected ExtConstantInitialParameters(String pathname) {
		this.pathname = pathname;
		try {
			init();
		} catch (IOException e) {
			throw new InvalidParameterException("An error occurred while reading the constant initial parameters");
		}
	}

	protected String getPathname() {return pathname;}
	
	/**
	 * This method loads the constant parameters.
	 * @throws IOException
	 */
	protected abstract void init() throws IOException;

}