/*
 * This file is part of the lerfob-foresttools library.
 *
 * Copyright (C) 2010-2013 Frederic Mothe and Mathieu Fortin 
 * for LERFOB INRA/AgroParisTech, 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed with the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * Please see the license at http://www.gnu.org/copyleft/lesser.html.
 */
package capsis.util.nutrientmodel;

/**
 * The NutrientExtendedList interface incompasses some other nutrients that are not considered in 
 * this model. The carbon is a typical example.
 * @author Mathieu Fortin - April 2013
 */
public interface NutrientExtendedList {
	
	/**
	 * This method returns the name of the nutrient.
	 * @return a single-character string
	 */
	public String name();

}
