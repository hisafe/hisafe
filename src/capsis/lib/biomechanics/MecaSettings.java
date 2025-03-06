/* 
 * Biomechanics library for Capsis4.
 * 
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import capsis.kernel.AbstractCloneable;

/**
 * MecaSettings - Settings for biomechanics stuff.
 *
 * @author Ph. Ancelin - October 2001
 */
public class MecaSettings extends AbstractCloneable {
//public class MecaSettings extends AbstractSettings {

	// fc-14.10.2020 Replaced AbstractSettings by AbstractCloneable to separate
	// simple tools from Capsis modules' architecture 1.0	
	// checked for c4.1.1_08 - fc-4.2.2003
	
	// Default settings
	public static final double WOOD_DENSITY = 0.8; // g.cm-3
	public static final double YOUNG_MODULUS = 6300d; // MPa
	public static final double CROWN_DENSITY = 0.9; // kg.m-3
	public static final double CROWN_STEM_RATIO = 0.5; // 50 %
	public static final double CROWN_DRAG_COEFFICIENT = 0.35;
	public static final double CDC_VARIATION = 0.0;

	public double woodDensity = WOOD_DENSITY;
	public double youngModulus = YOUNG_MODULUS;
	public double crownDensity = CROWN_DENSITY;
	public double crownStemRatio = CROWN_STEM_RATIO;
	public double crownDragCoefficient = CROWN_DRAG_COEFFICIENT;
	public double cdcVariation = CDC_VARIATION;

}


