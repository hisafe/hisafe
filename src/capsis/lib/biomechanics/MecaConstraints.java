/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.lib.biomechanics;

import capsis.kernel.AbstractCloneable;

/**
 * MecaConstraints - Constraints to apply on trees.
 *
 * @author Ph. Ancelin - October 2001
 */
public class MecaConstraints extends AbstractCloneable {
//public class MecaConstraints extends AbstractSettings {

	// fc-14.10.2020 Replaced AbstractSettings by AbstractCloneable to separate
	// simple tools from Capsis modules' architecture 1.0	
	// checked for c4.1.1_08 - fc-3.2.2003

	// Default settings
	public static final double WIND_SPEED_EDGE_AT_H = 20; // m/s (old: km.h-1)
	public static final double WIND_SPEED_EDGE_AT_10M = 45; // m/s
	public static final String LOCATION = "stand"; // edge (default) or stand
	public static final String STAND_HEIGHT = "mean"; // mean (default) or dom
	public static final boolean WIND_AT_10M = true;

	public double windSpeedEdgeAtH = WIND_SPEED_EDGE_AT_H;
	public double windSpeedEdgeAt10m = WIND_SPEED_EDGE_AT_10M; // � 10 m pour la dynamique et sensibilit� !!!!!
	public String location = LOCATION;
	public String standHeight = STAND_HEIGHT;
	public boolean windAt10m = WIND_AT_10M;
}
