/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
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

package capsis.lib.samsaralight.tag;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * A superclass for tags in the SamsaraLight library. Tag mode is optional,if
 * activated, detailed results at tag level are provided by the library in the
 * lightable objects (trees, cells). E.g. if some beams are tagged
 * "BUD_BURST_DATE" and tag mode is activated, it will be possible to get the
 * results directly associated with these beams.
 * 
 * It is possible to build subclasses of this Tag with extra properties, e.g.
 * name, doy, hour.
 * 
 * @author F. de Coligny, Frédéric André - April 2017
 */
public class SLTag implements Serializable {

	// Tags are optional, depend on slSettings.tagMode
	// LEGACY: historical process, without tags // fc+fa-24.4.2017

	// Beams tags
	// Possible to create user tags with free names
	public static final SLTag LEGACY = new SLTag ("LEGACY"); 
	
	// Interception tags // fc+fa-19.5.2017
	public static final SLTag TRUNK = new SLTag ("TRUNK"); 
	public static final SLTag CROWN = new SLTag ("CROWN"); 
	
	// Energy tags
	public static final SLTag DIRECT = new SLTag ("DIRECT"); 
	public static final SLTag DIFFUSE = new SLTag ("DIFFUSE"); 
	public static final SLTag CROWN_POTENTIAL = new SLTag ("CROWN_POTENTIAL"); 
	public static final SLTag LOWER_CROWN_POTENTIAL = new SLTag ("LOWER_CROWN_POTENTIAL"); 
	public static final SLTag CROWN_POTENTIAL_DIRECT = new SLTag ("CROWN_POTENTIAL_DIRECT"); 
	public static final SLTag LOWER_CROWN_POTENTIAL_DIRECT = new SLTag ("LOWER_CROWN_POTENTIAL_DIRECT"); 
	public static final SLTag CROWN_POTENTIAL_DIFFUSE = new SLTag ("CROWN_POTENTIAL_DIFFUSE"); 
	public static final SLTag LOWER_CROWN_POTENTIAL_DIFFUSE = new SLTag ("LOWER_CROWN_POTENTIAL_DIFFUSE"); 
	
	// fa-08.01.2018: Orientation tags, for target (cell, sensor...)
	public static final SLTag HORIZONTAL = new SLTag ("HORIZONTAL"); 
	public static final SLTag SLOPE = new SLTag ("SLOPE"); 
	
	private static Set<SLTag> predefinedTags;
	
	static {
		predefinedTags = new HashSet<> ();
		predefinedTags.add(LEGACY);
		predefinedTags.add(DIRECT);
		predefinedTags.add(DIFFUSE);
		predefinedTags.add(CROWN_POTENTIAL);
		predefinedTags.add(LOWER_CROWN_POTENTIAL);
		predefinedTags.add(CROWN_POTENTIAL_DIRECT);
		predefinedTags.add(LOWER_CROWN_POTENTIAL_DIRECT);
		predefinedTags.add(CROWN_POTENTIAL_DIFFUSE);
		predefinedTags.add(LOWER_CROWN_POTENTIAL_DIFFUSE);
		predefinedTags.add(HORIZONTAL); // fa-08.01.2018
		predefinedTags.add(SLOPE); // fa-08.01.2018
		
	}
	
	public String name;

	public SLTag(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public boolean isPredefinedTag () {
		return predefinedTags.contains(this);
	}
	
	public boolean isUserTag () {
		return !isPredefinedTag();
	}
	
	/**
	 * Two tags are equal if their name is the same.
	 */
	@Override
	public boolean equals(Object o) {
		SLTag other = (SLTag) o;
		return this.name.equals(other.getName());
	}

	@Override
	public String toString() {
		return "SLTag " + name;
	}
}














