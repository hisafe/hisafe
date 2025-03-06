/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
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

package capsis.util;

import java.util.StringTokenizer;

/**
 * A line with known format : key = value. Allows to retrieve the value 
 * for a known key.
 *
 * @author F. de Coligny
 */
public class SettingsLine {
	private String line;

	public SettingsLine (String str) {
		line = str.trim ();
	}

	public String getValue (String keyword) throws Exception {
		String value = "";
		int begin = line.indexOf (keyword);
		if (begin == -1) {
			throw new Exception ("SettingsLine.getValue (): keyword <"+keyword+"> not found in line <"+line+">.");
		}
		int afterK = begin + keyword.length ();
		int i = afterK;
		while (line.charAt (i) == ' ' ||
				line.charAt (i) == '=' ||
				line.charAt (i) == '\t' ||
				line.charAt (i) == '\n') {
			i++;
		}

		// value is at the beginning of this String
		String toExplore = line.substring (i);

		// we find a " => format: <keyword = "value"> or <keyword "value"> (...)
		if (line.charAt (i) == '"') {
			StringTokenizer st = new StringTokenizer (toExplore, "\"");
			if (st.hasMoreTokens ()) {
				value = st.nextToken ().trim ();
			} else {
				throw new Exception ("SettingsLine.getValue (): lacking \" in value field for keyword <"
					+keyword+"> in line <"+line+">.");
			}
		// we find no " => format: <keyword = value> or <keyword value> (...)
		} else {
			StringTokenizer st = new StringTokenizer (toExplore, " ,;\t\"");
			if (st.hasMoreTokens ()) {
				value = st.nextToken ().trim ();
			} else {
				throw new Exception ("SettingsLine.getValue (): wrong value field for keyword <"
					+keyword+"> in line <"+line+">.");
			}
		}
		return value;
	}


}
