/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
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

package safe.extension.ioformat.safeExport;

import java.util.Comparator;

/**
 * Comparator on module, class and description.
 * The compare method deals with two instances of SafeExportParam.
 *
 * @author R. Tuquet Laburre - july 2003
 */
public class SafeExportRecordComparator implements Comparator {

	public int compare (Object param1, Object param2) throws ClassCastException {
		if (!(param1 instanceof SafeExportRecord)) {
				throw new ClassCastException ("param1 is not a SafeExportRecord : "+param1);}
		if (!(param2 instanceof SafeExportRecord)) {
				throw new ClassCastException ("param2 is not a SafeExportRecord : "+param2);}

		SafeExportRecord p1 = (SafeExportRecord) param1;
		SafeExportRecord p2 = (SafeExportRecord) param2;

		if (p1.getKey1() < p2.getKey1()) { return -1; }
		if (p1.getKey1() > p2.getKey1()) { return 1; }
		if (p1.getKey2() < p2.getKey2()) { return -1; }
		if (p1.getKey2() > p2.getKey2()) { return 1; }
		if (p1.getKey3() < p2.getKey3()) { return -1; }
		if (p1.getKey3() > p2.getKey3()) { return 1; }

		return - p1.getRecord().compareToIgnoreCase(p2.getRecord());
	}

}
