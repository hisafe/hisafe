/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.commongui.projectmanager;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import jeeb.lib.util.ListMap;

/**
 * RelatedColorProvider provides user with colors. Main colors are supposed to
 * be very different and related colors are supposed to be close. See
 * http://www.colorschemedesigner.com/ to choose nice color palettes.
 * 
 * @author F. de Coligny - September 2012
 */
public class RelatedColorProvider {

	// fc-5.11.2020 Reproductible draws
	static Random random = new Random(1);

	static String[] bag1 = { "8346A4", "D5ABEC", "684DA9", "C2AFED", "BC4A97", "F2ABDB" };
	static String[] bag2 = { "D46A6A", "AA3939", "801515", "D49A6A", "AA6C39", "804515" };
	static String[] bag3 = { "4F628E", "2E4272", "162955", "615192", "403075", "261758" };
	static String[] bag4 = { "55AA55", "2D882D", "116611", "407F7F", "226666", "0D4D4D"};
	static String[] bag5 = { "AA5585", "882D61", "661141", "D46A6A", "AA3939", "801515"};
	static String[] bag6 = { "D49A6A", "AA6C39", "804515", "D4B16A", "AA8439", "805C15"};
	static String[] bag7 = { "407F7F", "226666", "0D4D4D", "4F628E", "2E4272", "162955"};

//	static String[] bag1 = { "850bee", "8346A4", "D5ABEC", "684DA9", "C2AFED", "BC4A97", "F2ABDB" };
//	static String[] bag2 = { "fb003a", "D46A6A", "AA3939", "801515", "D49A6A", "AA6C39", "804515" };
//	static String[] bag3 = { "0f5eee", "4F628E", "2E4272", "162955", "615192", "403075", "261758" };
//	static String[] bag4 = { "339415" , "55AA55", "2D882D", "116611", "407F7F", "226666", "0D4D4D"};
//	static String[] bag5 = { "ff00ff" , "AA5585", "882D61", "661141", "D46A6A", "AA3939", "801515"};
//	static String[] bag6 = { "ffa500" , "D49A6A", "AA6C39", "804515", "D4B16A", "AA8439", "805C15"};
//	static String[] bag7 = { "40e0d0" , "407F7F", "226666", "0D4D4D", "4F628E", "2E4272", "162955"};

//	static private String[][] defaultHexs = { { "850bee", "fb003a", "0f5eee", "339415", "ff00ff", "808080", "d7acfb",
//		"ffacbf", "b4cdfa", "64e33c", "ffb3ff", "dadada" } };

	private List<Color> mainColors;

	// Key is the first color, this color is also the first value in the matching
	// list
	private ListMap<Color, Color> colorMap;

	private int mainIndex;

	/**
	 * Default constructor, relies on default colors.
	 */
	public RelatedColorProvider() {
		init();
	}

	private void init() {

		// Create the map, add the color bags in it, n bags -> n entries
		colorMap = new ListMap<>();
		mainColors = new ArrayList<>();

		addBag(bag1);
		addBag(bag2);
		addBag(bag3);
		addBag(bag4);
		addBag(bag5);
		addBag(bag6);
		addBag(bag7);

		mainIndex = 0;

	}

	public Color getRelatedColor(Color someColor, int rank) {

		try {
			List<Color> list = colorMap.get(someColor);
			int n = list.size();
			int candidateIndex = rank % n;
			Color candidateColor = list.get(candidateIndex);
			if (candidateColor != null)
				return candidateColor;
			
		} catch (Exception e) {
			// See security below
		}

		// Not found, do differently
		// (there may be a bag with one single color and same then someColor)

		// Get a related color, e.g. change blue and alpha
//		Color color = new Color(someColor.getRed(), someColor.getGreen(), random.nextInt(255), random.nextInt(255));

		// fc-9.11.2020 Other option (was in AbstractDataExtractor.getcolor ()
		// Color families for a single step : 6 different (1/6 = 0.17)
		float[] hsb = ColorManager.getHSB(someColor);
		Color color = ColorManager.getColor(hsb[0] + (rank + 1) * 0.17f, hsb[1] / 3f, hsb[2]); // / 3f
//		Color color = ColorManager.getColor(hsb[0] + (rank + 1) * 0.17f, hsb[1], hsb[2]);

		if (someColor.equals(color))
				System.out.println("RelatedColorProvider getRelatedColor () returned same color, rank; "+rank);
		
		return color;

	}

	/**
	 * Draws and returns a main color. The returned colors by successive calls
	 * should be very different.
	 */
	public Color getMainColor() {

		int nEntries = mainColors.size();
		int i = mainIndex % nEntries;

		// For next call
		mainIndex++;

		return mainColors.get(i);

	}

	/**
	 * Decodes a bag of colors and add it in a new entry in colorMap
	 */
	private void addBag(String[] encodedBag) {

		// First color will be the main color for this color bag
		Color mainColor = null;

		for (int j = 0; j < encodedBag.length; j++) {

			try {
				String hexCode = encodedBag[j]; // may fail
				int[] rgb = hexStringToIntArray(hexCode);

				Color c = new Color(rgb[0], rgb[1], rgb[2]);
				if (mainColor == null)
					mainColor = c;

				colorMap.addObject(mainColor, c);

			} catch (Exception e) {
			} // no matter

		}

		mainColors.add(mainColor);

	}

	/**
	 * int[] rgb = hexStringToIntArray(hexCode);
	 */
	private static int[] hexStringToIntArray(String s) {
		int len = s.length();
		int[] data = new int[len / 2];
		for (int i = 0; i < len; i += 2) {
			data[i / 2] = (int) ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16));
		}
		return data;
	}

}
