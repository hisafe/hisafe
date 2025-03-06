package capsis.lib.cstability.util;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * A class to convert dateString to list of date
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
public class Date {

	/**
	 * read()
	 */
	public static List<Integer> read(String dateString) throws Exception {

		List<Integer> dates = new ArrayList<>();

		String s = dateString.replace("[", "");
		s = s.replace("]", "");
		// à ce stade, s est de la forme x,y,z où x y et z peuvent être des entiers ou
		// de la forme a:b:c

		StringTokenizer st = new StringTokenizer(s, ",");

		while (st.hasMoreElements()) {
			String temp = st.nextToken().trim();
			if (temp.contains(":")) {
				StringTokenizer st2 = new StringTokenizer(temp, ":");
				Integer min = Integer.parseInt(st2.nextToken().trim());
				Integer step = Integer.parseInt(st2.nextToken().trim());
				Integer max = Integer.parseInt(st2.nextToken().trim());
				if (max < min)
					throw new Exception("Date.read(), a dateString element " + temp + " is not readable");

				Integer val = min;
				while (val < max) {
					dates.add(val);
					val += step;
				}
				dates.add(max);

			} else {
				dates.add(Integer.parseInt(temp));
			}
		}

		return dates;
	}

}
