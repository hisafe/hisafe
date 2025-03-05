/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2020 INRA 
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski. 
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
 * along with Capsis. If not, see <http://www.gnu.org/licenses/>.
 */
package capsis.extension.memorizer;

import java.io.BufferedReader;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Memorizer;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * A Project Memorizer based on a list of dates.
 * 
 * @author F. de Coligny - May 2020
 */
public class FlexibleMemorizer implements Memorizer {

	static {
		Translator.addBundle("capsis.extension.memorizer.FlexibleMemorizer");
	}

	// We use a set to be faster on contains ()
	private Set<Integer> dates;

	/**
	 * Default constructor.
	 */
	public FlexibleMemorizer() {
		// dates is never null
		dates = new HashSet<>();
	}

	/**
	 * Constructor 2, preferred.
	 */
	public FlexibleMemorizer(String fileName) throws Exception {
		dates = new HashSet<>();

		try {
			BufferedReader in = new BufferedReader(new FileReader(fileName));

			String line;
			while ((line = in.readLine()) != null) {

				int i = Integer.parseInt(line);
				dates.add(i);

			}

			in.close();

		} catch (Exception e) {
			Log.println(Log.ERROR, "FlexibleMemorizer.c (fileName)",
					"Error while reading from file (expected an int per line): " + fileName, e);
			throw e;
		}

		if (dates.isEmpty()) {
			Log.println(Log.ERROR, "FlexibleMemorizer.c (fileName)",
					"Could not find dates in this file (expected an int per line): " + fileName);
			throw new Exception("Could not find dates in this file (expected an int per line): " + fileName);
		}

	}

	/**
	 * Constructor 3.
	 */
	public FlexibleMemorizer(Set<Integer> dates) {
		this.dates = dates;
	}

	/**
	 * Constructor 4. dates is a string containing dates separated by separator
	 * (e.g. a blank).
	 */
	public FlexibleMemorizer(String dates, String separator) throws Exception {

		if (dates == null)
			throw new Exception("FlexibleMemorizer, error in constructor, dates is null");
		if (separator == null)
			throw new Exception("FlexibleMemorizer, error in constructor, separator is null");
		if (dates.isEmpty())
			throw new Exception("FlexibleMemorizer, error in constructor, dates is empty");

		Set<Integer> list = new HashSet<>();

		for (StringTokenizer st = new StringTokenizer(dates, separator); st.hasMoreTokens();) {

			String token = st.nextToken();

			try {
				int i = Integer.parseInt(token);
				list.add(i);
			} catch (Exception e) {
				throw new Exception(
						"FlexibleMemorizer, error in constructor, could not parse this date into an integer: " + token);
			}
		}

		this.dates = list;
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;

		} catch (Exception e) {
			Log.println(Log.ERROR, "FlexibleMemorizer.matchWith ()", "Exception in matchWith () method, returned false",
					e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("FlexibleMemorizer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FlexibleMemorizer.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	public Set<Integer> getDates() {
		return dates;
	}

	public List<Integer> getSortedDates() {
		List<Integer> copy = new ArrayList<>(dates);
		Collections.sort(copy);
		return copy;
	}

	public void setDates(Set<Integer> dates) {
		this.dates = dates;

		reinit();

	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public void reinit() {
		// nothing here
	}

	/**
	 * Memorizer interface. Memorization strategy: memorizes only the planned
	 * dates.
	 */
	@Override
	public void memorize(Project project, Step fatherStep, Step newStep) {

		if (fatherStep == null) {
			project.tieStep(fatherStep, newStep);

		} else if (fatherStep.isRoot() || dates.contains(fatherStep.getScene().getDate())) {
			project.tieStep(fatherStep, newStep);

		} else {
			project.replaceStep(fatherStep, newStep);

		}

	}

	/**
	 * Memorizer interface.
	 */
	@Override
	public String getCaption() {
		String caption = getName() + "(#dates: " + dates.size()+")";
		return caption;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public String toString() {
		return getCaption();
	}

}
