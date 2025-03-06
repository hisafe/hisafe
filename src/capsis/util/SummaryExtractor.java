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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.kernel.GModel;
import capsis.kernel.Step;
import capsis.kernel.automation.AfterAutomation;
import capsis.kernel.automation.AutomationSummary;
import capsis.kernel.automation.VarColumn;
import jeeb.lib.util.Log;
import jeeb.lib.util.StringUtil;
import jeeb.lib.util.Translator;

/**
 * Base class for step summary synthesis.
 * 
 * @author S. Dufour-Kowalski - January 2010
 */
public class SummaryExtractor {

	private Map<String, Method> methodMap;
	private Map<String, Method> afterMap;
	private Map<String, Method> varMap;

	/** Constructor */
	public SummaryExtractor(Class<?> c) {

		if (c == null) {
			return;
		}
		methodMap = new HashMap<String, Method>();
		afterMap = new HashMap<String, Method>();
		varMap = new HashMap<String, Method>();

		Method[] ms = c.getDeclaredMethods();

		// parse methods
		for (Method m : ms) {
			int modifiers = m.getModifiers();
			Class<?> parameters[] = m.getParameterTypes();

			// AfterAutomation methods
			if (m.getAnnotation(AfterAutomation.class) != null) {

				if (Modifier.isStatic(modifiers) && parameters.length == 2 && parameters[0] == GModel.class
						&& parameters[1] == AutomationSummary.class) {
					afterMap.put(m.getName(), m);
				}

			} else {

				if (Modifier.isStatic(modifiers) && parameters.length == 2 && parameters[0] == GModel.class
						&& parameters[1] == Step.class) {
					// normal or variable method ?
					if (m.getAnnotation(VarColumn.class) != null) {
						varMap.put(m.getName(), m);
					} else {
						methodMap.put(m.getName(), m);
					}
				}
			}
		}
	}

	/** Return a list of summary Strings for a step */
	public List<String> getSummaryHeaders(GModel mod) {

		Map<String, Method> map = methodMap;
		List<String> ret = new ArrayList<String>();

		try {
			for (String key : map.keySet()) {
				Method m = map.get(key);

				Description d = m.getAnnotation(Description.class);
				String title = key;
				if (d != null) {
					title = Translator.swap(d.val());
				}
				ret.add(title);
			}
		} catch (Exception e) {
			// fc-22.7.2019 added try / catch to manage exceptions (PP3)
		}

		return ret;
	}

	/** Return list of objects for a step */
	public List<Object> getStepSummary(GModel mod, Step s) {

		List<Object> ret = new ArrayList<Object>();
		Map<String, Method> map = methodMap;

		try {
			for (String key : map.keySet()) {
				Method m = map.get(key);
				m.setAccessible(true);

				try {
					Object o = m.invoke(null, mod, s);
					ret.add(o);

				} catch (InvocationTargetException e) {
					Log.println(Log.ERROR, "Summary", "getStepSummary", e.getCause());
					ret.add("error");
				} catch (Exception e) {

					e.printStackTrace();
				}
			}
		} catch (Exception e) {
			// fc-22.7.2019 added try / catch to manage exceptions (PP3)
		}

		return ret;
	}

	/** Return a map corresponding to the variable columns */
	public Map<String, Object> getVarStepSummary(GModel mod, Step s) {

		Map<String, Object> ret = new HashMap<String, Object>();
		Map<String, Method> map = varMap;

		for (String key : map.keySet()) {
			Method m = map.get(key);
			m.setAccessible(true);

			Description d = m.getAnnotation(Description.class);
			String title = key;
			if (d != null) {
				title = Translator.swap(d.val());
			}

			try {
				Map<String, Object> o = (Map<String, Object>) m.invoke(null, mod, s);

				// add prefix in title
				for (String k : o.keySet()) {
					ret.put(title + "_" + k, o.get(k));
				}

			} catch (InvocationTargetException e) {
				Log.println(Log.ERROR, "Summary", "getStepSummary", e.getCause());

			} catch (Exception e) {

				e.printStackTrace();
			}
		}

		return ret;
	}

	/** Return summary String for a step */
	public String getStepSummaryString(GModel mod, Step s) {

		// fc-4.5.2020 Added if (!isEmpty ()) to avoid return \n\n
		String ret = "";
		
		List<String> h = getSummaryHeaders(mod);
		if (!h.isEmpty ())
			ret += StringUtil.join(h, "\t") + "\n";
		
		List<Object> sm = getStepSummary(mod, s);
		if (!sm.isEmpty())
			ret += StringUtil.join(sm, "\t") + "\n";
		
		return ret;
	}

	public void callAfterAutomation(GModel mod, AutomationSummary output) {

		Map<String, Method> map = afterMap;

		for (String key : map.keySet()) {
			Method m = map.get(key);
			m.setAccessible(true);

			try {
				m.invoke(null, mod, output);

			} catch (InvocationTargetException e) {
				Log.println(Log.ERROR, "Summary", "callAfterAutomation", e.getCause());

			} catch (Exception e) {

				e.printStackTrace();
			}
		}

	}

}
