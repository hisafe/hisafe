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
package capsis.kernel.extensiontype;

import java.util.Collection;

import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import jeeb.lib.util.extensionmanager.ExtensionInitialData;

/**
 * GenericExtensionStarter replaces DefaultExtensionStarter.
 * 
 * @author S. Dufour - February 2009
 */
public class GenericExtensionStarter extends ExtensionInitialData {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/**
	 * Default constructor
	 */
	public GenericExtensionStarter() {
		super();
	}

	/**
	 * Constructor 2
	 */
	public GenericExtensionStarter(Object... objs) {
		super(objs);
	}

	public void setParam(String s, Object o) {
		put(s, o);
	}

	public Object getParam(String s) {
		try {
			return get(s);
		} catch (Exception e) {
			return null;
		}
	}

	public boolean hasParam(String s) {
		return containsKey(s);
	}

	public Collection getCollection() {
		return (Collection) getParam("collection");
	}

	public void setCollection(Collection c) {
		setParam("collection", c);
	}

	public Step getStep() {
		return (Step) getParam("step");
	}

	public void setStep(Step s) {
		setParam("step", s);
	}

	public GScene getScene() {
		return (GScene) getParam("scene");
	}

	public void setScene(GScene s) {
		setParam("scene", s);
	}

	public GModel getModel() {
		return (GModel) getParam("model");
	}

	public void setModel(GModel m) {
		setParam("model", m);
	}

	public Object getObject() {
		return getParam("object");
	}

	public void setObject(Object o) {
		setParam("object", o);
	}

	public String getString() {
		return (String) getParam("string");
	}

	public void setString(String s) {
		setParam("string", s);
	}

	public String getType() {
		return (String) getParam("type");
	}

	public void setType(String t) {
		setParam("type", t);
	}

}
