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
package capsis.kernel.automation;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.Automatable;
import jeeb.lib.util.ClassUtils;
import jeeb.lib.util.ParamMap;
import jeeb.lib.util.PathManager;
import capsis.kernel.Engine;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import capsis.kernel.extensiontype.OFormat;

import com.thoughtworks.xstream.XStream;
import com.thoughtworks.xstream.mapper.MapperWrapper;

/**
 * A Experiment allows to replay a project
 * @author samuel dufour
 *
 */
public class Automation implements Serializable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	public String name;
	public String model;
	public String memorizer;

	public boolean ok = true; // if false Automation will be not usable
	protected String reason = "";


	/** Initialisation parameters */
	public static class Event implements Serializable  {
		
		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		public String label;
		public ParamMap parameters;
		public int id;

		public Event(int i, Object obj, ParamMap p, String l) {
			id = i;
			label = l;
			parameters = p;

		}

		public String toString() {
			return "[" + label + "] (" + id + ") class : " + parameters.className;
		}

	}


	public Event initEvent;
	public List<Event> events; 
	protected AutomationVariation variation;

	public int repetition = 1;


	/** Constructor */
	public Automation(String name, String model) {

		this.name = name;
		this.model = model;
		events = new ArrayList<Event>();
		variation = new AutomationVariation();

	}


	/** Validity */
	public boolean isValid() {

		return ok;
	}

	/** Accessor */
	public AutomationVariation getVariation() { return variation; }


	/**
	 * Add an event
	 * Object should be automatable
	 * @param stepId : if < 0 -> initial parameters
	 * @param o : Object containing data
	 */
	public Event addEvent(int stepId, Object o, String label)  {

		if((!(o instanceof Automatable))) {
			ok = false;
			reason += o.getClass().getName() + "\n";
			return null;
		}

		ParamMap p = new ParamMap(o);

		// create event
		Event ev;
		if(stepId < 0) {
			ev =  new Event(-1, o, p, label);
			initEvent = ev; 
		} else {
			ev = new Event(stepId, o, p, label);
			events.add(ev);
		}

		return ev;
	}



	/**
	 * return Object for a particular step
	 * @param m
	 * @param s
	 * @return null if there are no evolution for this step
	 * @throws Exception
	 */
	public Object getObject(GModel m, Step s, Event ev) throws Exception {

		// Get Class
		//String className = m.getEvolutionParameterClassName();
		String className = ev.parameters.className;
		Object obj = null;
		if(className == null || className.equals("")) { return null; }


		if(s == null) { // init
			obj = m.getSettings();
		}

		// Instantiate object
		if(obj == null) {
			Class<?> c = m.getClass().getClassLoader().loadClass (className);
			obj  = ClassUtils.instantiateClass(c);
		}

		ev.parameters.setName(name);
		ev.parameters.setRoot(PathManager.getInstallDir());
		ev.parameters.setMembers(obj);

		if(obj instanceof OFormat) {
			((OFormat) obj).initExport(m, s);
		} else if(obj instanceof Intervener) {
			((Intervener) obj).init(m, s, s.getScene().getInterventionBase(), null);
		}

		return obj;

	}


	/**
	 * Execute an event
	 * @throws Exception 
	 */
	public void processEvent(Project p, Step s, Event ev) throws Exception {

		Object o = getObject(p.getModel(), s, ev);

		if(o instanceof EvolutionParameters) {
			EvolutionParameters ep = (EvolutionParameters) o;
			p.evolve(s, ep);

		} else if (o instanceof Intervener) {

			Intervener i = (Intervener) o;
			p.intervention(s, i, true);

		} else if (o instanceof OFormat) {

			OFormat io = (OFormat) o;
			p.export(s, io, (String)ev.parameters.get("filename"));

		}

	}


	/** Instantiate the model 
	 * @throws Exception */
	public GModel instantiateModel() throws Exception  {
		Engine engine = Engine.getInstance ();
		GModel m = engine.loadModel (model);
		return m;
	}


	/**
	 * Execute automation
	 * @throws Exception
	 */
	public Project run() throws Exception {

		GModel m = instantiateModel();

		// Create Project
		Project p = Engine.getInstance ().processNewProject (name, m);

		p.setMemorizer (memorizer);

		InitialParameters ip = (InitialParameters) getObject(m, null, initEvent);
		ip.buildInitScene(m);


		p.initialize(ip);


		// Apply evolution
		for(Event ev : events) {

			Step s = p.getStep(ev.id);
			if(s == null) {
				throw new Exception ("Unknown step id : " + ev.id);
			}

			processEvent(p, s, ev);

		}

		return p;
	}

	/** Serialisation 
	 * @throws IOException */
	public static Automation buildFromXML(String filename) throws IOException {

		// code to ignore error during deserialisation
		XStream xstream = new XStream() {
			@Override
			protected MapperWrapper wrapMapper(MapperWrapper next) {
				return new MapperWrapper(next) {
					@Override
					public boolean shouldSerializeMember(Class definedIn,
							String fieldName) {
						if (fieldName.equals("nbMaxSimu")) {
							return false;
						}
						return super.shouldSerializeMember(definedIn, fieldName);
					}
				};
			}
		};

		// Load and run automation
		//XStream xstream = new XStream();
		FileInputStream in = new FileInputStream(filename);

		Automation exp = (Automation) xstream.fromXML(in);
		in.close();
		return exp;
	}

	public void copytoXML(String filename) throws IOException {

		XStream xstream = new XStream();
		FileOutputStream dest = new FileOutputStream(filename);
		BufferedOutputStream buff = new BufferedOutputStream(dest);
		xstream.toXML(this, buff);
		dest.close();
	}




	/** return the list of variation parameters */
	public List<String> getInputHeader() {

		List<String> ret = new ArrayList<String>();
		ret.add("name");
		if(variation == null) { return ret; }

		ret.addAll( variation.getParamNames(initEvent.id) );
		for(Event ev : events){
			ret.addAll( variation.getParamNames(ev.id) );
		}


		return ret;
	}

	public List<Object> getInputParameters() {

		List<Object> ret = new ArrayList<Object>();
		ret.add(name);

		for(String p : variation.getParamNames(initEvent.id)) {
			ret.add( initEvent.parameters.get(p) );
		}

		for(Event ev : events){
			for(String p : variation.getParamNames(ev.id)) {
				ret.add( ev.parameters.get(p) );
			}
		}

		return ret;
	}
}
