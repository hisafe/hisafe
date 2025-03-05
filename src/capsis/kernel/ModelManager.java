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
package capsis.kernel;

import java.io.FileInputStream;
import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;

import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Translator;

/**
 * Manages models. Adapted for models outside Capsis. This class is a almost a
 * singleton: the constructor must be called once and only once, then
 * getInstance () returns the ModelManager reference.
 * 
 * @author S. Dufour, F. de Coligny - December 2008, September 2010
 */
public class ModelManager {

	// fc-10.4.2019 Complete review, simplification
	// fc-10.4.2019 = false in the modelFile now means 'technical model, not
	// proposed' to user in GUI' (can be used programmatically, e.g. Metatrom
	// module)

	/**
	 * Almost a Singleton pattern. The constructor must be called once (and only
	 * once). Then (and only then), the instance can be retrieved with
	 * ModelManager.getInstance ().
	 */
	private static ModelManager instance;

	/** The name of the file containing the list of available models */
	private String modelsFileName;

	/** Model package names, e.g. 'mountain', 'lerfob.abial' */
	private List<String> packageNames;

	/**
	 * Package names of models that will not be shown to user in GUI, can be used
	 * programmatically only, e.g. Metatrom
	 */
	private List<String> technicalPackageNames; // fc-10.4.2019

	/** Key: modelName, value: packageName */
	private Map<String, String> packageMap;

	/** Key: modelPackageName, value: idCard */
	private Map<String, IdCard> idCardMap;

	/** Model IdCards contain info on each model */
	private List<IdCard> idCards;

	/**
	 * Used to get an instance of ModelManager. Use the constructor once before to
	 * build the instance.
	 */
	public synchronized static ModelManager getInstance() {
		if (instance == null) {
			throw new Error(
					"Design error: Constructor must be CALLED ONCE before calling ModelManager.getInstance (), aborted.");
		}
		return instance;
	}

	/**
	 * Constructor
	 */
	public ModelManager(String modelsFileName) throws Exception {
		// The constructor can only be run once
		if (instance != null) {
			throw new Error(
					"Design error: ModelManager constructor must be called ONLY ONCE, then use getInstance (), aborted.");
		}
		instance = this;

		this.modelsFileName = modelsFileName;

		// fc-10.4.2019 Code simplification
		init();

	}

	private void init() {

		loadPackageNames();

		loadIdCards();

		idCardMap = new HashMap<String, IdCard>();
		packageMap = new HashMap<String, String>();

		for (IdCard card : idCards) {
			idCardMap.put(card.getModelPackageName(), card);
			packageMap.put(card.getModelName(), card.getModelPackageName());
		}

	}

	/**
	 * Loads the model package names from the models file.
	 */
	private void loadPackageNames() {

		packageNames = new LinkedList<String>();
		technicalPackageNames = new LinkedList<String>();

		// Read the models file
		Properties properties = new Properties();
		try {
			properties.load(new FileInputStream(modelsFileName));

		} catch (Exception e) {
			Log.println(Log.ERROR, "ModelManager.loadPackageNames ()", "Could not load models file: " + modelsFileName,
					e);
			throw new Error("ModelManager: aborted (see Log), could not load models file: " + modelsFileName, e);
		}

		// Dispatch models into technical / others
		for (Entry<Object, Object> pair : properties.entrySet()) {
			String k = (String) pair.getKey();
			String v = (String) pair.getValue();

			if (v.equalsIgnoreCase("true")) {
				packageNames.add(k);
			} else {
				technicalPackageNames.add(k);
			}
		}

	}


	// fc-13.10.2021 Possible to manage technical packages programatically (e.e.
	// during Ecoaf / SamsaraLightLoader connection)
	public void addTechnicalPackageName(String packageName) {
		technicalPackageNames.add(packageName);
	}

	// fc-13.10.2021 Possible to manage technical packages programatically (e.e.
	// during Ecoaf / SamsaraLightLoader connection)
	public void removeTechnicalPackageName(String packageName) {
		if (technicalPackageNames.contains(packageName))
			technicalPackageNames.remove(packageName);
	}

	private void loadIdCards() {

		idCards = new ArrayList<IdCard>();
		Collection<String> packages = this.getPackageNames();

		// Load IdCard for each module
		for (String pkg : packages) {

			try {
				pkg = pkg.replaceAll("\\.", "/");

				URL f = getClass().getClassLoader().getResource(pkg + "/idcard.properties"); // fc-24.11.2014
				String buildDir = "bin/"; // fc-24.11.2014 bin/ in AMAPstudio
				if (f.toString().contains("class/")) // fc-24.11.2014
					buildDir = "class/"; // fc-24.11.2014 class/ in Capsis

				// Added capsis4 installation directory to path.
				// fc+nb-17.04.2019
				// String idFile = buildDir + pkg + "/idcard.properties"; //
				// fc-24.11.2014
				String idFile = PathManager.getInstallDir() + "/" + buildDir + pkg + "/idcard.properties";

				Properties properties = new Properties();
				properties.load(new FileInputStream(idFile));

				IdCard card = new IdCard(properties);

				idCards.add(card);

				// Language bundle (internationalization)
				// Here, we load early the internationalized label files for
				// the modules. In case of trouble during new project or
				// open project, error labels will be translated.
				String bundleBaseName = card.getModelBundleBaseName();
				Translator.addBundle(bundleBaseName);

			} catch (Exception e) {
				Log.println(Log.WARNING, "ModelManager.loadIdCards ()", "Could not load idcard.properties for " + pkg,
						e);
				// Continue with next package

			}

		}

	}

	/**
	 * Loads the 'Model class' of a module given its package name (e.g. 'mountain').
	 */
	public GModel loadModel(String pkgName)
			throws ClassNotFoundException, InstantiationException, IllegalAccessException {

		// fc-20.8.2019 changed public method to protected. The main method to
		// use is Engine.loafModel (), calls this one, protected is better

		// fc-18.10.2019 rechanged to protected, classes in AMAPstudio need it

		GModel model = null;

		try {
			IdCard card = idCardMap.get(pkgName);
			String className = card.modelMainClassName;

			// Class<?> c = urlclassloader.loadClass (className);
			Engine.initGroovy();
			Class<?> c = Engine.loadClass(className);

			// Instantiation using the default constructor
			model = (GModel) c.newInstance();

			// Add the IdCard
			model.init(card);

			Log.println("Model [" + className + "] was correctly loaded");

		} catch (Exception e) {
			Log.println(Log.ERROR, "ModelManager.loadModel ()", "Model [" + pkgName + "]: load failed", e);
			// Returns null
		}

		return model;
	}

	/**
	 * Creates the relay object for the given model and the current pilot. The relay
	 * is built from conventions. The relay class is
	 * <modelPackage>.<pilotName>.<modelPrefix>Relay. This method is used at the end
	 * of the model loading process and when a project is re-opened (the project was
	 * saved without its relay classes).
	 */
	public Relay createRelay(GModel model, String pilotName, AbstractPilot pilot) throws Exception {
		// Convert pilotName (?)
		if (!pilotName.equals("gui")) {
			pilotName = "script";
		}

		// Try to load the Relay
		String modelPackage = model.getIdCard().getModelPackageName();
		String modelPrefix = model.getIdCard().getModelPrefix();

		String className = modelPackage + "." + pilotName + "." + modelPrefix + "Relay"; // relays
																							// names
																							// are
																							// normalized

		Relay relay = null;
		try {
			Class<?> c = getClass().getClassLoader().loadClass(className);
			Constructor<?> ctr = c.getConstructor(new Class[] { GModel.class });
			relay = (Relay) ctr.newInstance(new Object[] { model });

		} catch (ClassNotFoundException e) {

			// Load default relay
			relay = pilot.getDefaultRelay(model);

		} catch (Throwable e) {

			Log.println(Log.ERROR, "ModelManager.createRelay ()", "Error while creating relay: " + className, e);
			throw new Exception("Error while creating relay: " + className, e);
		}

		return relay;
	}

	// --------------------------------------------- End-of-init methods

	/**
	 * modelName -> packageName.
	 */
	public String getPackageName(String modelName) throws Exception {

		String ret = packageMap.get(modelName);

		if (ret == null)
			throw new Exception("ModelManager.getIdCard (), unknown model name : " + modelName);

		return ret;
	}

	/**
	 * Returns the list of available model package names.
	 */
	synchronized public List<String> getPackageNames() {
		// fc-10.4.2019 Including technical ones
		return getPackageNames(true);
	}

	/**
	 * Returns the list of available model packages, including or not the technical
	 * ones.
	 */
	synchronized public List<String> getPackageNames(boolean includingTechnicalPackageNames) {

		// fc-10.4.2019

		List<String> ret = new ArrayList<>(packageNames);

		if (includingTechnicalPackageNames)
			ret.addAll(technicalPackageNames);

		return ret;
	}

	/**
	 * packageName -> IdCard.
	 */
	public IdCard getIdCard(String pkgName) throws Exception {

		IdCard ret = idCardMap.get(pkgName);
		if (ret == null)
			throw new Exception("ModelManager.getIdCard (), unknown package name: " + pkgName);

		return ret;

	}

	/**
	 * Creates a list with the model names to be presented to the user in a GUI list
	 * widget.
	 */
	public Collection<String> getModelNames() {

		ArrayList<String> modelNames = new ArrayList<String>();

		for (IdCard card : idCards)
			if (!technicalPackageNames.contains(card.getModelPackageName())) // fc-10.4.2019
				modelNames.add(card.getModelName());

		return modelNames;
	}

	// fc-10.4.2019 Unused
	// /**
	// * Returns the package map: modelName -> modelPackageName.
	// */
	// synchronized private Map<String, String> getPackageMap() {
	// return packageMap;
	// }

	// fc-10.4.2019 Unused
	// /**
	// * Returns the idCard map.
	// */
	// synchronized private Map<String, IdCard> getIdCardMap() {
	// return idCardMap;
	// }

	// fc-10.4.2019 Unused
	// /**
	// * Returns the idCards collection.
	// */
	// synchronized private Collection<IdCard> getIdCards() {
	// return idCards;
	// }

}
