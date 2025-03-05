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

import java.io.Serializable;
import java.util.Properties;

/**
 * IdCard is an identity Card of the model, given for each model, used by the
 * Engine to instanciate the model's main class.
 * 
 * @author F. de Coligny - june 1999, september 2010
 */
public class IdCard implements Serializable, Comparable<IdCard> {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	protected String modelName = "-";
	protected String modelAuthor = "-";
	protected String modelInstitute = "-";
	protected String modelType = "-";
	protected String modelPrefix = "-";
	protected String modelPackageName = "-";
	protected String modelMainClassName = "-";
	protected String modelBundleBaseName = "-";
	protected String modelVersion = "-";
	protected String modelDescription = "-";

	/**
	 * Default constructor
	 */
	public IdCard() {
	}

	/**
	 * Builds an IdCard from a property file
	 */
	public IdCard(Properties p) {

		// fc-18.2.2022 Added trim () below to prevent an extra space to enter the
		// system (trouble with Pinus pinaster in the GraphicalExtensionManager due to
		// an extra trailing space)

		// We use p.getProperty (key, defaultValue)
		modelName = p.getProperty("Name", modelName).trim();
		modelAuthor = p.getProperty("Author", modelAuthor).trim();
		modelInstitute = p.getProperty("Institute", modelInstitute).trim();
		modelType = p.getProperty("Type", modelType).trim();
		modelPrefix = p.getProperty("Prefix", modelPrefix).trim();
		modelPackageName = p.getProperty("Package", modelPackageName).trim();
		modelMainClassName = p.getProperty("MainClass", modelMainClassName).trim();
		modelBundleBaseName = p.getProperty("BundleBaseName", modelBundleBaseName).trim();
		modelVersion = p.getProperty("Version", modelVersion).trim();
		modelDescription = p.getProperty("Description", modelDescription).trim();
	}

	public String getModelName() {
		return modelName;
	}

	public String getModelAuthor() {
		return modelAuthor;
	}

	public String getModelInstitute() {
		return modelInstitute;
	}

	public String getModelType() {
		return modelType;
	}

	public String getModelPrefix() {
		return modelPrefix;
	}

	public String getModelPackageName() {
		return modelPackageName;
	}

	public String getModelMainClassName() {
		return modelMainClassName;
	}

	public String getModelBundleBaseName() {
		return modelBundleBaseName;
	}

	public String getModelVersion() {
		return modelVersion;
	}

	public String getModelDescription() {
		return modelDescription;
	}

	/**
	 * Comparable interface: compare on model names. Can be used to sort lists.
	 */
	public int compareTo(IdCard object) throws ClassCastException {
		IdCard other = object;
		return getModelName().compareTo(other.getModelName());
	}

	/**
	 * toString method
	 */
	public String toString() {
		StringBuffer b = new StringBuffer(getClass().getSimpleName());
		b.append("\nName = ");
		b.append(modelName);
		b.append("\nAuthor = ");
		b.append(modelAuthor);
		b.append("\nInstitute = ");
		b.append(modelInstitute);
		b.append("\nType = ");
		b.append(modelType);
		b.append("\nPrefix = ");
		b.append(modelPrefix);
		b.append("\nPackageName = ");
		b.append(modelPackageName);
		b.append("\nMainClassName = ");
		b.append(modelMainClassName);
		b.append("\nBundleBaseName = ");
		b.append(modelBundleBaseName);
		b.append("\nVersion = ");
		b.append(modelVersion);
		b.append("\nDescription = ");
		b.append(modelDescription);

		return b.toString();
	}

}
