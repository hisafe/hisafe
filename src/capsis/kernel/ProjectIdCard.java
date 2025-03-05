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

import java.io.File;
import java.io.Serializable;
import java.text.DateFormat;
import java.util.Date;
import java.util.Locale;

import jeeb.lib.util.RecordSet;
import jeeb.lib.util.Translator;
import jeeb.lib.util.serial.Reader;
import jeeb.lib.util.serial.SerializerFactory;

/**
 * Used to mark the project files when they are saved. Designed to appear nicely
 * in an IntrospectionPanel (e.g. in a fileChooser accessory).
 * 
 * @author F. de Coligny - march 2001, september 2010
 */
public class ProjectIdCard implements Serializable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 6455201991835935862L;

	// private static String CAPSIS_PROJECT = "ProjectIdCard.capsisProject"; //
	// multi app: Capsis/Simeo/Xplo...
	private String compactString;

	private String projectName;
	private String fileType;
	private String version;
	private String sessionDate;
	private String modelName;
	private String modelAuthor;
	private String modelInstitute;
	private String stepNumber;
	private String originalSource;

	/**
	 * Constructor 1: for export.
	 */
	public ProjectIdCard(Project project) {

		// fc-4.10.2021 Changed Project unique name in Session management
		projectName = project.getRawName();
//		projectName = project.getName();

		fileType = createFileType();
		// fileType = CAPSIS_PROJECT; // to be translated at read time

		version = Engine.getVersionAndRevision();

		Date now = new Date();
		DateFormat fmt = DateFormat.getDateTimeInstance(DateFormat.MEDIUM, DateFormat.SHORT, Locale.getDefault());
		sessionDate = fmt.format(now); // formated at save time

		IdCard idcard = project.getModel().getIdCard();

		modelName = idcard.getModelName() + " v" + idcard.getModelVersion();
		modelAuthor = idcard.getModelAuthor();
		modelInstitute = idcard.getModelInstitute();
		stepNumber = "" + project.getSize();

		String s = ((Step) project.getRoot()).getScene().getSourceName();
		originalSource = s != null ? s : "-";

	}

	/**
	 * Constructor 2: for import.
	 */
	public ProjectIdCard(File f) throws Exception {

//		try {
			Reader reader = SerializerFactory.getReader(f.getAbsolutePath());

			String desc = reader.readDescription();

			String separator = desc.substring(0, 1);
			String[] entries = desc.split(separator);

			// Check this is a project and for the current application
			String applicationName = Engine.getInstance().getApplicationName().toLowerCase();
			fileType = new RecordSet.KeyRecord(entries[2]).value;

//			System.out.println("ProjectIdCard fileType " + fileType);
//			System.out.println("project " + fileType.toLowerCase().indexOf("project"));
//			System.out.println("applicationName: " + applicationName + " "
//					+ fileType.toLowerCase().indexOf(applicationName));

			// Do not check app name, thus, Capsis can show preview for Simeo
			// project files
			if (fileType.toLowerCase().indexOf("project") == -1) {
				// || fileType.toLowerCase ().indexOf (applicationName) == -1) {
				throw new Exception("Not a " + applicationName + " project");
			}

			projectName = new RecordSet.KeyRecord(entries[1]).value;
			fileType = Translator.swap(new RecordSet.KeyRecord(entries[2]).value);
			version = new RecordSet.KeyRecord(entries[3]).value;
			sessionDate = new RecordSet.KeyRecord(entries[4]).value;
			modelName = new RecordSet.KeyRecord(entries[5]).value;
			modelAuthor = new RecordSet.KeyRecord(entries[6]).value;
			modelInstitute = new RecordSet.KeyRecord(entries[7]).value;
			stepNumber = new RecordSet.KeyRecord(entries[8]).value;
			originalSource = new RecordSet.KeyRecord(entries[9]).value;

//		} catch (Exception e) {
//			// Degbug fc-14.12.2017
//			System.out.println("ProjectIdCard exception");
//			e.printStackTrace(System.out);
//			throw e;
//		}

	}

	private String createFileType() {
		return Engine.getInstance().getApplicationName() + " project";
	}

	// ------ Accessors ------
	
	public String getProjectName() {
		return projectName;
	}

	public String getFileType() {
		return fileType;
	}
	
	/**
	 * This is the versionAndRevision of the app when the Project was saved. If
	 * a Serialization error occurs when reopening the file with a more recent
	 * version of the app, will tell that the project may be reopened with the
	 * app with this versionAndRevision. Note: the version does not really
	 * matter but the revision is the svn revision and should be enough to
	 * recreate the app if needed.
	 */
	public String getVersion() {
		return version;
	}

	public String getSessionDate() {
		return sessionDate;
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

	public String getStepNumber() {
		return stepNumber;
	}

	public String getOriginalSource() {
		return originalSource;
	}

	/**
	 * This compact String is to be written in the files, it is less sensitive
	 * to Serialization trouble when changing: if fields are added / changed or
	 * removed in the String, it will still be a String.
	 */
	public String getCompactString() {
		if (compactString == null) {

			StringBuffer b = new StringBuffer();
			String separator = Engine.getStringSeparator();
			b.append(separator);
			b.append(new RecordSet.KeyRecord("projectName", projectName));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("fileType", fileType)); // to be
																		// translated
																		// at
																		// read
																		// time
			b.append(separator);
			b.append(new RecordSet.KeyRecord("version", version));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("sessionDate", sessionDate)); // formated
																			// at
																			// save
																			// time
			b.append(separator);
			b.append(new RecordSet.KeyRecord("modelName", modelName));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("modelAuthor", modelAuthor));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("modelInstitute", modelInstitute));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("stepNumber", stepNumber));
			b.append(separator);
			b.append(new RecordSet.KeyRecord("originalSource", originalSource));
			b.append(separator);
			compactString = b.toString();

		}
		return compactString;

	}

}