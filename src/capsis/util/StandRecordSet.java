/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2020 INRAE 
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

import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;
import jeeb.lib.util.RecordSet;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * StandRecordSet is a superclass for loaders of stand inventories, often used
 * at module initialisation time. A subclass implementing a loader for a given
 * file format may be adapted to also be an export (i.e. able to write a stand
 * at a given date to a file with the same format). In this case, the subclass
 * must implement the OFormat interface to be detected as an Export extension by
 * the Capsis extension manager. It is possible to extend directly the RecordSet
 * superclass for other configuration files (not stands). See also the more
 * recent FileLoader class on http://amapstudio.cirad.fr/private/filereader
 *
 * @author F. de Coligny - April 2001, reviewed December 2020
 */
abstract public class StandRecordSet extends RecordSet {
//abstract public class StandRecordSet extends RecordSet implements OFormat {
//abstract public class StandRecordSet extends RecordSet implements IFormat, OFormat {

	// fc-8.12.2020 OFormat clarification: RecordSet can be used for file reading or
	// writing , based on a list of Record instances. The RecordSet / Record feature
	// was made to ease text files reading and interpreting, based on inner classes
	// (sub classing Record) and reflection. It can be used for writing: build and
	// add a list of Record subclasses then call save (fileName), the writing will
	// used their toString () methods.
	// StandRecordSet is a specific subclass aimed at reading or writing so called
	// inventory files describing a stand (i.e. a Capsis scene). StandRecordSets are
	// generally file loaders, and optionally file writers. In the latter case,
	// they have to implement OFormat which is thus optional.
	// Before today, OFormat was implemented by StandRecordSet and all subclasses
	// were supposed to be exports. Removed OFormat from this level, it must be
	// added only for the subclasses really implementing an export.

	// fc-4.12.2020 Checked, not an export, no need for OFormat (maybe subclasses
	// will)

	// fc-9.10.2020 Removed IFormat, unused

	/**
	 * Default constructor.
	 */
	public StandRecordSet() {
		super();
	}

	/**
	 * Export. Specific for export subclasses implementing OFormat. StandRecordSet
	 * is an abstract class. For subclasses implementing OFormat (exports,
	 * optional), this method redefines OFormat.initExport () and redirects to
	 * prepareExport ().
	 */
	// fc-8.12.2020 StandRecordSet is not an OFormat any more, subclasses may still
	// be exports, removed @Override
//	@Override
	public void initExport(GModel m, Step s) throws Exception {
		clear();
		prepareExport(s.getScene());
	}

	/**
	 * Export. Specific for export subclasses implementing OFormat. It is suggested
	 * to redefine prepareExport (GScene) to 1. call super.prepareExport (GScene)
	 * (this method) to get the standard Header, then 2. Build and add () (we are a
	 * List) the Records to be written in the file. save(fileName) is to be called
	 * after.
	 */
	public void prepareExport(GScene scene) throws Exception {

		// fc-8.10.2020 renamed createRecordSet () into prepareExport, was same name for
		// prepareImport (in superclass) and export, clarified

		Project scenario = scene.getStep().getProject();
		GModel model = scenario.getModel();

		// Prepare the standard 'source' header, can be written or omitted later
		StringBuffer b = new StringBuffer();
		b.append(RecordSet.commentMark);
		b.append(" Model\t: ");
		b.append(model.getPackageName());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append(" Author\t: ");
		b.append(model.getIdCard().getModelAuthor());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append(" Project\t: ");
		b.append(scenario.getName());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append(" Stand date\t: ");
		b.append(scene.getDate());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append(" Initial source\t: ");
		b.append(scene.getSourceName());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append(" File format : \n");
		b.append(RecordSet.commentMark);
		b.append("    Name\t: ");
		b.append(ExtensionManager.getName(this.getClass().getName()));
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append("    Class name\t: ");
		b.append(getClass().getName());
//		b.append(getClassName());
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append("    Version\t: ");
		b.append(ExtensionManager.getVersion(this.getClass().getName()));
		b.append("\n");
		b.append(RecordSet.commentMark);
		b.append("    Author\t: ");
		b.append(ExtensionManager.getAuthor(this.getClass().getName()));
		b.append("\n");

		source = b.toString();
	}

	/**
	 * Import. Main method of a StandRecordSet. This is called after the file lines
	 * interpretation and changing into a list of Records. To be redefined to
	 * interpret the Records (e.g. by looping on this.iterator ()). E.g. finding a
	 * TreeRecord will trigger the creation of a Tree object and its addition in the
	 * initial scene. Reference to GModel allows to use some settings or values
	 * known by the model at load time (e.g. a treeIdDispenser to get unique tree
	 * ids for the created trees). If not used (if the StandRecordSet is an export
	 * only), the redefined method may return null or throw an Exception.
	 */
	abstract public GScene load(GModel model) throws Exception;

	////////////////////////////////////////////////// Extension stuff

	// fc-7.12.2020 Deprecated
//	/**
//	 * From Extension interface.
//	 */
//	public String getType() {
//		return CapsisExtensionManager.IO_FORMAT;
//	}

	// fc-7.12.2020 Deprecated
//	/**
//	 * From Extension interface.
//	 */
//	public String getClassName() {
//		return this.getClass().getName();
//	}

	// --- deprecated methods, should not be used, for compatibility with old
	// libraries only

	/**
	 * This method was kept for compatibility with old libraries in .jar files (e.g.
	 * castanea2018a.jar), it should not be used, call directly prepareExport
	 * (scene) instead.
	 */
	@Deprecated
	public void createRecordSet(GScene scene) throws Exception {
		System.out.println("Warning, " + this.getClass().getName()
				+ " uses deprecated RecordSet.createRecordSet(GScene), prepareExport(GScene) should be used instead");
		prepareExport(scene);
	}

	// fc-12.10.2020 removed
//	/**
//	 * From Extension interface. May be redefined by subclasses. Called after
//	 * constructor at extension creation (ex : for view2D zoomAll ()).
//	 */
//	public void activate() {
//	}

}
