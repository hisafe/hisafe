/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.ioformat;

import java.util.Date;
import java.util.Iterator;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.lib.amapsim.AMAPsimRequestableTree;
import capsis.lib.amapsim.AMAPsimTreeData;
import capsis.lib.amapsim.AMAPsimTreeStep;
import capsis.util.StandRecordSet;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * AMAPsimExport : exportation of data computed by AMAPsim.
 *
 * @author F. de Coligny, C. Meredieu - July 2003
 */
public class AMAPsimExportForIFN extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.AMAPsimExportForIFN");
	}

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public AMAPsimExportForIFN () {}

	
	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			Step root = (Step) ((GModel) referent).getProject ().getRoot ();
			GScene s = root.getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			Tree t = ((TreeCollection) s).getTrees ().iterator ().next ();
			if (!(t instanceof AMAPsimRequestableTree)) {return false;}


		} catch (Exception e) {
			Log.println (Log.ERROR, "AMAPsimExportForIFN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("AMAPsimExportForIFN.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "L. Saint-André & Y. Caraglio";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("AMAPsimExportForIFN.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	/**
	 * Export: Stand -> RecordSet - Implementation here.
	 * (RecorSet -> File in superclass)
	 */
	@Override
	public void prepareExport (GScene stand) throws Exception {
		super.prepareExport (stand);		// deals with RecordSet's source

		Step step = stand.getStep ();
		Project scenario = step.getProject ();
		GModel model = scenario.getModel ();


		// file headers
		add (new CommentRecord (scenario.getName ()+" "+new Date ()));
		add (new CommentRecord ("TreeId"+"\t"+"AgeCap"+"\t"+"AgeAmap"+"\t"
			+"dbhCap"+"\t"+"dbhAmap"+"\t"+"TrunkD260"+"\t"+"MedDiam"+"\t"
			+"HtCap"+"\t"+"HtAmap"+"\t"+"HghtD7"+"\t"+"HgtD2"+"\t"+"MedHgt"+"\t"
			+"trVol"+"\t"+"trVol+20"+"\t"+"trVol20to7"+"\t"+"trVol7to4"+"\t"+"trVol4to0"+"\t"
			+"trVol260"+"\t"+"trVol+7"+"\t"+"trVol7to2"+"\t"
			+"BrVol"+"\t"+"BrVol+20"+"\t"+"BrVol20to7"+"\t"+"BrVol7to4"+"\t"+"BrVol4to0"));

		TreeList s = (TreeList) step.getScene ();

		for (Iterator k = s.getTrees ().iterator (); k.hasNext ();) {
			AMAPsimRequestableTree amapt = (AMAPsimRequestableTree) k.next ();
			AMAPsimTreeData data = amapt.getAMAPsimTreeData ();
			AMAPsimTreeStep treeStep = data.treeStep;		// fc - 21.1.2004

			if (data == null) {continue;}

			Tree t = (Tree) amapt;
			StringBuffer treeBuffer = new StringBuffer ();
			treeBuffer.append (t.getId ());
			treeBuffer.append ("\t");
			treeBuffer.append (t.getAge ());
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.age);
			treeBuffer.append ("\t");

			treeBuffer.append (t.getDbh ());
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.dbh);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkDiameter260);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.mediumDiameter);
			treeBuffer.append ("\t");

			treeBuffer.append (t.getHeight ());
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.height);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.heightDiameter7);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.heightDiameter2);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.mediumHeight);
			treeBuffer.append ("\t");

			treeBuffer.append (treeStep.trunkVolume);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeDplus20);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeD20to7);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeD7to4);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeD4to0);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolume260);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeDplus7);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.trunkVolumeD7to2);
			treeBuffer.append ("\t");

			treeBuffer.append (treeStep.branchVolume);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.branchVolumeDplus20);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.branchVolumeD20to7);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.branchVolumeD7to4);
			treeBuffer.append ("\t");
			treeBuffer.append (treeStep.branchVolumeD4to0);

			add (new FreeRecord (treeBuffer.toString ()));

		}
	}


	/**
	 * Import: RecordSet -> Stand - Implementation here.
	 * (File -> RecordSet in superclass).
	 */
	public GScene load (GModel model) throws Exception {
		return null;
	}

	// fc-7.12.2020 Deprecated
//	////////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

}
