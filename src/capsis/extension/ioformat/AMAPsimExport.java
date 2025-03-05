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

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.Vector;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.lib.amapsim.AMAPsimBranch;
import capsis.lib.amapsim.AMAPsimLayer;
import capsis.lib.amapsim.AMAPsimRequestableTree;
import capsis.lib.amapsim.AMAPsimTreeData;
import capsis.lib.amapsim.AMAPsimTreeStep;
import capsis.util.StandRecordSet;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;

/**
 * AMAPsimExport : exportation of data computed by AMAPsim.
 *
 * @author F. de Coligny, C. Meredieu - July 2003
 */
public class AMAPsimExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	static {
		Translator.addBundle("capsis.extension.ioformat.AMAPsimExport");
	}

	public static final String TREE_SUFFIX = ".Atr";
	public static final String BRANCH_SUFFIX = ".Abr";
	public static final String LAYER_SUFFIX = ".Ala";
	public static final String CYCLE_SUFFIX = ".Acy";

	private Collection branches;
	private Collection layers;
	private Collection cycles;

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public AMAPsimExport () {}

	

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
			Log.println (Log.ERROR, "AMAPsimExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("AMAPsimExport.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny, C. Meredieu";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("AMAPsimExport.description");
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

		//~ GTCStand s = (GTCStand) stand;		// Real type of Mountain stands

		branches = new ArrayList ();
		layers = new ArrayList ();
		cycles = new ArrayList ();

		//~ add (new CommentRecord ("Compatible extensions for module "+idCard.getModelPackageName ()
				//~ +" ("+idCard.getModelName ()+" by "+idCard.getModelAuthor ()+")"));
		//~ add (new EmptyRecord ());	// a blank line

		Step step = stand.getStep ();
		Project scenario = step.getProject ();
		GModel model = scenario.getModel ();

		// Retrieve Steps from root to this step
		Vector steps = scenario.getStepsFromRoot (step);


		// file headers
		add (new CommentRecord (scenario.getName ()+" "+new Date ()));
		add (new CommentRecord ("TreeId"+"\t"+"AgeCap"+"\t"+"AgeAmap"+"\t"+"dbhCap"+"\t"+"dbhAmap"+"\t"
								+"HtCap"+"\t"+"HtAmap"+"\t"+"TrunkVol"+"\t"+"LeafSurf"+"\t"
								+"VolBr"+"\t"+"VolBrO2"+"\t"+"VolBrO3"+"\t"+"VolBrOn"+"\t"+"NbBr"+"\t"+"NbUc"+"\t"
								+"CrDiam"+"\t"+"CrBaseH"));
		branches.add (new CommentRecord (scenario.getName ()+" "+new Date ()));
		layers.add (new CommentRecord (scenario.getName ()+" "+new Date ()));
		cycles.add (new CommentRecord (scenario.getName ()+" "+new Date ()));

		for (Iterator w = steps.iterator (); w.hasNext ();) {
			Step stp = (Step) w.next ();
			TreeList s = (TreeList) stp.getScene ();

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
				treeBuffer.append (t.getHeight ());
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.height);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.trunkVolume);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.leafSurface);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.branchVolume); // Lsa and YC - 16.3.2004
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.branchVolumeOrder2);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.branchVolumeOrder3);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.branchVolumeOrdern);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.numberOfBranches);
				treeBuffer.append ("\t");
				treeBuffer.append (treeStep.numberOfUCs);
				treeBuffer.append ("\t");
				treeBuffer.append (amapt.getCrownDiameter());
				treeBuffer.append ("\t");
				treeBuffer.append (amapt.getCrownBaseHeight());

				add (new FreeRecord (treeBuffer.toString ()));

				if (treeStep.branches != null) {	// fc - 29.10.2003
					for (Iterator i = treeStep.branches.iterator (); i.hasNext ();) {
						AMAPsimBranch b = (AMAPsimBranch) i.next ();
						StringBuffer buffer = new StringBuffer ();
						buffer.append (t.getId ());
						buffer.append ("\t");
						buffer.append (treeStep.age);
						buffer.append ("\t");
						buffer.append (b.branchId);
						buffer.append ("\t");
						buffer.append (b.branchStatus);
						buffer.append ("\t");
						buffer.append (b.branchDiameter);
						buffer.append ("\t");
						buffer.append (b.branchLength);
						buffer.append ("\t");
						buffer.append (b.branchAngle);
						buffer.append ("\t");
						buffer.append (b.branchHeight);
						branches.add (new FreeRecord (buffer.toString ()));

					}
				}

				if (treeStep.layers != null) {	// fc - 29.10.2003
					for (Iterator i = treeStep.layers.iterator (); i.hasNext ();) {
						AMAPsimLayer l = (AMAPsimLayer) i.next ();
						StringBuffer buffer = new StringBuffer ();
						buffer.append (t.getId ());
						buffer.append ("\t");
						buffer.append (treeStep.age);
						buffer.append ("\t");
						buffer.append (l.layerHeight);
						buffer.append ("\t");
						buffer.append (l.layerDiameter);
						layers.add (new FreeRecord (buffer.toString ()));

					}
				}
			//~ ... UT > Cycles
				//~ if (data.getAMAPsimCycles () != null) {	// fc - 29.10.2003
					//~ for (Iterator i = data.getAMAPsimCycles ().iterator (); i.hasNext ();) {
						//~ AMAPsimCycle c = (AMAPsimCycle) i.next ();
						//~ StringBuffer buffer = new StringBuffer ();
						//~ buffer.append (t.getId ());
						//~ buffer.append ("\t");
						//~ buffer.append (data.getAMAPsimAge ());
						//~ buffer.append ("\t");
						//~ buffer.append (c.getAMAPsimCycleHeight ());
						//~ buffer.append ("\t");
						//~ buffer.append (c.getAMAPsimCycleNumberOfBranches ());
						//~ cycles.add (new FreeRecord (buffer.toString ()));

					//~ }
				//~ }

			}

		}
	}

	/**
	 * RecordSet save redefinition : several files.
	 */
	public void save (String fileName) throws Exception {
System.out.println ("fileName="+fileName);
		String radical = fileName;
		if (fileName.indexOf (".") != -1) {
			radical = fileName.substring (0, fileName.indexOf ("."));
		}
System.out.println ("radical="+radical);

		// trees
		BufferedWriter out = null;
		try {
			out = new BufferedWriter (new FileWriter (radical+AMAPsimExport.TREE_SUFFIX));
		} catch (Exception e) {
			throw new Exception ("File name "+radical+AMAPsimExport.TREE_SUFFIX+" causes error : "+e.toString ());
		}

		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record r = (Record) i.next ();
			out.write (r.toString ());		// Automatic toString () is used
			out.newLine ();
		}
		try {out.close ();} catch (Exception e) {}	// no exception reported if trouble while closing

		// branches
		out = null;
		try {
			out = new BufferedWriter (new FileWriter (radical+AMAPsimExport.BRANCH_SUFFIX));
		} catch (Exception e) {
			throw new Exception ("File name "+radical+AMAPsimExport.BRANCH_SUFFIX+" causes error : "+e.toString ());
		}

		for (Iterator i = branches.iterator (); i.hasNext ();) {
			Record r = (Record) i.next ();
			out.write (r.toString ());		// Automatic toString () is used
			out.newLine ();
		}
		try {out.close ();} catch (Exception e) {}	// no exception reported if trouble while closing

		// layers
		out = null;
		try {
			out = new BufferedWriter (new FileWriter (radical+AMAPsimExport.LAYER_SUFFIX));
		} catch (Exception e) {
			throw new Exception ("File name "+radical+AMAPsimExport.LAYER_SUFFIX+" causes error : "+e.toString ());
		}

		for (Iterator i = layers.iterator (); i.hasNext ();) {
			Record r = (Record) i.next ();
			out.write (r.toString ());		// Automatic toString () is used
			out.newLine ();
		}
		try {out.close ();} catch (Exception e) {}	// no exception reported if trouble while closing

		// cycles
		out = null;
		try {
			out = new BufferedWriter (new FileWriter (radical+AMAPsimExport.CYCLE_SUFFIX));
		} catch (Exception e) {
			throw new Exception ("File name "+radical+AMAPsimExport.CYCLE_SUFFIX+" causes error : "+e.toString ());
		}

		for (Iterator i = cycles.iterator (); i.hasNext ();) {
			Record r = (Record) i.next ();
			out.write (r.toString ());		// Automatic toString () is used
			out.newLine ();
		}
		try {out.close ();} catch (Exception e) {}	// no exception reported if trouble while closing


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
