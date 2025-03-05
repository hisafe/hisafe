/*
 * The Genetics library for Capsis4
 *
 * Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
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
package capsis.extension.dataextractor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.genetics.AlleleDiversity;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.GeneticTools;
import capsis.lib.genetics.Genotypable;
import capsis.util.FishGroupHelper;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Allele frequences per Locus for a group of genotypable objects (must be
 * instances of Genotypable (genetics library)).
 * 
 * @author I. Seynave - July 2002, F. de Coligny & C. Pichot - December 2004
 */
public class DEAlleleFrequencies extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEAlleleFrequencies");
	}

	private Vector labels;
	private int max;

	/**
	 * Constructor.
	 */
	public DEAlleleFrequencies() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEAlleleFrequencies(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

		updateLocusList(null);

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof GeneticScene)) {
				return false;
			}
			GeneticScene scene = (GeneticScene) s;

			// fc - 10.10.2003
			// DEAlleleFrequencies accepts :
			// - all indivs are Genotypables of same species
			// - Individual or MultiGenotype may be mixed

			// We need at least one Genotypable
			for (Iterator i = scene.getGenotypables().iterator(); i.hasNext();) {
				if (i.next() instanceof Genotypable) {
					return true;
				}
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEAlleleFrequencies.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return false;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEAlleleFrequencies.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "I. Seynave";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEAlleleFrequencies.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "2.1";
	}

	// nb-08.08.2018
	// public static final String VERSION = "2.1";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRTable";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);

		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.COMMON); 
		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.INDIVIDUAL); 

		// jl-19.07.2005
//		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.COMMON); 
//		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.INDIVIDUAL); 


		addRadioProperty(new String[] { "afLabel_AlleleName", "afLabel_Both", "afLabel_Frequency", "afLabel_None" }); // for
																														// radio
																														// buttons
		// ~ System.out.println
		// ("setConfigProperties: addSetProperty afLociIds");
		addSetProperty("afLociIds", new String[] { "n_1" }, null);
	}

	// Try to guess type of genotypables in the stand (ex: TreeList.GROUP_ALIVE_TREE,
	// FishGroupHelper.GROUP_FISH...)
	//
	private GroupableType getIndivType() {
		Collection types = Group.getPossibleTypes(step.getScene());
		if (types == null || types.isEmpty()) { // trouble
			return null;
		} else {
			if (types.size() == 1) { // one single type : return it
				return (GroupableType) types.iterator().next();
			} else { // several types : get the good one
				for (Iterator i = types.iterator(); i.hasNext();) {
					GroupableType type = (GroupableType) i.next();
					Collection os = Group.whichCollection(step.getScene(), type);
					if (os != null && !os.isEmpty()) {
						for (Iterator j = os.iterator(); j.hasNext();) {
							Object o = j.next();
							if (o instanceof Genotypable) {
								return type;
							}
						}
					}

				}
			}
		}
		// fc-1.7.2019
		return null; // nothing found
//		return Group.UNKNOWN; // nothing found
	}

	/**
	 * Called when a group is set or changed.
	 */
	public void grouperChanged(String grouperName) { // cp - 30.11.2004
		if (grouperName == null || grouperName.equals("null")) {
			grouperName = "";
		} // no null allowed here
			// ~ System.out.println
			// ("group changed ---------- grouperName="+grouperName);

		// fc - 8.10.2003
		settings.c_grouperMode = (grouperName.equals("") ? false : true);
		settings.c_grouperName = grouperName;

		GroupableType indivType = getIndivType();
		System.out.println("getIndivType " + indivType);
		try {
			Collection gees = doFilter(step.getScene(), indivType); // fc -
																	// 6.4.2004

			Genotypable gee = null;
			if (!gees.isEmpty()) {
				gee = (Genotypable) gees.iterator().next();
			}

			updateLocusList(gee);
		} catch (Exception e) {
			return;
		}
	}

	private void updateLocusList(Genotypable gee) {

		System.out.println("DEAlleleFrequencies updateLocusList ()...");

		System.out.println("updateLocusList gee " + gee);
		if (gee == null) { // get a genotypable
			GroupableType indivType = getIndivType();
			Collection gees = Group.whichCollection(step.getScene(), indivType);
			for (Iterator i = gees.iterator(); i.hasNext();) {
				Object o = i.next();
				if (o instanceof Genotypable && ((Genotypable) o).getGenotype() != null) {
					gee = (Genotypable) o;
					break;
				}
			}
		}
		System.out.println("found gee: " + gee);
		if (gee == null) {
			return;
		}

		int nuclear;
		int mcyto;
		int pcyto;

		AlleleDiversity alleleDiversity = gee.getGenoSpecies().getAlleleDiversity();
		nuclear = alleleDiversity.getNuclearAlleleDiversity().length;
		mcyto = alleleDiversity.getMCytoplasmicAlleleDiversity().length;
		pcyto = alleleDiversity.getPCytoplasmicAlleleDiversity().length;

		System.out.println("DEAlleleFrequencies updateLocusList () alleleDiversity: " + alleleDiversity);
		System.out.println("DEAlleleFrequencies updateLocusList () nuclear: " + nuclear);
		System.out.println("DEAlleleFrequencies updateLocusList () mcyto: " + mcyto);
		System.out.println("DEAlleleFrequencies updateLocusList () pcyto: " + pcyto);

		List ids = new ArrayList(); // fc-3.7.2018 list keeps order
		for (int i = 0; i < nuclear; i++) {
			ids.add("n_" + (i + 1));
		}

		for (int i = 0; i < mcyto; i++) {
			ids.add("m_" + (i + 1));
		}

		for (int i = 0; i < pcyto; i++) {
			ids.add("p_" + (i + 1));
		}
		System.out.println("DEAlleleFrequencies updateLocusList () ids: " + ids);

		updateSetProperty("afLociIds", ids);

		System.out.println("DEAlleleFrequencies end-of-updateLocusList ()");

	}

	/**
	 * Synchronize on new step.
	 */
	public void setStep(Step newStep) {
		super.setStep(newStep);
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {

		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		try {
			GroupableType indivType = getIndivType();
			System.out.println("indivType " + indivType);
			Collection gees = doFilter(step.getScene(), indivType); // fc -
																	// 6.4.2004

			Set loci = new HashSet(getSetProperty("afLociIds"));

			// fc - 10.10.2003 - preliminary tests
			if (gees.isEmpty()) {
				Log.println(Log.WARNING, "DEAlleleFrequencies.doExtraction ()",
						"DEAlleleFrequencies aborted because individuals collection is empty");
				curves.clear(); // no data -> user message "see configuration" -
								// fc - 10.10.2003
				return false;
			}
			for (Iterator i = gees.iterator(); i.hasNext();) {
				if (!(i.next() instanceof Genotypable)) {
					Log.println(Log.WARNING, "DEAlleleFrequencies.doExtraction ()",
							"DEAlleleFrequencies aborted because not all individuals are Genotypables");
					curves.clear(); // no data -> user message
									// "see configuration" - fc - 10.10.2003
					return false;
				}
			}

			// fc - november 2004
			if (!GeneticTools.haveAllSameSpecies(gees)) {
				Log.println(Log.WARNING, "DEAlleleFrequencies.doExtraction ()",
						"DEAlleleFrequencies aborted because all individuals have not same species");
				curves.clear(); // no data -> user message "see configuration" -
								// fc - 9.10.2003
				return false;
			}

			Map alleleFrequence = GeneticTools.computeAlleleFrequencies(gees, loci, false);
			int nb = loci.size();
			// Create output data

			// determine the max number of allele per loci.
			max = 0;
			int n;
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				double[][] tab = (double[][]) alleleFrequence.get(s);
				n = tab[0].length;
				if (n > max) {
					max = n;
				}
			}
			curves.clear();
			Vector c = new Vector();
			int x = 0;
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				x = x + 1;
				c.add(new Integer(x));
			}
			curves.add(c);
			Vector v1 = new Vector();
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				double[][] tab = (double[][]) alleleFrequence.get(s);
				v1.add(new Double(100 * tab[1][0]));
			}
			curves.add(v1);

			for (int j = 1; j < max; j++) {
				Vector v = new Vector();
				for (Iterator ite = loci.iterator(); ite.hasNext();) {
					String s = (String) ite.next();
					double[][] tab = (double[][]) alleleFrequence.get(s);
					int t = tab[0].length;
					double d = 0;
					if (j < t) {
						for (int k = 0; k < j + 1; k++) {
							d = d + tab[1][k];
						}
						v.add(new Double(100 * d));
					} else {
						v.add(new Double(Double.NaN));
					}
				}
				curves.add(v);
			}

			// construction of labels for x axis vector.
			// construction of label
			labels.clear();
			Vector l1 = new Vector();
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				l1.add(s);
			}
			labels.add(l1);

			if (isSet("afLabel_None")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						l.add("");
					}
					labels.add(l);
				}
			} else if (isSet("afLabel_Frequency")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						double[][] tab = (double[][]) alleleFrequence.get(s);
						int t = tab[0].length;
						if (j < t) {
							if (tab[1][j] != 0) {
								l.add("" + new Double((double) (((int) (tab[1][j] * 10 * 100)) / (double) 10)));
							} else {
								l.add("");
							}
						} else {
							l.add("");
						}
					}
					labels.add(l);
				}
			} else if (isSet("afLabel_AlleleName")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						double[][] tab = (double[][]) alleleFrequence.get(s);
						int t = tab[0].length;
						if (j < t) {
							if (tab[1][j] != 0) {
								l.add("" + (int) tab[0][j]);
							} else {
								l.add("");
							}
						} else {
							l.add("");
						}
					}
					labels.add(l);
				}
			} else {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						double[][] tab = (double[][]) alleleFrequence.get(s);
						int t = tab[0].length;
						if (j < t) {
							if (tab[1][j] != 0) {
								String ss = "" + new Integer((int) tab[0][j]) + "_"
										+ new Double((double) (((int) (tab[1][j] * 10 * 100)) / (double) 10));
								l.add(ss);
							} else {
								l.add("");
							}
						} else {
							l.add("");
						}
					}
					labels.add(l);
				}
			}

			// ~ } else {
			// ~ Log.println (Log.WARNING,
			// "DEAlleleFrequencies.doExtraction ()",
			// ~
			// "DEAlleleFrequencies aborted because all individuals have not
			// same species");
			// ~ curves.clear (); // no data -> user message "see configuration"
			// - fc - 9.10.2003
			// ~ return false;
			// ~ }

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEAlleleFrequencies.doExtraction ()",
					"DEAlleleFrequencies aborted due to exception : ", exc);
			curves.clear(); // no data -> user message "see configuration" - fc
							// - 9.10.2003
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return max;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DEAlleleFrequencies.xLabel"));
		v.add(Translator.swap("DEAlleleFrequencies.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
