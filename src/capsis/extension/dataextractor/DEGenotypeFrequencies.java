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
 * subclasses of GeneticTree (genetics library)).
 * 
 * @author I. Seynave - July 2002, F. de Coligny & C. Pichot - December 2004
 */
public class DEGenotypeFrequencies extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEGenotypeFrequencies");
	}

	private Vector labels;
	private int max;

	/**
	 * Constructor.
	 */
	public DEGenotypeFrequencies() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEGenotypeFrequencies(GenericExtensionStarter s) {
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
			// DEGenotypeFrequencies accepts :
			// - all indivs are Genotypable of same species
			// - IndividualGenotype only : *** no MultiGenotype allowed here

			// We need at least one Genotypable
			for (Iterator i = scene.getGenotypables().iterator(); i.hasNext();) {
				if (i.next() instanceof Genotypable) {
					return true;
				}
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEGenotypeFrequencies.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return false;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEGenotypeFrequencies.name");
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
		return Translator.swap("DEGenotypeFrequencies.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "2.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "2.1";

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

		addRadioProperty(new String[] { "gfLabel_GenotypeName", "gfLabel_Both", "gfLabel_Frequency", "gfLabel_None" }); // for
																														// radio
																														// buttons

		addSetProperty("gfLociIds", new String[] { "n_1" }, null);
	}

	// Try to guess type of genotypables in the stand 
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
		return null; // nothing found
//		return Group.UNKNOWN; // nothing found
	}

	/**
	 * Called when a group is set or changed.
	 */
	public void grouperChanged(String grouperName) {
		if (grouperName == null) {
			grouperName = "";
		} // no null allowed here

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

		List ids = new ArrayList(); // fc-3.7.2018 list keeps order
		for (int i = 0; i < nuclear; i++) {
			ids.add("n_" + (i + 1));
		}

		updateSetProperty("gfLociIds", ids);
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

			Set loci = new HashSet (getSetProperty("gfLociIds"));

			// fc - 10.10.2003 - preliminary tests
			if (gees.isEmpty()) {
				Log.println(Log.WARNING, "DEGenotypeFrequencies.doExtraction ()",
						"DEGenotypeFrequencies aborted because individuals collection is empty");
				curves.clear(); // no data -> user message "see configuration" -
								// fc - 10.10.2003
				return false;
			}
			for (Iterator i = gees.iterator(); i.hasNext();) {
				if (!(i.next() instanceof Genotypable)) {
					Log.println(Log.WARNING, "DEGenotypeFrequencies.doExtraction ()",
							"DEGenotypeFrequencies aborted because not all individuals are Genotypables");
					curves.clear(); // no data -> user message
									// "see configuration" - fc - 10.10.2003
					return false;
				}
			}

			if (!GeneticTools.haveAllSameSpecies(gees)) {
				Log.println(Log.WARNING, "DEGenotypeFrequencies.doExtraction ()",
						"DEGenotypeFrequencies aborted because all genotypables have not same species");
				curves.clear(); // no data -> user message "see configuration" -
								// fc - 9.10.2003
				return false;
			}

			Map genotypeFrequence = GeneticTools.computeGenotypeFrequencies(gees, loci);
			int nb = loci.size();
			// Create output data

			// determine the max number of genotype per loci.
			max = 0;
			int n;
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				double[][] tab = (double[][]) genotypeFrequence.get(s);
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
				double[][] tab = (double[][]) genotypeFrequence.get(s);
				v1.add(new Double(100 * tab[2][0]));
			}
			curves.add(v1);

			for (int j = 1; j < max; j++) {
				Vector v = new Vector();
				for (Iterator ite = loci.iterator(); ite.hasNext();) {
					String s = (String) ite.next();
					double[][] tab = (double[][]) genotypeFrequence.get(s);
					int t = tab[0].length;
					int nba = (int) (-1 + Math.sqrt(1 + 8 * t)) / 2;
					double d = 0;
					if (j < t) {
						for (int k = 0; k <= Math.min(j, nba - 1); k++) {
							d = d + tab[2][(nba * k) - (k * (k + 1) / 2) + k];
						}
						if (j >= nba && nba > 1) {
							int k = 1;
							int l = 1;
							while (l <= j - nba + 1) {
								if (tab[0][k] != tab[1][k]) {
									d = d + tab[2][k];
									l = l + 1;
								}
								k = k + 1;
							}
						}
						v.add(new Double(100 * d));
					} else {
						v.add(new Double(Double.NaN));
					}
				}
				curves.add(v);
			}

			// construction of labels for x axis vector.
			// construction of labels
			labels.clear();
			Vector l1 = new Vector();
			for (Iterator ite = loci.iterator(); ite.hasNext();) {
				String s = (String) ite.next();
				l1.add(s);
			}
			labels.add(l1);

			if (isSet("gfLabel_None")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						l.add("");
					}
					labels.add(l);
				}
			} else if (isSet("gfLabel_Frequency")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						double[][] tab = (double[][]) genotypeFrequence.get(s);
						int t = tab[0].length;
						int nba = (int) (-1 + Math.sqrt(1 + 8 * t)) / 2;
						double freq = 0;
						double y = 0;
						if (j < t) {
							if (j < nba) {
								freq = tab[2][(nba * j) - (j * (j + 1) / 2) + j];
							}
							if (j >= nba && nba > 1) {
								y = nba + 0.5 - Math.sqrt(Math.pow(nba, 2) + nba - 2 * j);
								freq = tab[2][j - nba + (int) y];
							}
							if (freq != 0) {
								l.add("" + new Double((double) (((int) (freq * 10 * 100)) / (double) 10)));
							} else {
								l.add("");
							}
						} else {
							l.add("");
						}
					}
					labels.add(l);
				}
			} else if (isSet("gfLabel_GenotypeName")) {
				for (int j = 0; j < max; j++) {
					Vector l = new Vector();
					for (Iterator ite = loci.iterator(); ite.hasNext();) {
						String s = (String) ite.next();
						double[][] tab = (double[][]) genotypeFrequence.get(s);
						int t = tab[0].length;
						int nba = (int) (-1 + Math.sqrt(1 + 8 * t)) / 2;
						String genotype = "";
						int eff = 0;
						double y = 0;
						if (j < t) {
							if (j < nba) {
								genotype = "" + (int) tab[0][(nba * j) - (j * (j + 1) / 2) + j] + "_"
										+ (int) tab[1][(nba * j) - (j * (j + 1) / 2) + j];
								if (tab[2][(nba * j) - (j * (j + 1) / 2) + j] == 0) {
									eff = 1;
								}
							}
							if (j >= nba && nba > 1) {
								y = nba + 0.5 - Math.sqrt(Math.pow(nba, 2) + nba - 2 * j);
								genotype = "" + (int) tab[0][j - nba + (int) y] + "_" + (int) tab[1][j - nba + (int) y];
								if (tab[2][j - nba + (int) y] == 0) {
									eff = 1;
								}
							}
							if (eff == 0) {
								l.add(genotype);
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
						double[][] tab = (double[][]) genotypeFrequence.get(s);
						int t = tab[0].length;
						int nba = (int) (-1 + Math.sqrt(1 + 8 * t)) / 2;
						String genotype = "";
						double freq = 0;
						double y = 0;
						if (j < t) {
							if (j < nba) {
								freq = tab[2][(nba * j) - (j * (j + 1) / 2) + j];
								genotype = "" + (int) tab[0][(nba * j) - (j * (j + 1) / 2) + j] + "_"
										+ (int) tab[1][(nba * j) - (j * (j + 1) / 2) + j];
							}
							if (j >= nba && nba > 1) {
								y = nba + 0.5 - Math.sqrt(Math.pow(nba, 2) + nba - 2 * j);
								freq = tab[2][j - nba + (int) y];
								genotype = "" + (int) tab[0][j - nba + (int) y] + "_" + (int) tab[1][j - nba + (int) y];
							}
							if (freq != 0) {
								l.add("" + genotype + " / "
										+ new Double((double) (((int) (freq * 10 * 100)) / (double) 10)));
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
			// "DEGenotypeFrequencies.doExtraction ()",
			// ~
			// "DEGenotypeFrequencies aborted because all genotypables have not same species");
			// ~ curves.clear (); // no data -> user message "see configuration"
			// - fc - 9.10.2003
			// ~ return false;
			// ~ }

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEGenotypeFrequencies.doExtraction ()",
					"DEGenotypeFrequencies aborted due to exception : ", exc);
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
		v.add(Translator.swap("DEGenotypeFrequencies.xLabel"));
		v.add(Translator.swap("DEGenotypeFrequencies.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
