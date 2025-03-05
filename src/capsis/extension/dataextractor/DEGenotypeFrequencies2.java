package capsis.extension.dataextractor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFListOfCategories;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
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
 * Genotype frequencies (bases on DEGenotypeFrequencies)
 * 
 * @author F. de Coligny - July 2018
 */
public class DEGenotypeFrequencies2 extends AbstractDataExtractor implements DFListOfCategories {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEGenotypeFrequencies2");
	}

	// nb-06.08.2018
	//public static final String NAME = Translator.swap("DEGenotypeFrequencies2");
	//public static final String DESCRIPTION = Translator.swap("DEGenotypeFrequencies2.description");
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String VERSION = "1.0";

	private List<Categories> listOfCategories;

	/**
	 * Init method, receives the Step to be synchronized on.
	 */
	@Override
	public void init(GModel model, Step step) throws Exception {

		super.init(model, step);
		listOfCategories = new ArrayList<>();

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		if (!(referent instanceof GModel))
			return false;
		GModel m = (GModel) referent;
		GScene s = ((Step) m.getProject().getRoot()).getScene();
		if (!(s instanceof GeneticScene))
			return false;
		GeneticScene scene = (GeneticScene) s;

		// DEGenotypeFrequencies2 accepts :
		// - all indivs are Genotypables of same species
		// - Individual or MultiGenotype may be mixed

		// We need at least one Genotypable
		for (Iterator i = scene.getGenotypables().iterator(); i.hasNext();) {
			if (i.next() instanceof Genotypable)
				return true;
		}
		return false;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEGenotypeFrequencies2.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEGenotypeFrequencies2.description");
	}

	/**
	 * From Extension interface.
	 */
	public String getVersion() {
		return "1.0";
	}

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drgraph.DRBarGraph";
	}

	@Override
	public void setConfigProperties() {
		// Possible to restrict to groups of trees
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		
		
		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.COMMON);
		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.INDIVIDUAL);
//		// jl-19.07.2005 ... or groups of fish
//		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.COMMON);
//		addGroupProperty(FishGroupHelper.GROUP_FISH, PaleoDataExtractor.INDIVIDUAL);

		// Removed this feature, normalized locusNames instead
		// Which kind of label in the chart
		// addRadioProperty(new String[] { "afLabel_AlleleName", "afLabel_Both",
		// "afLabel_Frequency", "afLabel_None" });

		// System.out.println("DEAFreq2 setConfigProperties ()...");

		String[] possibleValues = findLocusNames(null);

		if (possibleValues == null)
			possibleValues = new String[0];

		// fc-2.8.2018 THIS WAS MOVED in AbstractDataExtractor
		// fc-2.8.2018 Remember what was selected for this property and this
		// given set of possible values (if nothing found, selected = possible)
//		String encodedString = Settings.getProperty("DEGenotypeFrequencies2.locusNames" + Tools.encode(possibleValues),
//				Tools.encode(possibleValues));
//		String[] selectedValues = Tools.decode(encodedString);

		// Which loci do we want to see
		addSetProperty("DEGenotypeFrequencies2.locusNames", possibleValues, possibleValues);

		addBooleanProperty("DEGenotypeFrequencies2.cumulatedFrequences", false);

		// System.out.println("DEAFreq2 end-of-setConfigProperties ()...");

	}

	/**
	 * Try to guess type of genotypables in the scene (e.g. TreeList.GROUP_ALIVE_TREE,
	 * FishGroupHelper.GROUP_FISH...)
	 * 
	 */
	private GroupableType getIndivType() {
		Collection types = Group.getPossibleTypes(step.getScene());
		if (types == null || types.isEmpty()) { // trouble
			return null;
		} else {

			// fc-3.7.2018 general case even if single type
			for (Iterator i = types.iterator(); i.hasNext();) {
				GroupableType type = (GroupableType) i.next();
				Collection os = Group.whichCollection(step.getScene(), type);
				if (os != null && !os.isEmpty()) {
					for (Iterator j = os.iterator(); j.hasNext();) {
						Object o = j.next();
						// First type of elements instanceof Genotypable
						if (o instanceof Genotypable)
							return type;

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
	public void grouperChanged(String grouperName) { // cp - 30.11.2004
		if (grouperName == null || grouperName.equals("null"))
			grouperName = "";

		settings.c_grouperMode = (grouperName.equals("") ? false : true);
		settings.c_grouperName = grouperName;

		GroupableType indivType = getIndivType();
		// System.out.println("getIndivType " + indivType);
		try {
			Collection gees = doFilter(step.getScene(), indivType);

			Genotypable gee = null;
			if (!gees.isEmpty())
				gee = (Genotypable) gees.iterator().next();

			// Changing groups: there may be two species in the scene, update
			// the locusNames
			String[] locusNames = findLocusNames(gee);
			updateSetProperty("DEGenotypeFrequencies2.locusNames", locusNames);

		} catch (Exception e) {
			return;
		}
	}

	private String[] findLocusNames(Genotypable gee) {

		// System.out.println("DEAFreq2 findLocusNames ()...");

		// System.out.println("DEAFreq2 findLocusNames () gee " + gee);
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
		// System.out.println("DEAFreq2 findLocusNames () found gee: " + gee);
		if (gee == null)
			return null;

		int nuclear;

		AlleleDiversity alleleDiversity = gee.getGenoSpecies().getAlleleDiversity();
		nuclear = alleleDiversity.getNuclearAlleleDiversity().length;

		// System.out.println("DEAFreq2 findLocusNames () alleleDiversity: " +
		// alleleDiversity);
		// System.out.println("DEAFreq2 findLocusNames () nuclear: " + nuclear);

		// fc-3.7.2018 Sring[] instead of Set
		String[] locusNames = new String[nuclear];
		// Set ids = new TreeSet();

		int k = 0;
		for (int i = 0; i < nuclear; i++)
			locusNames[k++] = "n_" + (i + 1);
		// ids.add("n_" + (i + 1));

		// System.out.println("DEGenotypeFrequencies2 findLocusNames ()
		// locusNames: " + AmapTools.toString(locusNames));

		// updateSetProperty("locusNames", locusNames);

		// System.out.println("DEGenotypeFrequencies2 end-of-findLocusNames
		// ()");

		return locusNames;
	}

	@Override
	public boolean doExtraction() throws Exception {

		if (upToDate)
			return true;
		if (step == null)
			return false;

		try {

			listOfCategories.clear();

			GroupableType indivType = getIndivType();
			// System.out.println("DEGenotypeFrequencies2 doExtraction()
			// indivType: " + indivType);

			// Apply groups if any (groupProperties)
			Collection gees = doFilter(step.getScene(), indivType);

			// Get the user chosen loci
			List loci = getSetProperty("DEGenotypeFrequencies2.locusNames");

			// Preliminary tests
			if (gees.isEmpty()) {
				Log.println(Log.WARNING, "DEGenotypeFrequencies2.doExtraction ()",
						"DEGenotypeFrequencies2 aborted because individuals collection is empty");
				return false;
			}

			for (Iterator i = gees.iterator(); i.hasNext();) {
				if (!(i.next() instanceof Genotypable)) {
					Log.println(Log.WARNING, "DEGenotypeFrequencies2.doExtraction ()",
							"DEGenotypeFrequencies2 aborted because not all individuals are Genotypables");
					return false;
				}
			}

			if (!GeneticTools.haveAllSameSpecies(gees)) {
				Log.println(Log.WARNING, "DEGenotypeFrequencies2.doExtraction ()",
						"DEGenotypeFrequencies2 aborted because all individuals have not same species");
				return false;
			}

			Categories cat = new Categories("Genotype frequencies", this.getColor());
			listOfCategories.add(cat);

			Map freqMap = GeneticTools.computeGenotypeFrequencies(gees, new HashSet (loci));

			boolean cumulate = isSet("DEGenotypeFrequencies2.cumulatedFrequences");
			
			// Iterate in the list to have good order
			for (Object o : loci) {

				String locusName = (String) o; // e.g. n_1
				double[][] values = (double[][]) freqMap.get(locusName);
				double[] firstAlleleNames = values[0];
				double[] secondAlleleNames = values[1];
				double[] freqs = values[2];
				double sum = 0;

				for (int i = 0; i < firstAlleleNames.length; i++) {

					int firstAlleleName = (int) firstAlleleNames[i]; // this is
																		// an
																		// int
					int secondAlleleName = (int) secondAlleleNames[i]; // this
																		// is an
																		// int
					String label = locusName + ": " + firstAlleleName + "/" + secondAlleleName;

					double freq = freqs[i];

					sum += freq;

					double v = freq;
					if (cumulate)
						v = sum;

					// freq is multiplied by 100
					cat.addEntry(label, v);

				}

			}

			// fc-3.7.2018 Checking...

			// GraphConverter c = GraphConverter.convertDFListOfXYSeries(this);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEGenotypeFrequencies2.doExtraction ()",
					"DEGenotypeFrequencies2 aborted due to exception : ", e);
			return false;
		}

		upToDate = true;
		return true;

	}

	@Override
	public List<Categories> getListOfCategories() {
		return listOfCategories;
	}

	@Override
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DEGenotypeFrequencies2.xLabel"));
		v.add(Translator.swap("DEGenotypeFrequencies2.yLabel"));
		return v;
	}

}
