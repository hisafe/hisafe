package capsis.extension.extractorgroup;

import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.Translator;
import capsis.extensiontype.ExtractorGroup;

/**
 * A group of data extractors in Capsis.
 */
public class ExtractorGroup1 extends ExtractorGroup {
	// reviewed fc-12.9.2014, fixed several bugs
	
	// fc-29.8.2018 Was missing
	static {
		Translator.addBundle ("capsis.extension.extractorgroup.ExtractorGroup1");
	}

	// nb-06.08.2018
	//public static final String NAME = Translator.swap("ExtractorGroup1.standardCharts");
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String DESCRIPTION = "ExtractorGroup.description";

	static List<String> extractorClassNames;
	static {
		extractorClassNames = new ArrayList<String>();
		extractorClassNames.add("capsis.extension.dataextractor.DETimeG2"); //G / Temps (ou �ge) ou / Hdom : Ph. Dreyfus
		extractorClassNames.add("capsis.extension.dataextractor.DETimeDbh"); //Diam�tre / Temps : F. de Coligny
		extractorClassNames.add("capsis.extension.dataextractor.DETimeH"); //Hauteur / Temps : F. de Coligny
		extractorClassNames.add("capsis.extension.dataextractor.DEDbhClassN"); //N / Classes de diam�tre : F. de Coligny
		extractorClassNames.add("capsis.extension.dataextractor.DEHeightClassN"); //N / Classes de hauteur : B. Courbaud
		extractorClassNames.add("capsis.extension.dataextractor.DETimeN"); //N / Temps (ou �ge) ou / Hdom : F. de Coligny

	}

	/**
	 * Constructor.
	 */
	public ExtractorGroup1() {
		super();
	}

	/**
	 * Returns the list of extractors this group contains.
	 */
	public List<String> getExtractorClassNames() {
		return extractorClassNames;
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		return ExtractorGroup.matchWith(extractorClassNames, referent); // needed
	}

	@Override
	public String getName() {
		return Translator.swap("ExtractorGroup1.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("ExtractorGroup1.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}	
}
