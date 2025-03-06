package capsis.lib.genetics;

import java.util.ArrayList;
import java.util.List;

/**
 * Contains several methods which export genetic data to a file with standard
 * format of genetic data.
 * 
 * @author F. de Coligny, S. Muratorio - February 2019
 */
public class ExportGenotypeData {

	/**
	 * Exports a nuclearDNA short[][] into a String, can be used at export time in
	 * Capsis exporters.
	 * 
	 * E.g. {2;1;2;2;1;2;1;2;1;1;2;2;2;2;2;1}
	 * 
	 * <pre>
	 * String ndna = ExportGenotypeData.exportNuclearDNAToString(individualGenotype.getNuclearDNA());
	 * </pre>
	 */
	static public String exportNuclearDNAToString(short[][] nuclearDNA) {

		// fc+vf-5.4.2022

		StringBuffer b = new StringBuffer("{");

		boolean first = true;
		for (int j = 0; j < nuclearDNA.length; j++) {
			for (int k = 0; k < nuclearDNA[0].length; k++) {
				if (first)
					first = false; // no ; before first allele value
				else
					b.append(";");

				b.append("" + nuclearDNA[j][k]);
			}
		}
		b.append("}");
		return b.toString();
	}

	/**
	 * Exports a (paternal or maternal) cytoplasmicDNA short[] into a String, can be
	 * used at export time in Capsis exporters.
	 * 
	 * E.g. {3}
	 * 
	 * <pre>
	 * String mCytoplasmicDNA = ExportGenotypeData
	 * 		.exportCytoplasmicDNAToString(individualGenotype.getMCytoplasmicDNA());
	 * </pre>
	 */
	static public String exportCytoplasmicDNAToString(short[] cytoplasmicDNA) {

		// fc+vf-5.4.2022

		StringBuffer b = new StringBuffer("{");

		boolean first = true;
		for (int j = 0; j < cytoplasmicDNA.length; j++) {
			if (first)
				first = false; // no ; before first allele value
			else
				b.append(";");
			b.append("" + cytoplasmicDNA[j]);
		}
		b.append("}");
		return b.toString();
	}

	/**
	 * Exports a nuclearDNA short[][] into a list of short values, can be used at
	 * export time in Capsis exporters.
	 * 
	 * <pre>
	 * List nuclearDNAList = ExportGenotypeData.exportNuclearDNA(individualGenotype.getNuclearDNA());
	 * </pre>
	 */
	static public List<Short> exportNuclearDNA(short[][] nuclearDNA) {
		List<Short> nuclearDNAList = new ArrayList<>();
		if (nuclearDNA != null) {
			for (int i = 0; i < nuclearDNA.length; i++) {
				for (int j = 0; j < nuclearDNA[i].length; j++) {
					nuclearDNAList.add(nuclearDNA[i][j]);
				}
			}
		}
		return nuclearDNAList;
	}

	/**
	 * Exports a cytoplasmicDNA short[] into a list of short values, can be used at
	 * export time in Capsis exporters.
	 * 
	 * <pre>
	 * List mCytoplasmicDNAList = ExportGenotypeData.exportCytoplasmicDNA(individualGenotype.getMCytoplasmicDNA());
	 * </pre>
	 */
	static public List<Short> exportCytoplasmicDNA(short[] cytoplasmicDNA) {

		List<Short> cytoplasmicDNAList = new ArrayList<>();
		if (cytoplasmicDNA != null) {
			for (int i = 0; i < cytoplasmicDNA.length; i++) {
				cytoplasmicDNAList.add(cytoplasmicDNA[i]);
			}
		}
		return cytoplasmicDNAList;
	}

	/**
	 * Exports a allelicFrequences int[][] into a list of VertexND values, can be
	 * used at export time in Capsis exporters.
	 * 
	 * <pre>
	 * r.nuclearAllelesFrequences = ExportGenotypeData
	 * 		.exportAlleleFrequences(multiGenotype.getNuclearAlleleFrequency());
	 * </pre>
	 */
	static public List<VertexND> exportAlleleFrequences(int[][] alleleFrequency) {

		List<VertexND> alleleFrequencyList = new ArrayList<>();
		if (alleleFrequency != null) {
			for (int i = 0; i < alleleFrequency.length; i++) {
				int size = alleleFrequency[i].length;
				double[] naf = new double[size];
				for (int j = 0; j < size; j++) {
					naf[j] = alleleFrequency[i][j];
				}
				VertexND vertex = new VertexND(naf);
				alleleFrequencyList.add(vertex);
			}
		}
		return alleleFrequencyList;

	}

	/**
	 * Exports a recombinationProbas float[] into a list of Float values, can be
	 * used at export time in Capsis exporters.
	 * 
	 * <pre>
	 * speciesRecord.recombinationProbas = ExportGenotypeData
	 * 		.exportRecombinationProbas(species.getGeneticMap().getRecombinationProbas());
	 * </pre>
	 */
	static public List<Float> exportRecombinationProbas(float[] recombinationProbas) {
		List<Float> recombinationProbasList = new ArrayList<>();
		if (recombinationProbas != null) {
			for (int i = 0; i < recombinationProbas.length; i++) {
				recombinationProbasList.add(recombinationProbas[i]);
			}
		}
		return recombinationProbasList;

	}

	/**
	 * Exports a alleleDiversity short[][] into a list of VertexND values, can be
	 * used at export time in Capsis exporters.
	 * 
	 * <pre>
	 * speciesRecord.allelesNuclear = ExportGenotypeData
	 * 		.exportAlleleDiversity(species.getAlleleDiversity().getNuclearAlleleDiversity());
	 * </pre>
	 */
	static public List<VertexND> exportAlleleDiversity(short[][] alleleDiversity) {
		List<VertexND> alleleDiversityList = new ArrayList<>();
		if (alleleDiversity != null) {
			for (int i = 0; i < alleleDiversity.length; i++) {
				int size = alleleDiversity[i].length;
				double[] nad = new double[size];
				for (int j = 0; j < size; j++) {
					nad[j] = alleleDiversity[i][j];
				}
				VertexND vertex = new VertexND(nad);
				alleleDiversityList.add(vertex);
			}
		}
		return alleleDiversityList;
	}

	/**
	 * Exports a phiArray double[][] into a list of VertexND values, can be used at
	 * export time in Capsis exporters.
	 * 
	 * <pre>
	 * speciesRecord.allelesNuclear = ExportGenotypeData
	 * 		.exportAlleleDiversity(species.getAlleleDiversity().getNuclearAlleleDiversity());
	 * </pre>
	 */
	static public List<VertexND> exportPhiArray(double[][] phiArray) {
		List<VertexND> initialPhiArrayList = new ArrayList<>();
		if (phiArray != null) {
			for (int i = 0; i < phiArray.length; i++) {
				int size = phiArray[i].length;
				double[] ipa = new double[size];
				for (int j = 0; j < size; j++) {
					ipa[j] = phiArray[i][j];
				}
				VertexND vertex = new VertexND(ipa);
				initialPhiArrayList.add(vertex);
			}
		}

		return initialPhiArrayList;

	}

	/**
	 * Exports a nuclearAlleleEffect short[][] into a list of VertexND values, can
	 * be used at export time in Capsis exporters.
	 * 
	 * 
	 * <pre>
	 * parameterEffectRecord.nuclearEffect = ExportGenotypeData
	 * 		.exportNuclearAlleleEffect(parameterEffect.getNuclearAlleleEffect());
	 * </pre>
	 * 
	 */
	// fc+som-15.2.2019
	// ip.metatromOutputFileReader.getSelectedLocusGenotypicScale()... not generic
	// Could possibly become :
	// nuclearAlleleEffect[i][1] = nuclearAlleleEffect[i][1];
	// nuclearAlleleEffect[i][2] = nuclearAlleleEffect[i][2];
	//
//	static public List<VertexND> exportNuclearAlleleEffect(short[][] nuclearAlleleEffect) {
//		List<VertexND> nuclearAlleleEffectList = new ArrayList<>();
//
//		if (nuclearAlleleEffect != null) {
//
//			for (int i = 0; i < nuclearAlleleEffect.length; i++) {
//
//				int locusPosition = nuclearAlleleEffect[i][0];
//				double absoluteValueOfLocusEffect = ip.metatromOutputFileReader.getSelectedLocusGenotypicScale()
//						.getValue(locusPosition);
//
//				nuclearAlleleEffect[i][1] = (short) -absoluteValueOfLocusEffect;
//				nuclearAlleleEffect[i][2] = (short) absoluteValueOfLocusEffect;
//
//				double[] nae = { nuclearAlleleEffect[i][0], (double) (nuclearAlleleEffect[i][1]) / 2,
//						(double) (nuclearAlleleEffect[i][2]) / 2 };
//				VertexND vertex = new VertexND(nae);
//				nuclearAlleleEffectList.add(vertex);
//			}
//		}
//		return nuclearAlleleEffectList;
//
//	}

	/**
	 * Exports a CytoplasmicAlleleEffect short[][] into a list of VertexND values,
	 * can be used at export time in Capsis exporters.
	 * 
	 * <pre>
	 * List mCytoplasmicDNAList = ExportGenotypeData.exportCytoplasmicDNA(individualGenotype.getMCytoplasmicDNA());
	 * </pre>
	 */
	static public List<VertexND> exportCytoplasmicAlleleEffect(short[][] cytoplasmicAlleleEffect) {

		List<VertexND> cytoplasmicAlleleEffectList = new ArrayList();
		if (cytoplasmicAlleleEffect != null) {
			for (int i = 0; i < cytoplasmicAlleleEffect.length; i++) {
				int size = cytoplasmicAlleleEffect[i].length;
				double[] cytae = new double[size];
				for (int j = 0; j < size; j++) {
					cytae[j] = cytoplasmicAlleleEffect[i][j];
				}
				VertexND vertex = new VertexND(cytae);
				cytoplasmicAlleleEffectList.add(vertex);
			}
		}
		return cytoplasmicAlleleEffectList;

	}

}
