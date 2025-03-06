package capsis.lib.cstability.filereader;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.MicrobeSpecies;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.Microbe;

/**
 * A decoder to create MicrobeSpecies instances
 *
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class MicrobeDecoder implements Decodable {

	private Map<String, MicrobeSpecies> microbeSpeciesMap;
	private Map<String, Microbe> microbeMap;

	/**
	 * Constructor
	 */
	public MicrobeDecoder() {
		microbeSpeciesMap = new HashMap<>();
		microbeMap = new HashMap<>();
	}

	/**
	 * decode(): this method is called several times to create and fill several
	 * microbeSpecies instances, e.g. from a file.
	 */
	public Decodable decode(String encodedString, Parameters p, Context c) throws Exception {

		try {
			String s = encodedString.trim();

			StringTokenizer st = new StringTokenizer(s, "\t");
			String flag = st.nextToken().trim();

			if (flag.equals(CodedFlags.MICROBE_SIGNATURE)) {
				// e.g. SIGNATURE brown_rot_fungi lipid 0.12
				// gaussianTruncatedNormalized(lipid;1;0.4;[0,2])
				String microbeName = st.nextToken().trim();
				MicrobeSpecies ms = microbeSpeciesMap.get(microbeName);
				if (ms == null) {
					ms = new MicrobeSpecies(microbeName);
					microbeSpeciesMap.put(microbeName, ms);
				}

				String biochemicalClassName = st.nextToken().trim();
				BiochemicalClass bc = p.getBiochemicalClassMap().get(biochemicalClassName);

				double proportion = Double.parseDouble(st.nextToken().trim());
				Function polymerizationFunction = Function.getFunction(st.nextToken().trim(), p, c);

				ms.addSignatureElement(bc, polymerizationFunction, proportion, p, c);

			} else if (flag.equals(CodedFlags.MICROBE_ENZYME_PRODUCTION)) {
				// e.g. ENZYME_PRODUCTION brown_rot_fungi lipidase 0.1
				String microbeName = st.nextToken().trim();
				MicrobeSpecies ms = microbeSpeciesMap.get(microbeName);
				if (ms == null)
					throw new Exception("Unknown microbeName for" + CodedFlags.MICROBE_ENZYME_PRODUCTION
							+ ", should appear previously in " + CodedFlags.MICROBE_SIGNATURE);

				String producedEnzyme = st.nextToken().trim();
				Function productionFunction = Function.getFunction(st.nextToken().trim(), p, c);

				ms.addEnzyme(producedEnzyme, productionFunction);

			} else if (flag.equals(CodedFlags.MICROBE_ASSIMILATION)) {
				// e.g. ASSIMILATION brown_rot_fungi cellulose uniformLinear([0,0.4];1)
				// constant(0.3)
				String microbeName = st.nextToken().trim();
				MicrobeSpecies ms = microbeSpeciesMap.get(microbeName);
				if (ms == null)
					throw new Exception("Unknown microbeName for" + CodedFlags.MICROBE_ASSIMILATION
							+ ", should appear previously in " + CodedFlags.MICROBE_SIGNATURE);

				String biochemicalClassName = st.nextToken().trim();
				Function uptakeFluxFunction = Function.getFunction(st.nextToken().trim(), p, c);
				Function carbonUseEfficiencyFunction = Function.getFunction(st.nextToken().trim(), p, c);

				ms.addAssimilationElement(biochemicalClassName, uptakeFluxFunction, carbonUseEfficiencyFunction);

			} else if (flag.equals(CodedFlags.MICROBE_MORTALITY)) {
				// e.g. MORTALITY brown_rot_fungi linear(0.1)
				String microbeName = st.nextToken().trim();
				MicrobeSpecies ms = microbeSpeciesMap.get(microbeName);
				if (ms == null)
					throw new Exception("Unknown microbeName for" + CodedFlags.MICROBE_MORTALITY
							+ ", should appear previously in " + CodedFlags.MICROBE_SIGNATURE);
				Function mortalityFunction = Function.getFunction(st.nextToken().trim(), p, c);

				ms.setMortalityFunction(mortalityFunction);

			} else if (flag.equals(CodedFlags.MICROBE_INITIALIZATION)) {
				// e.g. MICROBE_INITIALIZATION brown_rot_fungi 1.0
				String microbeName = st.nextToken().trim();
				MicrobeSpecies ms = microbeSpeciesMap.get(microbeName);
				if (ms == null)
					throw new Exception("Unknown microbeName for" + CodedFlags.MICROBE_INITIALIZATION
							+ ", should appear previously in " + CodedFlags.MICROBE_SIGNATURE);
				double microbeMass = Double.parseDouble(st.nextToken().trim());
				microbeMap.put(microbeName, new Microbe(ms, microbeMass));

			} else {

				throw new Exception("Wrong flag for microbeSpecies line: " + flag
						+ ", expected SIGNATURE, ENZYME_PRODUCTION, ASSIMILATION, MORTALITY or MICROBE_INITIALIZATION");
			}

			return this;

		} catch (Exception e) {
			throw new Exception("MicrobeSpeciesDecoder.decode (), could not parse this encodedString: " + encodedString,
					e);
		}
	}

	/**
	 * getMicrobeSpeciesMap()
	 */
	public Map<String, MicrobeSpecies> getMicrobeSpeciesMap() {
		return microbeSpeciesMap;
	}

	/**
	 * getMicrobeMap()
	 */
	public Map<String, Microbe> getMicrobeMap() {
		return microbeMap;
	}

}
