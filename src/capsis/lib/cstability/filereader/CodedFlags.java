package capsis.lib.cstability.filereader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A class for flags used in encode/decode Objects.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
public class CodedFlags {

	public static final String INITIAL_DATE = "initialDate";
	public static final String FINAL_DATE = "finalDate";
	public static final String USER_TIME_STEP = "userTimeStep";
	public static final String USER_POLYMERIZATION_STEP = "userPolymerizationStep";

	public static final String TIME_UNIT = "timeUnit";
	public static final String INTEGRATION_METHOD = "integrationMethod";

	public static final String ENVIRONMENT_FILE_NAME = "environmentFileName";

	public static final String BIOCHEMICAL_CLASS = "BIOCHEMICAL_CLASS";

	public static final String POOL_ACCESSIBILITY = "POOL_ACCESSIBILITY";
	public static final String POOL_INITIALIZATION = "POOL_INITIALIZATION";

	public static final String ACCESSIBLE = "ACCESSIBLE";
	public static final String INACCESSIBLE_AGGREGATION = "INACCESSIBLE_AGGREGATION";
	public static final String INACCESSIBLE_MINERAL_ASSOCIATION = "INACCESSIBLE_MINERAL_ASSOCIATION";
	public static final String INACCESSIBLE_EMBEDMENT = "INACCESSIBLE_EMBEDMENT";

	public static final String ENZYME_TRAITS = "ENZYME_TRAITS";

	public static final String MICROBE_SIGNATURE = "SIGNATURE";
	public static final String MICROBE_ENZYME_PRODUCTION = "ENZYME_PRODUCTION";
	public static final String MICROBE_ASSIMILATION = "ASSIMILATION";
	public static final String MICROBE_MORTALITY = "MORTALITY";
	public static final String MICROBE_INITIALIZATION = "MICROBE_INITIALIZATION";

	public static final String POOL_TRANSFER = "POOL_TRANSFER";

	public static final List<String> FLAGS = new ArrayList<>(Arrays.asList(INITIAL_DATE, FINAL_DATE, USER_TIME_STEP,
			USER_POLYMERIZATION_STEP, TIME_UNIT, INTEGRATION_METHOD, ENVIRONMENT_FILE_NAME, BIOCHEMICAL_CLASS, POOL_ACCESSIBILITY,
			POOL_INITIALIZATION, ACCESSIBLE, INACCESSIBLE_AGGREGATION, INACCESSIBLE_MINERAL_ASSOCIATION,
			INACCESSIBLE_EMBEDMENT, ENZYME_TRAITS, MICROBE_SIGNATURE, MICROBE_ENZYME_PRODUCTION, MICROBE_ASSIMILATION,
			MICROBE_MORTALITY, MICROBE_INITIALIZATION, POOL_TRANSFER));

}
