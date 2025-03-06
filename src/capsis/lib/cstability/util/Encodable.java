package capsis.lib.cstability.util;

import java.util.Map;
import java.util.Set;

/**
 * An interface for object which can be encoded in a String.
 * 
 * @author J. Sainte-Marie, June 2023
 */
public interface Encodable {

	public static final String TAB = "\t";
	public static final String LS = System.lineSeparator();

	public static final class Encoder {
		String encoder;

		public Encoder(String encoder) {
			this.encoder = encoder;
		}

		@Override
		public String toString() {
			return "Encodable.Encoder, name: " + encoder;
		}
	}

	public static final String MAIN_HEADER = "########################################################################"
			+ LS //
			+ "# Setup file of C-STABILITY." + LS //
			+ "#" + LS //
			+ "# File description:" + LS //
			+ "# 	file generated with file editor" + LS //
			+ "#" + LS //
			+ "# File editing rules:" + LS //
			+ "# 	- empty lines and lines starting with '#' are skipped by the program." + LS //
			+ "# 	- simple values are defined with '=' and a blank separator." + LS //
			+ "# 	- complex lines starts with a specific flag and all fields are" + LS //
			+ "#	separated by a tabulation noted \\t in documentation." + LS //
			+ "########################################################################" + LS;

	public static final String TIMELINE_HEADER = "########################################################################"
			+ LS //
			+ "# Simulation timeline:" + LS //
			+ "#" + LS //
			+ "# 	This section should contain:" + LS //
			+ "#		timeUnit = (string)" + LS //
			+ "#		initialDate = (integer)" + LS //
			+ "#		finalDate = (integer)" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		timeUnit = day" + LS //
			+ "#		initialDate = 0" + LS //
			+ "#		finalDate = 300" + LS //
			+ "########################################################################" + LS;

	public static final String METHODS_HEADER = "########################################################################"
			+ LS //
			+ "# Numerical scheme and methods:" + LS //
			+ "#" + LS //
			+ "#	This section should contain:" + LS //
			+ "#		userTimeStep = (double) in ]0,1[" + LS //
			+ "#		userPolymerizationStep = (double)" + LS //
			+ "#		integrationMethod = (string) available options:" + LS //
			+ "#			- INTEGRATION_TRAPEZE," + LS //
			+ "#			- INTEGRATION_RECTANGLE_LEFT" + LS //
			+ "#			- INTEGRATION_RECTANGLE_RIGHT" + LS //
			+ "#" + LS //
			+ "#	Exampl" + LS // e:
			+ "#		userTimeStep = 0.1" + LS //
			+ "#		userPolymerizationStep = 0.01" + LS //
			+ "#		integrationMethod = INTEGRATION_TRAPEZE" + LS //
			+ "########################################################################" + LS;

	public static final String BIOCHEMICAL_CLASS_HEADER = "########################################################################"
			+ LS //
			+ "# Biochemical classes:" + LS //
			+ "#" + LS //
			+ "#	This section should contain at least two biochemical classes." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		BIOCHEMICAL_CLASS \\t bcName \\t polymerization" + LS //
			+ "#		- bcName (String) is the name of the biochemical class" + LS //
			+ "#		- polymerization (Interval) is the interval used to describe" + LS //
			+ "# 		  the polymerization of the biochemical class" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		BIOCHEMICAL_CLASS \\t cellulose \\t [0,2]" + LS //
			+ "########################################################################" + LS;

	public static final String SUBSTRATE_POOL_HEADER = "########################################################################"
			+ LS //
			+ "# Substrate pools:" + LS //
			+ "#" + LS //
			+ "#	This section should contain at least an accessible pool per" + LS //
			+ "#	biochemical class." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		POOL_ACCESSIBILITY \\t bcName \\t accessibility" + LS //
			+ "#		- bcName (String) has to be defined previously" + LS //
			+ "#		- accessibility (String) is a list of accessibility status" + LS //
			+ "# 		  available for the biochemical class. Status are:" + LS //
			+ "#			* ACCESSIBLE" + LS //
			+ "#			* INACCESSIBLE_AGGREGATION" + LS //
			+ "#		 	* INACCESSIBLE_MINERAL_ASSOCIATION" + LS //
			+ "#		 	* INACCESSIBLE_MINERAL_ASSOCIATION" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		POOL_ACCESSIBILITY \\t cellulose" + LS //
			+ "#							...\\t [ACCESSIBLE,INACCESSIBLE_AGGREGATION]" + LS //
			+ "########################################################################" + LS;

	public static final String ENZYME_TRAITS_HEADER = "########################################################################"
			+ LS //
			+ "# Enzyme traits:" + LS //
			+ "#" + LS //
			+ "#	This section permits to define the simulation enzymes traits." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		ENZYME_TRAITS \\t enzName \\t bcName \\t depolymerizationDomain" + LS //
			+ "#			... \\t depolymerizationRateFunction \\t kernelFunction" + LS //
			+ "#				... \\t kernelIntegrationMethod" + LS //
			+ "#		- enzName (String) is the name of the enzyme" + LS //
			+ "#		- bcName (String) is the biochemical class associated to the" + LS //
			+ "#		  enzyme" + LS //
			+ "#		- depolymerizationDomain (Interval) is the polymerization" + LS //
			+ "#		  interval of the biochemical class targeted by the enzyme" + LS //
			+ "#		- depolymerizationRateFunction (String) is the function" + LS //
			+ "# 		  defining the depolymerization rate of the enzyme" + LS //
			+ "#		- kernelFunction (String) is the function defining the" + LS //
			+ "#		  transformation kernel of the enzyme" + LS //
			+ "# 		- kernelIntegrationMethod (String): STANDARD_KERNEL_INTEGRATION" + LS //
			+ "#										 or INTEGRAL_KERNEL_INTEGRATION" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		ENZYME_TRAITS \\t cellulolysis \\t cellulose \\t [0,2]" + LS //
			+ "#				...\\t uniformLinear([0,2];1.8) \\t kernelAlpha([0,2];5)" + LS //
			+ "#					...\\t INTEGRAL_KERNEL_INTEGRATION" + LS //
			+ "########################################################################" + LS;

	public static final String MICROBE_SIGNATURE_HEADER = "########################################################################"
			+ LS //
			+ "# Microbe signature:" + LS //
			+ "#" + LS //
			+ "#	This section describes the microbes involved in the simulation and" + LS //
			+ "# 	their biochemical and polymerization composition." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "# 		SIGNATURE \\t micName \\t bcName \\t proportion" + LS //
			+ "#									... \\t polymerizationFunction" + LS //
			+ "# 		- micName (String) is the name of the microbial community" + LS //
			+ "# 		- bcName (String) is the name of the biochemical class " + LS //
			+ "# 		  composing the microbes" + LS //
			+ "# 		- proportion (double in [0,1]) is the proportion of the" + LS //
			+ "# 		  components of the considered biochemical class composing the" + LS //
			+ "# 		  microbes" + LS //
			+ "# 		- polymerizationFunction (String) is the function describing" + LS //
			+ "#		  the polymerization of biochemical elements considered" + LS //
			+ "#" + LS //
			+ "#	Example: " + LS //
			+ "#		SIGNATURE \\t cellulose_degrader \\t microbe_sugar \\t 1" + LS //
			+ "#		  ...\\t gaussianTruncatedNormalized(microbe_sugar;1.5;0.1;[0,2])" + LS //
			+ "########################################################################" + LS;

	public static final String MICROBE_ENZYME_PRODUCTION_HEADER = "########################################################################"
			+ LS //
			+ "# Microbe enzyme production (secretome):" + LS //
			+ "#" + LS //
			+ "#	This section describes how enzymes are produced by the microbes." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "# 		ENZYME_PRODUCTION \\t micName \\t producedEnzyme" + LS //
			+ "# 									... \\t productionRateFunction" + LS //
			+ "#		- micName (String) is the name of the microbial community" + LS //
			+ "#		- producedEnzyme (String) is the name of the enzymes produces by" + LS //
			+ "# 		  the microbes" + LS //
			+ "#		- productionRateFunction (String) is the function describing the" + LS //
			+ "# 		  production rate of the enzyme" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		ENZYME_PRODUCTION \\t cellulose_degrader" + LS //
			+ "#										... \\t cellulase \\t linear(1.)" + LS //
			+ "########################################################################" + LS;

	public static final String MICROBE_ASSIMILATION_HEADER = "########################################################################"
			+ LS //
			+ "# Microbe assimilation:" + LS //
			+ "#" + LS //
			+ "#	This section describes how microbes assimilate C from the accessible" + LS //
			+ "#	substrate pools. Assimilation of each microbe has to be defined." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		ASSIMILATION \\t micName \\t bcName \\t" + LS //
			+ "#			uptakeFluxFunction \\t carbonUseEfficiencyFunction" + LS //
			+ "#		- micName (String) is the name of the microbial community" + LS //
			+ "#		- bcName (String) is the biochemical class where carbon is taken" + LS //
			+ "# 		  up by microbes," + LS //
			+ "#		- uptakeFluxFunction (String) is the function describing the" + LS //
			+ "# 		rate of uptake of carbon" + LS //
			+ "#		- carbonUseEfficiencyFunction (String) is the function" + LS //
			+ "# 		  describing the carbon use efficiency of the taken up carbon" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "# 		ASSIMILATION \\t cellulose_degrader \\t cellulose" + LS //
			+ "#						...\\t uniformLinear([0,0.4];5.) \\t constant(0.4)" + LS //
			+ "########################################################################" + LS;

	public static final String MICROBE_MORTALITY_HEADER = "########################################################################"
			+ LS //
			+ "# Microbe mortality:" + LS //
			+ "#" + LS //
			+ "#	This section describes the mortality rate of the microbes. The" + LS //
			+ "#	mortality of each microbe has to be defined." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		MORTALITY \\t micName \\t mortalityFunction" + LS //
			+ "#		- micName (String) is the name of the microbial community" + LS //
			+ "#		- mortalityFunction (String) is the function describing the" + LS //
			+ "# 		  mortality rate of microbes" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		MORTALITY \\t cellulose_degrader \\t linear(0.02)" + LS //
			+ "########################################################################" + LS;

	public static final String SUBSTRATE_POOL_TRANSFER_HEADER = "########################################################################"
			+ LS //
			+ "# Substrate pool transfers (optional):" + LS //
			+ "#" + LS //
			+ "#	This section is optional if the simulations contains only ACCESSIBLE" + LS //
			+ "#	pools. For a same biochemical class, exchange between accessible and" + LS //
			+ "#	inaccessible pool can occur." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		POOL_TRANSFER \\t bcName \\t origin \\t arrival \\t transferFunction" + LS //
			+ "# 		- bcName (String) is the biochemical class" + LS //
			+ "# 		- origin (String) is the accessibility of the origin pool" + LS //
			+ "# 		- arrival (String) is the accessibility of the arrival pool" + LS //
			+ "# 		- transferFunction (String) is the function describing the" + LS //
			+ "# 		  transfer rate between the pools" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		POOL_TRANSFER \\t cellulose \\t INACCESSIBLE_EMBEDMENT" + LS //
			+ "#			...\\t ACCESSIBLE \\t enzymaticLinearTransfer(lignolase;1)" + LS //
			+ "########################################################################" + LS;

	public static final String SUBSTRATE_POOL_INITIALIZATION_HEADER = "########################################################################"
			+ LS //
			+ "# Substrate pools initialization:" + LS //
			+ "#" + LS //
			+ "#	This section contain..." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		POOL_INITIALIZATION \\t bcName" + LS //
			+ "# 							...\\t accessibility \\t carbonPolymerization" + LS //
			+ "#		- bcName" + LS //
			+ "#		- accessibility" + LS //
			+ "#		- carbonPolymerization" + LS //
			+ "#	Example:" + LS //
			+ "#		POOL_INITIALIZATION \\t lipîd \\t ACCESSIBLE" + LS //
			+ "# 	...\\t gaussianTruncatedProportionalized(lipid;0.95;1.5;0.1;[0,2])" + LS //
			+ "########################################################################" + LS;

	public static final String SUBSTRATE_POOLS_TEMPORAL_INPUTS_HEADER = "########################################################################"
			+ LS //
			+ "# Substrate pools temporal inputs:" + LS //
			+ "#" + LS //
			+ "#	This section should contains the C composition of specified pools." + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "#		POOL_INPUT \\t bcName \\t accessibility \\t carbonInputFunction" + LS //
			+ "#		- bcName (String) is the biochemical class" + LS //
			+ "#		- accessibility (String) is the accessibility of the pool" + LS //
			+ "# 		- carbonInputFunction (String) is the function describing the" + LS //
			+ "# 		amount and polymerization of the carbon input" + LS //
			+ "#" + LS //
			+ "#	Example:" + LS //
			+ "#		POOL_INPUT \\t cellulose \\t ACCESSIBLE \\t" + LS //
			+ "#		...\\t constantInput(0.1:" + LS //
			+ "#			... gaussianTruncatedNormalized(cellulose;1.5;0.1;[0,2]))" + LS //
			+ "########################################################################" + LS;

	public static final String MICROBE_INITIAL_STATE_HEADER = "########################################################################"
			+ LS //
			+ "# Microbe initial state:" + LS //
			+ "#" + LS //
			+ "#	This section contains the initial biomass of each microbe" + LS //
			+ "#" + LS //
			+ "#	Line format:" + LS //
			+ "# 		MICROBE_INITIALIZATION \\t micName \\t mass" + LS //
			+ "#		- micName the name of the microbial community considered" + LS //
			+ "#		- mass (double>0) is the initial amount of carbon composing" + LS //
			+ "# 		  microbial community in g" + LS //
			+ "#" + LS //
			+ "#	Example: " + LS //
			+ "#		MICROBE_INITIALIZATION \\t cellulose_degrader \\t 0.05" + LS //
			+ "########################################################################" + LS;

	public static final String OBSERVATIONS_HEADER = "########################################################################"
			+ LS //
			+ "# Observations:" + LS //
			+ "#" + LS //
			+ "#	This section contains the observations needed by the user at the end" + LS //
			+ "#	of the simulation. If no observations are specified, all observers " + LS //
			+ "#	are generated." + LS //
			+ "# " + LS //
			+ "#	Line formats available:" + LS //
			+ "#		STATE_OBSERVER \\t observableVariable \\t datesToObserve" + LS //
			+ "# 		  - observableVariable is the observed variable (chosen among:" + LS //
			+ "# 		   'respiration')" + LS //
			+ "# 		  - datesToObserve can be formated as follow:" + LS //
			+ "#			* [0,2,5] to observe date 0, 2 and 5" + LS //
			+ "#			* [1:2:11] to observe date between 1 and 11 with a step 2" + LS //
			+ "#			* [1:2:14,25,33] which is a combination of the two previous" + LS //
			+ "#			  formats" + LS //
			+ "#		POOL_OBSERVER \\t [bcName, accessibility]" + LS //
			+ "# 						...\\t observableVariable \\t datesToObserve" + LS //
			+ "#		  - bcName (String) is the biochemical class considered" + LS //
			+ "# 		  - accessibility (String) is the accessibility of the pool" + LS //
			+ "#			considered, observableVariable is the observed variable " + LS //
			+ "#			(chosen among 'mass','mass_distribution')" + LS //
			+ "# 		  - datesToObserve can be formated as above" + LS //
			+ "#		POOL_TRANSFER_OBSERVER \\t [bcName,originPool,arrivalPool]" + LS //
			+ "# 		  					...\\t observableVariable \\t datesToObserve" + LS //
			+ "# 		  - bcName (String) is the biochemical class considered" + LS //
			+ "# 		  - originPool (String) is the accessibility of the origin " + LS //
			+ "#			pool" + LS //
			+ "# 		  - arrivalPool (String) is the accessibility of the arrival" + LS //
			+ "#			pool" + LS //
			+ "# 		  - observableVariable is the observed variable (chosen among" + LS //
			+ "#			'flux_distribution')" + LS //
			+ "# 		  - datesToObserve can be formated as above" + LS //
			+ "#		MICROBE_OBSERVER \\t micName " + LS //
			+ "#							...\\t observableVariable \\t datesToObserve" + LS //
			+ "#		  - micName (String) is the name of the considered microbes" + LS //
			+ "#		  - observableVariable is the observed variable (chosen among" + LS //
			+ "#			'mass', 'uptake_flux_distribution_map', 'respiration'," + LS //
			+ "#			'carbon_use_efficiency_distribution_map', 'mortality_flux')" + LS //
			+ "# 		  - datesToObserve can be formated as above" + LS //
			+ "#		ENZYME_OBSERVER \\t enzName " + LS //
			+ "#							...\\t observableVariable \\t datesToObserve" + LS //
			+ "#		  - enzName is the name of the considered enzymes" + LS //
			+ "#		  - observableVariable is the observed variable (chosen among" + LS //
			+ "#			'depolymerization_rate_distribution', " + LS //
			+ "#			'activity_distribution')" + LS //
			+ "# 		  - datesToObserve can be formated as above" + LS //
			+ "#" + LS //
			+ "#	Examples:" + LS //
			+ "#		STATE_OBSERVER \\t respiration \\t [0,1]" + LS //
			+ "#		POOL_OBSERVER \\t [cellulose,ACCESSIBLE] \\t mass \\t [0,1]" + LS //
			+ "#		POOL_TRANSFER_OBSERVER" + LS //
			+ "#				...\\t [cellulose,INACCESSIBLE_EMBEDMENT,ACCESSIBLE]" + LS //
			+ "#					...\\t flux_distribution \\t [0,1]" + LS //
			+ "#		MICROBE_OBSERVER \\t cellulose_degrader" + LS //
			+ "#				...\\t uptake_flux_distribution \\t [0,1]" + LS //
			+ "#		ENZYME_OBSERVER \\t cellulolysis" + LS //
			+ "#				...\\t depolymerization_rate_distribution \\t [0,1]" + LS //
			+ "########################################################################" + LS;

	public Map<Encoder, String> encode();

	public String encode(Encoder encoder);

	public Set<Encoder> getEncoders();

}
