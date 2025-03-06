//package capsis.lib.samsaralight.tag.heterofor;
//
//import java.io.Serializable;
//
///***
// * Container for the sums over the vegetation period of direct energies and
// * diffuse energies, computed in SLBeamSetFactoryOptimized.addBeamsForPeriod.
// * Can be used when interpreting tree's or cell's tag results of the period
// * (tagged LEAVED_PERIOD).
// * 
// * Update cr-25.02.2022 : I decided not to used period mode in PDG Light, then I disable this class.
// * 
// * @author Camille Rouet - January 2022
// */
// public class SLIncidentEnergySumsOverPeriod implements Serializable {
//
//	// For each direction of diffuse ray, we will have 24 rays with their
//	// hour tag
//	// So we need 24 sums of energies over the period
//	private double[] diffuseEnergies = new double[24];
//
//	// For each month, we will have 24 rays with their hour tag
//	// Note: hour fractions not yet implemented
//	private double[][] directEnergies = new double[12][24];
//	// first index is the month, second index is the hour
//
//	/**
//	 * Constructor
//	 */
//	public SLIncidentEnergySumsOverPeriod(double[] diffuseEnergies, double[][] directEnergies) {
//		super();
//		this.diffuseEnergies = diffuseEnergies;
//		this.directEnergies = directEnergies;
//	}
//
//	public double[] getDiffuseEnergies() {
//		return diffuseEnergies;
//	}
//
//	public double[][] getDirectEnergies() {
//		return directEnergies;
//	}
//
//}
