package capsis.lib.math.distribution;

import org.apache.commons.math.MathException;
import org.apache.commons.math.distribution.NormalDistributionImpl;



/**
 * @author sdufour
 * Truncated normal distribution
 * lo and hi are the lower and the upper trunctation limite
 */
public class TNormalDistributionImpl extends NormalDistributionImpl {

	private double plo;
	private double phi;
//	private double lo;
//	private double hi;
	
	
	public TNormalDistributionImpl(double mean, double sd, double lo, double hi) throws MathException {
		super(mean, sd);
		
//		this.lo = lo;
//		this.hi = hi;

		// min and max propability corresponding to lo and hi
		this.plo = super.cumulativeProbability(lo);
		this.phi = super.cumulativeProbability(hi);
	}
	
	//public double cumulativeProbability(double p){
		
	//}
	
	public double inverseCumulativeProbability(double p)throws MathException {
		
		p = p * (phi-plo) + plo;
	    return super.inverseCumulativeProbability(p);
	}
}
