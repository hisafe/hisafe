/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2013 Benoit Courbaud, Frédéric Gosselin, Francois Coligny
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program; if
 * not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */
package capsis.lib.math.distribution;

import java.util.Random;

import org.apache.commons.math3.random.RandomGenerator;
import org.apache.commons.math3.random.RandomGeneratorFactory;
import org.apache.commons.math3.distribution.GammaDistribution;
import org.apache.commons.math3.distribution.PoissonDistribution;

import jeeb.lib.util.Log;

/**
 * A negative binomial distribution.
 *
 * @author F. Gosselin, B. Courbaud - March 2013
 *
 * modified in october 2020 by J. Sainte-Marie to include a constructor with specified RandomGenerator
 */
public class NegativeBinomialDistribution {

	public static final String MU_SIZE = "MU_SIZE";
	public static final String R_P = "R_P";

	private GammaDistribution gamma;
	private RandomGenerator randomGenerator;
	
	/**
	 * Constructor. If mode is MU_SIZE, param1 is mu and param2 is size. If mode
	 * is R_P, they are resp. r and p.
	 */
	public NegativeBinomialDistribution(String mode, double param1, double param2) {
		this(RandomGeneratorFactory.createRandomGenerator(new Random()), mode, param1, param2);
	}

	/**
	 * Constructor with specified randomGenerator. If mode is MU_SIZE, param1 is mu and param2 is size. If mode
	 * is R_P, they are resp. r and p.
	 */
	public NegativeBinomialDistribution(RandomGenerator randomGenerator,
										String mode,
										double param1,
										double param2) {

		this.randomGenerator = randomGenerator;
		
		if (mode.equals(MU_SIZE)) {
			double mu = param1;
			double size = param2;
			initMuSize(mu, size);

		} else if (mode.equals(R_P)) {
			double r = param1;
			double p = param2;
			initRP(r, p);

		} else {
			// gamma stays null, sample () will fail
			Log.println(Log.ERROR, "NegativeBinomialDistribution.c ()",
					"Wrong mode: " + mode + ", expected " + MU_SIZE + " or " + R_P);
		}

	}
	
	/**
	 * Original function by F. Gosselin and B. Courbaud - March 2013.
	 */
	private void initMuSize(double mu, double size) {

		double p = 1 - 1 / size;
		double logitp = (1 - p) / p;
		double shape = mu * logitp;
		double scale = 1 / logitp;
		gamma = new GammaDistribution(randomGenerator, shape, scale);

	}

	/**
	 * Another way to build the object, with r (> 0, number of failures until
	 * the experiment stops) and p (success probability in each experiment in
	 * [0, 1]) from Wikipedia:
	 * en.wikipedia.org/wiki/Negative_binomial_distribution
	 * 
	 * @author Julien Sainte-Marie (Simcop_Qual), August 2020
	 */
	private void initRP(double r, double p) {

		double shape = r;
		double scale = p / (1 - p);
		gamma = new GammaDistribution(randomGenerator, shape, scale);

	}

	public int sample() throws Exception {

		double m = gamma.sample();
		PoissonDistribution poisson = new PoissonDistribution(randomGenerator,
															  m,
															  PoissonDistribution.DEFAULT_EPSILON,
															  PoissonDistribution.DEFAULT_MAX_ITERATIONS);

		return poisson.sample();
	}

}
