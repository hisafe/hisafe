/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.modeltool.rockfornet;

import java.util.Map;

import jeeb.lib.util.Command;

/**
 * Rockfornet simulator
 *
 * @author Eric Mermin, Eric Maldonado - november 2006
 */
public class Rockfornet implements Command {

	private RockfornetSettings settings;
	private RockfornetResult result;

	/**	
	*/
	public Rockfornet(RockfornetSettings settings) {
		this.settings = settings;
		result = new RockfornetResult();
		System.out.println("settings");
	}

	/**	
	*/
	public int execute() {
		System.out.println("------ Rockfornet.execute ()... ------");
		System.out.println("settings=" + settings);
		System.out.println("------ Rockfornet.execute () end-of-method");
		calcul();
		return 0;
		/*
		 * System.out.println("sum="+sum); System.out.println ("Rockfornet.execute ()");
		 */
	}

	public boolean testDisque() {
		boolean test = false;

		if (settings.rockDiameter1 < 0.3 * settings.rockDiameter2
				&& settings.rockDiameter1 < 0.3 * settings.rockDiameter3)
			test = true;
		else if (settings.rockDiameter2 < 0.3 * settings.rockDiameter1
				&& settings.rockDiameter2 < 0.3 * settings.rockDiameter3)
			test = true;
		else if (settings.rockDiameter3 < 0.3 * settings.rockDiameter2
				&& settings.rockDiameter3 < 0.3 * settings.rockDiameter1)
			test = true;
		else
			test = false;
		return test;
	}

	public double arrondi(double nombre, char precision) {

		int precis = 1;
		switch (precision) {
		case '0':
			precis = 1;
			break;
		case '1':
			precis = 10;
			break;
		case '2':
			precis = 100;
			break;
		case '3':
			precis = 1000;
			break;

		}

		nombre *= precis;

		nombre = Math.floor(Math.round(nombre));

		nombre /= precis;

		return nombre;

	}

	public RockfornetResult getResult() {
		return result;
	}

	public void calcul() {

		double Result, Required_stand_density, Required_DBH;
		double Rock_diameterin1, Rock_diameterin2, Rock_diameterin3;
		double Gradient, Fheight, Slength, Slength_unfp, DBH, Stand_density;
		double Abies, Larix, Picea, Pinusnigra, Pinussyl, Acer, Fagus, Quercus, Robinia;
		double Rock_diam, Rockvolume, Rock_mass, EkinKJ, DBHin, Distance_tot;
		double Gtot;
		int Rock_density;

		double Rock_diams11, Rock_diams12, Rock_diams21, Rock_diams22, Rock_diams31, rockDiameter, Rock_diams32;
		double Fin_Required_DBH, min_Required_DBH, Fin_Required_stand_density;
		double PercStopped, max_Required_DBH, G2bintercepted2, G2bintercepted;
		double Gintercepted_theoretical, DBHm, Nr_req_curtains, MaxEdiss, Ekin_a_diss, Mean_Tree_Ediss_factor,
				slope_length_plan;
		double Gtot_required, diam_varying_factor;
		double Fin_Result;

		double Result_fin[] = { 0, 0, 0 };
		double Required_stand_density_fin[] = { 0, 0, 0 };

		double Required_DBH_fin[] = { 0, 0, 0 };

		Rock_mass = 0;

		EkinKJ = 0;
		double Rockdiams[] = { 0, 0, 0 };
		Rock_diam = 0;

		double energy_factor = 2.8;

		double distance_between_curtains = 33;

		double Energy_line_angle_in = 32;
		diam_varying_factor = 0.05;

		DBH = settings.meanStemDbh_cm;
		Gtot = settings.standBasalArea_m2ha;
		Mean_Tree_Ediss_factor = 0;
		Rock_density = settings.rockType;
		Gradient = settings.slope;
		Slength = settings.lengthForestedSlope;
		Slength_unfp = settings.lengthNonForestedSlope;
		Stand_density = settings.meanStandDensity;
		Fheight = 0;

		Rock_diams11 = settings.rockDiameter1 - (diam_varying_factor * settings.rockDiameter1);
		Rock_diams12 = settings.rockDiameter1 + (diam_varying_factor * settings.rockDiameter1);
		double Rock_diam1_arr[] = { 0, Rock_diams11, Rock_diams12 };

		Rock_diams21 = settings.rockDiameter2 - (diam_varying_factor * settings.rockDiameter2);
		Rock_diams22 = settings.rockDiameter2 + (diam_varying_factor * settings.rockDiameter2);
		double Rock_diam2_arr[] = { 0, Rock_diams21, Rock_diams22 };

		Rock_diams31 = settings.rockDiameter3 - (diam_varying_factor * settings.rockDiameter3);
		Rock_diams32 = settings.rockDiameter3 + (diam_varying_factor * settings.rockDiameter3);
		double Rock_diam3_arr[] = { 0, Rock_diams31, Rock_diams32 };
		double Max_Required_stand_density;
		double Vmax = 0;
		double Rock_diameter, Rock_diameter1, Rock_diameter2, Rock_diameter3, Rradius, Rradiusin;
		double Rock_vol_disc, Rock_vol_discfin;
		double Rock_vol = 0;
		double Rock_volfin = 0;

		double triangle;

		double sl_plan;
		double Hdiff = 0;
		double MTFD = 0;
		double Ekin = 0;
		float m = 4;
		System.out.println("Rock_diam=" + Rock_diam);
		for (int i = 1; i <= 2; i++) {
			System.out.println("Rock_diam=" + Rock_diam);

			Rock_diameter1 = Rock_diam1_arr[i];
			Rock_diameter2 = Rock_diam2_arr[i];
			Rock_diameter3 = Rock_diam3_arr[i];
			System.out.println("Rock_diameter1=" + Rock_diameter1);
			System.out.println("Rock_diameter2=" + Rock_diameter2);
			System.out.println("Rock_diameter3=" + Rock_diameter3);

			Rock_diameter = (Rock_diameter1 + Rock_diameter2 + Rock_diameter3) / 3;
			Rradius = Rock_diameter / 2;
			rockDiameter = (settings.rockDiameter1 + settings.rockDiameter2 + settings.rockDiameter3) / 3;
			Rradiusin = rockDiameter / 2;

			if (settings.rockDiameter1 < (0.3 * settings.rockDiameter2)
					&& settings.rockDiameter1 < (0.3 * settings.rockDiameter3)) {
				System.out.println("test 1");
				Rock_diameter = Rock_diameter1;
				Rock_vol_disc = 4 * Math.PI * (Rock_diameter2 / 2) * (Rock_diameter3 / 2) * Rock_diameter1;
				Rock_vol_discfin = 4 * Math.PI * (settings.rockDiameter2 / 2) * (settings.rockDiameter3 / 2)
						* settings.rockDiameter1;
				System.out.println("Disc");

				settings.rockShape = RockfornetSettings.DISC;
			} else if (settings.rockDiameter2 < (0.3 * settings.rockDiameter1)
					&& settings.rockDiameter2 < (0.3 * settings.rockDiameter3)) {
				System.out.println("test 2");
				Rock_diameter = Rock_diameter2;
				Rock_vol_disc = 4 * Math.PI * (Rock_diameter1 / 2) * (Rock_diameter3 / 2) * Rock_diameter2;
				Rock_vol_discfin = 4 * Math.PI * (settings.rockDiameter1 / 2) * (settings.rockDiameter3 / 2)
						* settings.rockDiameter2;
				settings.rockShape = RockfornetSettings.DISC;

			} else if (settings.rockDiameter3 < (0.3 * settings.rockDiameter2)
					&& settings.rockDiameter3 < (0.3 * settings.rockDiameter1)) {
				Rock_diameter = Rock_diameter3;
				Rock_vol_disc = 4 * Math.PI * (Rock_diameter1 / 2) * (Rock_diameter2 / 2) * Rock_diameter3;
				Rock_vol_discfin = 4 * Math.PI * (settings.rockDiameter1 / 2) * (settings.rockDiameter2 / 2)
						* settings.rockDiameter3;
				System.out.println("test 3");
				settings.rockShape = RockfornetSettings.DISC;
			} else {

				Rock_vol_disc = (m / 3 * Math.PI * Rock_diameter1 / 2 * Rock_diameter2 / 2 * Rock_diameter3 / 2);
				Rock_vol_discfin = (m / 3 * Math.PI * settings.rockDiameter1 / 2 * settings.rockDiameter2 / 2
						* settings.rockDiameter3 / 2);
				System.out.println("test 4");
			}

			if (settings.rockShape == RockfornetSettings.RECTANGULAR) {
				System.out.println("rect");
				Rock_vol = Rock_diameter1 * Rock_diameter2 * Rock_diameter3;
				Rock_volfin = settings.rockDiameter1 * settings.rockDiameter2 * settings.rockDiameter3;
			} else if (settings.rockShape == RockfornetSettings.ELLIPSOID) {
				Rock_vol = (4.0 / 3.0) * Math.PI * (Rock_diameter1 / 2) * (Rock_diameter2 / 2) * (Rock_diameter3 / 2);
				Rock_volfin = (4.0 / 3.0) * Math.PI * (settings.rockDiameter1 / 2) * (settings.rockDiameter2 / 2)
						* (settings.rockDiameter3 / 2);
			} else if (settings.rockShape == RockfornetSettings.SPHERE) {
				System.out.println("sphere");

				Rock_vol = (m / 3) * Math.PI * Rradius * Rradius * Rradius;
				Rock_volfin = (m / 3) * Math.PI * Rradiusin * Rradiusin * Rradiusin;
				System.out.println("Rock_vol=" + Rock_vol);
				System.out.println("Rock_volfin =" + Rock_volfin);
				System.out.println("i=" + i);
				System.out.println("sphere");
			} else {

				Rock_vol = Rock_vol_disc;
				Rock_volfin = Rock_vol_discfin;
			}

			Rock_density = settings.rockType;
			System.out.println(Rock_density);

			energy_factor = 3.352;
			distance_between_curtains = 24;
			triangle = 10;
			Energy_line_angle_in = 31;

			if (DBH == 0) {
				DBH = arrondi(200 * Math.sqrt(Gtot / (Stand_density * Math.PI)), '1');
			} else {
				Gtot = arrondi(Stand_density * Math.PI * (DBH / 200) * (DBH / 200), '1');
			}
			Rockdiams[i] = Rock_diameter;
			Distance_tot = arrondi((Slength + Slength_unfp), '0');
			sl_plan = (Math.cos(Math.toRadians(Gradient)) * Slength)
					+ (Math.cos(Math.toRadians(Gradient)) * Slength_unfp);
			Hdiff = ((Math.tan(Gradient * 2 * Math.PI / 360) * sl_plan)
					- (Math.tan(Energy_line_angle_in * 2 * Math.PI / 360) * sl_plan)) + Fheight;

			if (Hdiff <= 0) {
				Result = 1;
			} else {
				Hdiff = ((Math.tan(Gradient * 2 * Math.PI / 360) * sl_plan)
						- (Math.tan(Energy_line_angle_in * 2 * Math.PI / 360) * sl_plan));
				Vmax = Math.sqrt(Hdiff * 2 * 9.81);
				MTFD = 10000 / (Stand_density * (Rock_diameter + (DBH / 100)));

				if ((Vmax > (0.64 * Gradient)) && (Gtot >= 10)) {
					Vmax = (0.64 * Gradient);
				} else if ((Vmax > (0.64 * Gradient)) && (Gtot < 10))

					Vmax = (0.8 * Gradient);

				Rock_mass = (Rock_vol * Rock_density);
				Ekin = (Vmax * Vmax * 0.5 * Rock_mass) + (0.25 * Rock_mass * 9.81 * Fheight);

				slope_length_plan = (Math.cos(Math.toRadians(Gradient)) * Slength);
				double Forest_area;
				Forest_area = slope_length_plan * slope_length_plan * Math.tan(Math.toRadians(triangle));

				// Mean_Tree_Ediss_factor = (Abies + 0.9*Larix + 0.9*Picea + 1.1*Pinusnigra +
				// 1.1*Pinussyl + 1.1*Acer + 1.5*Fagus + 2.2*Quercus + 2.7*Robinia)
				// / (Abies+Larix+Picea+Pinusnigra+Pinussyl+Acer+Fagus+Quercus+Robinia);

				// fc+bc-16.2.2022
				Map<String, Double> per = settings.percentage;

				// fc+bc-16.2.2022
				Mean_Tree_Ediss_factor = (per.get(RockfornetSpecies.REFERENCE_ABIES)
						+ 0.9 * per.get(RockfornetSpecies.REFERENCE_PICEA)
						+ 1.5 * per.get(RockfornetSpecies.REFERENCE_FAGUS)
						+ 0.9 * per.get(RockfornetSpecies.REFERENCE_OTHER))
						/ (per.get(RockfornetSpecies.REFERENCE_ABIES) + per.get(RockfornetSpecies.REFERENCE_PICEA)
								+ per.get(RockfornetSpecies.REFERENCE_FAGUS)
								+ per.get(RockfornetSpecies.REFERENCE_OTHER));
//		Mean_Tree_Ediss_factor = (settings.percentage[1]+ 0.9*settings.percentage[0]+ 1.5 * settings.percentage[2])
//				/ (settings.percentage[1]+settings.percentage[0]+settings.percentage[2]);

				if (DBH > 0) {
					MaxEdiss = Mean_Tree_Ediss_factor * 38.7 * (Math.exp(2.31 * Math.log(DBH)));

					DBHm = DBH / 100;
				} else {
					MaxEdiss = 1;
					DBHm = 0.001;
				}
				Ekin_a_diss = Ekin * energy_factor;

				if (Ekin_a_diss > 0) {
					Required_DBH = Math
							.exp((Math.log(((1 / (slope_length_plan / distance_between_curtains)) * (Ekin_a_diss))
									/ (Mean_Tree_Ediss_factor * 38.7))) / 2.31);
				} else {
					Required_DBH = 0;
				}

				Nr_req_curtains = Ekin_a_diss / MaxEdiss;

				G2bintercepted = (Nr_req_curtains * Math.PI * DBHm * DBHm) / 4;
				Gintercepted_theoretical = ((Rock_diameter * slope_length_plan) / 10000) * Gtot;
				PercStopped = (Gintercepted_theoretical * 100) / G2bintercepted;
				G2bintercepted2 = (Nr_req_curtains * Math.PI * (Required_DBH * Required_DBH / 10000)) / 4;
				Gtot_required = G2bintercepted2 / (((Rock_diameter * slope_length_plan * 0.95) / 10000));
				Required_stand_density = (4 * Gtot_required) / (Math.PI * Required_DBH * Required_DBH / 10000);

				if (PercStopped > 99) {
					PercStopped = 99;
				}
				Result = 100 - PercStopped;

				if (Result < 0) {
					Result = 0;
				}
				Result_fin[i] = Result;
				Required_stand_density_fin[i] = Required_stand_density;
				Required_DBH_fin[i] = Required_DBH;
			}
		}
		Fin_Result = (Result_fin[1] + Result_fin[2]) / 2;
		Fin_Required_stand_density = (Required_stand_density_fin[1] + Required_stand_density_fin[2]) / 2;
		Max_Required_stand_density = arrondi(
				arrondi(Math.max(Required_stand_density_fin[1], Required_stand_density_fin[2]), '0') / 10, '0') * 10;
		Fin_Required_DBH = (Required_DBH_fin[1] + Required_DBH_fin[2]) / 2;
		min_Required_DBH = arrondi(Math.min(Required_DBH_fin[1], Required_DBH_fin[2]), '0');
		max_Required_DBH = arrondi(Math.max(Required_DBH_fin[1], Required_DBH_fin[2]), '0');

		if (Fin_Result > 100) {
			Fin_Result = 100;
		}

		char precision;
		if (Rock_volfin < 0.01) {
			precision = '3';
		} else {
			precision = '2';
		}
		Rockvolume = arrondi(Rock_volfin, precision);

		Result = arrondi(Fin_Result, '0');

		if (DBH <= 0) {
			Required_stand_density = 0;
		}

		if (Fin_Required_DBH > 40 && DBH < 40) {
			Fin_Required_DBH = 40;
		}
		if (min_Required_DBH > 30) {
			min_Required_DBH = 30;
		}
		if (max_Required_DBH > 40 && DBH < 40) {
			max_Required_DBH = 40;
		}

		Required_DBH = arrondi(Fin_Required_DBH, '0');
		DBHin = arrondi(DBH, '0');
		Rock_diam = arrondi(((Rockdiams[1] + Rockdiams[2]) / 2), '2');
		Rock_mass = arrondi((Rock_density * Rock_volfin), '0');
		EkinKJ = arrondi(((Vmax * Vmax * 0.5 * Rock_mass + 0.25 * (Rock_mass * 9.81 * Fheight)) / 1000), '1');
		Required_stand_density = arrondi(Fin_Required_stand_density / 10, '0') * 10;

		if (EkinKJ < 1) {
			Result = 1;
		}

		// ~ result.probableResidualRockfallHazard= new Double(arrondi
		// (Fin_Result,'0')).toString();
		// ~ result.meanStemDiameter= new Double(min_Required_DBH).toString() + " - "
		// +new Double(max_Required_DBH).toString();
		// ~ result.standDensity = new Double(Required_stand_density).toString() + " - "
		// + new Double(Max_Required_stand_density).toString();

		// fc - 5.7.2007 - replaced the three above lines by the 5 following
		result.setProbableResidualRockfallHazard(new Double(arrondi(Fin_Result, '0')).doubleValue());
		result.setMeanStemDiameterMin(new Double(min_Required_DBH).doubleValue());
		result.setMeanStemDiameterMax(new Double(max_Required_DBH).doubleValue());
		result.setStandDensityMin(new Double(Required_stand_density).doubleValue());
		result.setStandDensityMax(new Double(Max_Required_stand_density).doubleValue());

		System.out.println("Required_DBH=" + Required_DBH);
		System.out.println("DBHin=" + DBHin);
		System.out.println("Rock_diam=" + Rock_diam);
		System.out.println("Rock_mass=" + Rock_mass);
		System.out.println("EkinKJ=" + EkinKJ);
		System.out.println("Required_stand_density=" + Required_stand_density);
		System.out.println("Fin_Result =" + Fin_Result);
		System.out.println("Fin_Required_stand_density=" + Fin_Required_stand_density);
		System.out.println("Max_Required_stand_density=" + Max_Required_stand_density);
		System.out.println("Fin_Required_DBH =" + Fin_Required_DBH);
		System.out.println("min_Required_DBH =" + min_Required_DBH);
		System.out.println("max_Required_DBH =" + max_Required_DBH);

	}

}
