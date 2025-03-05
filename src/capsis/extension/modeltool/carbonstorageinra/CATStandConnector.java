/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2021 Mathieu Fortin (Canadian Wood Fibre Centre)
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
package capsis.extension.modeltool.carbonstorageinra;

import java.util.Collection;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import lerfob.carbonbalancetool.CATCompatibleStand;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;

/**
 * The CATStandConnector interface adds default methods adapted to CAPSIS for the connection with
 * the Carbon Accounting Tool (CAT).
 * @author Mathieu Fortin - June 2021
 *
 */
public interface CATStandConnector extends CATCompatibleStand {

	@Override
	public default CATCompatibleStand getHarvestedStand() {
		GScene myScene = (GScene) this;
		GScene newScene = myScene.getInterventionBase();
		Collection<? extends Tree> trees = ((TreeList) newScene).getTrees();
		for (Tree tree : trees) {
			if (tree instanceof Numberable) {
				((TreeList) newScene).storeStatus((Numberable) tree, "cut", ((Numberable) tree).getNumber());
			} else {
				((TreeList) newScene).storeStatus(tree, "cut");
			}
		}
		((TreeList) newScene).clearTrees();
		try {
			// fc-7.12.2016 the line below was before the try {
			Step step = myScene.getStep();
			step.getProject().getModel().processPostIntervention(myScene, newScene);
			step.getProject().processNewStep(step, newScene, "Final cut");
		} catch (Exception e) { // fc-7.12.2016 was InterruptedException
			e.printStackTrace();
		}
		newScene.setInterventionResult(true);
		return (CATCompatibleStand) newScene;
	}

	@Override
	public default Collection<Tree> getTrees (StatusClass statusClass) {
		TreeList myScene = (TreeList) this;
		return myScene.getTrees(statusClass.name());
	}

	@Override
	public default String getStandIdentification() {
		Step step = ((GScene) this).getStep();
		return step.getProject().getName() + " - " + step.getName();
	}

	@Override
	public default double getAreaHa() {return ((GScene) this).getArea() * 0.0001;}

	@Override
	public default int getDateYr() {return ((GScene) this).getDate();}

}
