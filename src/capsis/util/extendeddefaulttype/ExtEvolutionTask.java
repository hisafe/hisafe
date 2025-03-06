package capsis.util.extendeddefaulttype;

import capsis.kernel.EvolutionParameters;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.ExtModel.MessageID;
import repicea.app.AbstractGenericTask;
import repicea.gui.genericwindows.REpiceaProgressBarDialog;

/**
 * The ExtEvolutionTask class is a SwingWorker instance that handles
 * the simulation.
 * @author Mathieu Fortin - 2015
 */
public class ExtEvolutionTask extends AbstractGenericTask {

	private Step stp;
	private final EvolutionParameters e;
	private final ExtModel model;
	Step newStp = null;
	
	protected ExtEvolutionTask(ExtModel model, Step stp, EvolutionParameters evolParms) {
		super();
		this.model = model;
		this.setName("EvolutionTask");
		this.stp = stp;
		this.e = evolParms;
	}
	
	
	@Override
	protected void doThisJob() throws Exception {
		ExtEvolutionParametersList<?> oVec = ((ExtEvolutionParametersList) e);
		int numberOfGrowthStepsSoFar = 0;
		for (ExtEvolutionParameters stepEvolution : oVec) {
			if (stepEvolution.isGoingToBeInterventionResult()) {
				displayThisMessage(MessageID.HarvestingStand.toString() + " " + stp.getCaption());
				newStp = model.processCutting(stp, stepEvolution);
				stp = newStp;
			} else {
				displayThisMessage(MessageID.EvolutionOf.toString() + " " + stp.getCaption());
				newStp = model.processEvolution(stp, stepEvolution, numberOfGrowthStepsSoFar, this);
				stp = newStp;
			}
			numberOfGrowthStepsSoFar += stepEvolution.getNbSteps();				
		}
		displayThisMessage(MessageID.EvolutionIsOver.toString());
	}
	
	public void displayThisMessage(String message) {
		firePropertyChange(REpiceaProgressBarDialog.LABEL, "", message);
	}
	
}
