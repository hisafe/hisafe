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

import java.awt.Window;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import javax.swing.JOptionPane;

import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.StepButton;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.modeltool.woodqualityworkshop.CapsisTreeLoggerDescription;
import capsis.extensiontype.ModelTool;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import lerfob.carbonbalancetool.CATCompartmentManager;
import lerfob.carbonbalancetool.CATCompatibleStand;
import lerfob.carbonbalancetool.CarbonAccountingTool.CATMode;
import repicea.gui.CommonGuiUtility;
import repicea.gui.UIControlManager;
import repicea.gui.dnd.DragGestureImpl;
import repicea.simulation.treelogger.TreeLoggerCompatibilityCheck;
import repicea.simulation.treelogger.TreeLoggerDescription;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * CarbonAccountingToolWrapper is the class that wraps the Carbon Accounting Tool (CAT) 
 * for Capsis Extension management.<p>
 * In script mode, one should proceed as follows:
 * <ol>
 * <li> Instantiate the class through the {@link CarbonAccountingToolWrapper#CarbonAccountingToolWrapper(GModel, Step)}
 * constructor. 
 * <li> A CAT instance is then obtained from the wrapper using the {@link CarbonAccountingToolWrapper#getCAT()} method.
 * </ol>
 * @author Mathieu Fortin - June 2021
 */
public final class CarbonAccountingToolWrapper implements ModelTool, Listener {
	
	/**
	 * The CAT class extends the original CarbonAccountingTool class.<p>
	 * It adds this features to CAT:
	 * <ul>
	 * <li> Allow the dragging and dropping of steps from CAPSIS directly into CAT GUI.  
	 * <li> Retrieve the TreeLogger instances in CAPSIS that applies to the model.
	 * <li> Clean up the additional harvested stands created by CAT when the app is shut down.
	 * </ul>
	 * @author Mathieu Fortin - June 2021
	 */
	public final class CAT extends lerfob.carbonbalancetool.CarbonAccountingTool {
		
		CAT(CATMode catMode) {
			super(catMode);
		}
		
		
		@Override
		protected Vector<TreeLoggerDescription> findMatchingTreeLoggers(TreeLoggerCompatibilityCheck check) {
			Vector<TreeLoggerDescription> treeLoggerDescriptions = super.findMatchingTreeLoggers(check);
			if (model != null) {
				List<TreeLoggerDescription> additionalTreeLoggers = new ArrayList<TreeLoggerDescription>();
				additionalTreeLoggers.addAll(CapsisTreeLoggerDescription.getMatchingTreeLoggers(model));
				for (TreeLoggerDescription desc : additionalTreeLoggers) {
					if (!treeLoggerDescriptions.contains(desc)) {
						treeLoggerDescriptions.add(desc);
					}
				}
			}
			return treeLoggerDescriptions;
		}

		/*
		 * For extended visibility
		 */
		@Override
		protected boolean isGuiEnabled() {
			return super.isGuiEnabled();
		}
		
		protected Window getParentFrame() {
			return this.parentFrame;
		}
		
		
		@Override
		protected void setStandList() {
			clearAdditionalStep();
			CATCompatibleStand s = waitingStandList.get(0);
			if (s instanceof GScene){
				model = ((GScene) s).getStep().getProject().getModel();
			} else {
				model = null;
			}
			
			super.setStandList();
		}
		
		@Override
		protected void shutdown(int shutdownCode) {
			clearAdditionalStep();
			if (!stepButtons.isEmpty()) {
				for (StepButton stepButton : stepButtons.keySet()) {
					stepButtons.get(stepButton).removeDragGestureListener(dgl);
				}
				stepButtons.clear();
				if (parentFrame instanceof MainFrame) {
					Current.getInstance().removeListener(CarbonAccountingToolWrapper.this);
				}
			}
			super.shutdown(shutdownCode);
		}

		private void clearAdditionalStep() {
			if (finalCutHadToBeCarriedOut) {
			/*	Step stepToBeDeleted = ((GScene) getCarbonCompartmentManager().getLastStand()).getStep();
				if (stepToBeDeleted != null) {
					stepToBeDeleted.setVisible(true);
					stepToBeDeleted.getProject().processDeleteStep(stepToBeDeleted);
					getCarbonCompartmentManager().init(null);
					System.out.println("Additional step eliminated!");
				}*/
			}
			

		}


		/*
		 * For extended visibility in the tests (non-Javadoc)
		 * @see lerfob.carbonbalancetool.CarbonAccountingTool#getCarbonCompartmentManager()
		 */
		@Override
		protected CATCompartmentManager getCarbonCompartmentManager() {
			return super.getCarbonCompartmentManager();
		}
	}
	
	
	protected static class ExtendedDragGestureImpl extends DragGestureImpl<ArrayList<CATCompatibleStand>> {
				
		@Override
		public void dragGestureRecognized(DragGestureEvent event) {
			if (event.getComponent() instanceof StepButton) {
				Step step = ((StepButton) event.getComponent()).getStep();
				if (step.getScene () instanceof CATCompatibleStand) {
					super.dragGestureRecognized(event);
				}
			}
		}

		
		@Override
		protected ArrayList<CATCompatibleStand> adaptSourceToTransferable(DragGestureEvent event) {
			Step step = ((StepButton) event.getComponent()).getStep();
			return CarbonAccountingToolWrapper.getStandList(step);
		}

	}
	
	protected static final String englishTitle = "LERFoB Carbon Accounting Tool (LERFoB-CAT)";
	protected static final String frenchTitle = "Outil de comptabilit\u00E9 du carbone LERFoB (LERFoB-CAT)";
	
	private static enum MessageID implements TextableEnum {

		Name(englishTitle, frenchTitle),
		Description("Accounting Tool for carbon, wood product carbon, substituted carbon, and wood-processing carbon emissions",
				"Outil de comptabilit\u00E9 du carbone sur pied, du carbone contenu dans les produits du bois, du carbone substitu\u00E9 et du carbone \u00E9mis par la production de produits du bois"),
		CATAlreadyRunning("The LERFoB-CAT tool is already running! Please refer to the appropriate window.", "L'outil LERFoB-CAT est d\u00E9j\u00E0 ouvert! Veuillez utiliser la fen\u00EAtre appropri\u00E9e.");
		
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}
	
	// nb-06.08.2018
	//static public final String AUTHOR="M. Fortin";
	//static public final String VERSION="2.0";
	//public static final String NAME = REpiceaTranslator.getString(MessageID.Name);
	//public static final String DESCRIPTION = REpiceaTranslator.getString(MessageID.Description);

	private static Map<StepButton, DragGestureRecognizer> stepButtons = new HashMap<StepButton, DragGestureRecognizer>();
	private static final ExtendedDragGestureImpl dgl = new ExtendedDragGestureImpl();
	private GModel model;
	private final CATMode catMode;
	private CAT cat;
	
	/**
	 * Empty constructor for the extension manager.
	 */
	public CarbonAccountingToolWrapper() {
		catMode = CATMode.FROM_OTHER_APP;
	}

//	public CarbonAccountingTool(CATMode mode) {
//		super(mode);
//	}
	
	/**
	 * Constructor in script mode.
	 * @param mod a GModel instance
	 * @param step a Step instance
	 */
	public CarbonAccountingToolWrapper(GModel mod, Step step) {
		catMode = CATMode.SCRIPT;
		init(mod, step);
	}
	
	
	/**
	 * This method initializes the carbon accounting tool either in script or in GUI mode.
	 * @param model a GModel instance
	 * @param lastStep the last step
	 */
	@Override
	public void init(GModel model, Step lastStep) {
		cat = new CAT(catMode);
		if (!stepButtons.isEmpty()) {	// means the application is already running
			JOptionPane.showMessageDialog(MainFrame.getInstance(), 
					MessageID.CATAlreadyRunning.toString(), 
					UIControlManager.InformationMessageTitle.Information.toString(), 
					JOptionPane.INFORMATION_MESSAGE);
			return;
		} else {
			Window parentFrame = null;
			if (cat.isGuiEnabled()) {
				parentFrame = MainFrame.getInstance();
				Current.getInstance().addListener(this);
			}
			try {
				cat.initializeTool(parentFrame);
				List<CATCompatibleStand> stands = getStandList(lastStep);
				cat.setStandList(stands);
				if (cat.isGuiEnabled()) {
					scanForStepButtons();
				}
			} catch (Exception e) {
				stepButtons.clear();
				Current.getInstance().removeListener(this);
			}
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private void scanForStepButtons() {
		DragSource ds = new DragSource();
		List<StepButton> localStepButtons = (List) CommonGuiUtility.mapComponents(cat.getParentFrame(), StepButton.class);
		for (StepButton stepButton : localStepButtons) {
			if (!stepButtons.containsKey(stepButton)) {
				DragGestureRecognizer dgr = ds.createDefaultDragGestureRecognizer(stepButton, DnDConstants.ACTION_REFERENCE, CarbonAccountingToolWrapper.dgl);
				stepButtons.put(stepButton, dgr);
			}
		}
	}
	
	


	



	/**	
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}		// the referent must inherit from the GModel class
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof CATStandConnector)) {return false;}
			Collection<Tree> trees = (Collection) ((TreeCollection) s).getTrees();
			if (!trees.isEmpty()) {
				Tree t = trees.iterator().next();
				if (!(t instanceof CATTreeConnector)) {return false;}		// provides the wood product	
			}
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			Log.println (Log.ERROR, "DETimeCarbonContentWithSubstitution.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return REpiceaTranslator.getString(MessageID.Name);
	}

	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	@Override
	public String getDescription() {
		return REpiceaTranslator.getString(MessageID.Description);
	}

	@Override
	public String getVersion() {
		return "2.0";
	}
	

	/**
	 * This method converts all the steps from the root to this step into CATCompatibleStand instances.
	 * @param step this Step instance
	 * @return a List of CATCompatibleStand instances
	 */
	public static ArrayList<CATCompatibleStand> getStandList(Step step) {
		ArrayList<CATCompatibleStand> stands = new ArrayList<CATCompatibleStand>();
		Vector<Step> steps = step.getProject().getStepsFromRoot(step);
		for (Step stp : steps) {
			if (stp.getScene () instanceof CATCompatibleStand) {
				stands.add((CATCompatibleStand) stp.getScene());
			}
		}
		return stands;
	}


	@Override
	public void somethingHappened (ListenedTo l, Object param) {
		scanForStepButtons();
	}
	
	public CAT getCAT() {
		return cat;
	}
}
