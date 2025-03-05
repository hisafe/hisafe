/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2013 Mathieu Fortin (AgroParisTech/INRA - UMR LERFoB)
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
import lerfob.carbonbalancetool.CATCompatibleTree;
import lerfob.carbonbalancetool.CATSettings;
import repicea.gui.CommonGuiUtility;
import repicea.gui.UIControlManager;
import repicea.gui.dnd.DragGestureImpl;
import repicea.simulation.treelogger.TreeLoggerCompatibilityCheck;
import repicea.simulation.treelogger.TreeLoggerDescription;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * CarbonAccountingTool is the class that implements a tool for the calculation of the carbon storage (LERFoB-CAT). 
 * <br>
 * <br>
 * This class is deprecated. The suggested usage is
 * <br> 
 * <br> 
 * 
 * {@code CarbonAccountingToolWrapper catWrapper = new CarbonAccountingToolWrapper(GModel mod, Step step);} </br>
 * {@code CAT cat = catWrapper.getCAT();} </br>
 * 
 * @author Mathieu Fortin (INRA) - January 2010
 */
@Deprecated
public final class CarbonAccountingTool extends lerfob.carbonbalancetool.CarbonAccountingTool implements ModelTool, Listener {
	
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
			return CarbonAccountingTool.getStandList(step);
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
	
	/**
	 * Empty constructor for the extension manager.
	 */
	public CarbonAccountingTool() {
		super(CATMode.FROM_OTHER_APP);		// not a stand alone application
	}

	public CarbonAccountingTool(CATMode mode) {
		super(mode);
	}
	
	/**
	 * Constructor in script mode.
	 * @param mod a GModel instance
	 * @param step a Step instance
	 */
	public CarbonAccountingTool(GModel mod, Step step) {
		super(CATMode.SCRIPT);
		init(mod, step);
	}
	
	
	/**
	 * This method initializes the carbon accounting tool either in script or in GUI mode.
	 * @param model a GModel instance
	 * @param lastStep the last step
	 */
	@Override
	public void init(GModel model, Step lastStep) {
		if (!stepButtons.isEmpty()) {	// means the application is already running
			JOptionPane.showMessageDialog(MainFrame.getInstance(), 
					MessageID.CATAlreadyRunning.toString(), 
					UIControlManager.InformationMessageTitle.Information.toString(), 
					JOptionPane.INFORMATION_MESSAGE);
			return;
		} else {
			Window parentFrame = null;
			if (isGuiEnabled()) {
				parentFrame = MainFrame.getInstance();
				Current.getInstance().addListener(this);
			}
			try {
				super.initializeTool(parentFrame);
				List<CATCompatibleStand> stands = getStandList(lastStep);
				setStandList(stands);
				if (isGuiEnabled()) {
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
		List<StepButton> localStepButtons = (List) CommonGuiUtility.mapComponents(parentFrame, StepButton.class);
		for (StepButton stepButton : localStepButtons) {
			if (!stepButtons.containsKey(stepButton)) {
				DragGestureRecognizer dgr = ds.createDefaultDragGestureRecognizer(stepButton, DnDConstants.ACTION_REFERENCE, CarbonAccountingTool.dgl);
				stepButtons.put(stepButton, dgr);
			}
		}
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
				Current.getInstance().removeListener(this);
			}
		}
		super.shutdown(shutdownCode);
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
			if (!(s instanceof CATCompatibleStand)) {return false;}
			Tree t = ((TreeCollection) s).getTrees().iterator().next();
			if (!(t instanceof CATCompatibleTree)) {return false;}		// provides the wood product
			
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
	
	/*
	 * For extended visibility in the tests (non-Javadoc)
	 * @see lerfob.carbonbalancetool.CarbonAccountingTool#getCarbonCompartmentManager()
	 */
	@Override
	protected CATCompartmentManager getCarbonCompartmentManager() {
		return super.getCarbonCompartmentManager();
	}

	/*
	 * This method should not be used anymore. It was kept to ensure that JUnit tests would
	 * run.
	 */
	@Deprecated
	@Override
	public CATSettings getCarbonToolSettings() {
		return super.getCarbonToolSettings();
	}
	
}
