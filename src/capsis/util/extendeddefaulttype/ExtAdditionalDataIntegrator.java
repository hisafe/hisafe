/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2019 Canadian Forest Service 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype;

import java.lang.reflect.Constructor;
import java.security.InvalidParameterException;
import java.util.List;

import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;

import capsis.commongui.projectmanager.StepButton;
import capsis.extensiontype.ModelTool;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.ExtInitialParameters.Backup;
import repicea.gui.CommonGuiUtility;
import repicea.gui.genericwindows.REpiceaGenericChooseFileDialog;
import repicea.io.tools.ImportFieldManager;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The ExtAdditionalDataIntegrator class makes it possible to add additional plots 
 * all along the simulation. It can be used for inventory updating or multiple forecasts
 * @author Mathieu Fortin - May 2019
 */
public class ExtAdditionalDataIntegrator implements ModelTool {

	private static enum MessageID implements TextableEnum {
		Name("Additional Plot Integrator", "Int\u00E9grateur de placettes additionnelles"),
		Author("Mathieu Fortin", "Mathieu Fortin"),
		Description("Makes it possible to integrate additional plots during the simulation.", 
				"Offre la possibilit\u00E9 d'ajouter des placettes additionnelles durant la simulation."),
		Version("1.0", "1.0"),
		ChooseFile("Please select a file.", "Veuillez choisir un fichier.");

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
	
	/**	
	 * Extension dynamic compatibility mechanism.
	 * This matchWith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		if (referent instanceof ExtModel) {
			return true;
		} else {
			return false;
		}
	}

	public void initInScriptMode(GModel model, Step step, String filename, ImportFieldManager importFieldManager, String selectedGroup) {
		internalInit(model, step, filename, importFieldManager, selectedGroup);
	}

	@Override
	public void init(GModel model, Step step) {
		String initialFilename = ((ExtInitialParameters) model.getSettings()).getFilename();
		REpiceaGenericChooseFileDialog dlg = new REpiceaGenericChooseFileDialog(MainFrame.getInstance(), 
				MessageID.Name.toString(),
				MessageID.ChooseFile.toString(),
				initialFilename,
				JFileChooser.FILES_ONLY);
		dlg.setVisible(true);
		if (dlg.isCancelled()) {
			return;
		} else {
			internalInit(model, step, dlg.getFile().getAbsolutePath(), null, null); // no need for an ImportManagerInstance and a selectedGroup
		}
	}

	private void internalInit(GModel model, Step step, String filename, ImportFieldManager importFieldManager, String selectedGroup) { 
		ExtModel mod = (ExtModel) model;
		ExtInitialParameters parms = mod.getSettings();
		Backup initParmsBackup = parms.getBackupForAdditionalData();
		ExtCompositeStand currentCompositeStand = (ExtCompositeStand) step.getScene();
		
		parms.setInitialDateYear(currentCompositeStand.getDateYr());

		parms.setFilename(filename);
		
		try {
			Constructor cstr = initParmsBackup.reader.getClass().getDeclaredConstructor(parms.getClass());
			cstr.setAccessible(true);
			ExtRecordReader newReader = (ExtRecordReader) cstr.newInstance(parms);
			parms.setRecordReader(newReader);
			if (parms.isGuiEnabled()) {
				parms.initImport();		
			} else {
				parms.getRecordReader().initInScriptMode(importFieldManager);
				if (selectedGroup != null) {
					List<String> groups = parms.getRecordReader().getGroupList();
					int selectedIndex = groups.indexOf(selectedGroup);
					if (selectedIndex == -1) {
						throw new InvalidParameterException("The selected group is not in the input data!");
					}
					parms.getRecordReader().setSelectedGroupId(selectedIndex);
				}
			}
			parms.buildInitScene(model);
			
			currentCompositeStand.merge(parms.getInitScene());
			if (parms.isGuiEnabled()) {
				Runnable doRun = new Runnable() {
					@Override
					public void run() {
						List<StepButton> localStepButtons = (List) CommonGuiUtility.mapComponents(MainFrame.getInstance(), StepButton.class);
						for (StepButton stepButton : localStepButtons) {
							GScene scene = stepButton.getStep().getScene();
							stepButton.setToolTipText(scene.getToolTip());
						}
					}
				};
				SwingUtilities.invokeLater(doRun);
			}
			initParmsBackup.restoreBackup();	// we restore the previous parameters in case these would be used again

		} catch (Exception e) {
			e.printStackTrace();
			throw new InvalidParameterException("The integration of additional plots failed! Please see the log!");
		} 
	}
	

	@Override
	public String getName() {return MessageID.Name.toString();}

	@Override
	public String getAuthor() {return MessageID.Author.toString();}

	@Override
	public String getDescription() {return MessageID.Description.toString();}

	@Override
	public String getVersion() {return MessageID.Version.toString();}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
