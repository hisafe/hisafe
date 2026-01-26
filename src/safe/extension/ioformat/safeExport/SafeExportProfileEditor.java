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

package safe.extension.ioformat.safeExport;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.WindowConstants;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.gui.MainFrame;
import capsis.kernel.GScene;
import capsis.util.WrapperPanel;

/**
 * SafeExportProfileEditor is a Dialog box for SafeExport profile edition.
 *
 * @author R. Tuquet Laburre - july 2003
 */
public class SafeExportProfileEditor extends AmapDialog implements ActionListener, ListSelectionListener {
	private static int WIDTH_FIELDS_DAYS=3;
	private static int WIDTH_FIELDS_IDS=5;
	private static int EXTEND_FREQUENCY_BEGIN_MIN=1;
	private static int EXTEND_FREQUENCY__MIN=1;
	private static int EXTEND_PERIOD_MIN=1;

	private SafeExportProfile profile;
	private Vector subjects;
	private String fileName;
	private GScene stand;

	private JTextField profileName;

	//STEP selection
	ButtonGroup bgStep;
	private JRadioButton rdExtendAllSteps;
	private JRadioButton rdExtendCurrentStep;
	private JRadioButton rdExtendFrequency;
	private JRadioButton rdExtendPeriod;

	private JLabel lblExtendFrequencyBegin;
	private JTextField fldExtendFrequencyBegin;
	private JLabel lblExtendFrequency;
	private JTextField fldExtendFrequencyValue;
	private JLabel lblExtendFrequencyUnity;
	private JLabel lblStepFromPeriod;
	private JTextField fldStepFromPeriod;
	private JLabel lblExtendPeriodTo;
	private JTextField fldExtendPeriodTo;
	private JCheckBox chkExtendRootStepToExport;

	//SUBJECTS selection
	//TREES
	JTabbedPane tpSubjects;
	private JCheckBox chkTreeAll;
	private JLabel lblTreeIds;
	private JTextField fldTreeIds;

	//CELLS
	private JCheckBox chkCellAll;
	private JLabel lblCellIds;
	private JTextField fldCellIds;
	private JButton btnCellChoose;

	//CROPS IL GT 19/12/07
	private JTextField fldCropIds;
	private JButton btnCropChoose;

	//VOXELS
	private JCheckBox chkVoxelAll;
	private JLabel lblVoxelCellIds;
	private JTextField fldVoxelCellIds;
	private JLabel lblVoxelDepths;
	private JTextField fldVoxelDepths;
	private JButton btnVoxelCellsChoose;

    private JCheckBox chkSubjects[];
    private JList lstVariables[];
	private JScrollPane jspVariables[];
	private JLabel lblVariablesSelectionInfos[];
	private JButton btnReloadSel[];
	private JButton btnSelAll[];
	private JButton btnSelNone[];
	private JButton btnInvSel[];

	private String subjectNames[];

	private Vector variables;
	private JButton ok;
	private JButton cancel;
	private JButton help;


	public SafeExportProfileEditor (SafeExportProfile profile, Vector subjects,String fileName,
									GScene stand) {
		super ();
		this.profile = profile;
		this.subjects = subjects;
		this.fileName=fileName;
		this.stand = stand;
		variables = new Vector();

		subjectNames= new String[subjects.size()];

		for (int i=0; i < subjects.size();i++) {
			SafeExportSubject subject = (SafeExportSubject) subjects.get(i);
			variables.add(subject.getVariableItemsForList());
			subjectNames[i] = subject.getName();
		}

		SafeExportSubject subject=null;
		createUI ();
		setScreenWithProfile();
		setDefaultCloseOperation (WindowConstants.DO_NOTHING_ON_CLOSE);
		addWindowListener (new WindowAdapter () {
			public void windowClosing (WindowEvent evt) {
				cancelAction ();
			}
		});
		pack ();
		show ();
	}

	/* Set Screen components with data of profile
	 *
	 */
	private void setScreenWithProfile() {
		// Extend
		rdExtendAllSteps.setSelected(profile.extendAllSteps);
		rdExtendCurrentStep.setSelected(profile.extendCurrentStep);
		rdExtendFrequency.setSelected(profile.extendFrequency);
		if (profile.extendFrequency) {
			fldExtendFrequencyBegin.setText(Integer.toString(profile.extendFrequencyBegin));
			fldExtendFrequencyValue.setText(Integer.toString(profile.extendFrequencyValue));
		}
		rdExtendPeriod.setSelected(profile.extendPeriod);
		if (profile.extendPeriod) {
			fldStepFromPeriod.setText(Integer.toString(profile.extendPeriodFrom));
			fldExtendPeriodTo.setText(Integer.toString(profile.extendPeriodTo));
		}
		chkExtendRootStepToExport.setSelected(profile.extendRootStepToExport);
		changeStateStep();

		// Subjects
		if (profile.subjectsToExport!=null) {
			for (int i=0 ; i< profile.subjectsToExport.size() ; i++) {
				SafeExportSubjectToExport profileSubject = (SafeExportSubjectToExport) profile.subjectsToExport.get(i);
				String subjectName=profileSubject.getName();
				int index=getSubjectIndex(subjectName);
				if (index!=-1) {
					chkSubjects[index].setSelected(true);
					//TREE
					if (index==getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)) {
						this.chkTreeAll.setSelected(((SafeExportSubjectTreeToExport) profileSubject).exportAll);
						//all trees are selected
						if (!chkTreeAll.isSelected()) {
							this.fldTreeIds.setText(SafeExportTools.collectionToString(((SafeExportSubjectTreeToExport) profileSubject).ids));
						}
					///CELL
					} else if (index==getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)) {
						//all cells are selected
						this.chkCellAll.setSelected(((SafeExportSubjectCellToExport) profileSubject).exportAll);
						if (!chkCellAll.isSelected()) {
							this.fldCellIds.setText(SafeExportTools.collectionToString(((SafeExportSubjectCellToExport) profileSubject).ids));
						}

					//VOXELS
					} else if (index==getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)) {
						//all voxels are selected
						this.chkVoxelAll.setSelected(((SafeExportSubjectVoxelToExport) profileSubject).exportAll);
						if (!chkVoxelAll.isSelected()) {
							this.fldVoxelCellIds.setText(SafeExportTools.collectionToString(((SafeExportSubjectVoxelToExport) profileSubject).cellIds));
							this.fldVoxelDepths.setText(SafeExportTools.collectionToString(((SafeExportSubjectVoxelToExport) profileSubject).depths));
						}
					}
					selVariablesSelection(profileSubject.getVariables(),index);
				}
			} // end for
		}
		for (int i=0 ; i< subjects.size(); i++ ) {
			changeStateSubject(i);
		}
	}


	private void selVariablesSelection(String[] savedVariableItems,int index) {
		int selectedItems[]=new int[savedVariableItems.length];
		SafeExportSubject subject= (SafeExportSubject) subjects.get(index);
		String listVariables[] = subject.getVariableItemsForList();
		int cpt=0;



		for (int i=0;i<savedVariableItems.length;i++) {
			String listItem= subject.getVariableItemForList(savedVariableItems[i]);
			for (int j=0; j< listVariables.length;j++) {
				if (listVariables[j].equals(listItem)) {
					selectedItems[cpt++]=j;
				}
			}
			lstVariables[index].setSelectedIndices(selectedItems);
		}
	}

	/* Set datas of profile with screen components
	 *
	 */
	private void setProfileWithScreen() {
		// Step
		profile.extendAllSteps = rdExtendAllSteps.isSelected();
		profile.extendCurrentStep=rdExtendCurrentStep.isSelected();

		profile.extendFrequency = rdExtendFrequency.isSelected();
		if (profile.extendFrequency) {
			profile.extendFrequencyBegin = Integer.parseInt(fldExtendFrequencyBegin.getText());
			profile.extendFrequencyValue = Integer.parseInt(fldExtendFrequencyValue.getText());
		}
		profile.extendPeriod = rdExtendPeriod.isSelected();
		if (profile.extendPeriod) {
			profile.extendPeriodFrom = Integer.parseInt(fldStepFromPeriod.getText());
			profile.extendPeriodTo = Integer.parseInt(fldExtendPeriodTo.getText());
		}
		profile.extendRootStepToExport=chkExtendRootStepToExport.isSelected();

		// Subjects
		Vector subjectsToExport = new Vector();
		int cpt=0;
		for (int i=0;i<subjects.size();i++) {
			SafeExportSubject subject = (SafeExportSubject) subjects.get(i);
			SafeExportSubjectToExport subjectToExport = null;
			if (chkSubjects[i].isSelected()) {
				//TREE
				if(i==getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)) {
					SafeExportSubjectTreeToExport subjectTree = new SafeExportSubjectTreeToExport (subjectNames[i],subject);
					subjectTree.exportAll=chkTreeAll.isSelected ();
					if (!chkTreeAll.isSelected()) {
						subjectTree.ids=SafeExportTools.stringToCollectionType(fldTreeIds.getText (),Integer.TYPE);
					}
					subjectToExport = subjectTree;
				//CELL
				} else if (i==getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)) {
					SafeExportSubjectCellToExport subjectCell = new SafeExportSubjectCellToExport (subjectNames[i],subject);
					subjectCell.exportAll=chkCellAll.isSelected ();
					if (!chkCellAll.isSelected()) {
						subjectCell.ids=SafeExportTools.stringToCollectionType(fldCellIds.getText (),Integer.TYPE);
					}
					subjectToExport = subjectCell;

				//VOXELS
				} else if (i==getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)) {
					SafeExportSubjectVoxelToExport subjectVoxel = new SafeExportSubjectVoxelToExport (subjectNames[i],subject);
					subjectVoxel.exportAll = chkVoxelAll.isSelected ();
					if (!chkVoxelAll.isSelected()) {
						subjectVoxel.cellIds=SafeExportTools.stringToCollectionType(fldVoxelCellIds.getText (),Integer.TYPE);
						subjectVoxel.depths=SafeExportTools.stringToBoundsCollection(fldVoxelDepths.getText ());
					}
					subjectToExport = subjectVoxel;
				} else {
					subjectToExport = new SafeExportSubjectToExport(subjectNames[i],subject);

				}
				if (subjectToExport!=null) {
					Object[] selectedItems =lstVariables[i].getSelectedValues();
					int totSel=selectedItems.length;
					String itemsToSave[] = new String[totSel];
					for (int j=0;j<totSel;j++) {
						itemsToSave[j]=subject.getVariableItemToSave((String) selectedItems[j]);
					}
					subjectToExport.setVariables(itemsToSave);
					subjectsToExport.add(cpt++,subjectToExport);
				}
			}
		}
		profile.subjectsToExport=subjectsToExport;
	}


	/**
	 * Guess what ?
	 */
	protected void escapePressed () {
		cancelAction ();
	}

	public int getSubjectIndex(String subjectName) {
		for (int i=0;i<subjectNames.length;i++) {
			if (subjectNames[i].equals(subjectName)) {
				return i;
			}
		}
	return -1;
	}


	public String getProfileName () {return profileName.getText ().trim ();}

	private void okAction () {

		// check if profileName is not empty
		if (Check.isEmpty (profileName.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("SafeExport.errorEmptyProfileName"));
			profileName.requestFocus();
			return;
		}

		// trim the component's value and update it
		profileName.setText(profileName.getText().trim ());
		String fileName=profileName.getText();

		// is it a new profile ?
		if (Check.isEmpty(this.fileName)) {
			// Check if filename is not an existing profile
			//Map profiles = SafeExportProfileCatalog.loadProfiles();
			//if (profiles.containsKey(fileName)) {
			//	if (!Question.ask (MainFrame.getInstance (),
			//		Translator.swap ("Shared.confirm"), Translator.swap ("SafeExport.overwritingThisFile")+fileName)) {
			//		return;
			//	}
			//}
		}


		// Check if TextFields 'StepFrequency' are correct
		if (rdExtendFrequency.isSelected()) {
			if (!Check.isInt(fldExtendFrequencyBegin.getText())) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFrequencyBeginStepIsNotValid"));
				fldExtendFrequencyBegin.requestFocus();
				return;
			}
			if (Integer.parseInt(fldExtendFrequencyBegin.getText()) <EXTEND_FREQUENCY_BEGIN_MIN) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFrequencyBeginStepIsSmallerThan") +
				" " + EXTEND_FREQUENCY_BEGIN_MIN+ " ! ");
				fldExtendFrequencyBegin.requestFocus();
				return;
			}
			if (!Check.isInt(fldExtendFrequencyValue.getText())) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFrequencyIsNotValid"));
				fldExtendFrequencyValue.requestFocus();
				return;
			}
			if (Integer.parseInt(fldExtendFrequencyValue.getText()) <EXTEND_FREQUENCY__MIN) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFrequencyIsSmallerThan") +
				" " + EXTEND_FREQUENCY__MIN+ " ! ");
				fldExtendFrequencyValue.requestFocus();
				return;
			}
		}

		// Check if TextFields 'StepPeriod' are correct
		if (rdExtendPeriod.isSelected()) {
			if (!Check.isInt(fldStepFromPeriod.getText())) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFromPeriodIsNotValid"));
				fldStepFromPeriod.requestFocus();
				return;
			}
			if (Integer.parseInt(fldStepFromPeriod.getText()) <EXTEND_PERIOD_MIN) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendFromPeriodIsSmallerThan") +
				" " + EXTEND_PERIOD_MIN + " ! ");
				fldStepFromPeriod.requestFocus();
				return;
			}
			if (!Check.isInt(fldExtendPeriodTo.getText())) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendToPeriodIsNotValid"));
				fldExtendPeriodTo.requestFocus();
				return;
			}
			if (Integer.parseInt(fldExtendPeriodTo.getText()) <EXTEND_PERIOD_MIN) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendToPeriodIsSmallerThan") +
				" " + EXTEND_PERIOD_MIN + " ! ");
				fldExtendPeriodTo.requestFocus();
				return;
			}
			if (Integer.parseInt(fldExtendPeriodTo.getText()) < Integer.parseInt(fldStepFromPeriod.getText())) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorExtendToPeriodIsSmallerThanFromPeriod"));
				fldExtendPeriodTo.requestFocus();
				return;
			}
		}
		// Count selected subjects and selected variables for each subject;
		int cptSelectedSubjects=0;
		int cptSelectedVariables[] = new int[subjects.size()];
		for (int i=0 ; i < subjects.size() ; i++) {
			cptSelectedVariables[i]=0;
			if (chkSubjects[i].isSelected()) {
				cptSelectedSubjects++;
				cptSelectedVariables[i]=lstVariables[i].getSelectedValues().length;
			}
		}
		// Check if, at least, 1 subject is selected
		if (cptSelectedSubjects==0) {
			MessageDialog.print (this, Translator.swap ("SafeExport.errorNoSubjectIsSelected"));
			return;
		}

		// Check if count of variables for checked subjects is not 0
		for (int i=0 ; i < subjects.size() ; i++) {
			if (chkSubjects[i].isSelected() && cptSelectedVariables[i]==0) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorNoSelectedVariableForThisSubject")+"'"
				+ tpSubjects.getTitleAt(i)+"'");
				return;
			}
		}

		// Check if list of trees is not empty;
		if (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)].isSelected() && !this.chkTreeAll.isSelected()) {
			Collection col = SafeExportTools.stringToCollectionType(fldTreeIds.getText(),Integer.TYPE);
			if (col==null || col.size()==0) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorTreeIdsIsEmpty"));
				return;
			}
		}
		// Check if list of cells is not empty;
		if (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)].isSelected() && !this.chkCellAll.isSelected()) {
			Collection col = SafeExportTools.stringToCollectionType(this.fldCellIds.getText(),Integer.TYPE);
			if (col==null || col.size()==0) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorCellIdsIsEmpty"));
				return;
			}
		}

		// Check if list of voxels' cells and voxels' depths are not empty;
		if (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)].isSelected() && !this.chkVoxelAll.isSelected()) {

			
			Collection col = SafeExportTools.stringToCollectionType(this.fldVoxelCellIds.getText(),Integer.TYPE);
			Collection col2 = SafeExportTools.stringToBoundsCollection(this.fldVoxelDepths.getText());
			if ((col==null || col.size()==0) && (col2==null || col2.size()==0)) {
				MessageDialog.print (this, Translator.swap ("SafeExport.errorVoxelCellIdsIsEmpty"));
				return;
			}

		}

		// update the profile with the screen's components
		setProfileWithScreen();
		// quit "ok"
		setValidDialog (true);
	}

	private void cancelAction () {
		setValidDialog (false);
	}

	public void valueChanged (ListSelectionEvent evt) {
	
		for (int i=0 ; i < subjects.size(); i++) {
			if (evt.getSource ().equals (lstVariables[i])) {
				JList src = (JList) evt.getSource ();
				if (src.getModel ().getSize () == 0) {return;}	// maybe list is empty -> no selection possible
				String selection = (String) src.getSelectedValue();
				changeVariablesSelectionInfos(i);
				return;
			}
		}
	}

	private void changeVariablesSelectionInfos(int index)
	{
		lblVariablesSelectionInfos[index].setVisible(chkSubjects[index].isSelected());
		lblVariablesSelectionInfos[index].setText(
			lstVariables[index].getSelectedValues().length + "/" +
			lstVariables[index].getModel().getSize() + " " +
			Translator.swap ("SafeExport.selectedVariables"));
	}
	/**
	 * Buttons management..
	 */
	public void actionPerformed (ActionEvent evt) {

		boolean foundEvt=false;
		if (evt.getSource (). equals (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)]) ||
			evt.getSource (). equals (chkTreeAll) ) {
			changeStateSubject (getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX));

		} else if (evt.getSource (). equals (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)]) ||
			evt.getSource (). equals (chkCellAll) ) {
			changeStateSubject (getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX));

		} else if (evt.getSource (). equals (chkSubjects[getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)]) ||
			evt.getSource (). equals (chkVoxelAll) ) {
			changeStateSubject (getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX));
		} else {
			for (int i=0 ; i< subjects.size() ; i++) {
				if (evt.getSource().equals(chkSubjects[i])) {
					changeStateSubject(i);
					return;
				}
			}
		}
		if (evt.getSource ().equals (rdExtendAllSteps) ||
		evt.getSource ().equals (rdExtendCurrentStep) ||
		evt.getSource ().equals (rdExtendPeriod) ||
		evt.getSource ().equals (rdExtendFrequency) ) {
			changeStateStep ();

		} else if (evt.getSource ().equals (btnCellChoose)) {
			btnChooseCellAction (fldCellIds);
		} else if (evt.getSource ().equals (btnVoxelCellsChoose)) {
			btnChooseCellAction (fldVoxelCellIds);

		//IL GT 19/12/07
		} else if (evt.getSource ().equals (btnCropChoose)) {
			btnChooseCropAction (fldCropIds);

		} else if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		} else {
			for (int i=0; i < subjects.size() ; i++) {
				if (evt.getSource().equals (btnReloadSel[i])) {
					selectionReloadAction (i);
					return;
				}
				if (evt.getSource().equals (btnSelAll[i])) {
					selectionAllAction (i);
					return;
				}
				if (evt.getSource ().equals (btnSelNone[i])) {
					selectionNoneAction (i);
					return;
				}
				if (evt.getSource ().equals (btnInvSel[i])) {
					selectionInvAction (i);
					return;
				}
			} // end for subjects.size();
		}
	}

	private void btnChooseCellAction (JTextField fldCell) {

		Vector idsFld = (Vector) SafeExportTools.stringToCollectionType (fldCell.getText (),Integer.TYPE);
		Collection alreadySelectedCellIds = new Vector ();
		if (idsFld!=null && idsFld.size ()>0) {
			alreadySelectedCellIds = (Collection) idsFld.clone ();
		}
		else { idsFld = new Vector (); }

		PlotOfCells plotc = (PlotOfCells) stand.getPlot (); // fc-30.10.2017
		
		Collection cells = plotc.getCells ();

		SafeExportCellSelectionDialog dlg = new SafeExportCellSelectionDialog (cells, alreadySelectedCellIds);
		Collection idsSel = dlg.getSelectedCellIds();
		if (dlg.isValidDialog()) {
			fldCell.setText(SafeExportTools.collectionToString(idsSel));
		}


		dlg.dispose();
	}


	//IL GT 19/12/07
	private void btnChooseCropAction (JTextField fldCrop) {
		Vector idsFld = (Vector) SafeExportTools.stringToCollectionType (fldCrop.getText (),Integer.TYPE);
		Collection alreadySelectedCropIds = new Vector ();
		if (idsFld!=null && idsFld.size ()>0) {
			alreadySelectedCropIds = (Collection) idsFld.clone ();
		}
		else { idsFld = new Vector (); }

		PlotOfCells plotc = (PlotOfCells) stand.getPlot (); // fc-30.10.2017
		
		Collection cells = plotc.getCells ();

		SafeExportCellSelectionDialog dlg = new SafeExportCellSelectionDialog (cells, alreadySelectedCropIds);
		Collection idsSel = dlg.getSelectedCellIds();
		if (dlg.isValidDialog()) {
			fldCrop.setText(SafeExportTools.collectionToString(idsSel));
		}


		dlg.dispose();
	}


	private void btnChooseCellAction_old(JTextField fldCell) {
		Vector idsFld = (Vector) SafeExportTools.stringToCollectionType(fldCell.getText(),Integer.TYPE);
		Collection idsFld2 = (Collection) idsFld.clone();

		PlotOfCells plotc = (PlotOfCells) stand.getPlot (); // fc-30.10.2017
		
		Collection cells = plotc.getCells();
		SafeExportCellSelectionDialog dlg = new SafeExportCellSelectionDialog(cells, (Collection) idsFld2);
		if (dlg.isValidDialog()) {
			Collection idsSel = dlg.getSelectedCellIds();
		
			for (Iterator it=idsSel.iterator(); it.hasNext();) {
				Integer id1= (Integer) it.next();

				boolean trouve=false;
				for (int j=0 ; j < idsFld.size() ; j++) {
					Integer id2 = (Integer) idsFld.get(j);
					if (id2.equals(id1)) {
						trouve=true;
		
					}
				}
				if (!trouve) {
	
					idsFld.add(id1);
				}
			}
			fldCell.setText(SafeExportTools.collectionToString(idsFld));
		}
		dlg.dispose();
	}

	private void selectionReloadAction(int index) {
		String[] variables = profile.getSubjectToExport(subjectNames[index]).getVariables();
		if (Question.ask (MainFrame.getInstance (),
			   Translator.swap ("Shared.confirm"), Translator.swap ("SafeExport.reloadSelectedVariablesFromProfile")+ "?")) {
			selVariablesSelection(variables,index);
		}
	}

	private void selectionAllAction(int index) {
		String[] variables = (String[]) this.variables.get(index);
		int[] select=new int[variables.length];
		for (int i=0;i<variables.length;i++) {
			select[i]=i;
		}
		lstVariables[index].setSelectedIndices(select);
	}
	private void selectionNoneAction(int index) {
		lstVariables[index].clearSelection();
	}
	private void selectionInvAction(int index) {
		String[] variables = (String[]) this.variables.get(index);
		int totSel=lstVariables[index].getSelectedIndices().length;
		if (totSel==0) {
			selectionAllAction(index);
			return;
		}
		if (totSel==variables.length) {
			selectionNoneAction(index);
			return;
		}
		int selected[]=lstVariables[index].getSelectedIndices();
		int[] select=new int[variables.length-selected.length];
		int cpt=0;
		for (int i=0;i<variables.length;i++) {
			if (Arrays.binarySearch(selected,i)<0) {
				select[cpt++]=i;
			}
		}
		lstVariables[index].setSelectedIndices(select);
	}

	private void changeStateStep() {
		lblExtendFrequencyBegin.setEnabled(rdExtendFrequency.isSelected());
		fldExtendFrequencyBegin.setEnabled(rdExtendFrequency.isSelected());
		lblExtendFrequency.setEnabled(rdExtendFrequency.isSelected());
		fldExtendFrequencyValue.setEnabled(rdExtendFrequency.isSelected());
		lblExtendFrequencyUnity.setEnabled(rdExtendFrequency.isSelected());

		lblStepFromPeriod.setEnabled(rdExtendPeriod.isSelected());
		fldStepFromPeriod.setEnabled(rdExtendPeriod.isSelected());
		lblExtendPeriodTo.setEnabled(rdExtendPeriod.isSelected());
		fldExtendPeriodTo.setEnabled(rdExtendPeriod.isSelected());
	}



	private void changeStateSubject(int index) {
		boolean selected=chkSubjects[index].isSelected();
		if (selected) {
			tpSubjects.setForegroundAt(index,Color.blue);
		} else {
			tpSubjects.setForegroundAt(index,Color.black);
		}
		if (index==getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)) {
			chkTreeAll.setEnabled(selected);
			if (selected && !chkTreeAll.isSelected()) {
				lblTreeIds.setEnabled(true);
				fldTreeIds.setEnabled(true);
			}
			else {
				lblTreeIds.setEnabled(false);
				fldTreeIds.setEnabled(false);
			}
		} else if (index==getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)) {
			chkCellAll.setEnabled(selected);
			if (selected && !chkCellAll.isSelected()) {
				lblCellIds.setEnabled(true);
				fldCellIds.setEnabled(true);
				btnCellChoose.setEnabled(true);
			}
			else {
				lblCellIds.setEnabled(false);
				fldCellIds.setEnabled(false);
				btnCellChoose.setEnabled(false);
			}


		} else if (index==getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)) {
			chkVoxelAll.setEnabled(selected);
			if (selected && !chkVoxelAll.isSelected()) {
				lblVoxelCellIds.setEnabled(true);
				fldVoxelCellIds.setEnabled(true);
				btnVoxelCellsChoose.setEnabled(true);
				lblVoxelDepths.setEnabled(true);
				fldVoxelDepths.setEnabled(true);
			}
			else {
				lblVoxelCellIds.setEnabled(false);
				fldVoxelCellIds.setEnabled(false);
				btnVoxelCellsChoose.setEnabled(false);
				lblVoxelDepths.setEnabled(false);
				fldVoxelDepths.setEnabled(false);
			}
		}
		lstVariables[index].setEnabled(selected);
		btnSelAll[index].setEnabled(selected);
		btnInvSel[index].setEnabled(selected);
		btnSelNone[index].setEnabled(selected);
		if (profile.subjectsToExport!=null && profile.getSubjectToExport(subjectNames[index])!=null) {
			btnReloadSel[index].setEnabled(selected);
		} else {
			btnReloadSel[index].setEnabled(false);
		}
		lblVariablesSelectionInfos[index].setVisible(selected);
		// changeVariablesSelectionInfos(index);
	}
	//
	// Initializes the dialog's graphical user interface.
	//
	private void createUI () {
		Border etched = BorderFactory.createEtchedBorder ();

		// 1. profile name
		JPanel pProfile = new LinePanel ();
		profileName = new JTextField (fileName,10);
		pProfile.add (profileName);
		Border borProfile = BorderFactory.createTitledBorder (etched,
			Translator.swap ("SafeExport.profileName"));
		pProfile.setBorder(borProfile);

		// 2 step (1=all days, 2 = current day, 3=frenqency, 4=period)
		bgStep = new ButtonGroup ();
		rdExtendAllSteps = new JRadioButton (Translator.swap ("SafeExport.extendAllSteps"));
		rdExtendAllSteps.addActionListener (this);
		bgStep.add (rdExtendAllSteps);
		JPanel p20 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p20.add (rdExtendAllSteps);
		// 2.1 step : all days
		rdExtendCurrentStep = new JRadioButton (Translator.swap ("SafeExport.extendCurrentStep"));
		rdExtendCurrentStep.addActionListener (this);
		bgStep.add (rdExtendCurrentStep);
		JPanel p21 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p21.add (rdExtendCurrentStep);
		// 2.2 step : frequency
		rdExtendFrequency = new JRadioButton (Translator.swap ("SafeExport.extendFrequency"));
		rdExtendFrequency.addActionListener (this);
		bgStep.add (rdExtendFrequency);
		lblExtendFrequencyBegin = new JLabel (Translator.swap ("SafeExport.extendFrequencyBegin")+ " : ");
		fldExtendFrequencyBegin= new JTextField(WIDTH_FIELDS_DAYS);
		fldExtendFrequencyBegin.addActionListener(this);
		JPanel p22 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p22.add (rdExtendFrequency);
		p22.add(lblExtendFrequencyBegin);
		p22.add(fldExtendFrequencyBegin);
		fldExtendFrequencyValue= new JTextField(WIDTH_FIELDS_DAYS);
		fldExtendFrequencyValue.addActionListener(this);
		lblExtendFrequency = new JLabel (Translator.swap ("SafeExport.extendFrequencyValue")+ " : ");
		lblExtendFrequencyUnity = new JLabel (Translator.swap ("SafeExport.extendFrequencyUnity"));
		p22.add(lblExtendFrequency);
		p22.add(fldExtendFrequencyValue);
		p22.add(lblExtendFrequencyUnity);
		// 2.3 step : period
		JPanel p23 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		rdExtendPeriod = new JRadioButton (Translator.swap ("SafeExport.extendPeriod"));
		rdExtendPeriod.addActionListener(this);
		bgStep.add (rdExtendPeriod);
		p23.add (rdExtendPeriod);
		fldStepFromPeriod= new JTextField (WIDTH_FIELDS_DAYS);
		fldStepFromPeriod.addActionListener (this);
		fldExtendPeriodTo= new JTextField (WIDTH_FIELDS_DAYS);
		fldExtendPeriodTo.addActionListener (this);
		lblStepFromPeriod = new JLabel (Translator.swap ("SafeExport.extendPeriodFrom")+ " : ");
		p23.add (lblStepFromPeriod);
		p23.add (fldStepFromPeriod);
		lblExtendPeriodTo = new JLabel (Translator.swap ("SafeExport.extendPeriodTo")+ " : ");
		p23.add (lblExtendPeriodTo);
		p23.add (fldExtendPeriodTo);
		// 2.4 rootSteptoExport
		chkExtendRootStepToExport = new JCheckBox(Translator.swap ("SafeExport.extendRootStepToExport"));
		chkExtendRootStepToExport.setSelected(true);
		chkExtendRootStepToExport.addActionListener(this);
		JPanel p24 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p24.add(chkExtendRootStepToExport);
		// 2 step (panel)
		ColumnPanel pExtend = new ColumnPanel ();
		pExtend.add (p20);
		pExtend.add (p21);
		pExtend.add (p22);
		pExtend.add (p23);
		pExtend.add (p24);
		Border borExtend = BorderFactory.createTitledBorder (etched,
			Translator.swap ("SafeExport.extend"));
		pExtend.setBorder(borExtend);

		// Subjects
		tpSubjects = new JTabbedPane();
		JPanel p1 ;
		Box pSubjects[] = new Box[subjects.size()];
		ColumnPanel pVariables[] = new ColumnPanel[subjects.size()];
		chkSubjects = new JCheckBox[subjects.size()];
		lstVariables = new JList[subjects.size()];
		jspVariables = new JScrollPane[subjects.size()];
		lblVariablesSelectionInfos = new JLabel[subjects.size()];
		btnReloadSel = new JButton[subjects.size()];
		btnSelAll = new JButton[subjects.size()];
		btnSelNone = new JButton[subjects.size()];
		btnInvSel = new JButton[subjects.size()];

		SafeExportSubject subject=null;
		int i=0;
		for (i=0; i < subjects.size() ;i++) {
			subject = (SafeExportSubject) subjects.get(i);
			String localName= ((SafeExportSubject) subject).getLocalName();
			String lblChk=Translator.swap("SafeExport.toExport");
			chkSubjects[i] = new JCheckBox(lblChk);
			chkSubjects[i].addActionListener(this);
			p1= new JPanel (new FlowLayout (FlowLayout.LEFT));
			p1.add(chkSubjects[i]);
			if (i==getSubjectIndex(SafeExport.SUBJECT_TREE_INDEX)) {
				chkTreeAll = new JCheckBox (Translator.swap ("SafeExport.all"));
				chkTreeAll.addActionListener(this);
				chkTreeAll.setSelected(true);
				lblTreeIds= new JLabel(Translator.swap("SafeExport.treeIds") + " : ");
				fldTreeIds= new JTextField(WIDTH_FIELDS_IDS);
				fldTreeIds.addActionListener(this);
				p1.add(chkTreeAll);
				p1.add(lblTreeIds);
				p1.add(fldTreeIds);
			} else if (i==getSubjectIndex(SafeExport.SUBJECT_CELL_INDEX)) {
				chkCellAll = new JCheckBox (Translator.swap ("SafeExport.all"));
				chkCellAll.addActionListener(this);
				chkCellAll.setSelected(true);
				lblCellIds= new JLabel(Translator.swap("SafeExport.cellIds") + " : ");
				fldCellIds= new JTextField(WIDTH_FIELDS_IDS);
				fldCellIds.addActionListener(this);
				btnCellChoose = new JButton ("...");
				btnCellChoose.setToolTipText (Translator.swap ("SafeExport.choose"));
				btnCellChoose.addActionListener (this);
				p1.add(chkCellAll);
				p1.add(lblCellIds);
				p1.add(fldCellIds);
				p1.add(btnCellChoose);
			}

				else if (i==getSubjectIndex(SafeExport.SUBJECT_VOXEL_INDEX)) {
				chkVoxelAll = new JCheckBox (Translator.swap ("SafeExport.all"));
				chkVoxelAll.addActionListener (this);
				chkVoxelAll.setSelected(true);
				lblVoxelCellIds = new JLabel (Translator.swap ("SafeExport.cellIds")+ " : ");
				fldVoxelCellIds= new JTextField(WIDTH_FIELDS_IDS);
				fldVoxelCellIds.addActionListener(this);
				btnVoxelCellsChoose = new JButton ("...");
				btnVoxelCellsChoose.setToolTipText (Translator.swap ("SafeExport.choose"));
				btnVoxelCellsChoose.addActionListener (this);
				lblVoxelDepths = new JLabel (Translator.swap ("SafeExport.depths")+ " : ");
				fldVoxelDepths= new JTextField(WIDTH_FIELDS_IDS);
				fldVoxelDepths.addActionListener(this);
				p1.add(chkVoxelAll);
				p1.add(lblVoxelCellIds);
				p1.add(fldVoxelCellIds);
				p1.add(lblVoxelCellIds);
				p1.add(fldVoxelCellIds);
				p1.add(btnVoxelCellsChoose);
				p1.add(lblVoxelDepths);
				p1.add(fldVoxelDepths);
			}
			pSubjects[i] = Box.createVerticalBox();
			pSubjects[i].add(p1);

			// Variables
			// 1. list of variables
			pVariables[i] = new ColumnPanel();
			Border borVariables = BorderFactory.createTitledBorder (etched,
				Translator.swap ("SafeExport.variables"));
			pVariables[i].setBorder(borVariables);
			String[] variableItems = (String[]) this.variables.get(i);
			lstVariables[i] = new JList(variableItems);
			lstVariables[i].addListSelectionListener (this);
			lstVariables[i].clearSelection();
			lstVariables[i].setSelectionMode (ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			lstVariables[i].setFixedCellWidth (200);
			lstVariables[i].setVisibleRowCount (5);
			jspVariables[i] = new JScrollPane (lstVariables[i]);
			jspVariables[i].getViewport ().setView (lstVariables[i]);
			pVariables[i].add (jspVariables[i]);
			// 2. Selection (4 buttons + 1 label)
			btnReloadSel[i] = new JButton (Translator.swap ("SafeExport.reloadSel")+"...");
			btnReloadSel[i].setToolTipText (Translator.swap ("SafeExport.reloadSel"));
			btnReloadSel[i].addActionListener (this);
			btnSelAll[i] = new JButton (Translator.swap ("SafeExport.selAll"));
			btnSelAll[i].setToolTipText (Translator.swap ("SafeExport.selAll"));
			btnSelAll[i].addActionListener (this);
			btnSelNone[i] = new JButton (Translator.swap ("SafeExport.selNone"));
			btnSelNone[i].setToolTipText (Translator.swap ("SafeExport.selNone"));
			btnSelNone[i].addActionListener (this);
			btnInvSel[i] = new JButton (Translator.swap ("SafeExport.invSel"));
			btnInvSel[i].setToolTipText (Translator.swap ("SafeExport.invSel"));
			btnInvSel[i].addActionListener (this);
			lblVariablesSelectionInfos[i] = new JLabel("",SwingConstants.CENTER);
			JPanel pVariables2 = new JPanel(new BorderLayout());
			JPanel pLblVariables= new LinePanel();
			pLblVariables.add(lblVariablesSelectionInfos[i]);
			JPanel pBtnVariables = new LinePanel();
			pBtnVariables.add(btnReloadSel[i]);
			pBtnVariables.add(btnSelAll[i]);
			pBtnVariables.add(btnSelNone[i]);
			pBtnVariables.add(btnInvSel[i]);
			pVariables2.add (pLblVariables,BorderLayout.WEST);
			pVariables2.add (pBtnVariables,BorderLayout.EAST);
			pVariables[i].add(pVariables2);

			pSubjects[i].add(pVariables[i]);
			tpSubjects.add(localName,pSubjects[i]);
		}

		JPanel p0 = new ColumnPanel();
		p0.add(pProfile);
		p0.add(pExtend);
		p0.add(tpSubjects);


		// 4 control panel
		ok = new JButton (Translator.swap ("Shared.ok"));
		ok.setToolTipText (Translator.swap ("Shared.ok"));
		ok.addActionListener (this);

		cancel = new JButton (Translator.swap ("Shared.cancel"));
		cancel.setToolTipText (Translator.swap ("Shared.cancel"));
		cancel.addActionListener (this);

		help = new JButton (Translator.swap ("Shared.help"));
		help.setToolTipText (Translator.swap ("Shared.help"));
		help.addActionListener (this);

		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);

		// Sets ok as default (see AmapDialog)
		setDefaultButton (ok);

		// General layout
		JPanel general = new JPanel (new BorderLayout ());
		general.add (pProfile, BorderLayout.NORTH);
		general.add (p0, BorderLayout.CENTER);
		general.add (pControl, BorderLayout.SOUTH);
		setContentPane (new WrapperPanel (general, 4, 4));

		setTitle (Translator.swap ("SafeExport.profileEditor"));
		setModal (true);
		setResizable (true);
	}
}

