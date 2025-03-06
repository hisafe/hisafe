/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Authors: M. Fortin, Canadian Forest Service 
 * Copyright (C) 2019 Her Majesty the Queen in right of Canada 
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
package capsis.util.extendeddefaulttype.disturbances;

import java.util.ArrayList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import capsis.kernel.MethodProvider;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.disturbances.DisturbanceParameters.DisturbanceMode;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.VProvider;
import jeeb.lib.util.Settings;
import repicea.gui.CommonGuiUtility;
import repicea.gui.components.NumberFormatFieldFactory;
import repicea.gui.components.NumberFormatFieldFactory.JFormattedNumericField;
import repicea.simulation.ApplicationScaleProvider.ApplicationScale;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * A user interface to set the thinning parameters.
 * @author Mathieu Fortin - March 2019
 */
public class ThinningDisturbanceParametersPanel extends DisturbanceParametersPanel {

	public static enum BoundaryVariable implements TextableEnum {
		G("G (m2/ha)", "G (m2/ha)"),
		V("V (m3/ha)", "V (m3/ha)"),
		;

		BoundaryVariable(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	

	private enum MessageID implements TextableEnum {
		AccordingToRuleLabel("According to this rule", "Selon cette r\u00E8gle"),
		MinimumLabel("Min","Min"),
		MaximumLabel("Max","Max"),
		HarvestOccurrence("Harvest occurrence", "Occurrence de coupe"),
		EnabledTargetDiameter("Final harvest at","Coupe finale \u00E0"),
		DominantDiameter("cm in dominant diameter", "cm en diam\u00E8tre dominant")
		;

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

	private JRadioButton accordingToRuleButton;
	private JComboBox<BoundaryVariable> boundaryVariableSelector;
	private JFormattedNumericField minBoundaryValueField;
	private JFormattedNumericField maxBoundaryValueField;
	private JCheckBox enabledTargetDiameterCheckBox;
	private JFormattedNumericField targetDiameterCmField;

	/**
	 * Provide a user interface for selecting the different thinning options.
	 * @param model an ExtModel instance
	 */
	public ThinningDisturbanceParametersPanel(ExtModel model) {
		super(DisturbanceType.Harvest, model, true);
	}
	
	@Override
	protected void initializeControls() {
		super.initializeControls();
		accordingToRuleButton = new JRadioButton(MessageID.AccordingToRuleLabel.toString());
		buttonGroup.add(accordingToRuleButton);
		List<BoundaryVariable> eligibleBoundaryVariables = new ArrayList<BoundaryVariable>();

		MethodProvider mp = null;
		if (model != null) {
			mp = model.getMethodProvider();
		}
		if (mp instanceof GProvider || mp == null) {
			eligibleBoundaryVariables.add(BoundaryVariable.G);
		}
		if (mp instanceof VProvider || mp == null) {
			eligibleBoundaryVariables.add(BoundaryVariable.V);
		}
		boundaryVariableSelector = new JComboBox<BoundaryVariable>();
		boundaryVariableSelector.setModel(new DefaultComboBoxModel<BoundaryVariable>(eligibleBoundaryVariables.toArray(new BoundaryVariable[]{})));
		minBoundaryValueField = NumberFormatFieldFactory.createNumberFormatField(10, NumberFormatFieldFactory.Type.Double, NumberFormatFieldFactory.Range.Positive, true);
		maxBoundaryValueField = NumberFormatFieldFactory.createNumberFormatField(10, NumberFormatFieldFactory.Type.Double, NumberFormatFieldFactory.Range.Positive, true);
		enabledTargetDiameterCheckBox = new JCheckBox(MessageID.EnabledTargetDiameter.toString());
		targetDiameterCmField = NumberFormatFieldFactory.createNumberFormatField(10, NumberFormatFieldFactory.Type.Double, NumberFormatFieldFactory.Range.Positive, true);
	}
	
	@Override
	public void loadDefaultValues() {
		super.loadDefaultValues();
		setIndex(Settings.getProperty(getPropertyName("boundaryselector"), 0), boundaryVariableSelector);
		setJFormattedNumericField(maxBoundaryValueField, Settings.getProperty(getPropertyName("maxbound"), 50d));
		setJFormattedNumericField(minBoundaryValueField, Settings.getProperty(getPropertyName("minbound"), 0d));
		accordingToRuleButton.setSelected(Settings.getProperty(getPropertyName("ruleButton"), false));
		if (accordingToRuleButton.isSelected() && !accordingToRulePanelEnabled()) { // that would  inconsistent
			nextStepButton.setSelected(true);	// default value
		}
		enabledTargetDiameterCheckBox.setSelected(Settings.getProperty(getPropertyName("targetDiameterButton"), false));
		setJFormattedNumericField(targetDiameterCmField, Settings.getProperty(getPropertyName("targetDiameterCm"), 60d));
	}
	
	private void setJFormattedNumericField(JFormattedNumericField field, Double value) {
		if (value == null || Double.isNaN(value)) {
			field.setText("");
		} else {
			field.setText(value.toString());
		}
		
	}
	
	@Override
	public void setEnabled(boolean bool) {
		super.setEnabled(bool);
		boolean checkBoxSelected = occurrenceChkBox.isSelected();
		accordingToRuleButton.setEnabled(bool && checkBoxSelected);
		boundaryVariableSelector.setEnabled(bool && checkBoxSelected);
		minBoundaryValueField.setEnabled(bool && checkBoxSelected);
		maxBoundaryValueField.setEnabled(bool && checkBoxSelected);
		enabledTargetDiameterCheckBox.setEnabled(bool && checkBoxSelected);
		targetDiameterCmField.setEnabled(bool && checkBoxSelected);
	}

	private void setIndex(int selectedIndex, JComboBox comboBox) {
		if (selectedIndex > comboBox.getModel().getSize() - 1) {
			comboBox.setSelectedIndex(0);
		} else {
			comboBox.setSelectedIndex(selectedIndex);
		}
	}

	@Override
	public void saveProperties() {
		super.saveProperties();
		Settings.setProperty(getPropertyName("boundaryselector"), boundaryVariableSelector.getSelectedIndex());
		Settings.setProperty(getPropertyName("maxbound"), maxBoundaryValueField.getValue().doubleValue());
		Settings.setProperty(getPropertyName("minbound"), minBoundaryValueField.getValue().doubleValue());
		Settings.setProperty(getPropertyName("ruleButton"), accordingToRuleButton.isSelected());
		Settings.setProperty(getPropertyName("targetDiameterButton"), enabledTargetDiameterCheckBox.isSelected());
		Settings.setProperty(getPropertyName("targetDiameterCm"), (Double) targetDiameterCmField.getValue()); 
	}
	
	private boolean accordingToRulePanelEnabled() {
		boolean showAccordingToRulePanel = model.getSettings().getApplicationScale() == ApplicationScale.Stand; // the panel is shown only at the stand scale
		return showAccordingToRulePanel;
	}
	
	
	@Override
	protected void createUI(){
		super.createUI();
		
		
		if (accordingToRulePanelEnabled()) {
			JPanel boundaryPanel = new JPanel();
			boundaryPanel.setLayout(new BoxLayout(boundaryPanel, BoxLayout.X_AXIS));
			boundaryPanel.add(Box.createHorizontalStrut(50));
			boundaryPanel.add(accordingToRuleButton);
			boundaryPanel.add(Box.createHorizontalStrut(10));
			boundaryPanel.add(boundaryVariableSelector);
			boundaryPanel.add(Box.createHorizontalStrut(10));
			boundaryPanel.add(new JLabel(MessageID.MinimumLabel.toString()));
			boundaryPanel.add(minBoundaryValueField);
			boundaryPanel.add(Box.createHorizontalStrut(5));
			boundaryPanel.add(new JLabel(MessageID.MaximumLabel.toString()));
			boundaryPanel.add(maxBoundaryValueField);
			boundaryPanel.add(Box.createHorizontalGlue());
			add(boundaryPanel);
		} 
		
		JPanel targetDiameterCmPanel = getTargetDiameterCmPanel();
		add(targetDiameterCmPanel);
	}

	protected JPanel getTargetDiameterCmPanel() {
		JPanel targetDiameterCmPanel = new JPanel();
		targetDiameterCmPanel.setLayout(new BoxLayout(targetDiameterCmPanel, BoxLayout.X_AXIS));
		targetDiameterCmPanel.add(Box.createHorizontalStrut(50));
		targetDiameterCmPanel.add(enabledTargetDiameterCheckBox);
		targetDiameterCmPanel.add(Box.createHorizontalStrut(5));
		targetDiameterCmPanel.add(targetDiameterCmField);
		targetDiameterCmPanel.add(Box.createHorizontalStrut(5));
		targetDiameterCmPanel.add(new JLabel(MessageID.DominantDiameter.toString()));
		targetDiameterCmPanel.add(Box.createHorizontalStrut(250));
		return targetDiameterCmPanel;
	}
	
	
	
	
	
	@Override
	protected void checkWhichFeatureShouldBeEnabled() {
		super.checkWhichFeatureShouldBeEnabled();
		boolean checkBoxSelected = occurrenceChkBox.isSelected();
		accordingToRuleButton.setEnabled(checkBoxSelected);
		boundaryVariableSelector.setEnabled(accordingToRuleButton.isSelected() && checkBoxSelected);
		maxBoundaryValueField.setEnabled(accordingToRuleButton.isSelected() && checkBoxSelected);
		minBoundaryValueField.setEnabled(accordingToRuleButton.isSelected() && checkBoxSelected);
		enabledTargetDiameterCheckBox.setEnabled(checkBoxSelected);
		targetDiameterCmField.setEnabled(enabledTargetDiameterCheckBox.isSelected() && checkBoxSelected);
		CommonGuiUtility.enableThoseComponents(this, JLabel.class, checkBoxSelected);
	}

	@Override
	public void listenTo() {
		super.listenTo();
		accordingToRuleButton.addItemListener(this);
		enabledTargetDiameterCheckBox.addItemListener(this);
	}



	@Override
	public void doNotListenToAnymore() {
		super.doNotListenToAnymore();
		accordingToRuleButton.removeItemListener(this);
		enabledTargetDiameterCheckBox.removeItemListener(this);
	}
	
	/**
	 * This method returns the disturbance parameters as set through the dialog
	 * @return a DisturbanceParameters instance
	 */
	@Override
	public ThinningDisturbanceParameters getDisturbanceParameters() {
		ThinningDisturbanceParameters outputParms;
		if (!occurrenceChkBox.isSelected()) {
			outputParms = new ModelBasedThinningDisturbanceParameters(DisturbanceMode.None);
		} else if (randomButton.isSelected()) {
			outputParms = new ModelBasedThinningDisturbanceParameters(Double.parseDouble(recurrence.getText()));
		} else if (nextStepButton.isSelected()) {
			outputParms = new ModelBasedThinningDisturbanceParameters(DisturbanceMode.NextStep);
		} else if (accordingToModelButton.isSelected()) {
			outputParms = new ModelBasedThinningDisturbanceParameters(DisturbanceMode.FullModelBased);
		} else {
			BoundaryVariable boundaryVariable = (BoundaryVariable) boundaryVariableSelector.getSelectedItem();
			double minimumValue = (Double) minBoundaryValueField.getValue();
			double maximumValue = (Double) maxBoundaryValueField.getValue();
			outputParms = new RuleBasedThinningDisturbanceParameters(boundaryVariable, minimumValue, maximumValue, model.getMethodProvider());
		}
		if (enabledTargetDiameterCheckBox.isSelected()) {
			outputParms.setTargetDiameterCm((Double) targetDiameterCmField.getValue());
		}
		return outputParms;
	}


}
