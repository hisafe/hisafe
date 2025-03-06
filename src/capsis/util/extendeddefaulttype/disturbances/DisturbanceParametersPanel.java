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

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.disturbances.DisturbanceParameters.DisturbanceMode;
import jeeb.lib.util.Settings;
import repicea.gui.CommonGuiUtility;
import repicea.gui.REpiceaPanel;
import repicea.gui.UIControlManager;
import repicea.gui.components.NumberFormatFieldFactory;
import repicea.gui.components.NumberFormatFieldFactory.Range;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * An abstract class that provides the different components for the user interface.
 * @author Mathieu Fortin - March 2019
 */
public abstract class DisturbanceParametersPanel extends REpiceaPanel implements ItemListener {
	
	private static enum MessageID implements TextableEnum {
		NumberOfYears("Number of years: ", "Nombre d'ann\u00E9es : "),
		InTheNextStep("In the next step", "Dans le prochain pas de croissance"),
		RandomEvent("Random with recurrence of ", "Al\u00E9atoire avec r\u00E9currence "),
		Years(" years"," ann\u00E9es"),
		AccordingToModelLabel("According to the model (business-as-usual)", "Selon le mod\u00E8le (business-as-usual)"),
		;
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}
	}

	protected final JCheckBox occurrenceChkBox;
	protected JRadioButton nextStepButton;
	protected JRadioButton randomButton;
	protected JRadioButton accordingToModelButton;

	protected JTextField recurrence;
	protected final ExtModel model;
	protected final ButtonGroup buttonGroup;
	private final DisturbanceType type;
	private final boolean showModelOption;

	/**
	 * Provide a user interface for setting some disturbance features.
	 * @param type the type of disturbance
	 * @param model an ExtModel instance
	 * @param showModelOption true to show the model-based option in the panel
 	 */
	public DisturbanceParametersPanel(DisturbanceType type, ExtModel model, boolean showModelOption) {
		removeAncestorListener(this);
		this.type = type;
		this.model = model;
		this.showModelOption = showModelOption;
		occurrenceChkBox = new JCheckBox(type.toString());
		buttonGroup = new ButtonGroup();
		initializeControls();
		createUI();
		refreshInterface();
	}

	/**
	 * Constructor without model option.
	 * @param type a DisturbanceType enum
	 */
	public DisturbanceParametersPanel(DisturbanceType type, ExtModel model) {
		this(type, model, false);
	}

	protected void initializeControls() {
		nextStepButton = new JRadioButton(MessageID.InTheNextStep.toString());
		randomButton = new JRadioButton(MessageID.RandomEvent.toString());
		accordingToModelButton = new JRadioButton(MessageID.AccordingToModelLabel.toString());
		recurrence = NumberFormatFieldFactory.createNumberFormatField(5,NumberFormatFieldFactory.Type.Double, Range.StrictlyPositive, false);
		buttonGroup.add(nextStepButton);
		buttonGroup.add(randomButton);
		buttonGroup.add(accordingToModelButton);
	}
	
	/**
	 * This method loads the properties that fill the different fields of the panel.
	 */
	public void loadDefaultValues() {
		nextStepButton.setSelected(Settings.getProperty(getPropertyName("nextstep"), true));
		randomButton.setSelected(Settings.getProperty(getPropertyName("random"), false));
		accordingToModelButton.setSelected(Settings.getProperty(getPropertyName("modelButton"), false));
		recurrence.setText(Settings.getProperty(getPropertyName("recurrence"), "20"));
	}

	protected final String getModelName() {
		if (model != null) {
			return model.getIdCard().getModelName();
		} else {
			return "";
		}
	}
	
	protected String getPropertyName(String property) {
		return "disturbance." + type.name() + "." + getModelName() + "." + property;
	}
	
	
	public void saveProperties() {
		Settings.setProperty(getPropertyName("nextstep"), nextStepButton.isSelected());
		Settings.setProperty(getPropertyName("random"), randomButton.isSelected());
		Settings.setProperty(getPropertyName("recurrence"), recurrence.getText());
		Settings.setProperty(getPropertyName("modelButton"), accordingToModelButton.isSelected());
	}
	
	
	protected void createUI() {
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

		JPanel occurrencePanel1 = new JPanel();
		occurrencePanel1.setLayout(new BoxLayout(occurrencePanel1, BoxLayout.X_AXIS));
		occurrencePanel1.add(Box.createHorizontalStrut(10));
		occurrencePanel1.add(occurrenceChkBox);
		occurrencePanel1.add(Box.createGlue());
		
		JPanel occurrencePanel2 = new JPanel();
		occurrencePanel2.setLayout(new BoxLayout(occurrencePanel2, BoxLayout.X_AXIS));
		occurrencePanel2.add(Box.createHorizontalStrut(50));
		occurrencePanel2.add(nextStepButton);
		occurrencePanel2.add(Box.createGlue());
		
		add(occurrencePanel1);
		add(occurrencePanel2);
		add(getRandomPanel());
		
		if (showModelOption && model.getSettings().isStochastic()) {	// only if the model is stochastic
			JPanel occurrencePanel3 = new JPanel();
			occurrencePanel3.setLayout(new BoxLayout(occurrencePanel3, BoxLayout.X_AXIS));
			occurrencePanel3.add(Box.createHorizontalStrut(50));
			occurrencePanel3.add(accordingToModelButton);
			occurrencePanel3.add(Box.createGlue());
			add(occurrencePanel3);
		}

	}

	/**
	 * This method can be overriden to customize the random panel.
	 * @return a JPanel
	 */
	protected JPanel getRandomPanel() {
		JPanel occurrencePanel3 = new JPanel();
		occurrencePanel3.setLayout(new BoxLayout(occurrencePanel3, BoxLayout.X_AXIS));
		occurrencePanel3.add(Box.createHorizontalStrut(50));
		occurrencePanel3.add(randomButton);
		occurrencePanel3.add(Box.createHorizontalStrut(5));
		occurrencePanel3.add(recurrence);
		occurrencePanel3.add(Box.createHorizontalStrut(5));
		occurrencePanel3.add(UIControlManager.getLabel(MessageID.Years));
		occurrencePanel3.add(Box.createHorizontalStrut(150));
		return occurrencePanel3;
	}
	
	@Override
	public void setEnabled(boolean bool) {
		occurrenceChkBox.setEnabled(bool);
		nextStepButton.setEnabled(bool && occurrenceChkBox.isSelected());
		randomButton.setEnabled(bool && occurrenceChkBox.isSelected());
		recurrence.setEnabled(bool && occurrenceChkBox.isSelected() && randomButton.isSelected());
		accordingToModelButton.setEnabled(bool && occurrenceChkBox.isSelected());
	}
	
	@Override
	public void refreshInterface() {
		doNotListenToAnymore();
		checkWhichFeatureShouldBeEnabled();
		listenTo();
	}



	@Override
	public void itemStateChanged(ItemEvent e) {
		refreshInterface();
	}



	@Override
	public void listenTo() {
		occurrenceChkBox.addItemListener(this);
		nextStepButton.addItemListener(this);
		randomButton.addItemListener(this);
		accordingToModelButton.addItemListener(this);
	}


	@Override
	public void doNotListenToAnymore() {
		occurrenceChkBox.removeItemListener(this);
		nextStepButton.removeItemListener(this);
		randomButton.removeItemListener(this);
		accordingToModelButton.removeItemListener(this);
	}
	
	
	protected void checkWhichFeatureShouldBeEnabled() {
		boolean isStochastic = false;		// default value
		if (model != null) {
			isStochastic = model.getSettings().isStochastic();
		}
		nextStepButton.setEnabled(occurrenceChkBox.isSelected());
		randomButton.setEnabled(occurrenceChkBox.isSelected() && isStochastic);
		recurrence.setEnabled(occurrenceChkBox.isSelected() && randomButton.isSelected() && isStochastic);
		if (!isStochastic && occurrenceChkBox.isSelected() && randomButton.isSelected()) {
			nextStepButton.setSelected(true);
		}
		CommonGuiUtility.enableThoseComponents(this, JLabel.class, occurrenceChkBox.isSelected());

		accordingToModelButton.setEnabled(occurrenceChkBox.isSelected() && isStochastic);
	}

	/**
	 * This method returns the disturbance parameters as set through the dialog
	 * @return a DisturbanceParameters instance
	 */
	public DisturbanceParameters getDisturbanceParameters() {
		DisturbanceMode mode;
		if (!occurrenceChkBox.isSelected()) {
			mode = DisturbanceMode.None;
		} else if (randomButton.isSelected()) {
			mode = DisturbanceMode.Random;
			return new NaturalDisturbanceParameters(type, Double.parseDouble(recurrence.getText()));
		} else if (accordingToModelButton.isSelected()){
			mode = DisturbanceMode.FullModelBased;
		} else {
			mode = DisturbanceMode.NextStep;
		}
		DisturbanceParameters parameters = new NaturalDisturbanceParameters(type, mode);
		return parameters;
	}
	
}
