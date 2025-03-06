/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
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

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JComboBox;

import capsis.kernel.AbstractSettings;
import capsis.util.extendeddefaulttype.ExtSimulationSettingsDialog.EnableDisableOption;
import repicea.serial.Memorizable;
import repicea.serial.MemorizerPackage;
import repicea.simulation.MonteCarloSettings.MonteCarloVarSource;

/**
 * The AbstractSimulationSettings contains all the other "light" parameters such as the time step,
 * the maximum number of steps and so on.
 * @author Mathieu Fortin - October 2013
 */
public abstract class ExtSimulationSettings extends AbstractSettings implements Serializable, ItemListener, Memorizable {

	public static int MAX_NUMBER_ITERATIONS_MC = 1000;
	
	public static final String ALL_SPECIES = "TOT";
	
	public static final double VERY_SMALL = 0.000001;
	
	public static final double MEMORY_MEG_FACTOR = 1 / Math.pow(1024.0, 2);

	public static final Font LABEL_FONT = new Font("ArialBold12",Font.BOLD,12);
	public static final Font TEXT_FONT = new Font("Arial12",Font.PLAIN,12);
	public static final Color POPUP_COLOR = new Color(.99f,.99f,.75f);
	
	public static final int NB_THREADS;
	static {
		int numberProcessors = Runtime.getRuntime().availableProcessors();
		if (numberProcessors >= 2) {
			NB_THREADS = 2; 
		} else {
			NB_THREADS = 1; 
		}
	}
	
	private String inventoryPath;
	private String modelPath;
	private int timeStep;
	private int maxNumberOfSteps;
	
	protected HashMap<Enum, String> infoMap; 

	protected HashMap<MonteCarloVarSource, Boolean> monteCarloSettings;
	protected final ArrayList<MonteCarloVarSource> possibleMonteCarloSources;

	protected double minimumDiameterCm;
	
	/**
	 * Constructor
	 */
	protected ExtSimulationSettings(double minimumDiameterCm) {
		super();
		possibleMonteCarloSources = new ArrayList<MonteCarloVarSource>();
		monteCarloSettings = new HashMap<MonteCarloVarSource, Boolean>();
		definePossibleMonteCarloSources();
		infoMap = new HashMap<Enum, String>();
		this.minimumDiameterCm = minimumDiameterCm;
	}

	/**
	 * By default, this method defines no Monte Carlo source. Should be overriden in derived class
	 * in order to enable Monte Carlo simulations.
	 */
	protected abstract void definePossibleMonteCarloSources();
	
	public Map<Enum, String> getInfoMap() {return infoMap;}

	public String getInventoryPath() {return inventoryPath;}
	protected void setInventoryPath(String inventoryPath) {this.inventoryPath = inventoryPath;}
	
	public String getModelPath() {return modelPath;}
	protected void setModelPath(String modelPath) {this.modelPath = modelPath;}
	
	public int getTimeStep() {return timeStep;}
	protected void setTimeStep(int timeStep) {this.timeStep = timeStep;}

	public int getMaxNumberOfSteps() {return maxNumberOfSteps;}
	protected void setMaxNumberOfSteps(int maxNumberOfSteps) {this.maxNumberOfSteps = maxNumberOfSteps;}

	
	/**
	 * This method returns the list of the possible Monte Carlo sources.
	 * @return a List of MonteCarloVarSource enum variables
	 */
	public List<MonteCarloVarSource> getPossibleMonteCarloSources() {
		return possibleMonteCarloSources;
	}

	
	/**
	 * This method returns true if a particular source of variability is enabled during stochastic simulation.
	 * If the MonteCarloVarSource variable is not contained in the possibleMonteCarloSources member, the method returns
	 * false by default.
	 * @param source an enum that refers to a particular source of variability (see MonteCarloVarSource enum)
	 * @return a boolean
	 */
	public boolean getMonteCarloSettings(MonteCarloVarSource source) {
		if (possibleMonteCarloSources.contains(source)) {
			return monteCarloSettings.get(source);
		} else {
			return false;
		}
	}
	
	/**
	 * This method enables or disables a particular source of variability in stochastic model. If the enum variable
	 * is not contained in the possibleMonteCarloSources member, the method does nothing.
	 * @param source an enum that refers to a particular source of variability (see MonteCarloVarSource enum)
	 * @param bool a boolean true to enable or false to disable
	 */
	public void setMonteCarloSettings(MonteCarloVarSource source, boolean bool) {
		if (possibleMonteCarloSources.contains (source)) {
			monteCarloSettings.put(source, bool);
		}
	}

	/**
	 * This method updates the monteCarloSettings maps. It sets the boolean to true or false depending on the
	 * value of the stochastic parameter. These booleans enables or disables the stochastic parts of the model.
	 * @param isStochastic a boolean true enables the stochastic simulation or false otherwise
	 */
	public void setDefaultMonteCarloSettings(boolean isStochastic) {
		for (MonteCarloVarSource source : possibleMonteCarloSources) {
			monteCarloSettings.put(source, isStochastic);
		}
	}
	
	@Override
	public MemorizerPackage getMemorizerPackage () {
		MemorizerPackage mp = new MemorizerPackage();
		mp.add(getInventoryPath());
		mp.add(getModelPath());
		mp.add(getTimeStep());
		mp.add(getMaxNumberOfSteps());
		mp.add(infoMap);
		mp.add(monteCarloSettings);
		return mp;
	}

	@Override
	public void unpackMemorizerPackage (MemorizerPackage wasMemorized) {
		setInventoryPath((String) wasMemorized.remove(0));
		setModelPath((String) wasMemorized.remove(0));
		setTimeStep((Integer) wasMemorized.remove(0));
		setMaxNumberOfSteps((Integer) wasMemorized.remove(0));
		infoMap = (HashMap) wasMemorized.remove(0);
		monteCarloSettings = (HashMap) wasMemorized.remove(0);
	}

	@Override
	public void itemStateChanged(ItemEvent e) {
		if (e.getSource() instanceof JComboBox) {
			JComboBox cb = (JComboBox) e.getSource();
			String name = cb.getName();
			if (name.startsWith(ExtSimulationSettingsDialog.MonteCarloVarSourcePrefix)) {
				name = name.substring(ExtSimulationSettingsDialog.MonteCarloVarSourcePrefix.length());
				MonteCarloVarSource source = MonteCarloVarSource.valueOf(name);
				EnableDisableOption cbOpt = (EnableDisableOption) cb.getSelectedItem();
				setMonteCarloSettings(source, cbOpt == EnableDisableOption.Enabled);
//				System.out.println("Source " + source.name() + " set to " + cbOpt.name());
			}
		}
	}

	/**
	 * This method returns the minimum diameter (cm) under which the trees are not considered.
	 * @return a double
	 */
	public double getMinimumDiameterCm() {return minimumDiameterCm;}
	
	/**
	 * This method sets the minimum diameter (cm) under which the trees are not considered. It should be used only
	 * to change this diameter in the JUnit test. Otherwise the minimum diameter is set through the constructor.
	 * @param minimumDiameterCm
	 */
	protected void setMinimumDiameterCm(double minimumDiameterCm) {
		this.minimumDiameterCm = minimumDiameterCm;
	}
	
}



