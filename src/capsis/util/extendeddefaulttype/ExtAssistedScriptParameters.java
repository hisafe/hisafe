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

import capsis.kernel.EvolutionParameters;


public class ExtAssistedScriptParameters {

	private ExtModel model;
	private EvolutionParameters oEvolutionParameters;
	private ExtExportTool exportTool;
	private boolean enableMultiThreading;


	public ExtAssistedScriptParameters(ExtModel model) {
		this.model = model;
	}
	
	public void setEvolutionParameters(EvolutionParameters evolParms) {oEvolutionParameters = evolParms;}
	public void setExportTool(ExtExportTool exportTool) {this.exportTool = exportTool;}
	public void setEnableMultiThreading(boolean enableMultiThreading) {this.enableMultiThreading = enableMultiThreading;}

	public ExtInitialParameters getInitialParameters() {return model.getSettings();}
	public ExtModel getModel() {return model;}
	public EvolutionParameters getEvolutionParameters() {return oEvolutionParameters;}
	public ExtExportTool getExportTool() {return exportTool;}
	
	public boolean isEnableMultiThreading() {return enableMultiThreading;}

	
}
