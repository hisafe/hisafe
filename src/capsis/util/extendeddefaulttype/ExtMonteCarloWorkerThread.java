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

/**
 * This class process the stand list in extended default type
 * @author Mathieu Fortin - March 2016
 */
public final class ExtMonteCarloWorkerThread implements Runnable {
	
	final Thread m_oThread;
	final ExtMultiThreadingStochasticModel m_oModel;
	final ExtCompositeStand m_oCurrentCompositeStand;
	final ExtCompositeStand m_oNewCompositeStand;
	final ExtEvolutionTask mainTask;
	final int m_iStartMC;
	final int m_iEndMC;
	double fStartTimeMs;
	double fEndTimeMs;
	Exception result;
	
	public ExtMonteCarloWorkerThread(ExtMultiThreadingStochasticModel oModel, 
			ExtCompositeStand oCurrentCompositeStand, 
			ExtCompositeStand oNewCompositeStand, 
			ExtEvolutionTask mainTask,
			int iStartMC, 
			int iEndMC) {
		m_oModel = oModel;
		m_oCurrentCompositeStand = oCurrentCompositeStand;
		m_oNewCompositeStand = oNewCompositeStand;
		this.mainTask = mainTask;
		m_iStartMC = iStartMC;
		m_iEndMC = iEndMC;
		
		m_oThread = new Thread(this, "MonteCarloWorker");
		m_oThread.start();
	}
	
	public void run() {
		fStartTimeMs = System.currentTimeMillis();
		try {
			m_oModel.processStandList(m_oCurrentCompositeStand, m_oNewCompositeStand, mainTask, m_iStartMC, m_iEndMC);
		} catch (Exception e) {
			result = e;
		}
		fEndTimeMs = System.currentTimeMillis();
	}
	
	public double getRunTimeMs() { return fEndTimeMs - fStartTimeMs; }
	public int getNumberOfIterations() { return m_iEndMC - m_iStartMC; }
	public boolean isAlive() {return m_oThread.isAlive();}
	public Exception getResult() {return result;}
}

