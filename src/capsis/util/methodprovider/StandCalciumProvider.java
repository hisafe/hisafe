/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;

/**
 * Stand calcium content in the trees
 *
 * @author P. Vallet - january 2007
 */
public interface StandCalciumProvider extends MethodProvider{

    /**
     * return the calcium content contained in the trees of the stand.
     * In Fagac�es, the table length is 5
     * table[0] -> calcium in stem > 7cm
     * table[1] -> calcium in branches > 7cm
     * table[2] -> calcium in branches between 4 and 7 cm
     * table[3] -> calcium in branches between 1 and 4 cm
     * table[4] -> calcium in branches between 0 and 1 cm
     */
    public double [] getStandCalcium (GScene stand, Collection trees);

}
