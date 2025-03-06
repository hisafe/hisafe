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
package safe.extension.standviewer.safe;

import java.awt.Color;

import capsis.extension.standviewer.SVSimpleSettings;

/**
 * SVsafeSettings for viewer SVSafe
 *
 * @author Isabelle LECOMTE - August 2002
 */
public class SVSafeSettings extends SVSimpleSettings {
// review - fc - 11.3.2004

	public int selectMode;
	public boolean groupMode;
	public String groupName;

// display options
	public static final int CELL_OPTION = 1;	//DISPLAY THE CROP
	public static final int LIGHT_CHOICE = 1;	//Default option for light = PAR global relatif
	public static final int GROUND_CHOICE = 1;	//Default option for soil = water
	public static final int CROP_CHOICE = 1;	//Default option for crop = lai
	public int cellOption;
	public int lightChoice;
	public int groundChoice;
	public int cropChoice;

// automatic ajustment of color gradient with min and max values given by the model
	public static final boolean AUTO_AJUST = false;
	public boolean colorAutoAjust;

// ajustment of color gradient with min and max values given by the user
	public static final double LIGHT_VALUE_MIN = 0;
	public static final double LIGHT_VALUE_MAX = 100;

	public static final double WATER_VALUE_MIN = 0;
	public static final double NITROGEN_VALUE_MIN = 0;
	public static final double TREE_ROOT_VALUE_MIN = 0;
	public static final double CROP_ROOT_VALUE_MIN = 0;
	public static final double WATER_VALUE_MAX = 0.4;
	public static final double NITROGEN_VALUE_MAX = 4;
	public static final double TREE_ROOT_VALUE_MAX = 2500;
	public static final double CROP_ROOT_VALUE_MAX = 100;

	public static final double LAI_VALUE_MIN = 0;
	public static final double YIELD_VALUE_MIN = 0;
	public static final double TEMPERATURE_ROOT_VALUE_MIN = 0;
	public static final double ROOT_DEPTH_VALUE_MIN = 0;
	public static final double LAI_VALUE_MAX = 10;
	public static final double YIELD_VALUE_MAX = 50;
	public static final double TEMPERATURE_VALUE_MAX = 40;
	public static final double ROOT_DEPTH_VALUE_MAX = 2;

	public double lightValueMin;
	public double lightValueMax;
	public double waterValueMin;
	public double waterValueMax;
	public double nitrogenValueMin;
	public double nitrogenValueMax;
	public double treeRootValueMin;
	public double treeRootValueMax;
	public double cropRootValueMin;
	public double cropRootValueMax;
	public double laiValueMin;
	public double laiValueMax;
	public double yieldValueMin;
	public double yieldValueMax;
	public double temperatureValueMin;
	public double temperatureValueMax;
	public double rootDepthValueMin;
	public double rootDepthValueMax;


// color definitions
	public static final Color NOCROP = Color.white;
	public static final Color CROP = new Color(207,255,193);
	public Color cropColor;
	protected Color noCropColor;
	protected Color colorNull;

	public Color [] cropColors;							//colors for the crop
	public Color [] lightColor;							//colors for the light
	public Color [] waterColor;							//colors for the water
	public Color [] nitrogenColor;						//colors for the nitrogen
	public Color [] rootColor;							//colors for the roots


	/**	Constructor.
	*/
	public SVSafeSettings () {
		resetSettings ();

		colorNull=new Color(255,255,255);

		lightColor=new Color[15];
		waterColor=new Color[15];
		cropColors=new Color[15];
		nitrogenColor=new Color[15];
		rootColor=new Color[15];

		int inc = 0;
		for(int i=0;i<10;i++)
		{
			lightColor[i]=new Color(255,255-inc,0);
			inc=inc+25;
		}
		inc = 0;
		for(int i=10;i<15;i++)
		{
			lightColor[i]=new Color(240-inc,0,0);
			inc=inc+15;
		}


		waterColor[0]=new Color(0,11,106);
		waterColor[1]=new Color(0,16,150);
		waterColor[2]=new Color(0,19,177);
		waterColor[3]=new Color(0,21,201);
		waterColor[4]=new Color(0,23,222);
		waterColor[5]=new Color(0,26,241);
		waterColor[6]=new Color(35,57,254);
		waterColor[7]=new Color(63,82,254);
		waterColor[8]=new Color(82,99,254);
		waterColor[9]=new Color(103,118,254);
		waterColor[10]=new Color(115,125,254);
		waterColor[11]=new Color(129,141,254);
		waterColor[12]=new Color(139,151,254);
		waterColor[13]=new Color(152,163,254);
		waterColor[14]=new Color(179,186,254);

		cropColors[0]=new Color(12,53,0);
		cropColors[1]=new Color(19,85,0);
		cropColors[2]=new Color(26,113,0);
		cropColors[3]=new Color(31,136,0);
		cropColors[4]=new Color(33,151,0);
		cropColors[5]=new Color(39,174,0);
		cropColors[6]=new Color(44,191,0);
		cropColors[7]=new Color(48,215,0);
		cropColors[8]=new Color(52,232,0);
		cropColors[9]=new Color(56,251,0);
		cropColors[10]=new Color(97,255,51);
		cropColors[11]=new Color(126,255,89);
		cropColors[12]=new Color(154,255,125);
		cropColors[13]=new Color(184,255,164);
		cropColors[14]=new Color(207,255,193);

		nitrogenColor[0]=new Color(90,0,77);
		nitrogenColor[1]=new Color(131,0,112);
		nitrogenColor[2]=new Color(158,0,135);
		nitrogenColor[3]=new Color(192,0,164);
		nitrogenColor[4]=new Color(214,0,182);
		nitrogenColor[5]=new Color(233,0,198);
		nitrogenColor[6]=new Color(254,0,217);
		nitrogenColor[7]=new Color(254,16,218);
		nitrogenColor[8]=new Color(254,54,224);
		nitrogenColor[9]=new Color(254,90,230);
		nitrogenColor[10]=new Color(255,117,235);
		nitrogenColor[11]=new Color(255,132,237);
		nitrogenColor[12]=new Color(255,155,239);
		nitrogenColor[13]=new Color(255,174,243);
		nitrogenColor[14]=new Color(255,191,245);

		rootColor[0]=new Color(100,100,50);
		rootColor[1]=new Color(114,114,56);
		rootColor[2]=new Color(135,135,67);
		rootColor[3]=new Color(141,141,71);
		rootColor[4]=new Color(153,153,77);
		rootColor[5]=new Color(163,163,82);
		rootColor[6]=new Color(171,171,89);
		rootColor[7]=new Color(180,180,103);
		rootColor[8]=new Color(184,184,114);
		rootColor[9]=new Color(194,194,133);
		rootColor[10]=new Color(201,201,148);
		rootColor[11]=new Color(209,209,163);
		rootColor[12]=new Color(218,218,182);
		rootColor[13]=new Color(231,231,207);
		rootColor[14]=new Color(241,241,228);

	}


	/**	Reset method.
	*/
	public void resetSettings () {
		super.resetSettings ();
		cellOption = CELL_OPTION;
		cropColor = CROP;
		noCropColor = NOCROP;
		lightChoice = LIGHT_CHOICE;
		groundChoice = GROUND_CHOICE;
		cropChoice = CROP_CHOICE;

		colorAutoAjust = AUTO_AJUST;

		lightValueMin = LIGHT_VALUE_MIN;
		lightValueMax = LIGHT_VALUE_MAX;

		waterValueMin = WATER_VALUE_MIN;
		nitrogenValueMin = NITROGEN_VALUE_MIN;
		treeRootValueMin = TREE_ROOT_VALUE_MIN;
		cropRootValueMin = CROP_ROOT_VALUE_MIN;
		waterValueMax = WATER_VALUE_MAX;
		nitrogenValueMax = NITROGEN_VALUE_MAX;
		treeRootValueMax = TREE_ROOT_VALUE_MAX;
		cropRootValueMax = CROP_ROOT_VALUE_MAX;

		laiValueMin = LAI_VALUE_MIN;
		yieldValueMin = YIELD_VALUE_MIN;
		temperatureValueMin = TEMPERATURE_ROOT_VALUE_MIN;
		rootDepthValueMin = ROOT_DEPTH_VALUE_MIN;
		laiValueMax = LAI_VALUE_MAX;
		yieldValueMax = YIELD_VALUE_MAX;
		temperatureValueMax = TEMPERATURE_VALUE_MAX;
		rootDepthValueMax = ROOT_DEPTH_VALUE_MAX;

	}


	/*public String toString () {
		return " SVSafe settings = "
			+super.toString ()
			+" cellLines="+cellLines
			+" ascendingSort="+ascendingSort
			+" crownView="+crownView
			+" alphaValue="+alphaValue
			+" cellView="+cellView;
	}*/

}

