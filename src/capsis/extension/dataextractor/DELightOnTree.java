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

package capsis.extension.dataextractor;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

import capsis.util.Shiftable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * A light table that should be compatible with instance of samsaralight
 * @author G. Ligot - 30-06-2021
 */
public class DELightOnTree extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("quergus.extension.dataextractor.LightExtractor"); //TODO
	}

	protected Collection<String [][]> tables;
	protected Collection<String> titles;
	
	protected MethodProvider methodProvider;
	private GenericExtensionStarter starter;
	
	protected NumberFormat f;

	/**
	 * Phantom constructor.
	 * Only to ask for extension properties (authorName, version...).
	 */
	public DELightOnTree () {}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	public DELightOnTree (GenericExtensionStarter s) {

		this.starter = s;

		try {
			tables = new ArrayList ();
			titles = new ArrayList ();

			// Used to format decimal part with 2 digits only
			f = NumberFormat.getInstance (Locale.ENGLISH);
			f.setGroupingUsed (false);
			f.setMaximumFractionDigits (3);
			f.setMinimumFractionDigits (3);

		} catch (Exception e) {
			Log.println (Log.ERROR, "DELightOnTree.c ()", "Exception occured while object construction : ", e);
		}
	}

	@Override
	public void init(GModel m, Step s) throws Exception {
		super.init (m, s);
		titles = new ArrayList<String>();
		tables = new ArrayList<String [][]>();
		titles.add (Translator.swap ("DELightOnCellAndSensor")); 
	}
	

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
						


		} catch (Exception e) {
			Log.println (Log.ERROR, "DELightOnTree.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap ("DELightOnTree.name");
	}

	@Override
	public String getAuthor () {
		return "G. Ligot";
	}

	@Override
	public String getDescription () {
		return Translator.swap ("DELightOnTree.description");
	}

	@Override
	public String getVersion () {
		return "1";
	}

	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.DRTables";
	}

	/**
	 * From DataExtractor Interface.
	 * Effectively process the extraction.
	 * Should only be called if needed (time consuming).
	 * One solution is to trigger real extraction by data renderer 
	 * paintComponent (). So, the work will only be done when needed.
	 * Return false if trouble.
	 */
	public boolean doExtraction () {
		
		
		
		return true;
		
	}
	
	/** 
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties () {
//		String[] tab = {"TREE"};
//		addRadioProperty (tab);
	}
	
	/**
	 * From DFTables interface.
	 */
	public Collection getTables () {return tables;}
	public Collection getTitles () {return titles;}



}
